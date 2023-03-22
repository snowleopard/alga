{-# LANGUAGE LambdaCase #-}
----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Bipartite.AdjacencyMap.Algorithm
-- Copyright  : (c) Andrey Mokhov 2016-2023
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for
-- the motivation behind the library, the underlying theory, and
-- implementation details.
--
-- This module provides several basic algorithms on undirected bipartite graphs.
----------------------------------------------------------------------------
module Algebra.Graph.Bipartite.AdjacencyMap.Algorithm (
    -- * Bipartiteness test
    OddCycle, detectParts,

    -- * Matchings
    Matching, pairOfLeft, pairOfRight, matching, isMatchingOf, matchingSize,
    maxMatching,

    -- * Vertex covers
    VertexCover, isVertexCoverOf, vertexCoverSize, minVertexCover,

    -- * Independent sets
    IndependentSet, isIndependentSetOf, independentSetSize, maxIndependentSet,

    -- * Miscellaneous
    augmentingPath, consistentMatching
    ) where

import Algebra.Graph.Bipartite.AdjacencyMap

import Control.Monad             (guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (State, runState, get, put, modify)
import Control.Monad.ST          (ST, runST)
import Data.Either               (fromLeft)
import Data.Foldable             (asum, foldl')
import Data.Functor              (($>))
import Data.List                 (sort)
import Data.Maybe                (fromJust)
import Data.STRef                (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import GHC.Generics

import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Sequence   as Seq

import Data.Map.Strict (Map)
import Data.Set        (Set)
import Data.Sequence   (Seq, ViewL (..), (|>))

-- TODO: Make this representation type-safe
-- | A cycle of odd length. For example, @[1,2,3]@ represents the cycle
-- @1@ @->@ @2@ @->@ @3@ @->@ @1@.
type OddCycle a = [a]

data Part = LeftPart | RightPart deriving (Show, Eq)

otherPart :: Part -> Part
otherPart LeftPart  = RightPart
otherPart RightPart = LeftPart

-- | Test the bipartiteness of a given "Algebra.Graph.AdjacencyMap". In case of
-- success, return an 'AdjacencyMap' with the same set of edges and each vertex
-- marked with the part it belongs to. In case of failure, return any cycle of
-- odd length in the graph.
--
-- The returned partition is lexicographically smallest, assuming that vertices
-- of the left part precede all the vertices of the right part.
--
-- The returned cycle is optimal in the following sense: there exists a path
-- that is either empty or ends in a vertex adjacent to the first vertex in the
-- cycle, such that all vertices in @path@ @++@ @cycle@ are distinct and
-- @path@ @++@ @cycle@ is lexicographically smallest among all such pairs of
-- paths and cycles.
--
-- /Note/: since 'AdjacencyMap' represents /undirected/ bipartite graphs, all
-- edges in the input graph are treated as undirected. See the examples and the
-- correctness property for a clarification.
--
-- Complexity: /O((n + m) * log(n))/ time and /O(n + m)/ memory.
--
-- @
-- detectParts 'Algebra.Graph.AdjacencyMap.empty'                                       == Right 'empty'
-- detectParts ('Algebra.Graph.AdjacencyMap.vertex' x)                                  == Right ('leftVertex' x)
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' x x)                                  == Left [x]
-- detectParts ('Algebra.Graph.AdjacencyMap.edge' 1 2)                                  == Right ('edge' 1 2)
-- detectParts (1 * (2 + 3))                               == Right ('edges' [(1,2), (1,3)])
-- detectParts (1 * 2 * 3)                                 == Left [1, 2, 3]
-- detectParts ((1 + 3) * (2 + 4) + 6 * 5)                 == Right ('swap' (1 + 3) * (2 + 4) + 'swap' 5 * 6)
-- detectParts ((1 * 3 * 4) + 2 * (1 + 2))                 == Left [2]
-- detectParts ('Algebra.Graph.AdjacencyMap.clique' [1..10])                            == Left [1, 2, 3]
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..10])                           == Right ('circuit' [(x, x + 1) | x <- [1,3,5,7,9]])
-- detectParts ('Algebra.Graph.AdjacencyMap.circuit' [1..11])                           == Left [1..11]
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' [] xs)                            == Right ('vertices' xs [])
-- detectParts ('Algebra.Graph.AdjacencyMap.biclique' ('map' Left (x:xs)) ('map' Right ys)) == Right ('biclique' ('map' Left (x:xs)) ('map' Right ys))
-- 'isRight' (detectParts ('Algebra.Graph.AdjacencyMap.star' x ys))                       == 'notElem' x ys
-- 'isRight' (detectParts ('fromBipartite' ('toBipartite' x)))   == True
-- @
--
-- The correctness of 'detectParts' can be expressed by the following property:
--
-- @
-- let undirected = 'Algebra.Graph.AdjacencyMap.symmetricClosure' input in
-- case detectParts input of
--     Left cycle -> 'mod' (length cycle) 2 == 1 && 'Algebra.Graph.AdjacencyMap.isSubgraphOf' ('Algebra.Graph.AdjacencyMap.circuit' cycle) undirected
--     Right result -> 'Algebra.Graph.AdjacencyMap.gmap' 'Data.Either.Extra.fromEither' ('fromBipartite' result) == undirected
-- @
detectParts :: Ord a => AM.AdjacencyMap a -> Either (OddCycle a) (AdjacencyMap a a)
detectParts x = case runState (runMaybeT dfs) Map.empty of
    (Nothing, partMap) -> Right $ toBipartiteWith (toEither partMap) g
    (Just c , _      ) -> Left  $ oddCycle c
  where
    -- g :: AM.AdjacencyMap a
    g = AM.symmetricClosure x

    -- type PartMap a = Map a Part
    -- type PartMonad a = MaybeT (State (PartMap a)) [a]
    -- dfs :: PartMonad a
    dfs = asum [ processVertex v | v <- AM.vertexList g ]

    -- processVertex :: a -> PartMonad a
    processVertex v = do partMap <- lift get
                         guard (Map.notMember v partMap)
                         inVertex LeftPart v

    -- inVertex :: Part -> a -> PartMonad a
    inVertex vertexPart v = (v :) <$> do
        lift $ modify (Map.insert v vertexPart)
        let otherVertexPart = otherPart vertexPart
        asum [ onEdge otherVertexPart u | u <- Set.toAscList (AM.postSet v g) ]

    {-# INLINE onEdge #-}
    -- onEdge :: Part -> a -> PartMonad a
    onEdge vertexPart v = do partMap <- lift get
                             case Map.lookup v partMap of
                                 Nothing   -> inVertex vertexPart v
                                 Just part -> do guard (vertexPart /= part)
                                                 return [v] -- found a cycle!

    -- toEither :: PartMap a -> a -> Either a a
    toEither partMap v = case fromJust (Map.lookup v partMap) of
                             LeftPart  -> Left  v
                             RightPart -> Right v

    -- oddCycle :: [a] -> [a]
    oddCycle pathToCycle = init $ dropWhile (/= lastVertex) pathToCycle
      where
        lastVertex = last pathToCycle

-- | A /matching/ is a set of pairwise non-adjacent edges between the two parts
-- of a bipartite graph.
--
-- The 'Show' instance is defined using the 'matching' function, with the edges
-- listed in the ascending order of left vertices.
--
-- @
-- show ('matching' [])                 == "matching []"
-- show ('matching' [(2,\'a\'), (1,\'b\')]) == "matching [(1,\'b\'),(2,\'a\')]"
-- @
data Matching a b = Matching {
    -- | The map of vertices covered by the matching in the left part to their
    -- neighbours in the right part.
    -- Complexity: /O(1)/ time.
    --
    -- @
    -- pairOfLeft ('matching' [])                 == Map.'Data.Map.Strict.empty'
    -- pairOfLeft ('matching' [(2,\'a\'), (1,\'b\')]) == Map.'Data.Map.Strict.fromList' [(1,\'b\'), (2,\'a\')]
    -- Map.'Map.size' . pairOfLeft                    == Map.'Map.size' . pairOfRight
    -- @
    pairOfLeft  :: Map a b,

    -- | The map of vertices covered by the matching in the right part to their
    -- neighbours in the left part.
    -- Complexity: /O(1)/.
    --
    -- @
    -- pairOfRight ('matching' [])                 == Map.'Data.Map.Strict.empty'
    -- pairOfRight ('matching' [(2,\'a\'), (1,\'b\')]) == Map.'Data.Map.Strict.fromList' [(\'a\',2), (\'b\',1)]
    -- Map.'Map.size' . pairOfRight                    == Map.'Map.size' . pairOfLeft
    -- @
    pairOfRight :: Map b a
} deriving Generic

instance (Show a, Show b) => Show (Matching a b) where
    showsPrec _ m = showString "matching " . showList (Map.toAscList $ pairOfLeft m)

instance (Eq a, Eq b) => Eq (Matching a b) where
    x == y = pairOfLeft x == pairOfLeft y

instance (Ord a, Ord b) => Ord (Matching a b) where
    compare x y = compare (pairOfLeft x) (pairOfLeft y)

addEdgeUnsafe :: (Ord a, Ord b) => a -> b -> Matching a b -> Matching a b
addEdgeUnsafe a b (Matching ab ba) = Matching (Map.insert a b ab) (Map.insert b a ba)

addEdge :: (Ord a, Ord b) => a -> b -> Matching a b -> Matching a b
addEdge a b (Matching ab ba) = addEdgeUnsafe a b (Matching ab' ba')
    where
        ab' = case b `Map.lookup` ba of
                  Nothing -> Map.delete a ab
                  Just a' -> Map.delete a (Map.delete a' ab)
        ba' = case a `Map.lookup` ab of
                  Nothing -> Map.delete b ba
                  Just b' -> Map.delete b (Map.delete b' ba)

leftCovered :: Ord a => a -> Matching a b -> Bool
leftCovered a = Map.member a . pairOfLeft

-- | Construct a 'Matching' from a list of edges.
-- Complexity: /O(L * log(L))/ time, where /L/ is the length of the given list.
--
-- Edges that appear closer to the end of the list supersede all previous edges.
-- That is, if two edges from the list share a vertex, the one that appears
-- closer to the beginning is ignored.
--
-- @
-- 'pairOfLeft'  (matching [])                     == Map.'Data.Map.Strict.empty'
-- 'pairOfRight' (matching [])                     == Map.'Data.Map.Strict.empty'
-- 'pairOfLeft'  (matching [(2,\'a\'), (1,\'b\')])     == Map.'Data.Map.Strict.fromList' [(2,\'a\'), (1,\'b\')]
-- 'pairOfLeft'  (matching [(1,\'a\'), (1,\'b\')])     == Map.'Data.Map.Strict.singleton' 1 \'b\'
-- matching [(1,\'a\'), (1,\'b\'), (2,\'b\'), (2,\'a\')] == matching [(2,\'a\')]
-- @
matching :: (Ord a, Ord b) => [(a, b)] -> Matching a b
matching = foldl' (flip (uncurry addEdge)) (Matching Map.empty Map.empty)

-- | Check if a given 'Matching' is a valid /matching/ of a bipartite graph.
-- Complexity: /O(S * log(n))/, where /S/ is the size of the matching.
--
-- @
-- isMatchingOf ('matching' []) x               == True
-- isMatchingOf ('matching' xs) 'empty'           == 'null' xs
-- isMatchingOf ('matching' [(x,y)]) ('edge' x y) == True
-- isMatchingOf ('matching' [(1,2)]) ('edge' 2 1) == False
-- @
isMatchingOf :: (Ord a, Ord b) => Matching a b -> AdjacencyMap a b -> Bool
isMatchingOf m@(Matching ab _) g = consistentMatching m
    && and [ hasEdge a b g | (a, b) <- Map.toList ab ]

-- | The number of edges in a matching.
-- Complexity: /O(1)/ time.
--
-- @
-- matchingSize ('matching' [])                 == 0
-- matchingSize ('matching' [(2,\'a\'), (1,\'b\')]) == 2
-- matchingSize ('matching' [(1,\'a\'), (1,\'b\')]) == 1
-- matchingSize ('matching' xs)                 <= 'length' xs
-- matchingSize                               == Map.'Data.Map.Strict.size' . 'pairOfLeft'
-- @
matchingSize :: Matching a b -> Int
matchingSize = Map.size . pairOfLeft

-- | Find a /maximum matching/ in a bipartite graph. A matching is maximum if it
-- has the largest possible size.
-- Complexity: /O(m * sqrt(n) * log(n))/ time.
--
-- @
-- maxMatching 'empty'                                          == 'matching' []
-- maxMatching ('vertices' xs ys)                               == 'matching' []
-- maxMatching ('path' [1,2,3,4])                               == 'matching' [(1,2), (3,4)]
-- 'matchingSize' (maxMatching ('circuit' [(1,2), (3,4), (5,6)])) == 3
-- 'matchingSize' (maxMatching ('star' x (y:ys)))                 == 1
-- 'matchingSize' (maxMatching ('biclique' xs ys))                == 'min' ('length' ('Data.List.nub' xs)) ('length' ('Data.List.nub' ys))
-- 'isMatchingOf' (maxMatching x) x                             == True
-- @
maxMatching :: (Ord a, Ord b) => AdjacencyMap a b -> Matching a b
maxMatching graph = runST (maxMatchingHK graph)

-- TODO: Should we use a more efficient data structure for the queue?
-- TODO: We could try speeding this up by representing vertices with 'Int's.
-- The state maintained by the Hopcroft-Karp algorithm implemented below
data HKState s a b = HKState
    { distance    :: STRef s (Map a Int)
    , curMatching :: STRef s (Matching a b)
    , queue       :: STRef s (Seq a)
    , visited     :: STRef s (Set a) }

-- See https://en.wikipedia.org/wiki/Hopcroft-Karp_algorithm
maxMatchingHK :: forall a b s. (Ord a, Ord b) => AdjacencyMap a b -> ST s (Matching a b)
maxMatchingHK g = do
    distance    <- newSTRef Map.empty
    curMatching <- newSTRef (Matching Map.empty Map.empty)
    queue       <- newSTRef Seq.empty
    visited     <- newSTRef Set.empty
    runHK (HKState distance curMatching queue visited)
    readSTRef curMatching
  where
    runHK :: HKState s a b -> ST s ()
    runHK state = do writeSTRef (distance state) Map.empty
                     foundAugmentingPath <- bfs state
                     when foundAugmentingPath $ do
                         writeSTRef (visited state) Set.empty
                         dfs state
                         runHK state

    currentlyUncovered :: HKState s a b -> ST s [a]
    currentlyUncovered state = do
        m <- readSTRef (curMatching state)
        return [ v | v <- leftVertexList g, not (leftCovered v m) ]


    bfs :: HKState s a b -> ST s Bool
    bfs state = do
        uncovered <- currentlyUncovered state
        mapM_ (enqueue state 1) uncovered
        bfsLoop state

    enqueue :: HKState s a b -> Int -> a -> ST s ()
    enqueue state d v = do modifySTRef (distance state) (Map.insert v d)
                           modifySTRef (queue    state) (|> v)

    dequeue :: HKState s a b -> ST s (Maybe a)
    dequeue state = do q <- readSTRef (queue state)
                       case Seq.viewl q of
                           a :< q -> writeSTRef (queue state) q $> Just a
                           EmptyL -> return Nothing

    bfsLoop :: HKState s a b -> ST s Bool
    bfsLoop state = dequeue state >>= \case
                        Just v  -> do p <- bfsVertex state v
                                      q <- bfsLoop state
                                      return (p || q)
                        Nothing -> return False

    bfsVertex :: HKState s a b -> a -> ST s Bool
    bfsVertex state v = do dist <- readSTRef (distance state)
                           let d = fromJust (v `Map.lookup` dist) + 1
                           or <$> mapM (bfsEdge state d) (neighbours v)

    checkEnqueue :: HKState s a b -> Int -> a -> ST s ()
    checkEnqueue state d v = do dist <- readSTRef (distance state)
                                when (v `Map.notMember` dist) (enqueue state d v)

    bfsEdge :: HKState s a b -> Int -> b -> ST s Bool
    bfsEdge state d u = do m <- readSTRef (curMatching state)
                           case u `Map.lookup` pairOfRight m of
                               Just v  -> checkEnqueue state d v $> False
                               Nothing -> return True

    dfs :: HKState s a b -> ST s ()
    dfs state = currentlyUncovered state >>= mapM_ (dfsVertex state 0)

    dfsVertex :: HKState s a b -> Int -> a -> ST s Bool
    dfsVertex state d v = do dist <- readSTRef (distance state)
                             vis  <- readSTRef (visited state)
                             let dv = fromJust (v `Map.lookup` dist)
                             case (d + 1 == dv) && (v `Set.notMember` vis) of
                                 False -> return False
                                 True  -> do modifySTRef (visited state) (Set.insert v)
                                             dfsEdges state dv v (neighbours v)

    dfsEdges :: HKState s a b -> Int -> a -> [b] -> ST s Bool
    dfsEdges _     _ _ []     = return False
    dfsEdges state d a (b:bs) = do m <- readSTRef (curMatching state)
                                   case b `Map.lookup` pairOfRight m of
                                       Nothing -> addEdge state a b $> True
                                       Just w  -> dfsVertex state d w >>= \case
                                            True  -> addEdge state a b $> True
                                            False -> dfsEdges state d a bs

    addEdge :: HKState s a b -> a -> b -> ST s ()
    addEdge state a b = modifySTRef (curMatching state) (addEdgeUnsafe a b)

    neighbours :: a -> [b]
    neighbours a = Set.toAscList $ fromJust $ Map.lookup a $ leftAdjacencyMap g

-- | A /vertex cover/ of a bipartite graph.
--
-- A /vertex cover/ is a subset of vertices such that every edge is incident to
-- some vertex in the subset. We represent vertex covers by storing two sets of
-- vertices, one for each part. An equivalent representation, which is slightly
-- less memory efficient, is @Set@ @(Either@ @a@ @b)@.
type VertexCover a b = (Set a, Set b)

-- | Check if a given pair of sets is a /vertex cover/ of a bipartite graph.
-- Complexity: /O(m * log(n))/.
--
-- @
-- isVertexCoverOf (xs             , ys             ) 'empty'          == Set.'Set.null' xs && Set.'Set.null' ys
-- isVertexCoverOf (xs             , ys             ) ('leftVertex' x) == Set.'Set.isSubsetOf' xs (Set.'Set.singleton' x) && Set.'Set.null' ys
-- isVertexCoverOf (Set.'Set.empty'      , Set.'Set.empty'      ) ('edge' x y)     == False
-- isVertexCoverOf (Set.'Set.singleton' x, ys             ) ('edge' x y)     == Set.'Set.isSubsetOf' ys (Set.'Set.singleton' y)
-- isVertexCoverOf (xs             , Set.'Set.singleton' y) ('edge' x y)     == Set.'Set.isSubsetOf' xs (Set.'Set.singleton' x)
-- @
isVertexCoverOf :: (Ord a, Ord b) => (Set a, Set b) -> AdjacencyMap a b -> Bool
isVertexCoverOf (as, bs) g = as `Set.isSubsetOf` leftVertexSet g
    && bs `Set.isSubsetOf` rightVertexSet g
    && and [ a `Set.member` as || b `Set.member` bs | (a, b) <- edgeList g ]

-- | The number of vertices in a vertex cover.
-- Complexity: /O(1)/ time.
vertexCoverSize :: VertexCover a b -> Int
vertexCoverSize (as, bs) = Set.size as + Set.size bs

-- | Find a /minimum vertex cover/ in a bipartite graph. A vertex cover is
-- minimum if it has the smallest possible size.
-- Complexity: /O(m * sqrt(n) * log(n))/.
--
-- @
-- minVertexCover 'empty'                              == (Set.'Set.empty', Set.'Set.empty')
-- minVertexCover ('vertices' xs ys)                   == (Set.'Set.empty', Set.'Set.empty')
-- minVertexCover ('path' [1,2,3])                     == (Set.'Set.empty', Set.'Set.singleton' 2)
-- minVertexCover ('star' x (1:2:ys))                  == (Set.'Set.singleton' x, Set.'Set.empty')
-- 'vertexCoverSize' (minVertexCover ('biclique' xs ys)) == 'min' ('length' ('Data.List.nub' xs)) ('length' ('Data.List.nub' ys))
-- 'vertexCoverSize' . minVertexCover                  == 'matchingSize' . 'maxMatching'
-- 'isVertexCoverOf' (minVertexCover x) x              == True
-- @
minVertexCover :: (Ord a, Ord b) => AdjacencyMap a b -> VertexCover a b
minVertexCover g = fromLeft panic $ augmentingPath (maxMatching g) g
  where
    panic = error "minVertexCover: internal error (found augmenting path)"

-- | An /independent set/ of a bipartite graph.
--
-- An /independent set/ is a subset of vertices such that no two of them are
-- adjacent. We represent independent sets by storing two sets of vertices, one
-- for each part. An equivalent representation, which is slightly less memory
-- efficient, is @Set@ @(Either@ @a@ @b)@.
type IndependentSet a b = (Set a, Set b)

-- | Check if a given pair of sets is an /independent set/ of a bipartite graph.
-- Complexity: /O(m * log(n))/.
--
-- @
-- isIndependentSetOf (xs             , ys             ) 'empty'          == Set.'Set.null' xs && Set.'Set.null' ys
-- isIndependentSetOf (xs             , ys             ) ('leftVertex' x) == Set.'Set.isSubsetOf' xs (Set.'Set.singleton' x) && Set.'Set.null' ys
-- isIndependentSetOf (Set.'Set.empty'      , Set.'Set.empty'      ) ('edge' x y)     == True
-- isIndependentSetOf (Set.'Set.singleton' x, ys             ) ('edge' x y)     == Set.'Set.null' ys
-- isIndependentSetOf (xs             , Set.'Set.singleton' y) ('edge' x y)     == Set.'Set.null' xs
-- @
isIndependentSetOf :: (Ord a, Ord b) => (Set a, Set b) -> AdjacencyMap a b -> Bool
isIndependentSetOf (as, bs) g = as `Set.isSubsetOf` leftVertexSet g
    && bs `Set.isSubsetOf` rightVertexSet g
    && and [ not (a `Set.member` as && b `Set.member` bs) | (a, b) <- edgeList g ]

-- | The number of vertices in an independent set.
-- Complexity: /O(1)/ time.
independentSetSize :: IndependentSet a b -> Int
independentSetSize (as, bs) = Set.size as + Set.size bs

-- | Find a /maximum independent set/ in a bipartite graph. An independent set
-- is maximum if it has the largest possible size.
-- Complexity: /O(m * sqrt(n) * log(n))/.
--
-- @
-- maxIndependentSet 'empty'                                 == (Set.'Set.empty', Set.'Set.empty')
-- maxIndependentSet ('vertices' xs ys)                      == (Set.'Set.fromList' xs, Set.'Set.fromList' ys)
-- maxIndependentSet ('path' [1,2,3])                        == (Set.'Set.fromList' [1,3], Set.'Set.empty')
-- maxIndependentSet ('star' x (1:2:ys))                     == (Set.'Set.empty', Set.'Set.fromList' (1:2:ys))
-- 'independentSetSize' (maxIndependentSet ('biclique' xs ys)) == 'max' ('length' ('Data.List.nub' xs)) ('length' ('Data.List.nub' ys))
-- 'independentSetSize' (maxIndependentSet x)                == 'vertexCount' x - 'vertexCoverSize' ('minVertexCover' x)
-- 'isIndependentSetOf' (maxIndependentSet x) x              == True
-- @
maxIndependentSet :: (Ord a, Ord b) => AdjacencyMap a b -> IndependentSet a b
maxIndependentSet g =
    (leftVertexSet g `Set.difference` as, rightVertexSet g `Set.difference` bs)
  where
    (as, bs) = minVertexCover g

-- | Given a matching in a bipartite graph, find either a /vertex cover/ of the
-- same size or an /augmenting path/ with respect to the matching, thereby
-- demonstrating that the matching is not maximum.
-- Complexity: /O((m + n) * log(n))/.
--
-- An /alternating path/ is a path whose edges belong alternately to the
-- matching and not to the matching. An /augmenting path/ is an alternating path
-- that starts from and ends on the vertices that are not covered by the
-- matching. A matching is maximum if and only if there is no augmenting path
-- with respect to it.
--
-- @
-- augmentingPath ('matching' [])      'empty'            == Left (Set.'Set.empty', Set.'Set.empty')
-- augmentingPath ('matching' [])      ('edge' 1 2)       == Right [1,2]
-- augmentingPath ('matching' [(1,2)]) ('path' [1,2,3])   == Left (Set.'Set.empty', Set.'Set.singleton' 2)
-- augmentingPath ('matching' [(3,2)]) ('path' [1,2,3,4]) == Right [1,2,3,4]
-- isLeft (augmentingPath ('maxMatching' x) x)          == True
-- @
augmentingPath :: (Ord a, Ord b) => Matching a b -> AdjacencyMap a b -> Either (VertexCover a b) (List a b)
augmentingPath = augmentingPathImpl

type AugPathMonad a b = MaybeT (State (VertexCover a b)) (List a b)

-- The implementation is in a separate function to avoid the "forall" in docs.
augmentingPathImpl :: forall a b. (Ord a, Ord b) => Matching a b -> AdjacencyMap a b -> Either (VertexCover a b) (List a b)
augmentingPathImpl m g = case runState (runMaybeT dfs) (leftVertexSet g, Set.empty) of
    (Nothing  , cover) -> Left cover
    (Just path, _    ) -> Right path
  where
    dfs :: AugPathMonad a b
    dfs = asum [ inVertex v | v <- leftVertexList g, not (leftCovered v m) ]

    inVertex :: a -> AugPathMonad a b
    inVertex a = do (as, bs) <- lift get
                    guard (a `Set.member` as)
                    lift $ put (Set.delete a as, bs)
                    asum [ onEdge a b | b <- neighbours a ]

    onEdge :: a -> b -> AugPathMonad a b
    onEdge a b = addEdge a b <$> do (as, bs) <- lift get
                                    lift $ put (as, Set.insert b bs)
                                    case b `Map.lookup` pairOfRight m of
                                        Just a  -> inVertex a
                                        Nothing -> return Nil

    addEdge :: a -> b -> List a b -> List a b
    addEdge a b = Cons a . Cons b

    neighbours :: a -> [b]
    neighbours a = Set.toAscList $ fromJust $ Map.lookup a $ leftAdjacencyMap g

-- | Check if the internal representation of a matching is consistent, i.e. that
-- every edge that is present in 'pairOfLeft' is also present in 'pairOfRight'.
-- Complexity: /O(S * log(S))/, where /S/ is the size of the matching.
--
-- @
-- consistentMatching ('matching' xs)   == True
-- consistentMatching ('maxMatching' x) == True
-- @
consistentMatching :: (Ord a, Ord b) => Matching a b -> Bool
consistentMatching (Matching ab ba) =
    Map.toAscList ab == sort [ (a, b) | (b, a) <- Map.toAscList ba ]
