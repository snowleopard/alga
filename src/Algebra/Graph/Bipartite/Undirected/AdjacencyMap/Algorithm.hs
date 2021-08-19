{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Algebra.Graph.Bipartite.Undirected.AdjacencyMap.Algorithm (
    -- * Maximum matchings
    Matching, pairOfLeft, pairOfRight, matching, swapMatching, matchingSize,
    consistentMatching, VertexCover, IndependentSet, maxMatching,
    minVertexCover, maxIndependentSet, augmentingPath,
    ) where

import Algebra.Graph.Bipartite.Undirected.AdjacencyMap

import Control.Monad             (guard, when)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State       (MonadState(..), State, runState, modify)
import Control.Monad.ST          (ST, runST)
import Data.Foldable             (asum)
import Data.List                 (sort)
import Data.Maybe                (fromJust)
import Data.STRef                (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import GHC.Generics

import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.Sequence   as Seq

data Part = LeftPart | RightPart
    deriving (Show, Eq)

otherPart :: Part -> Part
otherPart LeftPart  = RightPart
otherPart RightPart = LeftPart

type PartMap a = Map.Map a Part
type PartMonad a = MaybeT (State (PartMap a)) [a]

neighbours :: Ord a => a -> AM.AdjacencyMap a -> [a]
neighbours v = Set.toAscList . AM.postSet v

-- | A /matching/ of vertices of two parts.
--
-- The 'Show' instance is defined using the 'matching' function. The edges in
-- the argument are shown in ascending order of left vertices.
--
-- @
-- show ('matching' [])                   == "matching []"
-- show ('matching' [(3, "a"), (1, "b")]) == "matching [(1,\\"b\\"),(3,\\"a\\")]"
-- @
data Matching a b = Matching {
    -- | Map of covered vertices of the left part into their neighbours.
    -- Complexity: /O(1)/.
    --
    -- @
    -- pairOfLeft ('matching' [])                   == Map.'Data.Map.Strict.empty'
    -- pairOfLeft ('matching' [(3, "a"), (1, "b")]) == Map.'Data.Map.Strict.fromList' [(3, "a"), (1, "b")]
    -- @
    pairOfLeft  :: Map.Map a b,

    -- | Map of covered vertices of the right part into their neighbours.
    -- Complexity: /O(1)/.
    --
    -- @
    -- pairOfRight ('matching' [])                  == Map.'Data.Map.Strict.empty'
    -- pairOfRight ('matching' [(3, "a"), (1, "b")] == Map.'Data.Map.Strict.fromList' [("a", 3), ("b", 1)]
    -- @
    pairOfRight :: Map.Map b a
} deriving Generic

instance (Show a, Show b) => Show (Matching a b) where
    showsPrec _ m = showString "matching " . showList (Map.toAscList $ pairOfLeft m)

instance (Eq a, Eq b) => Eq (Matching a b) where
    (==) m n = (==) (pairOfLeft m) (pairOfLeft n)

addEdgeUnsafe :: (Ord a, Ord b) => a -> b -> Matching a b -> Matching a b
addEdgeUnsafe u v (Matching lr rl) = Matching (Map.insert u v lr) (Map.insert v u rl)

addEdge :: (Ord a, Ord b) => a -> b -> Matching a b -> Matching a b
addEdge u v (Matching lr rl) = addEdgeUnsafe u v (Matching lr' rl')
    where
        lr' = case v `Map.lookup` rl of
                   Nothing -> Map.delete u lr
                   Just w  -> Map.delete u (Map.delete w lr)
        rl' = case u `Map.lookup` lr of
                   Nothing -> Map.delete v rl
                   Just w  -> Map.delete v (Map.delete w rl)

leftCovered :: Ord a => a -> Matching a b -> Bool
leftCovered v = Map.member v . pairOfLeft

-- | Construct a matching from given list of edges.
-- Complexity: /O(L log(L))/, where /L/ is the length of the given list.
--
-- Edges that appear on the list closer to the end of the list overwrite
-- previous edges. That is, if two edges from the list share a vertex, one
-- that appears closer to the beginning is ignored.
--
-- @
-- 'pairOfLeft'  (matching [])                  == Map.'Data.Map.Strict.empty'
-- 'pairOfRight' (matching [])                  == Map.'Data.Map.Strict.empty'
-- 'pairOfLeft'  (matching [(3,"a"),(1,"b")])   == Map.'Data.Map.Strict.fromList' [(3,"a"),(1,"b")]
-- 'pairOfLeft'  (matching [(1,"a"),(1,"b")])   == Map.'Data.Map.Strict.singleton' 1 "b"
-- matching [(1,"a"),(1,"b"),(2,"b"),(2,"a")] == matching [(2,"a")]
-- @
matching :: (Ord a, Ord b) => [(a, b)] -> Matching a b
matching = foldl (flip (uncurry addEdge)) (Matching Map.empty Map.empty)

-- | Swap parts of the vertices in the matching.
-- Complexity: /O(1)/.
--
-- @
-- swapMatching ('matching' [])                == 'matching' []
-- swapMatching ('matching' [(3,"a"),(1,"b")]) == 'matching' [("a",3),("b",1)]
-- swapMatching . 'matching'                   == 'matching' . map 'Data.Tuple.swap'
-- @
swapMatching :: Matching a b -> Matching b a
swapMatching (Matching lr rl) = Matching rl lr

-- | Compute the number of edges in matching.
-- Complexity: /O(1)/.
--
-- @
-- matchingSize ('matching' [])                == 0
-- matchingSize ('matching' [(3,"a"),(1,"b")]) == 2
-- matchingSize ('matching' [(1,"a"),(1,"b")]) == 1
-- matchingSize ('matching' xs)                <= 'length' xs
-- matchingSize                              == Map.'Data.Map.Strict.size' . 'pairOfLeft'
-- @
matchingSize :: Matching a b -> Int
matchingSize = Map.size . pairOfLeft

-- | Check if the internal matching representation of matching is consistent,
-- i.e. that every edge that is present in 'pairOfLeft' is present in
-- 'pairOfRight'.
-- Complexity: /O(S log(S))/, where /S/ is the size of the matching.
--
-- @
-- consistent (matching xs) == True
-- @
consistentMatching :: (Ord a, Ord b) => Matching a b -> Bool
consistentMatching (Matching lr rl) = lrl == sort rll
    where
        lrl = Map.toAscList lr
        rll = [ (v, u) | (u, v) <- Map.toAscList rl ]

-- | A /vertex cover/ in a bipartite graph, represented by list of vertices.
--
-- Vertex cover is such subset of vertices that every edge is incident to some
-- vertex from it.
type VertexCover a b = [Either a b] -- TODO: Maybe set?

-- | An /independent set/ in a bipartite graph, represented by list of vertices.
--
-- A subset of vertices is independent if it contains no pair of adjacent
-- vertices.
type IndependentSet a b = [Either a b] -- TODO: Maybe set?

data HKState s a b = HKS {
    distance    :: STRef s (Map.Map a Int),
    curMatching :: STRef s (Matching a b),
    queue       :: STRef s (Seq.Seq a),
    visited     :: STRef s (Set.Set a)
}

-- | Find a /maximum mathcing/ in bipartite graph. A matching is maximum if it
-- has maximum possible size.
-- Complexity: /O(m sqrt(n) log(n))/
--
-- @
-- maxMatching 'empty'                                          == 'matching' []
-- maxMatching ('vertices' xs ys)                               == 'matching' []
-- maxMatching ('path' [1,2,3,4])                               == 'matching' [(1,2),(3,4)]
-- 'matchingSize' (maxMatching ('circuit' [(1,2),(3,4),(5,6)])) == 3
-- 'matchingSize' (maxMatching ('star' x (y:ys)))               == 1
-- 'matchingSize' (maxMatching ('biclique' xs ys))              == 'min' ('length' ('nub' xs)) ('length' ('nub' ys))
-- @
maxMatching :: forall a b. (Ord a, Ord b, Show a, Show b) => AdjacencyMap a b -> Matching a b
maxMatching g = runST $ do dist <- newSTRef Map.empty
                           m    <- newSTRef (Matching Map.empty Map.empty)
                           q    <- newSTRef Seq.empty
                           vis  <- newSTRef Set.empty
                           let state = HKS dist m q vis
                           runHK state
                           readSTRef m
    where
        dequeue :: HKState s a b -> ST s (Maybe a)
        dequeue state = do q <- readSTRef (queue state)
                           case Seq.viewl q of
                                a Seq.:< q' -> do writeSTRef (queue state) q'
                                                  return (Just a)
                                Seq.EmptyL  -> return Nothing

        enqueue :: HKState s a b -> Int -> a -> ST s ()
        enqueue state d v = do modifySTRef (distance state) (Map.insert v d)
                               modifySTRef (queue state)    (Seq.|> v)

        checkEnqueue :: HKState s a b -> Int -> a -> ST s ()
        checkEnqueue state d v = do dist <- readSTRef (distance state)
                                    let action = enqueue state d v
                                    when (v `Map.notMember` dist) action

        bfsEdge :: HKState s a b -> Int -> b -> ST s Bool
        bfsEdge state d u = do m <- readSTRef (curMatching state)
                               case u `Map.lookup` pairOfRight m of
                                    Just v  -> False <$ checkEnqueue state d v
                                    Nothing -> return True

        bfsVertex :: HKState s a b -> a -> ST s Bool
        bfsVertex state v = do dist <- readSTRef (distance state)
                               let d = fromJust (v `Map.lookup` dist) + 1
                               or <$> mapM (bfsEdge state d) (neighbours v)

        bfsLoop :: HKState s a b -> ST s Bool
        bfsLoop state = do mv <- dequeue state
                           case mv of
                                Just v  -> do p <- bfsVertex state v
                                              q <- bfsLoop state
                                              return (p || q)
                                Nothing -> return False

        bfs :: HKState s a b -> ST s Bool
        bfs state = do m <- readSTRef (curMatching state)
                       let uncovered = [ v | v <- leftVertexList g
                                           , not (leftCovered v m) ]
                       mapM_ (enqueue state 1) uncovered
                       bfsLoop state

        dfsEdges :: HKState s a b -> Int -> a -> [b] -> ST s Bool
        dfsEdges _     _ _ []     = return False
        dfsEdges state d v (u:us) = do m <- readSTRef (curMatching state)
                                       case u `Map.lookup` pairOfRight m of
                                            Nothing -> True <$ addEdge state v u
                                            Just w  -> do z <- dfsVertex state d w
                                                          case z of
                                                               True  -> True <$ addEdge state v u
                                                               False -> dfsEdges state d v us

        dfsVertex :: HKState s a b -> Int -> a -> ST s Bool
        dfsVertex state d v = do dist <- readSTRef (distance state)
                                 vis  <- readSTRef (visited state)
                                 let dv = fromJust (v `Map.lookup` dist)
                                 case (d + 1 == dv) && (v `Set.notMember` vis) of
                                      False -> return False
                                      True  -> do modifySTRef (visited state) (Set.insert v)
                                                  dfsEdges state dv v (neighbours v)

        addEdge :: HKState s a b -> a -> b -> ST s ()
        addEdge state v u = modifySTRef (curMatching state) (addEdgeUnsafe v u)

        dfs :: HKState s a b -> ST s ()
        dfs state = do m <- readSTRef (curMatching state)
                       let uncovered = [ v | v <- leftVertexList g
                                           , not (leftCovered v m) ]
                       mapM_ (dfsVertex state 0) uncovered

        runHK :: HKState s a b -> ST s ()
        runHK state = do writeSTRef (distance state) Map.empty
                         run <- bfs state
                         dist <- readSTRef (distance state)
                         when run $ do writeSTRef (visited state) Set.empty
                                       dfs state
                                       m <- readSTRef (curMatching state)
                                       runHK state

        neighbours :: a -> [b]
        neighbours v = Set.toAscList $ fromJust $ Map.lookup v $ leftAdjacencyMap g

-- | Find a /vertex cover/ of minimum possible size in bipartite graph.
-- Vertices in the returned list are sorted and unique.
-- Complexity: /O(m sqrt(n) log(n))/
--
-- @
-- minVertexCover 'empty'                     == []
-- minVertexCover ('vertices' xs ys)          == []
-- minVertexCover ('path' [1,2,3])            == [Right 2]
-- minVertexCover ('star' x (y:ys))           == [Left x]
-- 'length' (minVertexCover ('biclique' xs ys)) == 'min' ('length' ('nub' xs)) ('length' ('nub' ys))
-- 'length' . minVertexCover                  == 'matchingSize' . 'maxMatching'
-- @
minVertexCover :: (Ord a, Ord b, Show a, Show b) => AdjacencyMap a b -> VertexCover a b
minVertexCover g = fromLeft [] (augmentingPath (maxMatching g) g)
    where
        fromLeft :: a -> Either a b -> a
        fromLeft _ (Left  x) = x
        fromLeft x (Right _) = x

-- | Find an /independent set/ of maximum possible size in bipartite graph.
-- Vertices in the returned list are sorted and unique.
-- Complexity: /O(m sqrt(n) log(n))/
--
-- @
-- maxIndependentSet 'empty'                     == []
-- maxIndependentSet ('vertices' xs ys)          == [ Left  x | x <- 'Data.List.nub' ('Data.List.sort' xs) ]
--                                             ++ [ Right y | y <- 'Data.List.nub' ('Data.List.sort' ys) ]
-- maxIndependentSet ('path' [1,2,3])            == [Left 1,Left 3]
-- maxIndependentSet ('star' x (y:z:ys))         == [ Right w | w <- y:z:ys ]
-- 'length' (maxIndependentSet ('biclique' xs ys)) == 'max' ('length' ('nub' xs)) ('length' ('nub' ys))
-- 'length' (maxIndependentSet x)                == vertexCount x - length (minVertexCover x)
-- @
maxIndependentSet :: (Ord a, Ord b, Show a, Show b) => AdjacencyMap a b -> IndependentSet a b
maxIndependentSet g = Set.toAscList (vertexSet g `Set.difference` vc)
    where
        vc = Set.fromAscList (minVertexCover g)

type AugPathMonad a b = MaybeT (State (Set.Set a, Set.Set b)) (List a b)

-- | Given a matching in a graph, find either a /vertex cover/ of the same size
-- or an /augmeting path/ with respect to the given matching.
-- Complexity: /O((m + n) log(n))/
--
-- A path is /alternating/ with respect to a matching if its edges from the
-- matching are alternating with edges not from the matching. An alternating
-- path is augmenting if it starts and ends in vertices that are uncovered by
-- the matching.
--
-- @
-- augmentingPath ('matching' [])      'empty'            == Left []
-- augmentingPath ('matching' [])      ('edge' 1 2)       == Right [1,2]
-- augmentingPath ('matching' [(1,2)]) ('path' [1,2,3])   == Left [Right 2]
-- augmentingPath ('matching' [(3,2)]) ('path' [1,2,3,4]) == Right [1,2,3,4]
-- isLeft (augmentingPath ('maxMatching' x) x)          == True
-- @
augmentingPath :: forall a b. (Ord a, Ord b, Show a, Show b) =>
                  Matching a b -> AdjacencyMap a b -> Either (VertexCover a b) (List a b)
augmentingPath m g = case runState (runMaybeT dfs) (leftVertexSet g, Set.empty) of
                          (Nothing, (s, t)) -> Left $ map Left  (Set.toAscList s) ++
                                                      map Right (Set.toAscList t)
                          (Just l,  _)      -> Right l
    where
        inVertex :: a -> AugPathMonad a b
        inVertex u = do (s, t) <- get
                        guard (u `Set.member` s)
                        put (Set.delete u s, t)
                        asum [ onEdge u v | v <- neighbours u ]

        onEdge :: a -> b -> AugPathMonad a b
        onEdge u v = add u v <$> do (s, t) <- get
                                    put (s, Set.insert v t)
                                    case v `Map.lookup` pairOfRight m of
                                         Just w  -> inVertex w
                                         Nothing -> return Nil

        add :: a -> b -> List a b -> List a b
        add u v = Cons u . Cons v

        dfs :: AugPathMonad a b
        dfs = asum [ inVertex v | v <- leftVertexList g, not (leftCovered v m) ]

        neighbours :: a -> [b]
        neighbours v = Set.toAscList $ fromJust $ Map.lookup v $ leftAdjacencyMap g
