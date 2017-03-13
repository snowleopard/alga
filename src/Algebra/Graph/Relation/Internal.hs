-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Relation.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of binary relations. The API is unstable
-- and unsafe. Where possible use non-internal modules "Algebra.Graph.Relation",
-- "Algebra.Graph.Relation.Reflexive", "Algebra.Graph.Relation.Symmetric",
-- "Algebra.Graph.Relation.Transitive" and "Algebra.Graph.Relation.Preorder"
-- instead.
--
-----------------------------------------------------------------------------
module Algebra.Graph.Relation.Internal (
    -- * Binary relations
    Relation (..), consistent,

    -- * Operations on binary relations
    preset, postset, reflexiveClosure, symmetricClosure, transitiveClosure,
    preorderClosure, gmap, edgeList, edges, fromAdjacencyList,

    -- * Reflexive relations
    ReflexiveRelation (..),

    -- * Symmetric relations
    SymmetricRelation (..),

    -- * Transitive relations
    TransitiveRelation (..),

    -- * Preorders
    PreorderRelation (..)
  ) where

import Data.Tuple
import Data.Set (Set, union)

import Algebra.Graph.Classes

import qualified Data.Set as Set

-- | The 'Relation' data type represents a binary relation over a set of elements.
-- The 'Show' instance is defined using basic graph construction primitives:
--
-- @
-- show ('empty'     :: Relation Int) == "empty"
-- show (1         :: Relation Int) == "vertex 1"
-- show (1 + 2     :: Relation Int) == "vertices [1,2]"
-- show (1 * 2     :: Relation Int) == "edge 1 2"
-- show (1 * 2 * 3 :: Relation Int) == "edges [(1,2),(1,3),(2,3)]"
-- show (1 * 2 + 3 :: Relation Int) == "graph [1,2,3] [(1,2)]"
-- @
data Relation a = Relation {
    -- | The /domain/ of the relation.
    domain :: Set a,
    -- | The set of pairs of elements that are /related/. It is guaranteed that
    -- each element belongs to the domain.
    relation :: Set (a, a)
  } deriving Eq

instance (Ord a, Show a) => Show (Relation a) where
    show (Relation d r)
        | vs == []     = "empty"
        | es == []     = if Set.size d > 1 then "vertices " ++ show vs
                                           else "vertex "   ++ show v
        | d == related = if Set.size r > 1 then "edges " ++ show es
                                           else "edge "  ++ show e ++ " " ++ show f
        | otherwise    = "graph " ++ show vs ++ " " ++ show es
      where
        vs      = Set.toAscList d
        es      = Set.toAscList r
        v       = head $ Set.toAscList d
        (e, f)  = head $ Set.toAscList r
        related = Set.fromList . uncurry (++) $ unzip es

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation Set.empty Set.empty
    vertex  x   = Relation (Set.singleton x) Set.empty
    overlay x y = Relation (domain x `union` domain y) (relation x `union` relation y)
    connect x y = Relation (domain x `union` domain y) (relation x `union` relation y
        `union` (domain x >< domain y))

(><) :: Set a -> Set a -> Set (a, a)
x >< y = Set.fromDistinctAscList [ (a, b) | a <- Set.elems x, b <- Set.elems y ]

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

-- | Check if the internal representation of a relation is consistent, i.e. if all
-- pairs of elements in the 'relation' refer to existing elements in the 'domain'.
-- It should be impossible to create an inconsistent 'Relation', and we use this
-- function in testing.
--
-- @
-- consistent 'empty'                  == True
-- consistent ('vertex' x)             == True
-- consistent ('overlay' x y)          == True
-- consistent ('connect' x y)          == True
-- consistent ('Algebra.Graph.edge' x y)             == True
-- consistent ('edges' xs)             == True
-- consistent ('Algebra.Graph.graph' xs ys)          == True
-- consistent ('fromAdjacencyList' xs) == True
-- @
consistent :: Ord a => Relation a -> Bool
consistent r = Set.fromList (uncurry (++) $ unzip $ edgeList r)
    `Set.isSubsetOf` (domain r)

-- | Transform a given relation by applying a function to each of its elements.
-- This is similar to @Functor@'s 'fmap' but can be used with non-fully-parametric
-- 'Relation'.
--
-- @
-- gmap f 'empty'      == 'empty'
-- gmap f ('vertex' x) == 'vertex' (f x)
-- gmap f ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edge' (f x) (f x)
-- gmap id           == id
-- gmap f . gmap g   == gmap (f . g)
-- @
gmap :: (Ord a, Ord b) => (a -> b) -> Relation a -> Relation b
gmap f (Relation d r) = Relation (Set.map f d) (Set.map (\(x, y) -> (f x, f y)) r)

-- | The /preset/ of an element @x@ is the set of elements that are related to
-- it on the /left/, i.e. @preset x == { a | aRx }@. In the context of directed
-- graphs, this corresponds to the set of /direct predecessors/ of vertex @x@.
--
-- @
-- preset x 'empty'      == Set.empty
-- preset x ('vertex' x) == Set.empty
-- preset x ('Algebra.Graph.edge' x y) == Set.empty
-- preset y ('Algebra.Graph.edge' x y) == Set.fromList [x]
-- @
preset :: Ord a => a -> Relation a -> Set a
preset x = Set.mapMonotonic fst . Set.filter ((== x) . snd) . relation

-- | The /postset/ of an element @x@ is the set of elements that are related to
-- it on the /right/, i.e. @postset x == { a | xRa }@. In the context of directed
-- graphs, this corresponds to the set of /direct successors/ of vertex @x@.
--
-- @
-- postset x 'empty'      == Set.empty
-- postset x ('vertex' x) == Set.empty
-- postset x ('Algebra.Graph.edge' x y) == Set.fromList [y]
-- postset y ('Algebra.Graph.edge' x y) == Set.empty
-- @
postset :: Ord a => a -> Relation a -> Set a
postset x = Set.mapMonotonic snd . Set.filter ((== x) . fst) . relation

-- | Extract the list of related pairs of elements in a relation.
--
-- @
-- edgeList 'empty'          == []
-- edgeList ('vertex' x)     == []
-- edgeList ('Algebra.Graph.edge' x y)     == [(x,y)]
-- edgeList ('Algebra.Graph.star' 2 [1,3]) == [(2,1), (2,3)]
-- edgeList . 'edges'        == 'Data.List.nub' . 'Data.List.sort'
-- @
edgeList :: Ord a => Relation a -> [(a, a)]
edgeList = Set.toAscList . relation

-- | Construct a relation from a list of related pairs of elements.
--
-- @
-- edges []         == 'empty'
-- edges [(x, y)]   == 'Algebra.Graph.edge' x y
-- 'edgeList' . edges == 'Data.List.nub' . 'Data.List.sort'
-- @
edges :: Ord a => [(a, a)] -> Relation a
edges es = Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = uncurry (++) $ unzip es

-- | Construct a relation from the /adjacency list/ of a graph.
--
-- @
-- fromAdjacencyList []                                  == 'empty'
-- fromAdjacencyList [(x, [])]                           == 'vertex' x
-- fromAdjacencyList [(x, [y])]                          == 'Algebra.Graph.edge' x y
-- 'overlay' (fromAdjacencyList xs) (fromAdjacencyList ys) == fromAdjacencyList (xs ++ ys)
-- @
fromAdjacencyList :: Ord a => [(a, [a])] -> Relation a
fromAdjacencyList as = Relation (Set.fromList vs) (Set.fromList es)
  where
    vs = concatMap (\(x, ys) -> x : ys) as
    es = [ (x, y) | (x, ys) <- as, y <- ys ]

-- | Compute the /reflexive closure/ of a 'Relation'.
--
-- @
-- reflexiveClosure 'empty'      == 'empty'
-- reflexiveClosure ('vertex' x) == 'Algebra.Graph.edge' x x
-- @
reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure (Relation d r) =
    Relation d $ r `union` Set.fromDistinctAscList [ (a, a) | a <- Set.elems d ]

-- | Compute the /symmetric closure/ of a 'Relation'.
--
-- @
-- symmetricClosure 'empty'      == 'empty'
-- symmetricClosure ('vertex' x) == 'vertex' x
-- symmetricClosure ('Algebra.Graph.edge' x y) == 'Algebra.Graph.edges' [(x, y), (y, x)]
-- @
symmetricClosure :: Ord a => Relation a -> Relation a
symmetricClosure (Relation d r) = Relation d $ r `union` (Set.map swap r)

-- | Compute the /transitive closure/ of a 'Relation'.
--
-- @
-- transitiveClosure 'empty'      == 'empty'
-- transitiveClosure ('vertex' x) == 'vertex' x
-- transitiveClosure ('Algebra.Graph.path' xs)  == 'Algebra.Graph.clique' xs
-- @
transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure old@(Relation d r)
    | r == newR = old
    | otherwise = transitiveClosure $ Relation d newR
  where
    newR = Set.unions $ r : [ preset x old >< postset x old | x <- Set.elems d ]

-- TODO: Optimise the implementation by caching the results of reflexive closure.
-- | The 'ReflexiveRelation' data type represents a binary reflexive relation
-- over a set of elements.
--
-- @
-- show (1     :: ReflexiveRelation Int) == "edge 1 1"
-- show (1 * 2 :: ReflexiveRelation Int) == "edges [(1,1),(1,2),(2,2)]"
-- @
newtype ReflexiveRelation a = ReflexiveRelation { fromReflexive :: Relation a }
    deriving Num

instance Ord a => Eq (ReflexiveRelation a) where
    x == y = reflexiveClosure (fromReflexive x) == reflexiveClosure (fromReflexive y)

instance (Ord a, Show a) => Show (ReflexiveRelation a) where
    show = show . reflexiveClosure . fromReflexive

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (ReflexiveRelation a) where
    type Vertex (ReflexiveRelation a) = a
    empty       = ReflexiveRelation empty
    vertex      = ReflexiveRelation . vertex
    overlay x y = ReflexiveRelation $ fromReflexive x `overlay` fromReflexive y
    connect x y = ReflexiveRelation $ fromReflexive x `connect` fromReflexive y

instance Ord a => Reflexive (ReflexiveRelation a)

-- TODO: Optimise the implementation by caching the results of symmetric closure.
-- | The 'SymmetricRelation' data type represents a binary symmetric relation
-- over a set of elements.
--
-- @
-- show (1     :: SymmetricRelation Int) == "vertex 1"
-- show (1 * 2 :: SymmetricRelation Int) == "edges [(1,2),(2,1)]"
-- @
newtype SymmetricRelation a = SymmetricRelation { fromSymmetric :: Relation a }
    deriving Num

instance Ord a => Eq (SymmetricRelation a) where
    x == y = symmetricClosure (fromSymmetric x) == symmetricClosure (fromSymmetric y)

instance (Ord a, Show a) => Show (SymmetricRelation a) where
    show = show . symmetricClosure . fromSymmetric

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (SymmetricRelation a) where
    type Vertex (SymmetricRelation a) = a
    empty       = SymmetricRelation empty
    vertex      = SymmetricRelation . vertex
    overlay x y = SymmetricRelation $ fromSymmetric x `overlay` fromSymmetric y
    connect x y = SymmetricRelation $ fromSymmetric x `connect` fromSymmetric y

instance Ord a => Undirected (SymmetricRelation a)

-- TODO: Optimise the implementation by caching the results of transitive closure.
-- | The 'TransitiveRelation' data type represents a binary transitive relation
-- over a set of elements.
--
-- @
-- show (1 * 2         :: TransitiveRelation Int) == "edge 1 2"
-- show (1 * 2 + 2 * 3 :: TransitiveRelation Int) == "edges [(1,2),(1,3),(2,3)]"
-- @
newtype TransitiveRelation a = TransitiveRelation { fromTransitive :: Relation a }
    deriving Num

instance Ord a => Eq (TransitiveRelation a) where
    x == y = transitiveClosure (fromTransitive x) == transitiveClosure (fromTransitive y)

instance (Ord a, Show a) => Show (TransitiveRelation a) where
    show = show . transitiveClosure . fromTransitive

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (TransitiveRelation a) where
    type Vertex (TransitiveRelation a) = a
    empty       = TransitiveRelation empty
    vertex      = TransitiveRelation . vertex
    overlay x y = TransitiveRelation $ fromTransitive x `overlay` fromTransitive y
    connect x y = TransitiveRelation $ fromTransitive x `connect` fromTransitive y

instance Ord a => Transitive (TransitiveRelation a)

-- TODO: Optimise the implementation by caching the results of preorder closure.
-- | The 'Preorder' data type represents a binary transitive relation
-- over a set of elements.
--
-- @
-- show (1             :: PreorderRelation Int) == "edge 1 1"
-- show (1 * 2         :: PreorderRelation Int) == "edges [(1,1),(1,2),(2,2)]"
-- show (1 * 2 + 2 * 3 :: PreorderRelation Int) == "edges [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]"
-- @
newtype PreorderRelation a = PreorderRelation { fromPreorder :: Relation a }
    deriving Num

instance (Ord a, Show a) => Show (PreorderRelation a) where
    show = show . preorderClosure . fromPreorder

-- | Compute the /preorder closure/ of a 'Relation'.
--
-- @
-- preorderClosure 'empty'      == 'empty'
-- preorderClosure ('vertex' x) == 'Algebra.Graph.edge' x x
-- preorderClosure ('Algebra.Graph.path' xs)  == 'Algebra.Graph.clique' xs
-- @
preorderClosure :: Ord a => Relation a -> Relation a
preorderClosure = reflexiveClosure . transitiveClosure

instance Ord a => Eq (PreorderRelation a) where
    x == y = preorderClosure (fromPreorder x) == preorderClosure (fromPreorder y)

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (PreorderRelation a) where
    type Vertex (PreorderRelation a) = a
    empty       = PreorderRelation empty
    vertex      = PreorderRelation . vertex
    overlay x y = PreorderRelation $ fromPreorder x `overlay` fromPreorder y
    connect x y = PreorderRelation $ fromPreorder x `connect` fromPreorder y

instance Ord a => Reflexive  (PreorderRelation a)
instance Ord a => Transitive (PreorderRelation a)
instance Ord a => Preorder   (PreorderRelation a)
