-----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Graph.Relation.Internal
-- Copyright   :  (c) Andrey Mokhov 2016-2017
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  andrey.mokhov@gmail.com
-- Stability   :  unstable
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
    Relation (..),

    -- * Operations on binary relations
    preset, postset, reflexiveClosure, symmetricClosure, transitiveClosure,
    preorderClosure,

    -- * Reflexive relations
    ReflexiveRelation (..),

    -- * Symmetric relations
    SymmetricRelation (..),

    -- * Transitive relations
    TransitiveRelation (..),

    -- * Preorders
    PreorderRelation (..)
  ) where

import           Data.Set hiding (empty, map)
import qualified Data.Set as Set
import Data.Tuple

import Algebra.Graph.Classes

-- | The 'Relation' data type represents a binary relation over a set of elements.
data Relation a = Relation {
    -- | The /domain/ of the relation.
    domain :: Set a,
    -- | The set of pairs of elements that are /related/. It is guaranteed that
    -- each element belongs to the domain.
    relation :: Set (a, a)
  } deriving (Eq, Show)

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = Relation Set.empty Set.empty
    vertex  x   = Relation (singleton x) Set.empty
    overlay x y = Relation (domain x `union` domain y) (relation x `union` relation y)
    connect x y = Relation (domain x `union` domain y) (relation x `union` relation y
        `union` (domain x >< domain y))

(><) :: Set a -> Set a -> Set (a, a)
x >< y = fromDistinctAscList [ (a, b) | a <- elems x, b <- elems y ]

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

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

-- | Compute the /reflexive closure/ of a 'Relation'.
--
-- @
-- reflexiveClosure 'empty'      == 'empty'
-- reflexiveClosure ('vertex' x) == 'Algebra.Graph.edge' x x
-- @
reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure (Relation d r) =
    Relation d $ r `union` fromDistinctAscList [ (a, a) | a <- elems d ]

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
    newR = unions $ r : [ preset x old >< postset x old | x <- elems d ]

-- TODO: Optimise the implementation by caching the results of reflexive closure.
-- | The 'ReflexiveRelation' data type represents a binary reflexive relation
-- over a set of elements.
newtype ReflexiveRelation a = ReflexiveRelation { fromReflexive :: Relation a }
    deriving (Num, Show)

instance Ord a => Eq (ReflexiveRelation a) where
    x == y = reflexiveClosure (fromReflexive x) == reflexiveClosure (fromReflexive y)

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
newtype SymmetricRelation a = SymmetricRelation { fromSymmetric :: Relation a }
    deriving (Num, Show)

instance Ord a => Eq (SymmetricRelation a) where
    x == y = symmetricClosure (fromSymmetric x) == symmetricClosure (fromSymmetric y)

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
newtype TransitiveRelation a = TransitiveRelation { fromTransitive :: Relation a }
    deriving (Num, Show)

instance Ord a => Eq (TransitiveRelation a) where
    x == y = transitiveClosure (fromTransitive x) == transitiveClosure (fromTransitive y)

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
newtype PreorderRelation a = PreorderRelation { fromPreorder :: Relation a }
    deriving (Num, Show)

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
