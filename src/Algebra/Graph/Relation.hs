module Algebra.Graph.Relation (
    Relation, domain, relation, preset, postset,
    reflexiveClosure, symmetricClosure, transitiveClosure,
    Reflexive, reflexiveRelation, Symmetric, symmetricRelation,
    Transitive, transitiveRelation
    ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Test.QuickCheck (Arbitrary (..))

import Algebra.Graph

data Relation a = R { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

instance Ord a => Graph (Relation a) where
    type Vertex (Relation a) = a
    empty       = R Set.empty Set.empty
    vertex  x   = R (Set.singleton x) Set.empty
    overlay x y = R (domain x `Set.union` domain y) (relation x `Set.union` relation y)
    connect x y = R (domain x `Set.union` domain y) (relation x `Set.union` relation y
        `Set.union` (domain x >< domain y))

(><) :: Set a -> Set a -> Set (a, a)
x >< y = Set.fromDistinctAscList [ (a, b) | a <- Set.elems x, b <- Set.elems y ]

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance (Arbitrary a, Ord a) => Arbitrary (Relation a) where
    arbitrary = do
        r <- arbitrary
        let (xs, ys) = unzip $ Set.toAscList r
        return $ R (Set.fromList $ xs ++ ys) r

preset :: Ord a => a -> Relation a -> Set a
preset x = Set.mapMonotonic fst . Set.filter ((== x) . snd) . relation

postset :: Ord a => a -> Relation a -> Set a
postset x = Set.mapMonotonic snd . Set.filter ((== x) . fst) . relation

reflexiveClosure :: Ord a => Relation a -> Relation a
reflexiveClosure (R d r) = R d $ r `Set.union`
    Set.fromDistinctAscList [ (a, a) | a <- Set.elems d ]

symmetricClosure :: Ord a => Relation a -> Relation a
symmetricClosure (R d r) = R d $ r `Set.union` (Set.map swap r)

transitiveClosure :: Ord a => Relation a -> Relation a
transitiveClosure old@(R d r)
    | r == newR = old
    | otherwise = transitiveClosure $ R d newR
  where
    newR = Set.unions $ r : [ preset x old >< postset x old | x <- Set.elems d ]

newtype Reflexive a = RR { reflexiveRelation :: Relation a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Reflexive a) where
    RR x == RR y = reflexiveClosure x == reflexiveClosure y

newtype Symmetric a = SR { symmetricRelation :: Relation a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Symmetric a) where
    SR x == SR y = symmetricClosure x == symmetricClosure y

newtype Transitive a = TR { transitiveRelation :: Relation a }
    deriving (Arbitrary, Num, Show)

instance Ord a => Eq (Transitive a) where
    TR x == TR y = transitiveClosure x == transitiveClosure y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Reflexive a) where
    type Vertex (Reflexive a) = a
    empty       = RR empty
    vertex      = RR . vertex
    overlay x y = RR $ reflexiveRelation x `overlay` reflexiveRelation y
    connect x y = RR $ reflexiveRelation x `connect` reflexiveRelation y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Symmetric a) where
    type Vertex (Symmetric a) = a
    empty       = SR empty
    vertex      = SR . vertex
    overlay x y = SR $ symmetricRelation x `overlay` symmetricRelation y
    connect x y = SR $ symmetricRelation x `connect` symmetricRelation y

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (Transitive a) where
    type Vertex (Transitive a) = a
    empty       = TR empty
    vertex      = TR . vertex
    overlay x y = TR $ transitiveRelation x `overlay` transitiveRelation y
    connect x y = TR $ transitiveRelation x `connect` transitiveRelation y
