module Algebra.Graph.Relation (
    Relation, domain, relation, reflexiveClosure, symmetricClosure, transitiveClosure
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

preset :: Ord a => a -> Relation a -> Set a
preset x = Set.mapMonotonic fst . Set.filter ((== x) . snd) . relation

postset :: Ord a => a -> Relation a -> Set a
postset x = Set.mapMonotonic snd . Set.filter ((== x) . fst) . relation
