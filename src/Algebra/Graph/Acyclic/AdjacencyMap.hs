module Algebra.Graph.Acyclic.AdjacencyMap where

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Set as Set

newtype AdjacencyMap a = AAM
  { aam :: AM.AdjacencyMap a
  } deriving (Show)

consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AAM m) = AM.consistent m && AM.isAcyclic m

empty :: AdjacencyMap a
empty = AAM AM.empty

vertex :: a -> AdjacencyMap a
vertex x = AAM $ AM.vertex x

vertices :: (Ord a) => [a] -> AdjacencyMap a
vertices xs = AAM $ AM.vertices xs

overlayD ::
     (Ord a, Ord b)
  => AdjacencyMap a
  -> AdjacencyMap b
  -> AdjacencyMap (Either a b)
overlayD (AAM a) (AAM b) = AAM (AM.overlay (AM.gmap Left a) (AM.gmap Right b))

connectD ::
     (Ord a, Ord b)
  => AdjacencyMap a
  -> AdjacencyMap b
  -> AdjacencyMap (Either a b)
connectD (AAM a) (AAM b) = AAM (AM.connect (AM.gmap Left a) (AM.gmap Right b))

transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure (AAM x) = AAM (AM.transitiveClosure x)

scc :: (Ord a) => AM.AdjacencyMap a -> AdjacencyMap (NonEmpty.AdjacencyMap a)
scc = AAM . AM.scc

type PartialOrder a = a -> a -> Bool

fromGraph :: Ord a => PartialOrder a -> Graph a -> AdjacencyMap a
fromGraph o = AAM . aMF . foldg AM.empty AM.vertex AM.overlay AM.connect
  where
    aMF = AM.AM . Map.mapWithKey (\k -> Set.filter (`o` k)) . AM.adjacencyMap






