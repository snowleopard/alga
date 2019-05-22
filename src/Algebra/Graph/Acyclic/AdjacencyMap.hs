module Algebra.Graph.Acyclic.AdjacencyMap where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import qualified Algebra.Graph.AdjacencyMap.Internal as AMI

newtype AdjacencyMap a = AAM
  { aam :: AM.AdjacencyMap a
  }

consistent :: Ord a => AdjacencyMap a -> Bool
consistent (AAM m) = AMI.consistent m && AMA.isAcyclic m

type AdjacencyMapP a = AdjacencyMap (Int, a)

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
