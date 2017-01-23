{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Graph.TopSort (
    TopSort, isTopSort, topSort, mapVertices, vertexSet
    ) where

import qualified Data.Graph as Std
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Set as Set
import           Data.Set (Set)

import Algebra.Graph
import qualified Algebra.Graph.AdjacencyMap as AM
import Algebra.Graph.AdjacencyMap hiding (mapVertices, vertexSet)

newtype TopSort a = TS { fromTopSort :: AdjacencyMap a } deriving (Show, Num)

instance Ord a => Eq (TopSort a) where
    x == y = topSort x == topSort y

mapVertices :: (Ord a, Ord b) => (a -> b) -> TopSort a -> TopSort b
mapVertices f = TS . AM.mapVertices f . fromTopSort

vertexSet :: TopSort a -> Set a
vertexSet = AM.vertexSet . fromTopSort

topSort :: Ord a => TopSort a -> Maybe [a]
topSort (TS x) = if isTopSort x result then Just result else Nothing
  where
    (g, get) = toDownStd x
    result   = map get $ Std.topSort g

toDownStd :: Ord a => AdjacencyMap a -> (Std.Graph, Std.Vertex -> a)
toDownStd x = (g, \v -> case get v of (_, Down u, _) -> u)
  where
    (g, get) = Std.graphFromEdges' . map (\(v, us) -> ((), Down v, map Down us))
             $ adjacencyList x

isTopSort :: forall a. Ord a => AdjacencyMap a -> [a] -> Bool
isTopSort x = go Set.empty
  where
    go :: Set a -> [a] -> Bool
    go seen []     = seen == Map.keysSet (adjacencyMap x)
    go seen (v:vs) = let newSeen = seen `seq` Set.insert v seen
        in postset v x `Set.intersection` newSeen == Set.empty && go newSeen vs

-- To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance Ord a => Graph (TopSort a) where
    type Vertex (TopSort a) = a
    empty       = TS empty
    vertex      = TS . vertex
    overlay x y = TS $ fromTopSort x `overlay` fromTopSort y
    connect x y = TS $ fromTopSort x `connect` fromTopSort y
