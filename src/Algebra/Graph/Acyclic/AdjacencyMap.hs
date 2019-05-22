module Algebra.Graph.Acyclic.AdjacencyMap where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import qualified Algebra.Graph.AdjacencyMap.Internal as AMI
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype AdjacencyMap a = AAM
  { aam :: AM.AdjacencyMap a
  } deriving (Show)

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

transitiveClosure :: Ord a => AdjacencyMap a -> AdjacencyMap a
transitiveClosure (AAM x) = AAM (AM.transitiveClosure x)

overlay :: (Ord a) => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
overlay (AAM x) (AAM y) = AAM (AM.overlay x y)

connect :: (Ord a) => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
connect (AAM x) (AAM y) =
  AAM . AMI.AM $
  Map.unionsWith Set.union $
  AM.adjacencyMap x :
  AM.adjacencyMap y :
  [ Map.fromSet
      (\k -> Set.filter (< k) . Map.keysSet $ AM.adjacencyMap y)
      (Map.keysSet $ AM.adjacencyMap x)
  ]

-- reverse and Map.keys results in an ascending list
topSort :: (Ord a) => AdjacencyMap a -> [a]
topSort (AAM (AMI.AM m)) = reverse $ Map.keys m

-- scc
scc :: (Ord a) => AM.AdjacencyMap a -> AdjacencyMapP (NonEmpty.AdjacencyMap a)
scc am = unsafeToAcyclic $ AMA.scc am

unsafeToAcyclic :: (Ord a) => AM.AdjacencyMap a -> AdjacencyMapP a
unsafeToAcyclic am = AAM (AM.gmap oF am)
  where
    oMap = execState (execStateT (exploreAll am) 0) Map.empty
    oF x = (oMap ! x, x)

exploreAll :: (Ord a) => AM.AdjacencyMap a -> StateT Int (State (Map a Int)) ()
exploreAll am@(AMI.AM m) = mapM_ (exploreOne am) . Map.keys $ m

exploreOne ::
     (Ord a)
  => AM.AdjacencyMap a
  -> a
  -> StateT Int (State (Map a Int)) ()
exploreOne am@(AMI.AM m) x = do
  oMap <- lift get
  case Map.lookup x oMap of
    Just _ -> return ()
    Nothing -> do
      mapM_ (exploreOne am) . Set.toList $ (m ! x)
      nOMap <- lift get
      o <- get
      lift . put $ Map.insert x o nOMap
      put $ o + 1

-- auxillary functions
-- incrementOrd a b am = increase the partial order of every element >= a by b in am
-- useful for insreting vertices after an unsafeAcyclic operation.
-- does not break the partial order invatiant
incrementOrd :: (Ord a) => Int -> Int -> AdjacencyMapP a -> AdjacencyMapP a
incrementOrd a b (AAM am) = AAM $ AM.gmap iF am
  where
    iF (x, y)
      | x >= a = (x + b, y)
      | otherwise = (x, y)


-- REPL testing
x = (1 * 2) + (1 * 3) + (3 * 2)
y = unsafeToAcyclic x



