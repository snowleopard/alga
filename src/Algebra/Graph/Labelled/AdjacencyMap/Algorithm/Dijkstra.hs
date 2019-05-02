module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Label (Capacity, Distance, Semiring(..), (<+>), zero)
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Control.Monad (sequence_, unless, when)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, put)
import Data.Heap (FstMaxPolicy, FstMinPolicy, Heap, HeapItem(..))
import qualified Data.Heap as Heap
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

type DijkstraForDistance a b = DijkstraState FstMinPolicy a (Distance b)

type DijkstraForCapacity a b = DijkstraState FstMaxPolicy a (Capacity b)

-- | /O(|V| + |E| * log|V|) `dijkstra` is the main function using
-- both `initialize` and `exploreVertices`.
dijkstra ::
     (Eq e, Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> DijkstraState p a e
dijkstra a am =
  flip execState (DijkstraState Heap.empty Map.empty Map.empty Map.empty) $ do
    initialize a am
    exploreVertices am

data DijkstraState p a e = DijkstraState
  { heap :: Heap p (e, a)
  , explored :: Map a Bool
  , distance :: Map a e
  , path :: Map a (Maybe a)
  }

instance (Show e, Show a) => Show (DijkstraState p a e) where
  show (DijkstraState _ explored' distance' path') =
    "explored :" ++
    show explored' ++
    "\n" ++ "distance: " ++ show distance' ++ "\n" ++ "path: " ++ show path'

-- | /O(|V|)/ `initialize` gives the initial state for the
-- State computation.
initialize ::
     (Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
initialize a am =
  put $
  DijkstraState
    (Heap.singleton (one, a))
    (Map.insert a True . Map.map (const False) $ adjacencyMap am)
    (Map.insert a one . Map.map (const zero) $ adjacencyMap am)
    (Map.map (const Nothing) $ adjacencyMap am)

-- | /O(|E| * log|V|)/ `exploreVertices` is an iterative function which explores all the vertices.
exploreVertices ::
     (Ord a, Eq e, HeapItem p (e, a), Semiring e)
  => AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertices am = do
  curHeap <- gets heap
  curExplored <- gets explored
  curS <- get
  case Heap.view curHeap of
    Nothing -> return ()
    (Just ((_, v), t)) -> do
      put $ curS {heap = t, explored = Map.insert v True curExplored}
      exploreVertex v am
      exploreVertices am

-- | /O(log|V|)/ `exploreVertex` explores the edges of a single vertex.
exploreVertex ::
     (Ord a, Semiring e, Eq e, HeapItem p (e, a))
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertex v1 am =
  sequence_ $
  Map.mapWithKey (\v2 e -> exploreEdge e v1 v2) (adjacencyMap am ! v1)

-- | /O(log|V|)/ `exploreEdge` takes an edge (v1 -> v2) with weight e.
-- It computes a new distance value for v2 and inserts it in the heap
-- if not explored.
exploreEdge ::
     (HeapItem p (e, a), Ord a, Semiring e, Eq e)
  => e
  -> a
  -> a
  -> State (DijkstraState p a e) ()
exploreEdge e v1 v2 = do
  curHeap <- gets heap
  curDistance <- gets distance
  curPath <- gets path
  curExplored <- gets explored
  unless (curExplored ! v2) $ do
    let curDV1 = curDistance ! v1
    let curDV2 = curDistance ! v2
    let newDV2 = curDV2 <+> (curDV1 <.> e)
    when (newDV2 /= curDV2) $
      put $
      DijkstraState
        (Heap.insert (newDV2, v2) curHeap)
        curExplored
        (Map.insert v2 newDV2 curDistance)
        (Map.insert v2 (Just v1) curPath)
