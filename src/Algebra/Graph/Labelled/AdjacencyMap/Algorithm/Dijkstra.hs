module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Label (Capacity, Distance, Semiring(..), (<+>), zero)
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Control.Monad (sequence_, when)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, put)
import Data.Heap (FstMaxPolicy, FstMinPolicy, Heap, HeapItem(..))
import qualified Data.Heap as Heap
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

type DijkstraForDistance a b = DijkstraState FstMinPolicy a (Distance b)

type DijkstraForCapacity a b = DijkstraState FstMaxPolicy a (Capacity b)

-- | `dijkstra` is the main function using bothe `initialize` and `exploreVertices`.
-- The time complexity of `dijkstra` is O(|V| + |V| * |E|) = O(|V| * |E|)
dijkstra ::
     (Eq e, Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> DijkstraState p a e
dijkstra a am =
  flip execState (DijkstraState Heap.empty Map.empty Map.empty) $ do
    initialize a am
    exploreVertices am

data DijkstraState p a e = DijkstraState
  { heap :: Heap p (e, a)
  , distance :: Map a e
  , path :: Map a (Maybe a)
  }

instance (Show e, Show a) => Show (DijkstraState p a e) where
  show (DijkstraState heap' distance' path') =
    "distance: " ++ show distance' ++ "\n" ++ "path: " ++ show path'

-- | This is a helper function to work with the heap in DijkstraState
putHeap :: Heap p (e, a) -> State (DijkstraState p a e) ()
putHeap h = do
  (DijkstraState _ distance' path') <- get
  put $ DijkstraState h distance' path'

-- | `initializes` gives the initial state for the State computation.
-- The time complexity is O(|V|)
initialize ::
     (Ord a, HeapItem p (e, a), Semiring e)
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
initialize a am =
  put $
  DijkstraState
    (Heap.singleton (one, a))
    (Map.insert a one . Map.map (const zero) $ adjacencyMap am)
    (Map.map (const Nothing) $ adjacencyMap am)

-- | `exploreVertices` is an iterative function which explores all the vertices.
-- The time complexity is O(|V| * |E|).
-- O(|E|) is due to the usage of `exploreVertex`.
exploreVertices ::
     (Ord a, Eq e, HeapItem p (e, a), Semiring e)
  => AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertices am = do
  curHeap <- gets heap
  case Heap.view curHeap of
    Nothing -> return ()
    (Just ((_, v), t)) -> do
      putHeap t
      exploreVertex v am
      exploreVertices am

-- | `exploreVertex` explores the edges of a single vertex.
-- The time complexity is hence O(|E|).
exploreVertex ::
     (Ord a, Semiring e, Eq e, HeapItem p (e, a))
  => a
  -> AdjacencyMap e a
  -> State (DijkstraState p a e) ()
exploreVertex v1 am =
  sequence_ $
  Map.mapWithKey (\v2 e -> exploreEdge e v1 v2) (adjacencyMap am ! v1)

-- | This is a helper function for modifying the weight of an element in the heap.
-- The current heap library does not provide a way to modify or delete any value.
-- Hence we first delete the value using the filter function and then we insert the value.
-- The time complexity of `Heap.filter` if O(|V|).
-- The time complexity of `Heap.insert` is O(log|V|).
-- The time complexity of `modifyHeapItemPrio` is O(|V|).
modifyHeapItemPrio ::
     (HeapItem p (e, a), Eq a) => a -> e -> Heap p (e, a) -> Heap p (e, a)
modifyHeapItemPrio vertex prio curHeap =
  Heap.insert (prio, vertex) $ Heap.filter (isNot vertex) curHeap
  where
    isNot v (_, i) = v /= i

-- | `exploreEdge` takes an edge (v1 -> v2) with weight e.
-- It computes a new distance value for v2 and changes it in the heap.
-- The time complexity for this function is O(|V|), this is directly dependent on `modifyHeapItemPrio`.
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
  let curDV1 = curDistance ! v1
  let curDV2 = curDistance ! v2
  let newDV2 = curDV2 <+> (curDV1 <.> e)
  when (newDV2 /= curDV2) $
    put $
    DijkstraState
      (modifyHeapItemPrio v2 newDV2 curHeap)
      (Map.insert v2 newDV2 curDistance)
      (Map.insert v2 (Just v1) curPath)
