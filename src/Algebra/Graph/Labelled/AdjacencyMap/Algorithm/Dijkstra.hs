module Algebra.Graph.Labelled.AdjacencyMap.Algorithm.Dijkstra where

import Algebra.Graph.Label (Distance, Semiring(..), (<+>), zero)
import Algebra.Graph.Labelled.AdjacencyMap.Internal (AdjacencyMap(..))
import Control.Monad (when)
import Control.Monad.Trans.State.Strict (State, execState, get, gets, put)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | A state that the Dijkstra algorithm defined will work on.
data DijkstraState a e = DijkstraState
  { heap :: Set (e, a)
  , distance :: Map a e
  , path :: Map a (Maybe a)
  } deriving (Show)

-- | /O(|V| + |E| * log|V|)/ `dijkstra` is the main function using both `initialize` and `exploreVertices`.
--
-- @
-- dijkstra 'x' (fromList []) = Error as 'x' does not exist in the adjacency map
--
-- dijkstra 'a' (fromList [(2, 'b', 'c'), (1, 'a', 'b'), (3, 'a', 'c')]) = ([], {'a': 0, 'b': 1, 'c': 3}, {'a': Nothing, 'b': Just 'a', 'c': Just 'b'})
-- @
dijkstra ::
     (Ord a, Num e, Ord e)
  => a
  -> AdjacencyMap (Distance e) a
  -> DijkstraState a (Distance e)
dijkstra a am =
  flip execState (DijkstraState Set.empty Map.empty Map.empty) $ do
    initialize a am
    exploreVertices am

-- | /O(|V|)/ `initialize` gives the initial state for the State computation.
initialize ::
     (Ord a, Num e, Ord e)
  => a
  -> AdjacencyMap (Distance e) a
  -> State (DijkstraState a (Distance e)) ()
initialize a am =
  put $
  DijkstraState
    (Set.singleton (one, a))
    (Map.insert a one . Map.map (const zero) $ adjacencyMap am)
    (Map.map (const Nothing) $ adjacencyMap am)

-- | /O(|E| * log|V|)/ `exploreVertices` is an iterative function which explores all the vertices.
exploreVertices ::
     (Ord a, Ord e, Num e)
  => AdjacencyMap (Distance e) a
  -> State (DijkstraState a (Distance e)) ()
exploreVertices am = do
  curHeap <- gets heap
  curS <- get
  case Set.minView curHeap of
    Nothing -> return ()
    (Just ((_, v), t)) -> do
      put $ curS {heap = t}
      exploreVertex v am
      exploreVertices am

-- | `exploreVertex` explores the edges of a single vertex.
-- Atmost, `exploreVertex` is called |V| times.
exploreVertex ::
     (Ord a, Ord e, Num e)
  => a
  -> AdjacencyMap (Distance e) a
  -> State (DijkstraState a (Distance e)) ()
exploreVertex v1 am =
  Map.foldrWithKey
    (\v2 e s -> s >> exploreEdge e v1 v2) (return ()) (adjacencyMap am ! v1)

-- | /O(log|V|)/ `exploreEdge` takes an edge (v1 -> v2) with weight e.
-- It computes a new distance value for v2 and inserts it in the heap if not explored.
exploreEdge ::
     (Ord a, Ord e, Num e)
  => Distance e
  -> a
  -> a
  -> State (DijkstraState a (Distance e)) ()
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
      (Set.insert (newDV2, v2) curHeap)
      (Map.insert v2 newDV2 curDistance)
      (Map.insert v2 (Just v1) curPath)
