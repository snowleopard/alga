module Algebra.Graph.Acyclic.AdjacencyMap where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type VID = Int

data Vertex a = Vertex
  { vid :: VID
  , edges :: [Vertex a]
  , label :: a
  } deriving (Show)

data DAG' a
  = Cons (Vertex a)
         (DAG' a)
  | Nil
  deriving (Show)

newtype DAG a =
  DAG (DAG' a)
  deriving (Show)

vertex :: VID -> [Vertex a] -> a -> Vertex a
vertex = Vertex

newId :: DAG' a -> VID
newId Nil = 0
newId (Cons v _) = vid v + 1

singleton :: a -> State (DAG' a) (Vertex a)
singleton a = edgeTo a []

edgeTo :: a -> [Vertex a] -> State (DAG' a) (Vertex a)
edgeTo a es = do
  s <- get
  let v = vertex (newId s) es a
  put $ Cons v s
  return v

withDAG :: DAG a -> State (DAG' a) ()
withDAG (DAG d) = put d

dag :: State (DAG' a) () -> DAG a
dag s = DAG $ execState s Nil

view :: DAG a -> Maybe (Vertex a, DAG a)
view (DAG Nil) = Nothing
view (DAG (Cons v d)) = Just (v, DAG d)

unsafeToAcyclic :: (Ord a) => AM.AdjacencyMap a -> DAG a
unsafeToAcyclic am = dag . void $ execStateT (visitAll am) Map.empty

scc :: (Ord a) => AM.AdjacencyMap a -> DAG (NonEmpty.AdjacencyMap a)
scc am = unsafeToAcyclic $ AMA.scc am

topSort :: Ord a => DAG a -> [(VID, a)]
topSort d =
  case view d of
    Nothing -> []
    Just (v, nD) -> (vid v, label v) : topSort nD

visitAll ::
     (Ord a)
  => AM.AdjacencyMap a
  -> StateT (Map a (Vertex a)) (State (DAG' a)) ()
visitAll am = do
  let vs = Map.keys . AM.adjacencyMap $ am
  mapM_ (visitOne am) vs

visitOne ::
     (Ord a)
  => AM.AdjacencyMap a
  -> a
  -> StateT (Map a (Vertex a)) (State (DAG' a)) (Vertex a)
visitOne am a = do
  vMap <- get
  let m = AM.adjacencyMap am
  case Map.lookup a vMap of
    Just x -> return x
    Nothing -> do
      es <- mapM (visitOne am) . Set.toList $ (m ! a)
      v <- lift $ edgeTo a es
      put $ Map.insert a v vMap
      return v
