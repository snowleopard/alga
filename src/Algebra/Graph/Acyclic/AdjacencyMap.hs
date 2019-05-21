{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Algebra.Graph.Acyclic.AdjacencyMap where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as NonEmpty
import Control.Monad (void)
import Control.Monad.Indexed ((>>>=), ireturn)
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Prelude hiding ((>>), (>>=), return)

data Nat
  = Z
  | S Nat

instance Show Nat where
  show = show . toIntNat

data Ordinal n where
  OZ :: Ordinal ('S n)
  OS :: Ordinal m -> Ordinal ('S m)

instance Show (Ordinal n) where
  show = show . toIntOrdinal

data DAG n a where
  Empty :: DAG 'Z a
  Node :: a -> [Ordinal n] -> DAG n a -> DAG ('S n) a

instance (Show a) => Show (DAG n a) where
  show Empty = "Empty"
  show (Node a es d) =
    "Node(" ++ show a ++ ", " ++ show (map toIntOrdinal es) ++ ") > " ++ show d

toIntOrdinal :: Ordinal n -> Int
toIntOrdinal OZ = 0
toIntOrdinal (OS x) = 1 + toIntOrdinal x

toIntNat :: Nat -> Int
toIntNat Z = 0
toIntNat (S x) = 1 + toIntNat x

getOrdinal :: DAG n a -> Ordinal n
getOrdinal (Node _ _ Empty) = OZ
getOrdinal (Node _ _ d) = OS $ getOrdinal d

singleton :: a -> IxState (DAG n a) (DAG ('S n) a) (Ordinal ('S n))
singleton a = edgeTo a []

edgeTo :: a -> [Ordinal n] -> IxState (DAG n a) (DAG ('S n) a) (Ordinal ('S n))
edgeTo a es = do
  s <- iget
  let nS = Node a es s
  iput nS
  return $ getOrdinal nS
  where
    (>>=) = (>>>=)
    return = ireturn :: forall i a. a -> IxState i i a
    f >> g = f >>= const g

view :: DAG ('S n) a -> ((a, [Ordinal n]), DAG n a)
view (Node a es d) = ((a, es), d)
  {-
visitOne ::
     AM.AdjacencyMap a
  -> a
  -> IxStateT (Map a [Ordinal n]) (Either (IxState (DAG n a) (DAG ('S n) a)) (IxState (DAG n a) (DAG n a))) (Either (IxState (DAG n a) (DAG ('S n) a)) (IxState (DAG n a) (DAG n a))) (Either (Ordinal n) (Ordinal ('S n)))
visitOne am a = do
  vMap <- iget
  let m = AM.adjacencyMap am
  case Map.lookup a vMap of
    Just x -> return (Left x)
    Nothing
      -- es <- isequence . map (visitOne am) . Set.toList $ (m ! a)
     -> do
      v <- ilift . Right $ edgeTo a []
      iput $ Map.insert a v vMap
      return (Right v)
-}
{-
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

-}
