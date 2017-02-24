{-# LANGUAGE MultiParamTypeClasses #-}
module Algebra.Graph.AdjacencyArray.Unboxed (
    GraphArray (..), Ints, Vector (..), Matrix (..), rowLength, row, matrixLength
    ) where

import Data.Array.Base
import Data.Array.ST
import Test.QuickCheck (Arbitrary (..))

import Algebra.Graph hiding (fromEdgeList)

-- NOTE: This instance doesn't allow to share common subgraphs and is
-- therefore typically very slow. Use with caution! It is here mostly
-- as an experiment.

type Ints = UArray Int Int

emptyArray :: Ints
emptyArray = array (1, 0) []

singleton :: Int -> Ints
singleton x = array (1, 1) [(1, x)]

data Vector = V { start :: Int, end :: Int, ints :: Ints } deriving (Eq, Show)

(#) :: Vector -> Int -> Int
(V _ _ a) # i = a!i

vector :: Ints -> Vector
vector a = V 1 (snd $ bounds a) a

restart :: Int -> Vector -> Vector
restart s (V _ e v) = V s e v

copy :: MArray a Int m => Vector -> a Int Int -> Int -> m ()
copy (V s e src) dst pos = go s
  where
    go i | i > e     = return ()
         | otherwise = writeArray dst (pos + i - s) (src!i) >> go (i + 1)

-- A sparse matrix stored as a single array in memory with indexed rows. We
-- store an additional index to the element after the last row to avoid
-- special cases in certain circumstances, e.g. see 'rowLength'.
data Matrix = M { rowOffset :: Ints, matrix :: Ints } deriving (Eq, Show)

matrixLength :: Matrix -> Int
matrixLength (M _ m) = snd $ bounds m

rowLength :: Int -> Matrix -> Int
rowLength r (M o _) = o!(r + 1) - o!r

row :: Int -> Matrix -> Vector
row r (M o m) = V (o!r) (o!(r + 1) - 1) m

data GraphArray = GA Ints Matrix deriving (Eq, Show)

unionLength :: Vector -> Vector -> Int
unionLength a b = go (start a) (start b) 0
  where
    go i j acc | i > end a = acc + end b - j + 1
               | j > end b = acc + end a - i + 1
               | otherwise = case compare (a#i) (b#j) of
                   LT -> go (i + 1)  j      $! acc + 1
                   EQ -> go (i + 1) (j + 1) $! acc + 1
                   GT -> go  i      (j + 1) $! acc + 1

unionLength3 :: Vector -> Vector -> Vector -> Int
unionLength3 a b c = go (start a) (start b) (start c) 0
  where
    go i j k acc
        | i > end a = acc + unionLength (restart j b) (restart k c)
        | j > end b = acc + unionLength (restart i a) (restart k c)
        | k > end c = acc + unionLength (restart i a) (restart j b)
        | otherwise = case compare (a#i) (b#j) of
            LT -> case compare (a#i) (c#k) of
                LT -> go (i + 1) j  k      $! acc + 1 -- a < b && a < c
                EQ -> go (i + 1) j (k + 1) $! acc + 1 -- a == c < b
                GT -> go  i      j (k + 1) $! acc + 1 -- c < a < b
            EQ -> case compare (a#i) (c#k) of
                LT -> go (i + 1) (j + 1)  k      $! acc + 1 -- a == b < c
                EQ -> go (i + 1) (j + 1) (k + 1) $! acc + 1 -- a == b == c
                GT -> go  i       j      (k + 1) $! acc + 1 -- c < a == b
            GT -> case compare (b#j) (c#k) of
                LT -> go i (j + 1)  k      $! acc + 1 -- b < a && b < c
                EQ -> go i (j + 1) (k + 1) $! acc + 1 -- b == c < a
                GT -> go i  j      (k + 1) $! acc + 1 -- c < b < a

union2 :: MArray a Int m => Vector -> Vector -> a Int Int -> Int -> m ()
union2 a b res pos = go pos (start a) (start b)
  where
    go p i j
        | i > end a = copy (restart j b) res p
        | j > end b = copy (restart i a) res p
        | otherwise = case compare (a#i) (b#j) of
            LT -> writeArray res p (a#i) >> go (p + 1) (i + 1)  j
            EQ -> writeArray res p (a#i) >> go (p + 1) (i + 1) (j + 1)
            GT -> writeArray res p (b#j) >> go (p + 1)  i      (j + 1)

union3 :: MArray a Int m => Vector -> Vector -> Vector -> a Int Int -> Int -> m ()
union3 a b c res pos = go pos (start a) (start b) (start c)
  where
    go p i j k
        | i > end a = union2 (restart j b) (restart k c) res p
        | j > end b = union2 (restart i a) (restart k c) res p
        | k > end c = union2 (restart i a) (restart j b) res p
        | otherwise = case compare (a#i) (b#j) of
            LT -> case compare (a#i) (c#k) of
                LT -> writeArray res p (a#i) >> go (p + 1) (i + 1) j  k      -- a < b && a < c
                EQ -> writeArray res p (a#i) >> go (p + 1) (i + 1) j (k + 1) -- a == c < b
                GT -> writeArray res p (c#k) >> go (p + 1)  i      j (k + 1) -- c < a < b
            EQ -> case compare (a#i) (c#k) of
                LT -> writeArray res p (a#i) >> go (p + 1) (i + 1) (j + 1)  k      -- a == b < c
                EQ -> writeArray res p (a#i) >> go (p + 1) (i + 1) (j + 1) (k + 1) -- a == b == c
                GT -> writeArray res p (c#k) >> go (p + 1)  i       j      (k + 1) -- c < a == b
            GT -> case compare (b#j) (c#k) of
                LT -> writeArray res p (b#j) >> go (p + 1) i (j + 1)  k      -- b < a && b < c
                EQ -> writeArray res p (b#j) >> go (p + 1) i (j + 1) (k + 1) -- b == c < a
                GT -> writeArray res p (c#k) >> go (p + 1) i  j      (k + 1) -- c < b < a

overlayGA :: GraphArray -> GraphArray -> GraphArray
overlayGA (GA us es) (GA vs fs) = GA ws (M is gs)
  where
    nu = numElements us
    nv = numElements vs
    nw = unionLength (vector us) (vector vs)
    ws = runSTUArray $ do
        a <- newArray (1, nw) 0
        union2 (vector us) (vector vs) a 1
        return a
    is = runSTUArray $ do
        a <- newArray (1, nw + 1) 0
        let buildIndex u v p r
                | u <= nu && v <= nv && us!u == vs!v = do
                    writeArray a p r
                    buildIndex (u + 1) (v + 1) (p + 1) $ r + unionLength (row u es) (row v fs)
                | u <= nu && (v > nv || us!u < vs!v) = do
                    writeArray a p r
                    buildIndex (u + 1) v (p + 1) $ r + rowLength u es
                | v <= nv && (u > nu || us!u > vs!v) = do
                    writeArray a p r
                    buildIndex u (v + 1) (p + 1) $ r + rowLength v fs
                | otherwise = writeArray a p r
        buildIndex 1 1 1 1
        return a
    gs = runSTUArray $ do
        a <- newArray (1, is!(nw + 1) - 1) 0
        let go u v p | u <= nu && v <= nv && us!u == vs!v = do
                         union2 (row u es) (row v fs) a (is!p)
                         go (u + 1) (v + 1) (p + 1)
                     | u <= nu && (v > nv || us!u < vs!v) = do
                         copy (row u es) a (is!p)
                         go (u + 1) v (p + 1)
                     | v <= nv && (u > nu || us!u > vs!v) = do
                         copy (row v fs) a (is!p)
                         go u (v + 1) (p + 1)
                     | otherwise = return ()
        go 1 1 1
        return a

connectGA :: GraphArray -> GraphArray -> GraphArray
connectGA (GA us es) (GA vs fs) = GA ws (M is gs)
  where
    nu = numElements us
    nv = numElements vs
    nw = unionLength (vector us) (vector vs)
    ws = runSTUArray $ do
        a <- newArray (1, nw) 0
        union2 (vector us) (vector vs) a 1
        return a
    is = runSTUArray $ do
        a <- newArray (1, nw + 1) 0
        let buildIndex u v p r
                | u <= nu && v <= nv && us!u == vs!v = do
                    writeArray a p r
                    buildIndex (u + 1) (v + 1) (p + 1) $ r + unionLength3 (row u es) (row v fs) (vector vs)
                | u <= nu && (v > nv || us!u < vs!v) = do
                    writeArray a p r
                    buildIndex (u + 1) v (p + 1) $ r + unionLength (row u es) (vector vs)
                | v <= nv && (u > nu || us!u > vs!v) = do
                    writeArray a p r
                    buildIndex u (v + 1) (p + 1) $ r + rowLength v fs
                | otherwise = writeArray a p r
        buildIndex 1 1 1 1
        return a
    gs = runSTUArray $ do
        a <- newArray (1, is!(nw + 1) - 1) 0
        let go u v p | u <= nu && v <= nv && us!u == vs!v = do
                         union3 (row u es) (row v fs) (vector vs) a (is!p)
                         go (u + 1) (v + 1) (p + 1)
                     | u <= nu && (v > nv || us!u < vs!v) = do
                         union2 (row u es) (vector vs) a (is!p)
                         go (u + 1) v (p + 1)
                     | v <= nv && (u > nu || us!u > vs!v) = do
                         copy (row v fs) a (is!p)
                         go u (v + 1) (p + 1)
                     | otherwise = return ()
        go 1 1 1
        return a

instance Graph GraphArray where
    type Vertex GraphArray = Int
    empty       = GA emptyArray (M (array (1, 1) [(1, 1)]) emptyArray)
    vertex  x   = GA (singleton x) (M (array (1, 2) [(1, 1), (2, 1)]) emptyArray)
    overlay x y = overlayGA x y
    connect x y = connectGA x y

instance Num GraphArray where
    fromInteger = vertex . fromInteger
    (+)         = overlay
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Arbitrary GraphArray where
    arbitrary = arbitraryGraph
