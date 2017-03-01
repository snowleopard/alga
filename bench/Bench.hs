import Criterion.Main
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Algebra.Graph
import Algebra.Graph.Relation (Relation, relation)
import Algebra.Graph.AdjacencyArray.Unboxed (GraphArray (..), matrixLength)
import Algebra.Graph.AdjacencyMap (AdjacencyMap, adjacencyMap)
import Algebra.Graph.Util (box, deBruijn, gmap, VertexSet, vertexSet, ToList, toList)
import qualified Algebra.Graph.AdjacencyMap.Int as Int
import qualified Algebra.Graph.Util.Int as Int

v :: VertexSet a -> Int
v = Set.size . vertexSet

l :: ToList a -> Int
l = length . toList

e :: AdjacencyMap a -> Int
e = foldr (\s t -> Set.size s + t) 0 . adjacencyMap

r :: Relation a -> Int
r = Set.size . relation

a :: GraphArray -> Int
a (GA _ es) = matrixLength es

vInt :: Int.VertexSet -> Int
vInt = IntSet.size . Int.vertexSet

eInt :: Int.AdjacencyMap -> Int
eInt = foldr (\s t -> IntSet.size s + t) 0 . Int.adjacencyMap

vDeBruijn :: Int -> Int
vDeBruijn n = v $ deBruijn n "0123456789"

lDeBruijn :: Int -> Int
lDeBruijn n = l $ deBruijn n "0123456789"

eDeBruijn :: Int -> Int
eDeBruijn n = e $ deBruijn n "0123456789"

rDeBruijn :: Int -> Int
rDeBruijn n = r $ deBruijn n "0123456789"

aDeBruijn :: Int -> Int
aDeBruijn n = a $ gmap fastRead $ deBruijn n "0123456789"

vIntDeBruijn :: Int -> Int
vIntDeBruijn n = v $ gmap fastRead $ deBruijn n "0123456789"

eIntDeBruin :: Int -> Int
eIntDeBruin n = e $ gmap fastRead $ deBruijn n "0123456789"

-- fastRead is ~3000x faster than read
fastRead :: String -> Int
fastRead = foldr (\c t -> t + ord c - ord '0') 0

fastReadInts :: Int -> Int
fastReadInts n = foldr (+) 0 $ map fastRead $ ints ++ ints
  where
    ints = mapM (const "0123456789") [1..n]

vMesh :: Int -> Int
vMesh n = v $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

lMesh :: Int -> Int
lMesh n = l $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

eMesh :: Int -> Int
eMesh n = e $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

rMesh :: Int -> Int
rMesh n = r $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

aMesh :: Int -> Int
aMesh n = a $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

vIntMesh :: Int -> Int
vIntMesh n = vInt $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

eIntMesh :: Int -> Int
eIntMesh n = eInt $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

vIntClique :: Int -> Int
vIntClique n = vInt $ clique [1..n]

eIntClique :: Int -> Int
eIntClique n = eInt $ clique [1..n]

lClique :: Int -> Int
lClique n = l $ clique [1..n]

rClique :: Int -> Int
rClique n = r $ clique [1..n]

aClique :: Int -> Int
aClique n = a $ clique [1..n]

main :: IO ()
main = defaultMain
    [ bgroup "vDeBruijn"
        [ bench "10^1" $ whnf vDeBruijn 1
        , bench "10^2" $ whnf vDeBruijn 2
        , bench "10^3" $ whnf vDeBruijn 3
        , bench "10^4" $ whnf vDeBruijn 4
        , bench "10^5" $ whnf vDeBruijn 5
        , bench "10^6" $ whnf vDeBruijn 6 ]
    , bgroup "lDeBruijn"
        [ bench "10^1" $ whnf lDeBruijn 1
        , bench "10^2" $ whnf lDeBruijn 2
        , bench "10^3" $ whnf lDeBruijn 3
        , bench "10^4" $ whnf lDeBruijn 4
        , bench "10^5" $ whnf lDeBruijn 5
        , bench "10^6" $ whnf lDeBruijn 6 ]
    , bgroup "eDeBruijn"
        [ bench "10^1" $ whnf eDeBruijn 1
        , bench "10^2" $ whnf eDeBruijn 2
        , bench "10^3" $ whnf eDeBruijn 3
        , bench "10^4" $ whnf eDeBruijn 4
        , bench "10^5" $ whnf eDeBruijn 5
        , bench "10^6" $ whnf eDeBruijn 6 ]
    , bgroup "rDeBruijn"
        [ bench "10^1" $ whnf rDeBruijn 1
        , bench "10^2" $ whnf rDeBruijn 2
        , bench "10^3" $ whnf rDeBruijn 3
        , bench "10^4" $ whnf rDeBruijn 4
        , bench "10^5" $ whnf rDeBruijn 5
        , bench "10^6" $ whnf rDeBruijn 6 ]
    , bgroup "aDeBruijn"
        [ bench "10^1" $ whnf aDeBruijn 1
        , bench "10^2" $ whnf aDeBruijn 2
        , bench "10^3" $ whnf aDeBruijn 3
        , bench "10^4" $ whnf aDeBruijn 4
        , bench "10^5" $ whnf aDeBruijn 5 ]
    , bgroup "vIntDeBruijn"
        [ bench "10^1" $ whnf vIntDeBruijn 1
        , bench "10^2" $ whnf vIntDeBruijn 2
        , bench "10^3" $ whnf vIntDeBruijn 3
        , bench "10^4" $ whnf vIntDeBruijn 4
        , bench "10^5" $ whnf vIntDeBruijn 5
        , bench "10^6" $ whnf vIntDeBruijn 6 ]
    , bgroup "eIntDeBruin"
        [ bench "10^1" $ whnf eIntDeBruin 1
        , bench "10^2" $ whnf eIntDeBruin 2
        , bench "10^3" $ whnf eIntDeBruin 3
        , bench "10^4" $ whnf eIntDeBruin 4
        , bench "10^5" $ whnf eIntDeBruin 5
        , bench "10^6" $ whnf eIntDeBruin 6 ]
    , bgroup "fastReadInts"
        [ bench "10^1" $ whnf fastReadInts 1
        , bench "10^2" $ whnf fastReadInts 2
        , bench "10^3" $ whnf fastReadInts 3
        , bench "10^4" $ whnf fastReadInts 4
        , bench "10^5" $ whnf fastReadInts 5
        , bench "10^6" $ whnf fastReadInts 6 ]
    , bgroup "vMesh"
        [ bench "1x1"       $ whnf vMesh 1
        , bench "10x10"     $ whnf vMesh 10
        , bench "100x100"   $ whnf vMesh 100
        , bench "1000x1000" $ whnf vMesh 1000 ]
    , bgroup "lMesh"
        [ bench "1x1"       $ whnf lMesh 1
        , bench "10x10"     $ whnf lMesh 10
        , bench "100x100"   $ whnf lMesh 100
        , bench "1000x1000" $ whnf lMesh 1000 ]
    , bgroup "eMesh"
        [ bench "1x1"       $ whnf eMesh 1
        , bench "10x10"     $ whnf eMesh 10
        , bench "100x100"   $ whnf eMesh 100
        , bench "1000x1000" $ whnf eMesh 1000 ]
    , bgroup "rMesh"
        [ bench "1x1"       $ whnf rMesh 1
        , bench "10x10"     $ whnf rMesh 10
        , bench "100x100"   $ whnf rMesh 100
        , bench "1000x1000" $ whnf rMesh 1000 ]
    , bgroup "aMesh"
        [ bench "1x1"       $ whnf aMesh 1
        , bench "10x10"     $ whnf aMesh 10
        , bench "100x100"   $ whnf aMesh 100 ]
    , bgroup "vIntMesh"
        [ bench "1x1"       $ whnf vIntMesh 1
        , bench "10x10"     $ whnf vIntMesh 10
        , bench "100x100"   $ whnf vIntMesh 100
        , bench "1000x1000" $ whnf vIntMesh 1000 ]
    , bgroup "eIntMesh"
        [ bench "1x1"       $ whnf eIntMesh 1
        , bench "10x10"     $ whnf eIntMesh 10
        , bench "100x100"   $ whnf eIntMesh 100
        , bench "1000x1000" $ whnf eIntMesh 1000 ]
    , bgroup "aClique"
        [ bench "1"      $ nf aClique 1
        , bench "10"     $ nf aClique 10
        , bench "100"    $ nf aClique 100
        , bench "1000"   $ nf aClique 1000 ]
    , bgroup "rClique"
        [ bench "1"       $ nf rClique 1
        , bench "10"      $ nf rClique 10
        , bench "100"     $ nf rClique 100
        , bench "1000"    $ nf rClique 1000
        , bench "10000"   $ nf rClique 10000 ]
    , bgroup "vIntClique"
        [ bench "1"      $ nf vIntClique 1
        , bench "10"     $ nf vIntClique 10
        , bench "100"    $ nf vIntClique 100
        , bench "1000"   $ nf vIntClique 1000
        , bench "10000"  $ nf vIntClique 10000
        , bench "44722"  $ nf vIntClique 44722 ]
    , bgroup "lClique"
        [ bench "1"      $ nf lClique 1
        , bench "10"     $ nf lClique 10
        , bench "100"    $ nf lClique 100
        , bench "1000"   $ nf lClique 1000
        , bench "10000"  $ nf lClique 10000
        , bench "44722"  $ nf lClique 44722 ]
    , bgroup "eIntClique"
        [ bench "1"      $ nf eIntClique 1
        , bench "10"     $ nf eIntClique 10
        , bench "100"    $ nf eIntClique 100
        , bench "1000"   $ nf eIntClique 1000
        , bench "10000"  $ nf eIntClique 10000
        , bench "44722"  $ nf eIntClique 44722 ] ]
