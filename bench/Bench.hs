import Criterion.Main
import Data.Char
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import Algebra.Graph
import Algebra.Graph.AdjacencyMap (AdjacencyMap, adjacencyMap)
import Algebra.Graph.Util (box, deBruijn, gmap, VertexSet, vertexSet)
import qualified Algebra.Graph.AdjacencyMap.Int as Int
import qualified Algebra.Graph.Util.Int as Int

v :: VertexSet a -> Int
v = Set.size . vertexSet

e :: AdjacencyMap a -> Int
e = foldr (\s r -> Set.size s + r) 0 . adjacencyMap

vInt :: Int.VertexSet -> Int
vInt = IntSet.size . Int.vertexSet

eInt :: Int.AdjacencyMap -> Int
eInt = foldr (\s r -> IntSet.size s + r) 0 . Int.adjacencyMap

vDeBruijn :: Int -> Int
vDeBruijn n = v $ deBruijn n "0123456789"

eDeBruijn :: Int -> Int
eDeBruijn n = e $ deBruijn n "0123456789"

vIntDeBruijn :: Int -> Int
vIntDeBruijn n = v $ gmap fastRead $ deBruijn n "0123456789"

eIntDeBruin :: Int -> Int
eIntDeBruin n = e $ gmap fastRead $ deBruijn n "0123456789"

-- fastRead is ~3000x faster than read
fastRead :: String -> Int
fastRead = foldr (\c r -> r + ord c - ord '0') 0

fastReadInts :: Int -> Int
fastReadInts n = foldr (+) 0 $ map fastRead $ ints ++ ints
  where
    ints = mapM (const "0123456789") [1..n]

vMesh :: Int -> Int
vMesh n = v $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

eMesh :: Int -> Int
eMesh n = e $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

vIntMesh :: Int -> Int
vIntMesh n = vInt $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

eIntMesh :: Int -> Int
eIntMesh n = eInt $ gmap (\(x, y) -> x * n + y) $ path [1..n] `box` path [1..n]

vIntClique :: Int -> Int
vIntClique n = vInt $ clique [1..n]

eIntClique :: Int -> Int
eIntClique n = eInt $ clique [1..n]

main :: IO ()
main = defaultMain
    [ bgroup "vDeBruijn"
        [ bench "10^1" $ whnf vDeBruijn 1
        , bench "10^2" $ whnf vDeBruijn 2
        , bench "10^3" $ whnf vDeBruijn 3
        , bench "10^4" $ whnf vDeBruijn 4
        , bench "10^5" $ whnf vDeBruijn 5
        , bench "10^6" $ whnf vDeBruijn 6 ]
    , bgroup "eDeBruijn"
        [ bench "10^1" $ whnf eDeBruijn 1
        , bench "10^2" $ whnf eDeBruijn 2
        , bench "10^3" $ whnf eDeBruijn 3
        , bench "10^4" $ whnf eDeBruijn 4
        , bench "10^5" $ whnf eDeBruijn 5
        , bench "10^6" $ whnf eDeBruijn 6 ]
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
    , bgroup "eMesh"
        [ bench "1x1"       $ whnf eMesh 1
        , bench "10x10"     $ whnf eMesh 10
        , bench "100x100"   $ whnf eMesh 100
        , bench "1000x1000" $ whnf eMesh 1000 ]
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
    , bgroup "vIntClique"
        [ bench "1"      $ nf vIntClique 1
        , bench "10"     $ nf vIntClique 10
        , bench "100"    $ nf vIntClique 100
        , bench "1000"   $ nf vIntClique 1000
        , bench "10000"  $ nf vIntClique 10000
        , bench "44722"  $ nf vIntClique 44722 ]
    , bgroup "eIntClique"
        [ bench "1"      $ nf eIntClique 1
        , bench "10"     $ nf eIntClique 10
        , bench "100"    $ nf eIntClique 100
        , bench "1000"   $ nf eIntClique 1000
        , bench "10000"  $ nf eIntClique 10000
        , bench "44722"  $ nf eIntClique 44722 ] ]
