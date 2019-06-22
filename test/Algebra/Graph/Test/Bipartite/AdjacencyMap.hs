-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Bipartite.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Bipartite.AdjacencyMap (
    -- * Testsuite
    testBipartiteAdjacencyMap
    ) where

import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Test

import qualified Algebra.Graph              as G
import qualified Algebra.Graph.AdjacencyMap as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Data.Tuple

import Data.Either (lefts, rights)
import Data.List   (nub)

type GII  = G.Graph (Either Int Int)
type AII  = AM.AdjacencyMap (Either Int Int)
type BAII = AdjacencyMap Int Int
type BAIS = AdjacencyMap Int String

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistent empty                                                         == True" $
        consistent (empty :: BAII)
    test "consistent (vertex x)                                                    == True" $ \x ->
        consistent (vertex x :: BAII)
    test "consistent (edge x y)                                                    == True" $ \x y ->
        consistent (edge x y :: BAII)
    test "consistent (edges x)                                                     == True" $ \x ->
        consistent (edges x :: BAII)
    test "consistent (fromGraph x)                                                 == True" $ \x ->
        consistent $ fromGraph (x :: GII)
    test "consistent (toBipartite x)                                               == True" $ \x ->
        consistent $ toBipartite (x :: AII)
    test "consistent (swap x)                                                      == True" $ \x ->
        consistent $ swap (x :: BAII)
    test "consistent (fromGraph (G.biclique (map Left [1..x]) (map Right [1..y]))) == True" $ \x y ->
        consistent $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)])

    putStrLn "\n============ Bipartite.AdjacencyMap.toBipartite ============"
    test "leftAdjacencyMap (toBipartite empty)                                                                                                   == Map.empty" $
        (leftAdjacencyMap $ toBipartite (AM.empty :: AII)) == Map.empty
    test "rightAdjacencyMap (toBipartite empty)                                                                                                  == Map.empty" $
        (rightAdjacencyMap $ toBipartite (AM.empty :: AII)) == Map.empty
    test "leftAdjacencyMap (toBipartite (vertex (Left 1)))                                                                                       == Map.singleton 1 Set.empty" $
        (leftAdjacencyMap $ toBipartite (AM.vertex (Left 1) :: AII)) == Map.singleton 1 Set.empty
    test "rightAdjacencyMap (toBipartite (vertex (Left 1)))                                                                                      == Map.empty" $
        (rightAdjacencyMap $ toBipartite (AM.vertex (Left 1) :: AII)) == Map.empty
    test "leftAdjacencyMap (toBipartite (vertex (Right 1)))                                                                                      == Map.empty" $
        (leftAdjacencyMap $ toBipartite (AM.vertex (Right 1) :: AII)) == Map.empty
    test "rightAdjacencyMap (toBipartite (vertex (Right 1)))                                                                                     == Map.singleton 1 Set.empty" $
        (rightAdjacencyMap $ toBipartite (AM.vertex (Right 1) :: AII)) == Map.singleton 1 Set.empty
    test "leftAdjacencyMap (toBipartite (edge (Left 1) (Right 2)))                                                                               == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ toBipartite (AM.edge (Left 1) (Right 2) :: AII)) == Map.singleton 1 (Set.singleton 2)
    test "rightAdjacencyMap (toBipartite (edge (Left 1) (Right 2)))                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ toBipartite (AM.edge (Left 1) (Right 2) :: AII)) == Map.singleton 2 (Set.singleton 1)
    test "leftAdjacencyMap (toBipartite (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                          == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 2)
    test "rightAdjacencyMap (toBipartite (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                         == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
          == Map.singleton 2 (Set.singleton 1)
    test "leftAdjacencyMap (toBipartite (edge (Left 1) (Right 1)))                                                                               == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ toBipartite (AM.edge (Left 1) (Right 1) :: AII)) == Map.singleton 1 (Set.singleton 1)
    test "rightAdjacencyMap (toBipartite (edge (Left 1) (Right 2)))                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ toBipartite (AM.edge (Left 1) (Right 1) :: AII)) == Map.singleton 1 (Set.singleton 1)
    test "leftAdjacencyMap (toBipartite (edges [(Left 1, Right 1), (Right 1, Left 1)]))                                                          == Map.singleton 1 (Set.singleton 1)" $
        (leftAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 1)
    test "rightAdjacencyMap (toBipartite (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                         == Map.singleton 1 (Set.singleton 1)" $
        (rightAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 1)
    test "leftAdjacencyMap (toBipartite (edges [(Left 1, Right 1), (Left 1, Right 2)]))                                                          == Map.singleton 1 (Set.fromAscList [1, 2])" $
        (leftAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightAdjacencyMap (toBipartite (edges [(Left 1, Right 1), (Left 1, Right 2)]))                                                         == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        (rightAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "leftAdjacencyMap (toBipartite (edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)]))  == <correct result>" $
        (leftAdjacencyMap $ toBipartite (AM.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]
    test "rightAdjacencyMap (toBipartite (edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)])) == <correct result>" $
        (rightAdjacencyMap $ toBipartite (AM.edges ([(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]
    test "leftAdjacencyMap (toBipartite (biclique (map Left [1..x]) (map Right [1..y])))                                                         == <correct result>" $ \x y ->
        (leftAdjacencyMap $ toBipartite $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap x y
    test "rightAdjacencyMap (toBipartite (biclique (map Left [1..x]) (map Right [1..y])))                                                        == <correct result>" $ \x y ->
        (rightAdjacencyMap $ toBipartite $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromGraph ============"
    test "leftAdjacencyMap (fromGraph empty)                                                                                                   == Map.empty" $
        (leftAdjacencyMap $ fromGraph (G.empty :: GII)) == Map.empty
    test "rightAdjacencyMap (fromGraph empty)                                                                                                  == Map.empty" $
        (rightAdjacencyMap $ fromGraph (G.empty :: GII)) == Map.empty
    test "leftAdjacencyMap (fromGraph (Vertex (Left 1)))                                                                                       == Map.singleton 1 Set.empty" $
        (leftAdjacencyMap $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.singleton 1 Set.empty
    test "rightAdjacencyMap (fromGraph (Vertex (Left 1)))                                                                                      == Map.empty" $
        (rightAdjacencyMap $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.empty
    test "leftAdjacencyMap (fromGraph (Vertex (Right 1)))                                                                                      == Map.empty" $
        (leftAdjacencyMap $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.empty
    test "rightAdjacencyMap (fromGraph (Vertex (Right 1)))                                                                                     == Map.singleton 1 Set.empty" $
        (rightAdjacencyMap $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.singleton 1 Set.empty
    test "leftAdjacencyMap (fromGraph (edge (Left 1) (Right 2)))                                                                               == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ fromGraph (G.edge (Left 1) (Right 2) :: GII)) == Map.singleton 1 (Set.singleton 2)
    test "rightAdjacencyMap (fromGraph (edge (Left 1) (Right 2)))                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ fromGraph (G.edge (Left 1) (Right 2) :: GII)) == Map.singleton 2 (Set.singleton 1)
    test "leftAdjacencyMap (fromGraph (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                          == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 2)
    test "rightAdjacencyMap (fromGraph (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                         == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
          == Map.singleton 2 (Set.singleton 1)
    test "leftAdjacencyMap (fromGraph (edge (Left 1) (Right 1)))                                                                               == Map.singleton 1 (Set.singleton 2)" $
        (leftAdjacencyMap $ fromGraph (G.edge (Left 1) (Right 1) :: GII)) == Map.singleton 1 (Set.singleton 1)
    test "rightAdjacencyMap (fromGraph (edge (Left 1) (Right 2)))                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightAdjacencyMap $ fromGraph (G.edge (Left 1) (Right 1) :: GII)) == Map.singleton 1 (Set.singleton 1)
    test "leftAdjacencyMap (fromGraph (edges [(Left 1, Right 1), (Right 1, Left 1)]))                                                          == Map.singleton 1 (Set.singleton 1)" $
        (leftAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 1)
    test "rightAdjacencyMap (fromGraph (edges [(Left 1, Right 2), (Right 2, Left 1)]))                                                         == Map.singleton 1 (Set.singleton 1)" $
        (rightAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 1)
    test "leftAdjacencyMap (fromGraph (edges [(Left 1, Right 1), (Left 1, Right 2)]))                                                          == Map.singleton 1 (Set.fromAscList [1, 2])" $
        (leftAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightAdjacencyMap (fromGraph (edges [(Left 1, Right 1), (Left 1, Right 2)]))                                                         == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        (rightAdjacencyMap $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "leftAdjacencyMap (fromGraph (edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)]))  == <correct result>" $
        (leftAdjacencyMap $ fromGraph $ (G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]
    test "rightAdjacencyMap (fromGraph (edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)])) == <correct result>" $
        (rightAdjacencyMap $ fromGraph $ (G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: GII))
            == Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]
    test "leftAdjacencyMap (fromGraph (biclique (map Left [1..x]) (map Right [1..y])))                                                         == <correct result>" $ \x y ->
        (leftAdjacencyMap $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap x y
    test "rightAdjacencyMap (fromGraph (biclique (map Left [1..x]) (map Right [1..y])))                                                        == <correct result>" $ \x y ->
        (rightAdjacencyMap $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromBipartite ============"
    test "fromBipartite empty                                                            == AM.empty" $
        fromBipartite (empty :: BAII) == AM.empty
    test "fromBipartite (leftVertex 1)                                                   == AM.vertex (Left 1)" $
        fromBipartite (leftVertex 1 :: BAII) == AM.vertex (Left 1)
    test "fromBipartite (rightVertex 1)                                                  == AM.vertex (Right 1)" $
        fromBipartite (rightVertex 1 :: BAII) == (AM.vertex (Right 1))
    test "fromBipartite (edge x y)                                                       == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \x y ->
        fromBipartite (edge x y :: BAII) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "fromBipartite (toBipartite (AM.edges [(Left x, Right y), (Right y, Left x)]))  == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \x y ->
        (fromBipartite $ toBipartite (AM.edges [(Left x, Right y), (Right y, Left x)] :: AII)) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "fromBipartite (edges [(1, 1), (1, 2)])  == <correct result>" $
        fromBipartite (edges [(1, 1), (1, 2)] :: BAII)
            == (AM.edges [(Left 1, Right 1), (Left 1, Right 2), (Right 1, Left 1), (Right 2, Left 1)])
    test "AM.consistent (fromBipartite x)                                                == True" $ \x ->
        AM.consistent $ fromBipartite (x :: BAII)
    test "fromBipartite (toBipartite (AM.biclique (map Left [1..x]) (map Right [1..y]))) == <correct result>" $ \x y ->
        (fromBipartite $ toBipartite $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
         == AM.overlay (AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
                       (AM.biclique (map Right [1..(y :: Int)]) (map Left [1..(x :: Int)]))

    putStrLn "\n============ Bipartite.AdjacencyMap.hasEdge ============"
    test "hasEdge x y (fromGraph empty)                                 == False" $ \x y ->
        not $ hasEdge (x :: Int) (y :: Int) $ fromGraph G.empty
    test "hasEdge x y (fromGraph (edge (Left x) (Right y)))             == True" $ \x y ->
        hasEdge (x :: Int) (y :: Int) $ fromGraph $ G.edge (Left x) (Right y)
    test "hasEdge 1 2 (fromGraph (edge (Left 1) (Left 2)))              == False" $
        not $ hasEdge 1 1 $ fromGraph $ (G.edge (Left 1) (Left 2) :: GII)
    test "hasEdge 2 3 (fromGraph (edge (Left 1) (Right 2)))             == False" $
        not $ hasEdge 2 3 $ fromGraph (G.edges [(Left 1, Right 2)] :: GII)
    test "hasEdge x y (fromGraph (Overlay z (edge (Left x) (Right y)))) == True" $ \z x y ->
        hasEdge x y $ fromGraph $ G.Overlay z $ G.edge (Left (x :: Int)) (Right (y :: Int))

    putStrLn "\n============ Bipartite.AdjacencyMap.empty ============"
    test "isEmpty empty     == True" $
        isEmpty empty
    test "hasVertex x empty == False" $ \x ->
        not $ hasVertex x (empty :: BAII)
    test "hasEdge x y empty == False" $ \x y ->
        not $ hasEdge x y (empty :: BAII)
    test "vertexCount empty == 0" $
        vertexCount (empty :: BAII) == 0
    test "edgeCount empty   == 0" $
        edgeCount   (empty :: BAII) == 0

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertex ============"
    test "leftAdjacencyMap (leftVertex 1)  == Map.singleton 1 Set.empty" $
        leftAdjacencyMap (leftVertex 1 :: BAII)  == Map.singleton 1 Set.empty
    test "rightAdjacencyMap (leftVertex 1) == Map.empty" $
        rightAdjacencyMap (leftVertex 1 :: BAII) == Map.empty
    test "hasEdge x y (leftVertex x)       == False" $ \x y ->
        not $ hasEdge x y (leftVertex x :: BAII)
    test "hasLeftVertex 1 (leftVertex 1)   == True" $
        hasLeftVertex 1 (leftVertex 1 :: BAII)
    test "hasRightVertex 1 (leftVertex 1)  == False" $
        not $ hasRightVertex 1 (leftVertex 1 :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertex ============"
    test "leftAdjacencyMap (rightVertex 1)  == Map.empty" $
        leftAdjacencyMap (rightVertex 1 :: BAII)  == Map.empty
    test "rightAdjacencyMap (rightVertex 1) == Map.singleton 1 Set.empty" $
        rightAdjacencyMap (rightVertex 1 :: BAII) == Map.singleton 1 Set.empty
    test "hasEdge x y (rightVertex y)       == False" $ \x y ->
        not $ hasEdge x y (rightVertex y :: BAII)
    test "hasLeftVertex 1 (rightVertex 1)   == False" $
        not $ hasLeftVertex 1 (rightVertex 1 :: BAII)
    test "hasRightVertex 1 (rightVertex 1)  == True" $
        hasRightVertex 1 (rightVertex 1 :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.vertex ============"
    test "vertex (Left 1)                == leftVertex 1" $
        vertex (Left 1)  == (leftVertex 1 :: BAII)
    test "vertex (Right 1)               == rightVertex 1" $
        vertex (Right 1) == (rightVertex 1 :: BAII)
    test "hasEdge x y (vertex (Left x))  == False" $ \x y ->
        not $ hasEdge x y (vertex (Left x) :: BAII)
    test "hasEdge x y (vertex (Right y)) == False" $ \x y ->
        not $ hasEdge x y (vertex (Right y) :: BAII)
    test "vertex (Left 1)                == leftVertex 1" $
        vertex (Left 1)  == (leftVertex 1 :: BAII)
    test "vertex (Right 1)               == rightVertex 1" $
        vertex (Right 1) == (rightVertex 1 :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.edge ============"
    test "leftAdjacencyMap (edge x y)    == Map.singleton x (Set.singleton y)" $ \x y ->
        leftAdjacencyMap (edge x y :: BAII)    == Map.singleton x (Set.singleton y)
    test "rightAdjacencyMap (edge x y)   == Map.singleton y (Set.singleton x)" $ \x y ->
        rightAdjacencyMap (edge x y :: BAII)   == Map.singleton y (Set.singleton x)
    test "hasEdge x y (edge x y)         == True" $ \x y ->
        hasEdge x y (edge x y :: BAII)
    test "hasEdge y x (edge x y)         == (x == y)" $ \x y ->
        hasEdge y x (edge x y :: BAII)         == (x == y)
    test "leftAdjacencyMap (edge 1 \"a\")  == Map.singleton 1 (Set.singleton \"a\")" $
        leftAdjacencyMap (edge 1 "a" :: BAIS)  == Map.singleton 1 (Set.singleton "a")
    test "rightAdjacencyMap (edge 1 \"a\") == Map.singleton \"a\" (Set.singleton 1)" $
        rightAdjacencyMap (edge 1 "a" :: BAIS) == Map.singleton "a" (Set.singleton 1)

    putStrLn "\n============ Bipartite.AdjacencyMap.overlay ============"
    test "overlay (leftVertex 1) (rightVertex 2) == vertices [1] [2]" $
        overlay (leftVertex 1) (rightVertex 2)        == (vertices [1] [2] :: BAII)
    test "overlay (leftVertex 1) (rightVertex 1) == vertices [1] [1]" $
        overlay (leftVertex 1) (rightVertex 1)        == (vertices [1] [1] :: BAII)
    test "isEmpty     (overlay x y)              == isEmpty   x   && isEmpty   y" $ \x y ->
        isEmpty     (overlay (x :: BAII) (y :: BAII)) == (isEmpty   x   && isEmpty   y)
    test "hasVertex z (overlay x y)              == hasVertex z x || hasVertex z y" $ \x y z ->
        hasVertex z (overlay (x :: BAII) (y :: BAII)) == hasVertex z x || hasVertex z y
    test "vertexCount (overlay x y)              >= vertexCount x" $ \x y ->
        vertexCount (overlay (x :: BAII) (y :: BAII)) >= vertexCount x
    test "vertexCount (overlay x y)              <= vertexCount x + vertexCount y" $ \x y ->
        vertexCount (overlay (x :: BAII) (y :: BAII)) <= vertexCount x + vertexCount y
    test "edgeCount   (overlay x y)              >= edgeCount x" $ \x y ->
        edgeCount   (overlay (x :: BAII) (y :: BAII)) >= edgeCount x
    test "edgeCount   (overlay x y)              <= edgeCount x   + edgeCount y" $ \x y ->
        edgeCount   (overlay (x :: BAII) (y :: BAII)) <= edgeCount x   + edgeCount y
    test "hasEdge x y (overlay (edge x y) z)     == True" $ \x y z ->
        hasEdge x y (overlay (edge x y) z :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.connect ============"
    test "connect (leftVertex 1) (rightVertex 2) == edge 1 2" $
        connect (leftVertex 1) (rightVertex 2) == (edge 1 2 :: BAII)
    test "connect (leftVertex 1) (rightVertex 1) == edge 1 1" $
        connect (leftVertex 1) (rightVertex 1) == (edge 1 1 :: BAII)
    test "connect (leftVertex 1) (leftVertex 2)  == vertices [1, 2] []" $
        connect (leftVertex 1) (leftVertex 2) == (vertices [1, 2] [] :: BAII)
    test "connect (vertices [1] [4]) (vertices [2] [3]) == edges [(1, 3), (2, 4)]" $
        connect (vertices [1] [4] :: BAII) (vertices [2] [3] :: BAII) == edges [(1, 3), (2, 4)]
    test "isEmpty     (connect x y) == isEmpty x && isEmpty y" $ \x y ->
        isEmpty     (connect (x :: BAII) (y :: BAII)) == (isEmpty x && isEmpty y)
    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \x y z ->
        hasVertex z (connect (x :: BAII) (y :: BAII)) == (hasVertex z x || hasVertex z y)
    test "vertexCount (connect x y) >= vertexCount x" $ \x y ->
        vertexCount (connect (x :: BAII) (y :: BAII)) >= vertexCount x
    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \x y ->
        vertexCount (connect (x :: BAII) (y :: BAII)) <= vertexCount x + vertexCount y
    test "vertexCount (connect x y) == vertexCount (overlay x y)" $ \x y ->
        vertexCount (connect (x :: BAII) (y :: BAII)) == vertexCount (overlay x y)
    test "edgeCount   (connect x y) >= edgeCount x" $ \x y ->
        edgeCount   (connect (x :: BAII) (y :: BAII)) >= edgeCount x
    test "edgeCount   (connect x y) >= leftVertexCount x * rightVertexCount y" $ \x y ->
        edgeCount   (connect (x :: BAII) (y :: BAII)) >= leftVertexCount x * rightVertexCount y
    test "edgeCount   (connect x y) <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y" $ \x y ->
        edgeCount   (connect (x :: BAII) (y :: BAII)) <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y

    putStrLn "\n============ Bipartite.AdjacencyMap.vertices ============"
    test "vertices [] []                 == empty" $
        vertices [] []  == (empty :: BAII)
    test "vertices [1] []                == leftVertex 1" $
        vertices [1] [] == (leftVertex 1 :: BAII)
    test "vertices [] [1]                == rightVertex 1" $
        vertices [] [1] == (rightVertex 1 :: BAII)
    test "hasEdge x y (vertices [x] [y]) == False" $ \x y ->
        not $ hasEdge x y (vertices [x] [y] :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.edges ============"
    test "edges []                                   == empty" $
        edges []                                     == (empty :: BAII)
    test "leftAdjacencyMap (edges [(1, 1), (1, 2)])  == Map.singleton 1 (Set.fromAscList [1, 2])" $
        leftAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightAdjacencyMap (edges [(1, 1), (1, 2)]) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        rightAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "edges [(x, y)]                             == edge x y" $ \x y ->
        edges [(x, y)]                               == (edge x y :: BAII)
    test "(edgeCount . edges) x                      == (length . nub) x" $ \x ->
        (edgeCount . edges) x                        == (length . nub) (x :: [(Int, Int)])
    test "hasEdge x y (edges [(x, y)])               == True" $ \x y ->
        hasEdge x y (edges [(x, y)] :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.overlays ============"
    test "overlays []           == empty" $
        overlays []                   == (empty :: BAII)
    test "overlays [x]          == x" $ \x ->
        overlays [x]                  == (x :: BAII)
    test "overlays [x, y]       == overlay x y" $ \x y ->
        overlays [x, y]               == (overlay x y :: BAII)
    test "overlays xs           == foldr overlay empty xs" $ \xs ->
        overlays xs                   == (foldr overlay empty xs :: BAII)
    test "isEmpty (overlays xs) == all isEmpty xs" $ \xs ->
        isEmpty (overlays xs :: BAII) == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.connects ============"
    test "connects []           == empty" $
        connects []                   == (empty :: BAII)
    test "connects [x]          == x" $ \x ->
        connects [x]                  == (x :: BAII)
    test "connects [x, y]       == connect x y" $ \x y ->
        connects [x, y]               == (connect x y :: BAII)
    test "connects xs           == foldr connect empty xs" $ \xs ->
        connects xs                   == (foldr connect empty xs :: BAII)
    test "isEmpty (connects xs) == all isEmpty xs" $ \xs ->
        isEmpty (connects xs :: BAII) == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.isEmpty ============"
    test "isEmpty empty                 == True" $
        isEmpty (empty :: BAII)
    test "isEmpty (overlay empty empty) == True" $
        isEmpty (overlay empty empty :: BAII)
    test "isEmpty (vertex x)            == False" $ \x ->
        not $ isEmpty (vertex x :: BAII)
    test "isEmpty x                     == (x == empty)" $ \x ->
        isEmpty (x :: BAII) == (x == empty)

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexCount ============"
    test "leftVertexCount empty                    == 0" $
        leftVertexCount (empty :: BAII)                  == 0
    test "leftVertexCount (leftVertex 1)           == 1" $
        leftVertexCount (leftVertex 1 :: BAII)           == 1
    test "leftVertexCount (rightVertex (-2))       == 0" $
        leftVertexCount (rightVertex (-2) :: BAII)       == 0
    test "leftVertexCount (edges [(1, 1), (1, 2)]) == 1" $
        leftVertexCount (edges [(1, 1), (1, 2)] :: BAII) == 1
    test "leftVertexCount (edges x)                == (length . nub . map fst) x" $ \x ->
        leftVertexCount (edges x :: BAII)               == (length . nub . map fst) x

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexCount ============"
    test "rightVertexCount empty                    == 0" $
        rightVertexCount (empty :: BAII)                  == 0
    test "rightVertexCount (rightVertex (-2))       == 1" $
        rightVertexCount (rightVertex (-2) :: BAII)       == 1
    test "rightVertexCount (leftVertex 1)           == 0" $
        rightVertexCount (leftVertex 1 :: BAII)           == 0
    test "rightVertexCount (edges [(1, 1), (1, 2)]) == 2" $
        rightVertexCount (edges [(1, 1), (1, 2)] :: BAII) == 2
    test "rightVertexCount (edges x)                == (length . nub . map snd) x" $ \x ->
        rightVertexCount (edges x :: BAII)                == (length . nub . map snd) x

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexCount ============"
    test "vertexCount empty                    == 0" $
        vertexCount (empty :: BAII)                  == 0
    test "vertexCount (leftVertex 1)           == 1" $
        vertexCount (leftVertex 1 :: BAII)           == 1
    test "vertexCount (rightVertex 1)          == 1" $
        vertexCount (rightVertex 1 :: BAII)          == 1
    test "vertexCount (edges [(1, 1), (1, 2)]) == 3" $
        vertexCount (edges [(1, 1), (1, 2)] :: BAII) == 3
    test "vertexCount x                        == leftVertexCount x + rightVertexCount x" $ \x ->
        vertexCount (x :: BAII)                      == leftVertexCount x + rightVertexCount x

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeCount ============"
    test "edgeCount empty      == 0" $
        edgeCount (empty :: BAII)    == 0
    test "edgeCount (vertex x) == 0" $ \x ->
        edgeCount (vertex x :: BAII) == 0
    test "edgeCount (edge 1 2) == 1" $
        edgeCount (edge 1 2 :: BAII) == 1
    test "edgeCount (edge 1 1) == 1" $
        edgeCount (edge 1 1 :: BAII) == 1
    test "edgeCount (edges x) == (length . nub) x" $ \ x ->
        edgeCount (edges x :: BAII)  == (length . nub) x

    putStrLn "\n============ Bipartite.AdjacencyMap.hasLeftVertex ============"
    test "hasLeftVertex x empty           == False" $ \x ->
        not $ hasLeftVertex x (empty :: BAII)
    test "hasLeftVertex x (leftVertex x)  == True" $ \x ->
        hasLeftVertex x (leftVertex x :: BAII)
    test "hasLeftVertex x (rightVertex x) == False" $ \x ->
        not $ hasLeftVertex x (rightVertex x :: BAII)
    test "hasLeftVertex 1 (leftVertex 2)  == False" $
        not $ hasLeftVertex 1 (leftVertex 2 :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.hasRightVertex ============"
    test "hasRightVertex x empty           == False" $ \x ->
        not $ hasRightVertex x (empty :: BAII)
    test "hasRightVertex x (rightVertex x) == True" $ \x ->
        hasRightVertex x (rightVertex x :: BAII) == True
    test "hasRightVertex x (leftVertex x)  == False" $ \x ->
        not $ hasRightVertex x (leftVertex x :: BAII)
    test "hasRightVertex 1 (rightVertex 2) == False" $
        not $ hasRightVertex 1 (rightVertex 2 :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.hasVertex ============"
    test "hasVertex x empty                   == False" $ \x ->
        not $ hasVertex x (empty :: BAII)
    test "hasVertex (Right x) (rightVertex x) == True" $ \x ->
        hasVertex (Right x) (rightVertex x :: BAII)
    test "hasVertex (Right x) (leftVertex x)  == False" $ \x ->
        not $ hasVertex (Right x) (leftVertex x :: BAII)
    test "hasVertex (Left 1) (leftVertex 2)   == False" $
        not $ hasVertex (Left 1) (leftVertex 2 :: BAII)
    test "hasVertex (Left x) y                == hasLeftVertex x y" $ \x y ->
        hasVertex (Left x) (y :: BAII)  == hasLeftVertex x y
    test "hasVertex (Right x) y               == hasRightVertex x y" $ \x y ->
        hasVertex (Right x) (y :: BAII) == hasRightVertex x y

    putStrLn "\n============ Bipartite.AdjacencyMap.swap ============"
    test "swap empty            == empty" $
        swap (empty :: BAII)          == empty
    test "swap (leftVertex 1)   == rightVertex 1" $
        swap (leftVertex 1 :: BAII)   == rightVertex 1
    test "swap (vertices ls rs) == (flip vertices) ls rs" $ \ls rs ->
        swap (vertices ls rs :: BAII) == (flip vertices) ls rs
    test "swap (edge 1 \"a\")     == edge \"a\" 1" $
        swap (edge 1 "a" :: BAIS)     == edge "a" 1
    test "swap (edges x)        == (edges . map Data.Tuple.swap) x" $ \x ->
        swap (edges x :: BAII)       == (edges . map Data.Tuple.swap) x
    test "swap (swap x)        == x" $ \x ->
        swap (swap x :: BAII)       == x

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexList ============"
    test "leftVertexList empty                     == []" $
        leftVertexList (empty :: BAII)                              == []
    test "leftVertexList (leftVertex 1)            == [1]" $
        leftVertexList (leftVertex 1 :: BAII)                       == [1]
    test "leftVertexList (rightVertex 1)           == []" $
        leftVertexList (rightVertex 1 :: BAII)                      == []
    test "(leftVertexList . (flip vertices) []) vs == (nub . sort) vs" $ \vs ->
        (leftVertexList . ((flip vertices) [] :: [Int] -> BAII)) vs == (nub . sort) vs
    test "isSorted (leftVertexList x)              == True" $ \x ->
        isSorted $ leftVertexList (x :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexList ============"
    test "rightVertexList empty              == []" $
        rightVertexList (empty :: BAII)                       == []
    test "rightVertexList (leftVertex 1)     == []" $
        rightVertexList (leftVertex 1 :: BAII)                == []
    test "rightVertexList (rightVertex 1)    == [1]" $
        rightVertexList (rightVertex 1 :: BAII)               == [1]
    test "(rightVertexList . vertices []) vs == (nub . sort) vs" $ \vs ->
        (rightVertexList . (vertices [] :: [Int] -> BAII)) vs == (nub . sort) vs
    test "isSorted (rightVertexList x)       == True" $ \x ->
        isSorted $ rightVertexList (x :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexList ============"
    test "vertexList empty                             == []" $
        vertexList (empty :: BAII)                           == []
    test "vertexList (vertex x)                        == [x]" $ \x ->
        vertexList (vertex x :: BAII)                        == [x]
    test "vertexList (vertices (lefts vs) (rights vs)) == nub (sort vs)" $ \vs ->
        vertexList (vertices (lefts vs) (rights vs) :: BAII) == (nub (sort vs))
    test "isSorted (vertexList x)                      == True" $ \x ->
        isSorted $ vertexList (x :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeList ============"
    test "edgeList empty           == []" $
        edgeList (empty :: BAII)               == []
    test "edgeList (leftVertex 1)  == []" $
        edgeList (leftVertex 1 :: BAII)        == []
    test "edgeList (rightVertex 1) == []" $
        edgeList (rightVertex 1 :: BAII)       == []
    test "edgeList (edge 1 1)      == [(1, 1)]" $
        edgeList (edge 1 1 :: BAII)            == [(1, 1)]
    test "edgeList (edge 1 2)      == [(1, 2)]" $
        edgeList (edge 1 2 :: BAII)            == [(1, 2)]
    test "(edgeList . edges) x     == (nub . sort) x" $ \x ->
        (edgeList . edges) (x :: [(Int, Int)]) == (nub . sort) x

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexSet ============"
    test "leftVertexList empty                     == []" $
        leftVertexList (empty :: BAII)                              == []
    test "leftVertexList (leftVertex 1)            == [1]" $
        leftVertexList (leftVertex 1 :: BAII)                       == [1]
    test "leftVertexList (rightVertex 1)           == []" $
        leftVertexList (rightVertex 1 :: BAII)                      == []
    test "(leftVertexList . (flip vertices) []) vs == (nub . sort) vs" $ \vs ->
        (leftVertexList . ((flip vertices) [] :: [Int] -> BAII)) vs == (nub . sort) vs
    test "isSorted (leftVertexList x)              == True" $ \x ->
        isSorted $ leftVertexList (x :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexSet ============"
    test "rightVertexSet empty              == Set.empty" $
        rightVertexSet (empty :: BAII)                       == Set.empty
    test "rightVertexSet (leftVertex 1)     == Set.singleton 1" $
        rightVertexSet (leftVertex 1 :: BAII)                == Set.empty
    test "rightVertexSet (rightVertex 1)    == Set.empty" $
        rightVertexSet (rightVertex 1 :: BAII)               == Set.singleton 1
    test "(rightVertexSet . vertices []) vs == Set.fromList vs" $ \vs ->
        (rightVertexSet . (vertices [] :: [Int] -> BAII)) vs == Set.fromList vs

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexSet ============"
    test "vertexSet empty                             == Set.empty" $
        vertexSet (empty :: BAII)                           == Set.empty
    test "vertexSet (leftVertex 1)                    == Set.singleton 1" $
        vertexSet (leftVertex 1 :: BAII)                    == Set.singleton (Left 1)
    test "vertexSet (rightVertex 1)                   == Set.empty" $
        vertexSet (rightVertex 1 :: BAII)                   == Set.singleton (Right 1)
    test "vertexSet (vertices (lefts vs) (rights vs)) == Set.fromList vs" $ \vs ->
        vertexSet (vertices (lefts vs) (rights vs) :: BAII) == Set.fromList vs

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeSet ============"
    test "edgeSet empty           == Set.empty" $
        edgeSet (empty :: BAII)         == Set.empty
    test "edgeSet (leftVertex 1)  == Set.empty" $
        edgeSet (leftVertex 1 :: BAII)  == Set.empty
    test "edgeSet (rightVertex 1) == Set.empty" $
        edgeSet (rightVertex 1 :: BAII) == Set.empty
    test "edgeSet (edge 1 1)      == Set.singleton (1, 1)" $
        edgeSet (edge 1 1 :: BAII)      == Set.singleton (1, 1)
    test "edgeSet (edge 1 2)      == Set.singleton (1, 2)" $
        edgeSet (edge 1 2 :: BAII)      == Set.singleton (1, 2)
    test "edgeSet (edges x)       == Set.fromList x" $ \x ->
        edgeSet (edges x :: BAII)       == Set.fromList x

    putStrLn "\n============ Num (Bipartite.AdjacencyMap a b) ============"
    test "0                         == rightVertex 0" $
        (0 :: BAII)                       == rightVertex 0
    test "swap 1                    == leftVertex 1" $
        (swap 1 :: BAII)                  == leftVertex 1
    test "(swap 1) + 2              == vertices [1] [2]" $
        ((swap 1) + 2 :: BAII)            == vertices [1] [2]
    test "(swap 1) + 2 * (swap 3)   == overlay (leftVertex 1) (edge 3 2)" $
        ((swap 1) + 2 * (swap 3) :: BAII) == overlay (leftVertex 1) (edge 3 2)
    test "(swap 1) * (2 + (swap 3)) == connect (leftVertex 1) (vertices [3] [2])" $
        (swap 1) * (2 + (swap 3) :: BAII) == connect (leftVertex 1) (vertices [3] [2])

    putStrLn "\n============ Eq (Bipartite.AdjacencyMap a b) ============"
    test "      x + y == y + x" $ \x y ->
        (x + y :: BAII) == y + x
    test "x + (y + z) == (x + y) + z" $ \x y z ->
        (x + (y + z) :: BAII) == (x + y) + z
    test "  x * empty == x" $ \x ->
        (x * empty :: BAII) == x
    test "  empty * x == x" $ \x ->
        (empty * x :: BAII) == x
    test "      x * y == y * x" $ \x y ->
        (x * y :: BAII) == y * x
    test "x * (y * z) == (x * y) * z" $ \x y z ->
        (x * (y * z) :: BAII) == (x * y) * z
    test "x * (y + z) == x * y + x * z" $ \x y z ->
        (x * (y + z) :: BAII) == x * (y + z)
    test "(x + y) * z == x * z + y * z" $ \x y z ->
        ((x + y) * z :: BAII) == x * z + y * z
    test "  x * y * z == x * y + x * z + y * z" $ \x y z ->
        (x * y * z :: BAII) == x * y + x * z + y * z
    test "  (leftVertex x) * (leftVertex y) == (leftVertex x) + (leftVertex y)" $ \x y ->
        ((leftVertex x) * (leftVertex y) :: BAII) == (leftVertex x) + (leftVertex y)
    test "(rightVertex x) * (rightVertex y) == (rightVertex x) + (rightVertex y)" $ \x y ->
        ((rightVertex x) * (rightVertex y) :: BAII) == (rightVertex x) + (rightVertex y)
    test "  x + empty == x" $ \x ->
        (x + empty :: BAII) == x
    test "  empty + x == x" $ \x ->
        (empty + x :: BAII) == x
    test "      x + x == x" $ \x ->
        (x + x :: BAII) == x
    test "x * y + x + y == x * y" $ \x y ->
        (x * y + x + y :: BAII) == x * y
    test "    x * x * x == x * x" $ \x ->
        (x * x * x :: BAII) == x * x

    putStrLn "\n============ Show (Bipartite.AdjacencyMap a b) ============"
    test "show (empty)                       == \"empty\"" $
        show (empty :: BAII)                       == "empty"
    test "show 1                             == \"rightVertex 1\"" $
        show (1 :: BAII)                           == "rightVertex 1"
    test "show (swap 2)                      == \"leftVertex 2\"" $
        show (swap 2 :: BAII)                      == "leftVertex 2"
    test "show 1 + 2                         == \"vertices [] [1,2]\"" $
        show (1 + 2 :: BAII)                       == "vertices [] [1,2]"
    test "show (swap (1 + 2))                == \"vertices [1,2] []\"" $
        show (swap (1 + 2) :: BAII)                == "vertices [1,2] []"
    test "show ((swap 1) * 2)                == \"edge 1 2\"" $
        show ((swap 1) * 2 :: BAII)                == "edge 1 2"
    test "show ((swap 1) * 2 * (swap 3))     == \"edges [(1,2),(3,2)]\"" $
        show ((swap 1) * 2 * (swap 3) :: BAII)     == "edges [(1,2),(3,2)]"
    test "show ((swap 1) * 2 + (swap 3))     == \"overlay (leftVertex 3) (edge 1 2)\"" $
        show ((swap 1) * 2 + (swap 3) :: BAII)     == "overlay (leftVertex 3) (edge 1 2)"
    test "show ((swap 1) * 2 + (swap 3) + 4) == \"overlay (vertices [3] [4]) (edge 1 2)\"" $
        show ((swap 1) * 2 + (swap 3) + 4 :: BAII) == "overlay (vertices [3] [4]) (edge 1 2)"

expectedBicliqueMap :: Int -> Int -> Map.Map Int (Set.Set Int)
expectedBicliqueMap n m = Map.fromAscList [ (u, Set.fromAscList [1..m]) | u <- [1..n] ]

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs $ tail xs
