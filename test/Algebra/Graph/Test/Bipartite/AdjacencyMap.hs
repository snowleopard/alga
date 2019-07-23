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

import Data.Bifunctor (bimap)
import Data.Either    (lefts, rights, isRight)
import Data.List      (nub)

type GII  = G.Graph (Either Int Int)
type AII  = AM.AdjacencyMap (Either Int Int)
type AI   = AM.AdjacencyMap Int
type BAII = AdjacencyMap Int Int
type BAIS = AdjacencyMap Int String

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistent empty            == True" $
        consistent (empty :: BAII)
    test "consistent (vertex x)       == True" $ \x ->
        consistent (vertex x :: BAII)
    test "consistent (edge x y)       == True" $ \(x :: Int) (y :: Int) ->
        consistent (edge x y)
    test "consistent (edges x)        == True" $ \(x :: [(Int, Int)]) ->
        consistent (edges x)
    test "consistent (fromGraph x)    == True" $ \(x :: GII) ->
        consistent $ fromGraph x
    test "consistent (toBipartite x)  == True" $ \(x :: AII) ->
        consistent $ toBipartite x
    test "consistent (swap x)         == True" $ \(x :: BAII) ->
        consistent $ swap x
    test "consistent (biclique xs ys) == True" $ \(xs :: [Int]) (ys :: [Int]) ->
        consistent $ biclique xs ys
    test "consistent (circuit xs)     == True" $ \(xs :: [(Int, Int)]) ->
        consistent $ circuit xs

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
    test "leftAdjacencyMap (toBipartite (biclique (map Left [1..x]) (map Right [1..y])))                                                         == <correct result>" $ \(x :: Int) (y :: Int) ->
        (leftAdjacencyMap $ toBipartite $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap x y
    test "rightAdjacencyMap (toBipartite (biclique (map Left [1..x]) (map Right [1..y])))                                                        == <correct result>" $ \(x :: Int) (y :: Int) ->
        (rightAdjacencyMap $ toBipartite $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.toBipartiteWith ============"
    test "toBipartiteWith parity empty           == empty" $
        toBipartiteWith parity AM.empty                 == empty
    test "toBipartiteWith Left g                 == vertices (vertexList g) []" $ \(g :: AI) ->
        toBipartiteWith Left g                          == (vertices (AM.vertexList g) [] :: BAII)
    test "toBipartiteWith Right g                == vertices [] (vertexList g)" $ \(g :: AI) ->
        toBipartiteWith Right g                         == (vertices [] (AM.vertexList g) :: BAII)
    test "toBipartiteWith parity (clique [1..3]) == biclique [1, 3] [2]" $
        toBipartiteWith parity (AM.clique [1..3] :: AI) == biclique [1, 3] [2]
    test "toBipartiteWith parity (edge 1 1)      == leftVertex 1" $
        toBipartiteWith parity (AM.edge 1 1)            == leftVertex 1
    test "toBipartiteWith id g                   == toBipartite g" $ \(g :: AII) ->
        toBipartiteWith id g                            == toBipartite g

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
    test "leftAdjacencyMap (fromGraph (biclique (map Left [1..x]) (map Right [1..y])))                                                         == <correct result>" $ \(x :: Int) (y :: Int) ->
        (leftAdjacencyMap $ fromGraph $ G.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap x y
    test "rightAdjacencyMap (fromGraph (biclique (map Left [1..x]) (map Right [1..y])))                                                        == <correct result>" $ \(x :: Int) (y :: Int) ->
        (rightAdjacencyMap $ fromGraph $ G.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromBipartite ============"
    test "fromBipartite empty                                                            == AM.empty" $
        fromBipartite (empty :: BAII) == AM.empty
    test "fromBipartite (leftVertex 1)                                                   == AM.vertex (Left 1)" $
        fromBipartite (leftVertex 1 :: BAII) == AM.vertex (Left 1)
    test "fromBipartite (rightVertex 1)                                                  == AM.vertex (Right 1)" $
        fromBipartite (rightVertex 1 :: BAII) == (AM.vertex (Right 1))
    test "fromBipartite (edge x y)                                                       == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \(x :: Int) (y :: Int) ->
        fromBipartite (edge x y) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "fromBipartite (toBipartite (AM.edges [(Left x, Right y), (Right y, Left x)]))  == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \(x :: Int) (y :: Int) ->
        (fromBipartite $ toBipartite (AM.edges [(Left x, Right y), (Right y, Left x)])) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "fromBipartite (edges [(1, 1), (1, 2)])  == <correct result>" $
        fromBipartite (edges [(1, 1), (1, 2)] :: BAII)
            == (AM.edges [(Left 1, Right 1), (Left 1, Right 2), (Right 1, Left 1), (Right 2, Left 1)])
    test "AM.consistent (fromBipartite x)                                                == True" $ \x ->
        AM.consistent $ fromBipartite (x :: BAII)
    test "fromBipartite (toBipartite (AM.biclique (map Left [1..x]) (map Right [1..y]))) == <correct result>" $ \(x :: Int) (y :: Int) ->
        (fromBipartite $ toBipartite $ AM.biclique (map Left [1..x]) (map Right [1..y]))
         == AM.overlay (AM.biclique (map Left [1..x]) (map Right [1..y]))
                       (AM.biclique (map Right [1..y]) (map Left [1..x]))

    putStrLn "\n============ Bipartite.AdjacencyMap.hasEdge ============"
    test "hasEdge x y empty                                == False" $ \(x :: Int) (y :: Int) ->
        not $ hasEdge x y empty
    test "hasEdge x y (edge x y)                           == True" $ \(x :: Int) (y :: Int) ->
        hasEdge x y $ edge x y
    test "hasEdge 1 2 (fromGraph (edge (Left 1) (Left 2))) == False" $
        not $ hasEdge 1 2 $ fromGraph $ (G.edge (Left 1) (Left 2) :: GII)
    test "hasEdge 2 3 (edge 1 2)                           == False" $
        not $ hasEdge 2 3 $ (edge 1 2 :: BAII)
    test "hasEdge x y (overlay z (edge x y))               == True" $ \(z :: BAII) (x :: Int) (y :: Int) ->
        hasEdge x y $ overlay z $ edge x y

    putStrLn "\n============ Bipartite.AdjacencyMap.leftAdjacencyMap ============"
    test "leftAdjacencyMap empty                    == Map.empty" $
        leftAdjacencyMap (empty :: BAII)                  == Map.empty
    test "leftAdjacencyMap (leftVertex 1)           == Map.singleton 1 Set.empty" $
        leftAdjacencyMap (leftVertex 1 :: BAII)           == Map.singleton 1 Set.empty
    test "leftAdjacencyMap (rightVertex 1)          == Map.empty" $
        leftAdjacencyMap (rightVertex 1 :: BAII)          == Map.empty
    test "leftAdjacencyMap (edge 1 1)               == Map.singleton 1 (Set.singleton 1)" $
        leftAdjacencyMap (edge 1 1 :: BAII)               == Map.singleton 1 (Set.singleton 1)
    test "leftAdjacencyMap (edge 1 \"a\")           == Map.singleton 1 (Set.singleton \"a\")" $
        leftAdjacencyMap (edge 1 "a" :: BAIS)             == Map.singleton 1 (Set.singleton "a")
    test "leftAdjacencyMap (edges [(1, 1), (1, 2)]) == Map.singleton 1 (Set.fromAscList [1, 2])" $
        leftAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.singleton 1 (Set.fromAscList [1, 2])

    putStrLn "\n============ Bipartite.AdjacencyMap.rightAdjacencyMap ============"
    test "rightAdjacencyMap empty                    == Map.empty" $
        rightAdjacencyMap (empty :: BAII)                  == Map.empty
    test "rightAdjacencyMap (leftVertex 1)           == Map.empty" $
        rightAdjacencyMap (leftVertex 1 :: BAII)           == Map.empty
    test "rightAdjacencyMap (rightVertex 1)          == Map.singleton 1 Set.empty" $
        rightAdjacencyMap (rightVertex 1 :: BAII)          == Map.singleton 1 Set.empty
    test "rightAdjacencyMap (edge 1 1)               == Map.singleton 1 (Set.singleton 1)" $
        rightAdjacencyMap (edge 1 1 :: BAII)               == Map.singleton 1 (Set.singleton 1)
    test "rightAdjacencyMap (edge 1 \"a\")           == Map.singleton \"a\" (Set.singleton 1)" $
        rightAdjacencyMap (edge 1 "a" :: BAIS)             == Map.singleton "a" (Set.singleton 1)
    test "rightAdjacencyMap (edges [(1, 1), (1, 2)]) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        rightAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]

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
    test "leftAdjacencyMap (edge x y)    == Map.singleton x (Set.singleton y)" $ \(x :: Int) (y :: Int) ->
        leftAdjacencyMap (edge x y)            == Map.singleton x (Set.singleton y)
    test "rightAdjacencyMap (edge x y)   == Map.singleton y (Set.singleton x)" $ \(x :: Int) (y :: Int) ->
        rightAdjacencyMap (edge x y)           == Map.singleton y (Set.singleton x)
    test "hasEdge x y (edge x y)         == True" $ \(x :: Int) (y :: Int) ->
        hasEdge x y (edge x y)
    test "hasEdge y x (edge x y)         == (x == y)" $ \(x :: Int) (y :: Int) ->
        hasEdge y x (edge x y)                 == (x == y)
    test "leftAdjacencyMap (edge 1 \"a\")  == Map.singleton 1 (Set.singleton \"a\")" $
        leftAdjacencyMap (edge 1 "a" :: BAIS)  == Map.singleton 1 (Set.singleton "a")
    test "rightAdjacencyMap (edge 1 \"a\") == Map.singleton \"a\" (Set.singleton 1)" $
        rightAdjacencyMap (edge 1 "a" :: BAIS) == Map.singleton "a" (Set.singleton 1)

    putStrLn "\n============ Bipartite.AdjacencyMap.overlay ============"
    test "overlay (leftVertex 1) (rightVertex 2) == vertices [1] [2]" $
        overlay (leftVertex 1) (rightVertex 2) == (vertices [1] [2] :: BAII)
    test "overlay (leftVertex 1) (rightVertex 1) == vertices [1] [1]" $
        overlay (leftVertex 1) (rightVertex 1) == (vertices [1] [1] :: BAII)
    test "isEmpty     (overlay x y)              == isEmpty   x   && isEmpty   y" $ \(x :: BAII) (y :: BAII) ->
        isEmpty     (overlay x y)              == (isEmpty   x   && isEmpty   y)
    test "hasVertex z (overlay x y)              == hasVertex z x || hasVertex z y" $ \(x :: BAII) (y :: BAII) z ->
        hasVertex z (overlay x y)              == hasVertex z x || hasVertex z y
    test "vertexCount (overlay x y)              >= vertexCount x" $ \(x :: BAII) (y :: BAII) ->
        vertexCount (overlay x y)              >= vertexCount x
    test "vertexCount (overlay x y)              <= vertexCount x + vertexCount y" $ \(x :: BAII) (y :: BAII) ->
        vertexCount (overlay x y)              <= vertexCount x + vertexCount y
    test "edgeCount   (overlay x y)              >= edgeCount x" $ \(x :: BAII) (y :: BAII) ->
        edgeCount   (overlay x y)              >= edgeCount x
    test "edgeCount   (overlay x y)              <= edgeCount x   + edgeCount y" $ \(x :: BAII) (y :: BAII) ->
        edgeCount   (overlay x y)              <= edgeCount x   + edgeCount y
    test "hasEdge x y (overlay (edge x y) z)     == True" $ \(x :: Int) (y :: Int) (z :: BAII) ->
        hasEdge x y (overlay (edge x y) z)

    putStrLn "\n============ Bipartite.AdjacencyMap.connect ============"
    test "connect (leftVertex 1) (rightVertex 2) == edge 1 2" $
        connect (leftVertex 1) (rightVertex 2) == (edge 1 2 :: BAII)
    test "connect (leftVertex 1) (rightVertex 1) == edge 1 1" $
        connect (leftVertex 1) (rightVertex 1) == (edge 1 1 :: BAII)
    test "connect (leftVertex 1) (leftVertex 2)  == vertices [1, 2] []" $
        connect (leftVertex 1) (leftVertex 2) == (vertices [1, 2] [] :: BAII)
    test "connect (vertices [1] [4]) (vertices [2] [3]) == edges [(1, 3), (2, 4)]" $
        connect (vertices [1] [4] :: BAII) (vertices [2] [3] :: BAII) == edges [(1, 3), (2, 4)]
    test "isEmpty     (connect x y) == isEmpty x && isEmpty y" $ \(x :: BAII) (y :: BAII) ->
        isEmpty     (connect x y) == (isEmpty x && isEmpty y)
    test "hasVertex z (connect x y) == hasVertex z x || hasVertex z y" $ \(x :: BAII) (y :: BAII) z ->
        hasVertex z (connect x y) == (hasVertex z x || hasVertex z y)
    test "vertexCount (connect x y) >= vertexCount x" $ \(x :: BAII) (y :: BAII) ->
        vertexCount (connect x y) >= vertexCount x
    test "vertexCount (connect x y) <= vertexCount x + vertexCount y" $ \(x :: BAII) (y :: BAII) ->
        vertexCount (connect x y) <= vertexCount x + vertexCount y
    test "vertexCount (connect x y) == vertexCount (overlay x y)" $ \(x :: BAII) (y :: BAII) ->
        vertexCount (connect x y) == vertexCount (overlay x y)
    test "edgeCount   (connect x y) >= edgeCount x" $ \(x :: BAII) (y :: BAII) ->
        edgeCount   (connect x y) >= edgeCount x
    test "edgeCount   (connect x y) >= leftVertexCount x * rightVertexCount y" $ \(x :: BAII) (y :: BAII) ->
        edgeCount   (connect x y) >= leftVertexCount x * rightVertexCount y
    test "edgeCount   (connect x y) <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y" $ \(x :: BAII) (y :: BAII) ->
        edgeCount   (connect x y) <= leftVertexCount x * rightVertexCount y + rightVertexCount x * leftVertexCount y + edgeCount x + edgeCount y

    putStrLn "\n============ Bipartite.AdjacencyMap.vertices ============"
    test "vertices [] []                    == empty" $
        vertices [] []                    == (empty :: BAII)
    test "vertices [1] []                   == leftVertex 1" $
        vertices [1] []                   == (leftVertex 1 :: BAII)
    test "vertices [] [1]                   == rightVertex 1" $
        vertices [] [1]                   == (rightVertex 1 :: BAII)
    test "hasEdge x y (vertices [x] [y])    == False" $ \(x :: Int) (y :: Int) ->
        not $ hasEdge x y (vertices [x] [y])
    test "hasLeftVertex x (vertices ys zs)  == elem x ys" $ \(x :: Int) (ys :: [Int]) (zs :: [Int]) ->
        hasLeftVertex x (vertices ys zs)  == elem x ys
    test "hasRightVertex x (vertices ys zs) == elem x zs" $ \(x :: Int) (ys :: [Int]) (zs :: [Int]) ->
        hasRightVertex x (vertices ys zs) == elem x zs

    putStrLn "\n============ Bipartite.AdjacencyMap.edges ============"
    test "edges []                                   == empty" $
        edges []                                     == (empty :: BAII)
    test "leftAdjacencyMap (edges [(1, 1), (1, 2)])  == Map.singleton 1 (Set.fromAscList [1, 2])" $
        leftAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightAdjacencyMap (edges [(1, 1), (1, 2)]) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        rightAdjacencyMap (edges [(1, 1), (1, 2)] :: BAII) == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "edges [(x, y)]                             == edge x y" $ \(x :: Int) (y :: Int) ->
        edges [(x, y)]                               == edge x y
    test "(edgeCount . edges) x                      == (length . nub) x" $ \(x :: [(Int, Int)]) ->
        (edgeCount . edges) x                        == (length . nub) x
    test "hasEdge x y (edges [(x, y)])               == True" $ \(x :: Int) (y :: Int) ->
        hasEdge x y (edges [(x, y)])

    putStrLn "\n============ Bipartite.AdjacencyMap.overlays ============"
    test "overlays []           == empty" $
        overlays []           == (empty :: BAII)
    test "overlays [x]          == x" $ \(x :: BAII) ->
        overlays [x]          == x
    test "overlays [x, y]       == overlay x y" $ \(x :: BAII) (y :: BAII) ->
        overlays [x, y]       == overlay x y
    test "overlays xs           == foldr overlay empty xs" $ \(xs :: [BAII]) ->
        overlays xs           == foldr overlay empty xs
    test "isEmpty (overlays xs) == all isEmpty xs" $ \(xs :: [BAII]) ->
        isEmpty (overlays xs) == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.connects ============"
    test "connects []           == empty" $
        connects []           == (empty :: BAII)
    test "connects [x]          == x" $ \(x :: BAII) ->
        connects [x]          == x
    test "connects [x, y]       == connect x y" $ \(x :: BAII) (y :: BAII) ->
        connects [x, y]       == connect x y
    test "connects xs           == foldr connect empty xs" $ \(xs :: [BAII]) ->
        connects xs           == foldr connect empty xs
    test "isEmpty (connects xs) == all isEmpty xs" $ \(xs :: [BAII]) ->
        isEmpty (connects xs) == all isEmpty xs

    putStrLn "\n============ Bipartite.AdjacencyMap.isEmpty ============"
    test "isEmpty empty                 == True" $
        isEmpty (empty :: BAII)
    test "isEmpty (overlay empty empty) == True" $
        isEmpty (overlay empty empty :: BAII)
    test "isEmpty (vertex x)            == False" $ \(x :: Either Int Int) ->
        not $ isEmpty (vertex x)
    test "isEmpty x                     == (x == empty)" $ \(x :: BAII) ->
        isEmpty x == (x == empty)

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexCount ============"
    test "leftVertexCount empty                    == 0" $
        leftVertexCount (empty :: BAII)                  == 0
    test "leftVertexCount (leftVertex 1)           == 1" $
        leftVertexCount (leftVertex 1 :: BAII)           == 1
    test "leftVertexCount (rightVertex (-2))       == 0" $
        leftVertexCount (rightVertex (-2) :: BAII)       == 0
    test "leftVertexCount (edges [(1, 1), (1, 2)]) == 1" $
        leftVertexCount (edges [(1, 1), (1, 2)] :: BAII) == 1
    test "leftVertexCount (edges x)                == (length . nub . map fst) x" $ \(x :: [(Int, Int)]) ->
        leftVertexCount (edges x)                        == (length . nub . map fst) x

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexCount ============"
    test "rightVertexCount empty                    == 0" $
        rightVertexCount (empty :: BAII)                  == 0
    test "rightVertexCount (rightVertex (-2))       == 1" $
        rightVertexCount (rightVertex (-2) :: BAII)       == 1
    test "rightVertexCount (leftVertex 1)           == 0" $
        rightVertexCount (leftVertex 1 :: BAII)           == 0
    test "rightVertexCount (edges [(1, 1), (1, 2)]) == 2" $
        rightVertexCount (edges [(1, 1), (1, 2)] :: BAII) == 2
    test "rightVertexCount (edges x)                == (length . nub . map snd) x" $ \(x :: [(Int, Int)]) ->
        rightVertexCount (edges x)                        == (length . nub . map snd) x

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexCount ============"
    test "vertexCount empty                    == 0" $
        vertexCount (empty :: BAII)                  == 0
    test "vertexCount (leftVertex 1)           == 1" $
        vertexCount (leftVertex 1 :: BAII)           == 1
    test "vertexCount (rightVertex 1)          == 1" $
        vertexCount (rightVertex 1 :: BAII)          == 1
    test "vertexCount (edges [(1, 1), (1, 2)]) == 3" $
        vertexCount (edges [(1, 1), (1, 2)] :: BAII) == 3
    test "vertexCount x                        == leftVertexCount x + rightVertexCount x" $ \(x :: BAII) ->
        vertexCount x                                == leftVertexCount x + rightVertexCount x

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeCount ============"
    test "edgeCount empty      == 0" $
        edgeCount (empty :: BAII)    == 0
    test "edgeCount (vertex x) == 0" $ \x ->
        edgeCount (vertex x :: BAII) == 0
    test "edgeCount (edge 1 2) == 1" $
        edgeCount (edge 1 2 :: BAII) == 1
    test "edgeCount (edge 1 1) == 1" $
        edgeCount (edge 1 1 :: BAII) == 1
    test "edgeCount (edges x) == (length . nub) x" $ \(x :: [(Int, Int)]) ->
        edgeCount (edges x)          == (length . nub) x

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
    test "hasVertex (Left x) y                == hasLeftVertex x y" $ \x (y :: BAII) ->
        hasVertex (Left x) y  == hasLeftVertex x y
    test "hasVertex (Right x) y               == hasRightVertex x y" $ \x (y :: BAII) ->
        hasVertex (Right x) y == hasRightVertex x y

    putStrLn "\n============ Bipartite.AdjacencyMap.swap ============"
    test "swap empty            == empty" $
        swap (empty :: BAII)        == empty
    test "swap (leftVertex 1)   == rightVertex 1" $
        swap (leftVertex 1 :: BAII) == rightVertex 1
    test "swap (vertices ls rs) == (flip vertices) ls rs" $ \(ls :: [Int]) (rs :: [Int]) ->
        swap (vertices ls rs)       == (flip vertices) ls rs
    test "swap (edge 1 \"a\")   == edge \"a\" 1" $
        swap (edge 1 "a" :: BAIS)   == edge "a" 1
    test "swap (edges x)        == (edges . map Data.Tuple.swap) x" $ \(x :: [(Int, Int)]) ->
        swap (edges x)              == (edges . map Data.Tuple.swap) x
    test "swap (swap x)         == x" $ \x ->
        swap (swap x :: BAII)       == x

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexList ============"
    test "leftVertexList empty            == []" $
        leftVertexList (empty :: BAII)         == []
    test "leftVertexList (leftVertex 1)   == [1]" $
        leftVertexList (leftVertex 1 :: BAII)  == [1]
    test "leftVertexList (rightVertex 1)  == []" $
        leftVertexList (rightVertex 1 :: BAII) == []
    test "leftVertexList (vertices vs us) == (nub . sort) vs" $ \(vs :: [Int]) (us :: [Int]) ->
        leftVertexList (vertices vs us)        == (nub . sort) vs
    test "isSorted (leftVertexList x)     == True" $ \(x :: BAII) ->
        isSorted $ leftVertexList x

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexList ============"
    test "rightVertexList empty            == []" $
        rightVertexList (empty :: BAII)         == []
    test "rightVertexList (leftVertex 1)   == []" $
        rightVertexList (leftVertex 1 :: BAII)  == []
    test "rightVertexList (rightVertex 1)  == [1]" $
        rightVertexList (rightVertex 1 :: BAII) == [1]
    test "rightVertexList (vertices vs us) == (nub . sort) us" $ \(vs :: [Int]) (us :: [Int]) ->
        rightVertexList (vertices vs us)        == (nub . sort) us
    test "isSorted (rightVertexList x)     == True" $ \(x :: BAII) ->
        isSorted $ rightVertexList x

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexList ============"
    test "vertexList empty                             == []" $
        vertexList (empty :: BAII)                   == []
    test "vertexList (vertex x)                        == [x]" $ \x ->
        vertexList (vertex x :: BAII)                == [x]
    test "vertexList (vertices (lefts vs) (rights vs)) == nub (sort vs)" $ \(vs :: [Either Int Int]) ->
        vertexList (vertices (lefts vs) (rights vs)) == nub (sort vs)
    test "isSorted (vertexList x)                      == True" $ \(x :: BAII) ->
        isSorted $ vertexList x

    putStrLn "\n============ Bipartite.AdjacencyMap.edgeList ============"
    test "edgeList empty           == []" $
        edgeList (empty :: BAII)         == []
    test "edgeList (leftVertex 1)  == []" $
        edgeList (leftVertex 1 :: BAII)  == []
    test "edgeList (rightVertex 1) == []" $
        edgeList (rightVertex 1 :: BAII) == []
    test "edgeList (edge 1 1)      == [(1, 1)]" $
        edgeList (edge 1 1 :: BAII)      == [(1, 1)]
    test "edgeList (edge 1 2)      == [(1, 2)]" $
        edgeList (edge 1 2 :: BAII)      == [(1, 2)]
    test "(edgeList . edges) x     == (nub . sort) x" $ \(x :: [(Int, Int)]) ->
        (edgeList . edges) x             == (nub . sort) x

    putStrLn "\n============ Bipartite.AdjacencyMap.leftVertexSet ============"
    test "leftVertexSet empty            == Set.empty" $
        leftVertexSet (empty :: BAII)         == Set.empty
    test "leftVertexSet (leftVertex 1)   == Set.singleton" $
        leftVertexSet (leftVertex 1 :: BAII)  == Set.singleton 1
    test "leftVertexSet (rightVertex 1)  == Set.empty" $
        leftVertexSet (rightVertex 1 :: BAII) == Set.empty
    test "leftVertexSet (vertices vs us) == Set.fromList vs" $ \(vs :: [Int]) (us :: [Int]) ->
        leftVertexSet (vertices vs us)        == Set.fromList vs

    putStrLn "\n============ Bipartite.AdjacencyMap.rightVertexSet ============"
    test "rightVertexSet empty              == Set.empty" $
        rightVertexSet (empty :: BAII)         == Set.empty
    test "rightVertexSet (leftVertex 1)     == Set.singleton 1" $
        rightVertexSet (leftVertex 1 :: BAII)  == Set.empty
    test "rightVertexSet (rightVertex 1)    == Set.empty" $
        rightVertexSet (rightVertex 1 :: BAII) == Set.singleton 1
    test "(rightVertexSet . vertices []) vs == Set.fromList vs" $ \(vs :: [Int]) (us :: [Int]) ->
        rightVertexSet (vertices vs us)        == Set.fromList us

    putStrLn "\n============ Bipartite.AdjacencyMap.vertexSet ============"
    test "vertexSet empty                             == Set.empty" $
        vertexSet (empty :: BAII)                   == Set.empty
    test "vertexSet (leftVertex 1)                    == Set.singleton 1" $
        vertexSet (leftVertex 1 :: BAII)            == Set.singleton (Left 1)
    test "vertexSet (rightVertex 1)                   == Set.empty" $
        vertexSet (rightVertex 1 :: BAII)           == Set.singleton (Right 1)
    test "vertexSet (vertices (lefts vs) (rights vs)) == Set.fromList vs" $ \(vs :: [Either Int Int]) ->
        vertexSet (vertices (lefts vs) (rights vs)) == Set.fromList vs

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
    test "edgeSet (edges x)       == Set.fromList x" $ \(x :: [(Int, Int)]) ->
        edgeSet (edges x)               == Set.fromList x

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
    test "(x == y) == ((leftAdjacencyMap x == leftAdjacencyMap y) && (rightAdjacencyMap x == rightAdjacencyMap y))" $ \(x :: BAII) (y :: BAII) ->
        (x == y) == ((leftAdjacencyMap x == leftAdjacencyMap y) && (rightAdjacencyMap x == rightAdjacencyMap y))
    test "      x + y == y + x" $ \(x :: BAII) (y :: BAII) ->
        x + y == y + x
    test "x + (y + z) == (x + y) + z" $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
        x + (y + z) == (x + y) + z
    test "  x * empty == x" $ \(x :: BAII) ->
        x * empty == x
    test "  empty * x == x" $ \(x :: BAII) ->
        empty * x == x
    test "      x * y == y * x" $ \(x :: BAII) (y :: BAII) ->
        x * y == y * x
    test "x * (y * z) == (x * y) * z" $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
        x * (y * z) == (x * y) * z
    test "x * (y + z) == x * y + x * z" $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
        x * (y + z) == x * (y + z)
    test "(x + y) * z == x * z + y * z" $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
        (x + y) * z == x * z + y * z
    test "  x * y * z == x * y + x * z + y * z" $ \(x :: BAII) (y :: BAII) (z :: BAII) ->
        x * y * z == x * y + x * z + y * z
    test "  (leftVertex x) * (leftVertex y) == (leftVertex x) + (leftVertex y)" $ \(x :: Int) (y :: Int) ->
        ((leftVertex x) * (leftVertex y) :: BAII) == (leftVertex x) + (leftVertex y)
    test "(rightVertex x) * (rightVertex y) == (rightVertex x) + (rightVertex y)" $ \(x :: Int) (y :: Int) ->
        ((rightVertex x) * (rightVertex y) :: BAII) == (rightVertex x) + (rightVertex y)
    test "  x + empty == x" $ \(x :: BAII) ->
        x + empty == x
    test "  empty + x == x" $ \(x :: BAII) ->
        empty + x == x
    test "      x + x == x" $ \(x :: BAII) ->
        x + x == x
    test "x * y + x + y == x * y" $ \(x :: BAII) (y :: BAII) ->
        x * y + x + y == x * y
    test "    x * x * x == x * x" $ \(x :: BAII) ->
        x * x * x == x * x

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
    test "show (swap 1 * 2)                  == \"edge 1 2\"" $
        show (swap 1 * 2 :: BAII)                  == "edge 1 2"
    test "show (swap 1 * 2 * swap 3)         == \"edges [(1,2),(3,2)]\"" $
        show (swap 1 * 2 * swap 3 :: BAII)         == "edges [(1,2),(3,2)]"
    test "show (swap 1 * 2 + swap 3)         == \"overlay (leftVertex 3) (edge 1 2)\"" $
        show (swap 1 * 2 + swap 3 :: BAII)         == "overlay (leftVertex 3) (edge 1 2)"
    test "show (swap 1 * 2 + swap 3 + 4)     == \"overlay (vertices [3] [4]) (edge 1 2)\"" $
        show (swap 1 * 2 + swap 3 + 4 :: BAII)     == "overlay (vertices [3] [4]) (edge 1 2)"
    test "show ((3 + swap 2) * (2 + swap 0)) == \"edges [(2,2),(3,0)]\"" $
        show ((3 + swap 2) * (2 + swap 0) :: BAII) == "edges [(0,3),(2,2)]"

    putStrLn "\n============ Bipartite.AdjacencyMap.circuit ============"
    test "circuit []                       == empty" $
        circuit []                       == (empty :: BAII)
    test "circuit [(x, y)]                 == edge x y" $ \(x :: Int) (y :: Int) ->
        circuit [(x, y)]                 == edge x y
    test "circuit [(x, y), (z, w)]         == biclique [x, z] [y, w]" $ \(x :: Int) (y :: Int) (z :: Int) (w :: Int) ->
        circuit [(x, y), (z, w)]         == biclique [x, z] [y, w]
    test "circuit [(1, 2), (3, 4), (5, 6)] == swap 1 * (2 + 6) + swap 3 * (2 + 4) + swap 5 * (4 + 6)" $
        circuit [(1, 2), (3, 4), (5, 6)] == (swap 1 * (2 + 6) + swap 3 * (2 + 4) + swap 5 * (4 + 6) :: BAII)
    test "circuit (reverse x)              == swap (circuit (map swap x))" $ \(x :: [(Int, Int)]) ->
        circuit (reverse x)              == swap (circuit (map Data.Tuple.swap x))

    putStrLn "\n============ Bipartite.AdjacencyMap.biclique ============"
    test "biclique [] [] == empty" $
        biclique [] [] == (empty :: BAII)
    test "biclique xs [] == vertices xs []" $ \(xs :: [Int]) ->
        biclique xs [] == (vertices xs [] :: BAII)
    test "biclique [] ys == vertices [] ys" $ \(ys :: [Int]) ->
        biclique [] ys == (vertices [] ys :: BAII)
    test "biclique xs ys == connect (vertices xs []) (vertices [] ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
        biclique xs ys == connect (vertices xs []) (vertices [] ys)

    putStrLn "\n============ Bipartite.AdjacencyMap.detectParts ============"
    test "detectParts empty                                       == Right empty" $
        detectParts (AM.empty :: AI)                               == Right empty
    test "detectParts (vertex 1)                                  == Right (leftVertex 1)" $
        detectParts (AM.vertex 1 :: AI)                            == Right (leftVertex 1)
    test "detectParts (edge 1 1)                                  == Left [1]" $
        detectParts (AM.edge 1 1 :: AI)                            == Left [1]
    test "detectParts (edge 1 2)                                  == Right (edge 1 2)" $
        detectParts (AM.edge 1 2 :: AI)                            == Right (edge 1 2)
    test "detectParts (edge 0 (-1))                               == Right (edge (-1) 0)" $
        detectParts (AM.edge 0 (-1) :: AI)                         == Right (edge (-1) 0)
    test "detectParts (1 * (2 + 3))                               == Right (edges [(1, 2), (1, 3)])" $
        detectParts (1 * (2 + 3) :: AI)                            == Right (edges [(1, 2), (1, 3)])
    test "detectParts ((1 + 3) * (2 + 4) + 6 * 5)                 == Right (swap (1 + 3) * (2 + 4) + swap 5 * 6" $
        detectParts ((1 + 3) * (2 + 4) + 6 * 5 :: AI)              == Right (swap (1 + 3) * (2 * 4) + swap 5 * 6)
    test "detectParts ((1 + 2) * (3 + 4) * (5 + 6))               == Left [1, 3, 2, 4, 5]" $
        detectParts ((1 + 2) * (3 + 4) * (5 + 6) :: AI)            == Left [1, 3, 2, 4, 5]
    test "detectParts ((1 + 2) * (3 + 4) + (3 + 4) * 5)           == Right (swap (1 + 2) * (3 + 4) + swap 5 * (3 + 4))" $
        detectParts ((1 + 2) * (3 + 4) + (3 + 4) * 5 :: AI)        == Right (swap (1 + 2) * (3 + 4) + swap 5 * (3 + 4))
    test "detectParts (1 * 2 * 3)                                 == Left [2, 3, 1]" $
        detectParts (1 * 2 * 3 :: AI)                              == Left [1, 2, 3]
    test "detectParts ((1 * 3 * 4) + 2 * (1 + 2))                 == Left [2]" $
        detectParts ((1 * 3 * 4) + 2 * (1 + 2) :: AI)              == Left [2]
    test "detectParts (clique [1..10])                            == Left [1, 2, 3]" $
        detectParts (AM.clique [1..10] :: AI)                      == Left [1, 2, 3]
    test "detectParts (circuit [1..11])                           == Left [1..11]" $
        detectParts (AM.circuit [1..11] :: AI)                     == Left [1..11]
    test "detectParts (circuit [1..10])                           == Right (circuit [(2 * x - 1, 2 * x) | x <- [1..5]])" $
        detectParts (AM.circuit [1..10] :: AI)                     == Right (circuit [(2 * x - 1, 2 * x) | x <- [1..5]])
    test "detectParts (biclique [] xs)                            == Right (vertices xs [])" $ \(xs :: [Int]) ->
        detectParts (AM.biclique [] xs :: AI)                      == Right (vertices xs [])
    test "detectParts (biclique (map Left (x:xs)) (map Right ys)) == Right (biclique (map Left (x:xs)) (map Right ys))" $ \(x :: Int) (xs :: [Int]) (ys :: [Int]) ->
        detectParts (AM.biclique (map Left (x:xs)) (map Right ys)) == Right (biclique (map Left (x:xs)) (map Right ys))
    test "isRight (detectParts (star x ys))                       == not (elem x ys)" $ \(x :: Int) (ys :: [Int]) ->
        isRight (detectParts (AM.star x ys))                       == (not $ elem x ys)
    test "isRight (detectParts (fromBipartite (toBipartite x)))   == True" $ \(x :: AII) ->
        isRight (detectParts (fromBipartite (toBipartite x)))
    test "((all ((flip Set.member) $ edgeSet $ symmetricClosure x) . edgeSet) <$> detectParts x) /= Right False" $ \(x :: AI) ->
        ((all ((flip Set.member) $ AM.edgeSet $ AM.symmetricClosure x) . edgeSet) <$> detectParts x) /= Right False
    test "(Set.map $ fromEither) <$> (vertexSet <$> (detectParts (fromBipartite (toBipartite x)))) == Right (vertexSet x)" $ \(x :: AII) ->
        ((Set.map $ fromEither) <$> (vertexSet <$> (detectParts (fromBipartite (toBipartite x))))) == Right (AM.vertexSet x)
    test "fromEither (bimap ((flip Set.isSubsetOf) (vertexSet x) . Set.fromList) (const True) (detectParts x)) == True" $ \(x :: AI) ->
        fromEither (bimap ((flip Set.isSubsetOf) (AM.vertexSet x) . Set.fromList) (const True) (detectParts x))
    test "fromEither (bimap ((flip Set.isSubsetOf) (edgeSet (symmetricClosure x)) . AM.edgeSet . circuit) (const True) (detectParts x)) == True" $ \(x :: AI) ->
        fromEither (bimap ((flip Set.isSubsetOf) (AM.edgeSet (AM.symmetricClosure x)) . AM.edgeSet . AM.circuit) (const True) (detectParts x))
    test "fromEither (bimap (((==) 1) . ((flip mod) 2) . length) (const True) (detectParts x)) == True" $ \(x :: AI) ->
        fromEither (bimap (((==) 1) . ((flip mod) 2) . length) (const True) (detectParts x))

expectedBicliqueMap :: Int -> Int -> Map.Map Int (Set.Set Int)
expectedBicliqueMap n m = Map.fromAscList [ (u, Set.fromAscList [1..m]) | u <- [1..n] ]

isSorted :: Ord a => [a] -> Bool
isSorted xs = and $ zipWith (<=) xs $ tail xs

fromEither :: Either a a -> a
fromEither (Left  x) = x
fromEither (Right y) = y

parity :: Int -> Either Int Int
parity x | x `mod` 2 == 1 = Left  x
         | otherwise      = Right x
