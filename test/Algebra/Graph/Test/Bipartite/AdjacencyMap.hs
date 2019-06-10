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

import qualified Algebra.Graph                       as G
import qualified Algebra.Graph.AdjacencyMap          as AM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type GII  = G.Graph (Either Int Int)
type AII  = AM.AdjacencyMap (Either Int Int)
type BAII = AdjacencyMap Int Int

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistent $ fromGraph G.empty == True" $ consistent $ fromGraph (G.empty :: GII)
    test "consistent $ fromGraph $ G.vertex x                                      == True" $ \x ->
        consistent $ fromGraph (G.Vertex x :: GII)
    test "consistent $ fromGraph $ G.edge x y                                      == True" $ \x y ->
        consistent $ fromGraph $ G.edge (x :: Either Int Int) (y :: Either Int Int)
    test "consistent $ fromGraph $ G.edges xs                                      == True" $ \xs ->
        consistent $ fromGraph $ G.edges (xs :: [(Either Int Int, Either Int Int)])
    test "consistent $ fromAdjacencyMap am                                         == True" $ \am ->
        consistent $ fromAdjacencyMap (am :: AII)
    test "consistent $ fromGraph g                                                 == True" $ \g ->
        consistent $ fromGraph (g :: GII)
    test "consistent $ fromGraph $ G.biclique (map Left [1..x]) (map Right [1..y]) == True" $ \x y ->
        consistent $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)])

    putStrLn "\n============ Bipartite.AdjacencyMap.fromAdjacencyMap ============"
    test "leftToRight $ fromAdjacencyMap AM.empty                                                                                                  == Map.empty" $
        (leftToRight $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty
    test "rightToLeft $ fromAdjacencyMap AM.empty                                                                                                  == Map.empty" $
        (rightToLeft $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty
    test "leftToRight $ fromAdjacencyMap $ AM.vertex $ Left 1                                                                                      == Map.singleton 1 Set.empty" $
        (leftToRight $ fromAdjacencyMap (AM.vertex (Left 1) :: AII)) == Map.singleton 1 Set.empty
    test "rightToLeft $ fromAdjacencyMap $ AM.vertex $ Left 1                                                                                      == Map.empty" $
        (rightToLeft $ fromAdjacencyMap (AM.vertex (Left 1) :: AII)) == Map.empty
    test "leftToRight $ fromAdjacencyMap $ AM.vertex $ Right 1                                                                                     == Map.empty" $
        (leftToRight $ fromAdjacencyMap (AM.vertex (Right 1) :: AII)) == Map.empty
    test "rightToLeft $ fromAdjacencyMap $ AM.vertex $ Right 1                                                                                     == Map.singleton 1 Set.empty" $
        (rightToLeft $ fromAdjacencyMap (AM.vertex (Right 1) :: AII)) == Map.singleton 1 Set.empty
    test "leftToRight $ fromAdjacencyMap $ AM.edge (Left 1) (Right 2)                                                                              == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromAdjacencyMap (AM.edge (Left 1) (Right 2) :: AII)) == Map.singleton 1 (Set.singleton 2)
    test "rightToLeft $ fromAdjacencyMap $ AM.edge (Left 1) (Right 2)                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromAdjacencyMap (AM.edge (Left 1) (Right 2) :: AII)) == Map.singleton 2 (Set.singleton 1)
    test "leftToRight $ fromAdjacencyMap $ AM.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 2)
    test "rightToLeft $ fromAdjacencyMap $ AM.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
          == Map.singleton 2 (Set.singleton 1)
    test "leftToRight $ fromAdjacencyMap $ AM.edge (Left 1) (Right 1)                                                                              == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromAdjacencyMap (AM.edge (Left 1) (Right 1) :: AII)) == Map.singleton 1 (Set.singleton 1)
    test "rightToLeft $ fromAdjacencyMap $ AM.edge (Left 1) (Right 2)                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromAdjacencyMap (AM.edge (Left 1) (Right 1) :: AII)) == Map.singleton 1 (Set.singleton 1)
    test "leftToRight $ fromAdjacencyMap $ AM.edges [(Left 1, Right 1), (Right 1, Left 1)]                                                         == Map.singleton 1 (Set.singleton 1)" $
        (leftToRight $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 1)
    test "rightToLeft $ fromAdjacencyMap $ AM.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 1 (Set.singleton 1)" $
        (rightToLeft $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII))
          == Map.singleton 1 (Set.singleton 1)
    test "leftToRight $ fromAdjacencyMap $ AM.edges [(Left 1, Right 1), (Left 1, Right 2)]                                                         == Map.singleton 1 (Set.fromAscList [1, 2])" $
        (leftToRight $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightToLeft $ fromAdjacencyMap $ AM.edges [(Left 1, Right 1), (Left 1, Right 2)]                                                         == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        (rightToLeft $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "leftToRight $ fromAdjacencyMap $ AM.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] == <correct result>" $
        (leftToRight $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]
    test "rightToLeft $ fromAdjacencyMap $ AM.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] == <correct result>" $
        (rightToLeft $ fromAdjacencyMap (AM.edges ([(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]
    test "leftToRight $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y])                                                        == <correct result>" $ \x y ->
        (leftToRight $ fromAdjacencyMap $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap x y
    test "rightToLeft $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y])                                                        == <correct result>" $ \x y ->
        (rightToLeft $ fromAdjacencyMap $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromGraph ============"
    test "leftToRight $ fromGraph G.empty                                                                                                  == Map.empty" $
        (leftToRight $ fromGraph (G.empty :: GII)) == Map.empty
    test "rightToLeft $ fromGraph G.empty                                                                                                  == Map.empty" $
        (rightToLeft $ fromGraph (G.empty :: GII)) == Map.empty
    test "leftToRight $ fromGraph $ G.Vertex $ Left 1                                                                                      == Map.singleton 1 Set.empty" $
        (leftToRight $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.singleton 1 Set.empty
    test "rightToLeft $ fromGraph $ G.Vertex $ Left 1                                                                                      == Map.empty" $
        (rightToLeft $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.empty
    test "leftToRight $ fromGraph $ G.Vertex $ Right 1                                                                                     == Map.empty" $
        (leftToRight $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.empty
    test "rightToLeft $ fromGraph $ G.Vertex $ Right 1                                                                                     == Map.singleton 1 Set.empty" $
        (rightToLeft $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.singleton 1 Set.empty
    test "leftToRight $ fromGraph $ G.edge (Left 1) (Right 2)                                                                              == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromGraph (G.edge (Left 1) (Right 2) :: GII)) == Map.singleton 1 (Set.singleton 2)
    test "rightToLeft $ fromGraph $ G.edge (Left 1) (Right 2)                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromGraph (G.edge (Left 1) (Right 2) :: GII)) == Map.singleton 2 (Set.singleton 1)
    test "leftToRight $ fromGraph $ G.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 2)
    test "rightToLeft $ fromGraph $ G.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
          == Map.singleton 2 (Set.singleton 1)
    test "leftToRight $ fromGraph $ G.edge (Left 1) (Right 1)                                                                              == Map.singleton 1 (Set.singleton 2)" $
        (leftToRight $ fromGraph (G.edge (Left 1) (Right 1) :: GII)) == Map.singleton 1 (Set.singleton 1)
    test "rightToLeft $ fromGraph $ G.edge (Left 1) (Right 2)                                                                              == Map.singleton 2 (Set.singleton 1)" $
        (rightToLeft $ fromGraph (G.edge (Left 1) (Right 1) :: GII)) == Map.singleton 1 (Set.singleton 1)
    test "leftToRight $ fromGraph $ G.edges [(Left 1, Right 1), (Right 1, Left 1)]                                                         == Map.singleton 1 (Set.singleton 1)" $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 1)
    test "rightToLeft $ fromGraph $ G.edges [(Left 1, Right 2), (Right 2, Left 1)]                                                         == Map.singleton 1 (Set.singleton 1)" $
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
          == Map.singleton 1 (Set.singleton 1)
    test "leftToRight $ fromGraph $ G.edges [(Left 1, Right 1), (Left 1, Right 2)]                                                         == Map.singleton 1 (Set.fromAscList [1, 2])" $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.singleton 1 (Set.fromAscList [1, 2])
    test "rightToLeft $ fromGraph $ G.edges [(Left 1, Right 1), (Left 1, Right 2)]                                                         == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]" $
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.fromAscList [(1, Set.singleton 1), (2, Set.singleton 1)]
    test "leftToRight $ fromGraph $ G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] == <correct result>" $
        (leftToRight $ fromGraph $ (G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]
    test "rightToLeft $ fromGraph $ G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] == <correct result>" $
        (rightToLeft $ fromGraph $ (G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4), (Left 1, Left 3)] :: GII))
            == Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]
    test "leftToRight $ fromGraph $ G.biclique (map Left [1..x]) (map Right [1..y])                                                        == <correct result>" $ \x y ->
        (leftToRight $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap x y
    test "rightToLeft $ fromGraph $ G.biclique (map Left [1..x]) (map Right [1..y])                                                        == <correct result>" $ \x y ->
        (rightToLeft $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.toAdjacencyMap ============"
    test "toAdjacencyMap $ fromAdjacencyMap AM.empty                                           == AM.empty" $
        (toAdjacencyMap $ fromAdjacencyMap (AM.empty :: AII)) == AM.empty
    test "toAdjacencyMap $ fromAdjacencyMap (AM.vertex $ Left 1)                               == AM.vertex (Left 1)" $
        (toAdjacencyMap $ fromAdjacencyMap (AM.vertex (Left 1) :: AII)) == (AM.vertex (Left 1))
    test "toAdjacencyMap $ fromAdjacencyMap (AM.vertex $ Right 1)                              == AM.vertex (Right 1)" $
        (toAdjacencyMap $ fromAdjacencyMap (AM.vertex (Right 1) :: AII)) == (AM.vertex (Right 1))
    test "toAdjacencyMap $ fromAdjacencyMap (AM.edge (Left x) (Right y))                       == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \x y ->
        (toAdjacencyMap $ fromAdjacencyMap (AM.edge (Left x) (Right y) :: AII)) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left x, Right y), (Right y, Left x)])  == AM.edges [(Left x, Right y), (Right y, Left x)]" $ \x y ->
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left x, Right y), (Right y, Left x)] :: AII)) == AM.edges [(Left x, Right y), (Right y, Left x)]
    test "toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Left 1, Right 2)])  == <correct result>" $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == (AM.edges [(Left 1, Right 1), (Left 1, Right 2), (Right 1, Left 1), (Right 2, Left 1)])
    test "AM.consistent $ toAdjacencyMap bam                                                   == True" $ \bam ->
        AM.consistent $ toAdjacencyMap (bam :: BAII)
    test "toAdjacencyMap $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y]) == <correct result>" $ \x y ->
        (toAdjacencyMap $ fromAdjacencyMap $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
         == AM.overlay (AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
                       (AM.biclique (map Right [1..(y :: Int)]) (map Left [1..(x :: Int)]))

    putStrLn "\n============ Bipartite.AdjacencyMap.hasEdge ============"
    test "hasEdge x y $ fromGraph G.empty                                   == False" $ \x y ->
        not $ hasEdge (x :: Int) (y :: Int) $ fromGraph G.empty
    test "hasEdge x y $ fromGraph $ G.edge (Left x) (Right y)               == True" $ \x y ->
        hasEdge (x :: Int) (y :: Int) $ fromGraph $ G.edge (Left x) (Right y)
    test "hasEdge 1 2 $ fromGraph $ G.edge (Left 1) (Left 2)                == False" $
        not $ hasEdge 1 1 $ fromGraph $ (G.edge (Left 1) (Left 2) :: GII)
    test "hasEdge 2 3 $ fromGraph $ G.edge (Left 1) (Right 2)               == False" $
        not $ hasEdge 2 3 $ fromGraph (G.edges [(Left 1, Right 2)] :: GII)
    test "hasEdge x y $ fromGraph $ G.Overlay g $ G.edge (Left x) (Right y) == True" $ \g x y ->
        hasEdge x y $ fromGraph $ G.Overlay g $ G.edge (Left (x :: Int)) (Right (y :: Int))

expectedBicliqueMap :: Int -> Int -> Map.Map Int (Set.Set Int)
expectedBicliqueMap n m = Map.fromAscList [ (u, Set.fromAscList [1..m]) | u <- [1..n] ]

