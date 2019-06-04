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
    test "empty"                   $ consistent $ fromAdjacencyMap (AM.empty :: AII)
    test "leftVertex"              $ consistent $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)
    test "rightVertex"             $ consistent $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)
    test "directedEdge"            $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII)
    test "undirectedEdge"          $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII)
    test "directedWithEqualEnds"   $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII)
    test "undirectedWithEqualEnds" $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII)
    test "tick"                    $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII)
    test "square"                  $ consistent $ fromAdjacencyMap squareAM
    test "diagSquare"              $ consistent $ fromAdjacencyMap diagSquareAM
    test "arbitraryAdjacencyMap"   $ \x -> consistent (fromAdjacencyMap (x :: AII))
    test "arbitraryBiclique"       $ \x y ->
        consistent $ fromAdjacencyMap $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)])

    putStrLn "\n============ Bipartite.AdjacencyMap.fromAdjacencyMap ============"
    test "empty"                   $
        (leftToRight $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty &&
        (rightToLeft $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty
    test "leftVertex"              $
        (leftToRight $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)) == Map.fromAscList [(1, Set.empty)] &&
        (rightToLeft $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)) == Map.empty
    test "rightVertex"             $
        (leftToRight $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)) == Map.empty &&
        (rightToLeft $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)) == Map.fromAscList [(1, Set.empty)]
    test "directedEdge"            $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "undirectedEdge"          $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "directedWithEqualEnds"   $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "undirectedWithEqualEnds" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "tick"                    $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1, 2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1]), (2, Set.fromAscList [1])]
    test "square"                  $
        (leftToRight $ fromAdjacencyMap squareAM) == squareLR &&
        (rightToLeft $ fromAdjacencyMap squareAM) == squareRL
    test "diagSquare"              $
        (leftToRight $ fromAdjacencyMap diagSquareAM) == squareLR &&
        (rightToLeft $ fromAdjacencyMap diagSquareAM) == squareRL
    test "arbitraryBiclique"       $ \x y ->
        (leftToRight $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap x y &&
        (rightToLeft $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.fromGraph ============"
    test "empty"                   $
        (leftToRight $ fromGraph (G.Empty :: GII)) == Map.empty &&
        (rightToLeft $ fromGraph (G.Empty :: GII)) == Map.empty
    test "leftVertex"              $
        (leftToRight $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.fromAscList [(1, Set.empty)] &&
        (rightToLeft $ fromGraph (G.Vertex (Left 1) :: GII)) == Map.empty
    test "rightVertex"             $
        (leftToRight $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.empty &&
        (rightToLeft $ fromGraph (G.Vertex (Right 1) :: GII)) == Map.fromAscList [(1, Set.empty)]
    test "directedEdge"            $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 2)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 2)] :: GII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "undirectedEdge"          $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 2), (Right 2, Left 1)] :: GII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "directedWithEqualEnds"   $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 1)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 1)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "undirectedWithEqualEnds" $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 1), (Right 1, Left 1)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "tick"                    $
        (leftToRight $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1, 2])] &&
        (rightToLeft $ fromGraph (G.edges [(Left 1, Right 1), (Left 1, Right 2)] :: GII))
            == Map.fromAscList [(1, Set.fromAscList [1]), (2, Set.fromAscList [1])]
    test "square"                  $
        (leftToRight $ fromGraph squareG) == squareLR &&
        (rightToLeft $ fromGraph squareG) == squareRL
    test "diagSquare"              $
        (leftToRight $ fromGraph diagSquareG) == squareLR &&
        (rightToLeft $ fromGraph diagSquareG) == squareRL
    test "arbitraryGraph"          $ \x ->
        consistent $ fromGraph (x :: GII)
    test "arbitraryBiclique"       $ \x y ->
        (leftToRight $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap x y &&
        (rightToLeft $ fromGraph $ G.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.toAdjacencyMap ============"
    test "empty"                   $
        (toAdjacencyMap $ fromAdjacencyMap (AM.empty :: AII)) == AM.empty
    test "leftVertex"              $
        (toAdjacencyMap $ fromAdjacencyMap (AM.vertex (Left 1) :: AII)) == AM.vertex (Left 1)
    test "rightVertex"             $
        (toAdjacencyMap $ fromAdjacencyMap (AM.vertex (Right 1) :: AII)) == AM.vertex (Right 1)
    test "directedEdge"            $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 2)] :: AII))
            == (AM.edges [(Left 1, Right 2), (Right 2, Left 1)])
    test "undirectedEdge"          $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
            == (AM.edges [(Left 1, Right 2), (Right 2, Left 1)])
    test "directedWithEqualEnds"   $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 1)] :: AII))
            == (AM.edges [(Left 1, Right 1), (Right 1, Left 1)])
    test "undirectedWithEqualEnds" $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII))
            == (AM.edges [(Left 1, Right 1), (Right 1, Left 1)])
    test "tick"                    $
        (toAdjacencyMap $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Left 1, Right 2)] :: AII))
            == (AM.edges [(Left 1, Right 1), (Left 1, Right 2), (Right 1, Left 1), (Right 2, Left 1)])
    test "square"                  $
        (toAdjacencyMap $ fromAdjacencyMap squareAM)     == fullSquareAM
    test "diagSquare"              $
        (toAdjacencyMap $ fromAdjacencyMap diagSquareAM) == fullSquareAM
    test "arbitraryAdjacencyMap"   $ \x -> AM.consistent $ toAdjacencyMap (x :: BAII)
    test "arbitraryBiclique"       $ \x y ->
        (toAdjacencyMap $ fromAdjacencyMap $
            AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
         == AM.overlay (AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)]))
                       (AM.biclique (map Right [1..(y :: Int)]) (map Left [1..(x :: Int)]))

    putStrLn "\n============ Bipartite.AdjacencyMap.hasEdge ============"
    test "empty"                   $ \x y ->
        not $ hasEdge (x :: Int) (y :: Int) $ fromAdjacencyMap AM.empty
    test "leftVertex"              $ \x y ->
        not $ hasEdge (x :: Int) (y :: Int) $ fromAdjacencyMap $ AM.vertex $ Left 1
    test "rightVertex"             $ \x y ->
        not $ hasEdge (x :: Int) (y :: Int) $ fromAdjacencyMap $ AM.vertex $ Right 1
    test "directedEdge"            $
        (hasEdge 1 2 $ fromAdjacencyMap (AM.edges [(Left 1, Right 2)] :: AII)) &&
        (not $ hasEdge 2 1 $ fromAdjacencyMap (AM.edges [(Left 1, Right 2)] :: AII))
    test "undirectedEdge"          $
        (hasEdge 1 2 $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII)) &&
        (not $ hasEdge 2 1 $ fromAdjacencyMap (AM.edges [(Left 1, Right 2), (Right 2, Left 1)] :: AII))
    test "directedWithEqualEnds"   $
        hasEdge 1 1 $ fromAdjacencyMap (AM.edges [(Left 1, Right 1)] :: AII)
    test "undirectedWithEqualEnds" $
        hasEdge 1 1 $ fromAdjacencyMap (AM.edges [(Left 1, Right 1), (Right 1, Left 1)] :: AII)
    test "unexistentVertices"      $
        not $ hasEdge 2 3 $ fromAdjacencyMap (AM.edges [(Left 1, Right 2)] :: AII)

squareAM :: AII
squareAM = AM.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4)]

squareG :: GII
squareG = G.edges [(Left 1, Right 2), (Left 1, Right 4), (Right 2, Left 3), (Left 3, Right 4)]

squareLR :: Map.Map Int (Set.Set Int)
squareLR = Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]

squareRL :: Map.Map Int (Set.Set Int)
squareRL = Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]

diagSquareAM :: AII
diagSquareAM = AM.overlay squareAM $ AM.edges [(Left 1, Left 3)]

diagSquareG :: GII
diagSquareG = G.overlay squareG $ G.edges [(Left 1, Left 3)]

fullSquareAM :: AII
fullSquareAM = AM.overlay (AM.edges [(u, v) | u <- map Left [1, 3], v <- map Right [2, 4]])
                          (AM.edges [(v, u) | u <- map Left [1, 3], v <- map Right [2, 4]])

expectedBicliqueMap :: Int -> Int -> Map.Map Int (Set.Set Int)
expectedBicliqueMap n m = Map.fromAscList [ (u, Set.fromAscList [1..m]) | u <- [1..n] ]

