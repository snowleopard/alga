module Algebra.Graph.Test.Bipartite.AdjacencyMap (
    -- * Testsuite
    testBipartiteAdjacencyMap
    ) where


import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Test

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AIM

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type AII  = AM.AdjacencyMap (Either Int Int)
type BAII = AdjacencyMap Int Int

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "consistentConstructedFromEmpty"       $ consistent $ fromAdjacencyMap (AM.empty :: AII)
    test "consistentConstructedFromLeftVertex"  $ consistent $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)
    test "consistentConstructedFromRIghtVertex" $ consistent $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)
    test "consistentWithOneDirectedEdge"        $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII)
    test "consistentWithOneUndirectedEdge"      $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII)
    test "consistentDirectedWithEqualEnds"      $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII)
    test "consistentUndirectedWithEqualEnds"    $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII)
    test "consistentTick"                       $
        consistent $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII)
    test "consistentSquare"                     $ consistent $ fromAdjacencyMap squareAM
    test "consistentDiagSquare"                 $ consistent $ fromAdjacencyMap diagSquareAM
    test "consistentArbitraryAdjacencyMap"      $ \x -> consistent (fromAdjacencyMap (x :: AII))
    test "consistentArbitraryBiclique"          $ \x y ->
        consistent $ fromAdjacencyMap $ AM.biclique (map Left [1..(x :: Int)]) (map Right [1..(y :: Int)])

    putStrLn "\n============ Bipartite.AdjacencyMap.fromAdjacencyMap ============"
    test "constructedFromEmpty" $
        (leftToRight $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty &&
        (rightToLeft $ fromAdjacencyMap (AM.empty :: AII)) == Map.empty
    test "constructedFromLeftVertex" $
        (leftToRight $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)) == Map.fromAscList [(1, Set.empty)] &&
        (rightToLeft $ fromAdjacencyMap ((AM.vertex (Left 1)) :: AII)) == Map.empty
    test "constructedFromRightVertex" $
        (leftToRight $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)) == Map.empty &&
        (rightToLeft $ fromAdjacencyMap ((AM.vertex (Right 1)) :: AII)) == Map.fromAscList [(1, Set.empty)]
    test "constructedFromDirectedEdge" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "constructedFromUndirectedEdge" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 2), (Right 2, Left 1)]) :: AII))
            == Map.fromAscList [(2, Set.fromAscList [1])]
    test "constructedFromDirectedWithEqualEnds" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "constructedFromUndirectedWithEqualEnds" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Right 1, Left 1)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1])]
    test "constructedFromTick" $
        (leftToRight $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1, 2])] &&
        (rightToLeft $ fromAdjacencyMap ((AM.edges [(Left 1, Right 1), (Left 1, Right 2)]) :: AII))
            == Map.fromAscList [(1, Set.fromAscList [1]), (2, Set.fromAscList [1])]
    test "constructedFromSquare" $
        (leftToRight $ fromAdjacencyMap squareAM) == squareLR &&
        (rightToLeft $ fromAdjacencyMap squareAM) == squareRL
    test "constructedFromDiagSquare" $
        (leftToRight $ fromAdjacencyMap diagSquareAM) == squareLR &&
        (rightToLeft $ fromAdjacencyMap diagSquareAM) == squareRL
    test "constructedFromArbitraryBiclique" $ \x y ->
        (leftToRight $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap x y &&
        (rightToLeft $ fromAdjacencyMap $ AM.biclique (map Left [1..x]) (map Right [1..y]))
            == expectedBicliqueMap y x

    putStrLn "\n============ Bipartite.AdjacencyMap.toAdjacencyMap ============"
    test "arbitraryBipartiteAdjacencyMap" $ \x -> AIM.consistent (toAdjacencyMap (x :: BAII))


squareAM :: AII
squareAM = AM.edges [(Left 1, Right 2), (Left 1, Right 3), (Left 2, Right 3), (Right 3, Left 4)]

squareLR :: Map.Map Int (Set.Set Int)
squareLR = Map.fromAscList [(1, Set.fromAscList [2, 4]), (3, Set.fromAscList [2, 4])]

squareRL :: Map.Map Int (Set.Set Int)
squareRL = Map.fromAscList [(2, Set.fromAscList [1, 3]), (4, Set.fromAscList [1, 3])]

diagSquareAM :: AII
diagSquareAM = AM.overlay squareAM $ AM.edges [(Left 1, Left 3)]

expectedBicliqueMap :: Int -> Int -> Map.Map Int (Set.Set Int)
expectedBicliqueMap n m = Map.fromAscList [ (u, Set.fromAscList [1..m]) | u <- [1..n] ]
