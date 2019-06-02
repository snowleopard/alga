module Algebra.Graph.Test.Bipartite.AdjacencyMap (
    -- * Testsuite
    testBipartiteAdjacencyMap
    ) where


import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Test

import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AIM

type AII  = AM.AdjacencyMap (Either Int Int)
type BAII = AdjacencyMap Int Int

testBipartiteAdjacencyMap :: IO ()
testBipartiteAdjacencyMap = do
    putStrLn "\n============ Bipartite.AdjacencyMap.consistent ============"
    test "arbitraryBipartiteAdjacencyMap" $ \x -> consistent (x :: BAII)

    putStrLn "\n============ Bipartite.AdjacencyMap.fromAdjacencyMap ============"
    test "arbitraryAdjacencyMap" $ \x -> consistent (fromAdjacencyMap (x :: AII))

    putStrLn "\n============ Bipartite.AdjacencyMap.toAdjacencyMap ============"
    test "arbitraryBipartiteAdjacencyMap" $ \x -> AIM.consistent (toAdjacencyMap (x :: BAII))
