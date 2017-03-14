import Data.List.Extra (nubOrd)
import Test.QuickCheck

import Algebra.Graph hiding (Graph, box, induce, removeVertex)
import Algebra.Graph.AdjacencyMap hiding (edges, edgeList)
import Algebra.Graph.Test
import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.IntAdjacencyMap
import Algebra.Graph.Test.Relation

main :: IO ()
main = do
    testAdjacencyMap
    testGraph
    testIntAdjacencyMap
    testRelation

    putStrLn "============ Directed graphs ============"

    test "DFS idempotence" $ \(x :: AdjacencyMap Int) ->
        dfsForest x == dfsForest (forest $ dfsForest x)
    test "DFS subgraph" $ \(x :: AdjacencyMap Int) ->
        forest (dfsForest x) `isSubgraphOf` x

    test "TopSort is a topological sort" $ \(x :: AdjacencyMap Int) ->
        fmap (flip isTopSort x) (topSort x) /= Just False

    test "TopSort of a cyclic graph" $ \(x :: AdjacencyMap Int) ys -> not (null ys) ==>
        topSort (x + circuit (nubOrd ys)) == Nothing

    test "TopSort idempotence" $ \(x :: AdjacencyMap Int) ->
        (topSort . path =<< topSort x) == (topSort x)
