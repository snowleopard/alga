import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Fold
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.IntAdjacencyMap
import Algebra.Graph.Test.Relation

main :: IO ()
main = do
    testAdjacencyMap
    testExport
    testFold
    testGraph
    testIntAdjacencyMap
    testRelation
