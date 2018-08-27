import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.LabelledAdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Fold
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.IntAdjacencyMap
import Algebra.Graph.Test.Internal
import Algebra.Graph.Test.NonEmptyGraph
import Algebra.Graph.Test.Relation

main :: IO ()
main = do
    testLabelledAdjacencyMap
    testAdjacencyMap
    testExport
    testFold
    testGraph
    testGraphNonEmpty
    testIntAdjacencyMap
    testInternal
    testRelation
