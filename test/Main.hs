import Algebra.Graph.Test.AdjacencyIntMap
import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.NonEmpty.AdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Fold
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.NonEmpty.Graph
import Algebra.Graph.Test.Internal
import Algebra.Graph.Test.Labelled.AdjacencyMap
import Algebra.Graph.Test.Relation
import Data.Graph.Test.Typed

main :: IO ()
main = do
    testAdjacencyIntMap
    testAdjacencyMap
    testExport
    testFold
    testGraph
    testInternal
    testLabelledAdjacencyMap
    testNonEmptyAdjacencyMap
    testNonEmptyGraph
    testRelation
    testTyped
