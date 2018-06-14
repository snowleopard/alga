import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Fold
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.AdjacencyIntMap
import Algebra.Graph.Test.Internal
import Algebra.Graph.Test.NonEmptyGraph
import Algebra.Graph.Test.Relation
import Data.Graph.Test.Typed

main :: IO ()
main = do
    testAdjacencyMap
    testExport
    testFold
    testGraph
    testGraphNonEmpty
    testAdjacencyIntMap
    testInternal
    testRelation
    testTyped
