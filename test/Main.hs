import Algebra.Graph.Test.Acyclic.AdjacencyMap
import Algebra.Graph.Test.AdjacencyIntMap
import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.NonEmpty.AdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.NonEmpty.Graph
import Algebra.Graph.Test.Internal
import Algebra.Graph.Test.Labelled.AdjacencyMap
import Algebra.Graph.Test.Labelled.Graph
import Algebra.Graph.Test.Relation
import Algebra.Graph.Test.Relation.SymmetricRelation
import Algebra.Graph.Test.Bipartite.AdjacencyMap
import Data.Graph.Test.Typed

import Control.Monad
import System.Environment

-- | By default, all testsuites will be executed, which takes a few minutes. If
-- you would like to execute only some specific testsuites, you can specify
-- their names in the command line. For example:
--
-- > stack test --test-arguments "Graph Symmetric.Relation"
--
-- will test the modules "Algebra.Graph" and "Algebra.Graph.Symmetric.Relation".
main :: IO ()
main = do
    selected <- getArgs
    let go current = when (null selected || current `elem` selected)
    go "Acyclic.AdjacencyMap"   testAcyclicAdjacencyMap
    go "AdjacencyIntMap"        testAdjacencyIntMap
    go "AdjacencyMap"           testAdjacencyMap
    go "Bipartite.AdjacencyMap" testBipartiteAdjacencyMap
    go "Export"                 testExport
    go "Graph"                  testGraph
    go "Internal"               testInternal
    go "Labelled.AdjacencyMap"  testLabelledAdjacencyMap
    go "Labelled.Graph"         testLabelledGraph
    go "NonEmpty.AdjacencyMap"  testNonEmptyAdjacencyMap
    go "NonEmpty.Graph"         testNonEmptyGraph
    go "Relation"               testRelation
    go "Symmetric.Relation"     testSymmetricRelation
    go "Typed"                  testTyped
