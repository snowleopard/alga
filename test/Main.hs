import Algebra.Graph.Test.Acyclic.AdjacencyMap
import Algebra.Graph.Test.AdjacencyIntMap
import Algebra.Graph.Test.AdjacencyMap
import Algebra.Graph.Test.Bipartite.AdjacencyMap
import Algebra.Graph.Test.Export
import Algebra.Graph.Test.Graph
import Algebra.Graph.Test.Internal
import Algebra.Graph.Test.Label
import Algebra.Graph.Test.Labelled.AdjacencyMap
import Algebra.Graph.Test.Labelled.Graph
import Algebra.Graph.Test.NonEmpty.AdjacencyMap
import Algebra.Graph.Test.NonEmpty.Graph
import Algebra.Graph.Test.Relation
import Algebra.Graph.Test.Relation.SymmetricRelation
import Algebra.Graph.Test.Undirected
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
    {-# SCC "Ann.Acyclic.AdjacencyMap"   #-} go "Acyclic.AdjacencyMap"   testAcyclicAdjacencyMap
    {-# SCC "Ann.AdjacencyIntMap"        #-} go "AdjacencyIntMap"        testAdjacencyIntMap
    {-# SCC "Ann.AdjacencyMap"           #-} go "AdjacencyMap"           testAdjacencyMap
    {-# SCC "Ann.Bipartite.AdjacencyMap" #-} go "Bipartite.AdjacencyMap" testBipartiteAdjacencyMap
    {-# SCC "Ann.Export"                 #-} go "Export"                 testExport
    {-# SCC "Ann.Graph"                  #-} go "Graph"                  testGraph
    {-# SCC "Ann.Internal"               #-} go "Internal"               testInternal
    {-# SCC "Ann.Label"                  #-} go "Label"                  testLabel
    {-# SCC "Ann.Labelled.AdjacencyMap"  #-} go "Labelled.AdjacencyMap"  testLabelledAdjacencyMap
    {-# SCC "Ann.Labelled.Graph"         #-} go "Labelled.Graph"         testLabelledGraph
    {-# SCC "Ann.NonEmpty.AdjacencyMap"  #-} go "NonEmpty.AdjacencyMap"  testNonEmptyAdjacencyMap
    {-# SCC "Ann.NonEmpty.Graph"         #-} go "NonEmpty.Graph"         testNonEmptyGraph
    {-# SCC "Ann.Relation"               #-} go "Relation"               testRelation
    {-# SCC "Ann.Symmetric.Relation"     #-} go "Symmetric.Relation"     testSymmetricRelation
    {-# SCC "Ann.Typed"                  #-} go "Typed"                  testTyped
