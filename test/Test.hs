import Data.Foldable (toList)
import Data.List (sort)
import Data.List.Extra (nubOrd)
import Test.QuickCheck

import Algebra.Graph hiding (Graph)
import Algebra.Graph.AdjacencyMap hiding (edges)
import Algebra.Graph.Data (Graph, fromGraph)
import Algebra.Graph.Dfs
import Algebra.Graph.Fold hiding (box, induce, removeVertex)
import Algebra.Graph.HigherKinded.Util
import Algebra.Graph.Relation
import Algebra.Graph.Relation.Preorder
import Algebra.Graph.Relation.Reflexive
import Algebra.Graph.Relation.Symmetric
import Algebra.Graph.Relation.Transitive
import Algebra.Graph.Test
import Algebra.Graph.Test.Arbitrary ()
import Algebra.Graph.TopSort

type G = Graph Int

test :: Testable a => String -> a -> IO ()
test str p = putStr (str ++ ": ") >> quickCheck p

main :: IO ()
main = do
    putStrLn "============ Graph ============"
    quickCheck (axioms   :: GraphTestsuite (Graph Int))
    quickCheck (theorems :: GraphTestsuite (Graph Int))

    putStrLn "============ Relation ============"
    quickCheck (axioms :: GraphTestsuite (Relation Int))

    putStrLn "============ Adjacency Map ============"
    quickCheck (axioms :: GraphTestsuite (AdjacencyMap Int))

    putStrLn "============ Directed graphs ============"
    test "Upper bound" $ \(x :: G) -> let xs = vertices (toList x) in
        x `isSubgraphOf` (xs * xs)

    test "Path-circuit order" $ \xs ->
        path xs `isSubgraphOf` (circuit xs :: G)

    let comm = fmap $ \(a, b) -> (b, a)
    test "Box commutativity" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
        x `box` y == comm (y `box` x)

    let assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "Box associativity" $ mapSize (min 10) $ \(x :: G) (y :: G) (z :: G) ->
        (x `box` y) `box` z == assoc (x `box` (y `box` z))

    test "Box-overlay distributivity" $ mapSize (min 10) $ \(x :: G) (y :: G) z ->
        x `box` (y `overlay` z) == (x `box` y) `overlay` (x `box` z)

    test "Induce full graph" $ \(x :: G) ->
        induce (const True) x == x

    test "Induce empty graph" $ \(x :: G) ->
        induce (const False) x == empty

    let i x s = induce (`elem` s) x
    test "Induce subgraph" $ \(s :: [Int]) (x :: G) ->
        (x `i` s) `isSubgraphOf` x

    test "Induce commutativity" $ \(x :: G) (s :: [Int]) (t :: [Int]) ->
        x `i` s `i` t == x `i` t `i` s

    test "Induce idempotence" $ \(x :: G) (s :: [Int]) ->
        x `i` s `i` s == x `i` s

    test "Induce homomorphism" $ \(x :: G) y (s :: [Int]) ->
        x `i` s + y `i` s == (x + y) `i` s && x `i` s * y `i` s == (x * y) `i` s

    test "Remove single vertex" $ \x ->
        removeVertex x (vertex x) == (empty :: G)

    let d x = (fromGraph x) :: DfsForest Int
    test "DFS idempotence" $ \(x :: AdjacencyMap Int) ->
        dfsForest x == dfsForest (forest $ dfsForest x)
    test "DFS subgraph" $ \(x :: AdjacencyMap Int) ->
        forest (dfsForest x) `isSubgraphOf` x
    test "DFS homomorphism" $ \x y ->
        d x + d y == d (x + y) && d x * d y == d (x * y)
    test "DFS reflexivity" $ \x ->
        (vertex x :: DfsForest Int) == vertex x * vertex x

    let ts x = (fromGraph x) :: TopSort Int
    test "TopSort is a topological sort" $ \(x :: AdjacencyMap Int) ->
        fmap (isTopSort x) (topSort x) /= Just False

    test "TopSort of a cyclic graph" $ \(x :: AdjacencyMap Int) ys -> not (null ys) ==>
        topSort (x + circuit (nubOrd ys)) == Nothing

    test "TopSort idempotence" $ \(x :: AdjacencyMap Int) ->
        (topSort . path =<< topSort x) == (topSort x)

    test "TopSort homomorphism" $ \x y ->
        ts x + ts y == ts (x + y) && ts x * ts y == ts (x * y)

    let t x = transpose (fromGraph x) :: G
    test "Transpose self-inverse" $ \x ->
        t (t x) == x

    test "Transpose antihomomorphism" $ \x y ->
        t x + t y == t (x + y) && t x * t y == t (y * x)

    test "EdgeList of edges" $ \xs ->
        nubOrd (sort xs) == edgeList (fromGraph (edges xs :: G))

    putStrLn "============ Reflexive relation ============"
    quickCheck (reflexiveAxioms :: GraphTestsuite (ReflexiveRelation Int))

    putStrLn "============ Symmetric relation ============"
    quickCheck (undirectedAxioms :: GraphTestsuite (SymmetricRelation Int))

    putStrLn "============ Transitive relation ============"
    quickCheck $ mapSize (min 20)
        (transitiveAxioms :: GraphTestsuite (TransitiveRelation Int))

    test "Path equals clique" $ mapSize (min 20) $ \xs ->
        path xs == (clique xs :: TransitiveRelation Int)

    putStrLn "============ Preorder relation ============"
    quickCheck $ mapSize (min 20)
        (preorderAxioms :: GraphTestsuite (PreorderRelation Int))

    test "Path equals clique" $ mapSize (min 20) $ \xs ->
        path xs == (clique xs :: PreorderRelation Int)

