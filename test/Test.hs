{-# LANGUAGE FlexibleContexts #-}
import Data.Foldable
import Data.List (sort)
import Data.List.Extra (nubOrd)
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.Basic
import Algebra.Graph.Relation
import Algebra.Graph.Test
import Algebra.Graph.Util

type G = Basic Int
type R = Reflexive Int
type U = Undirected Int
type P = PartialOrder Int

test :: Testable a => String -> a -> IO ()
test str p = putStr (str ++ ": ") >> quickCheck p

main :: IO ()
main = do
    putStrLn "============ Basic ============"
    quickCheck (axioms   :: GraphTestsuite (Basic Int))
    quickCheck (theorems :: GraphTestsuite (Basic Int))

    putStrLn "============ Relation ============"
    quickCheck (axioms :: GraphTestsuite (Relation Int))

    putStrLn "============ Adjacency Map ============"
    quickCheck (axioms :: GraphTestsuite (AdjacencyMap Int))

    putStrLn "============ Directed graphs ============"
    test "Upper bound" $ \(x :: G) ->
        x `isSubgraphOf` (vertices (toList x) * vertices (toList x))

    test "Path-circuit order" $ \xs ->
        path xs `isSubgraphOf` (circuit xs :: G)

    let comm  = fmap $ \(a, b) -> (b, a)
        assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "Box commutativity" $ mapSize (min 10) $ \(x :: G) (y :: G) ->
        x `box` y == comm (y `box` x)

    test "Box associativity" $ mapSize (min 10) $ \(x :: G) (y :: G) (z :: G) ->
        (x `box` y) `box` z == assoc (x `box` (y `box` z))

    test "Induce full graph" $ \(x :: G) ->
        induce (const True) x == x

    test "Induce empty graph" $ \(x :: G) ->
        induce (const False) x == empty

    let i x (s :: [Int]) = induce (`elem` s) x
    test "Induce subgraph" $ \s (x :: G) ->
        (x `i` s) `isSubgraphOf` x

    test "Induce commutativity" $ \(x :: G) s t ->
        x `i` s `i` t == x `i` t `i` s

    test "Induce idempotence" $ \(x :: G) s ->
        x `i` s `i` s == x `i` s

    test "Induce homomorphism" $ \(x :: G) y s ->
        x `i` s + y `i` s == (x + y) `i` s && x `i` s * y `i` s == (x * y) `i` s

    test "Remove single vertex" $ \x ->
        removeVertex x (vertex x) == (empty :: G)

    let d x = (foldBasic x) :: Dfs Int
    test "DFS idempotence" $ \x ->
        d x == d (forest . dfsForest $ d x)
    test "DFS subgraph" $ \x ->
        forest (dfsForest $ d x) `isSubgraphOf` x
    test "DFS homomorphism" $ \x y ->
        d x + d y == d (x + y) && d x * d y == d (x * y)
    test "DFS reflexivity" $ \x ->
        (vertex x :: Dfs Int) == vertex x * vertex x

    let ts x = (foldBasic x) :: TopSort Int
    test "TopSort is a topological sort" $ \x ->
        fmap (isTopSort $ foldBasic x) (topSort $ ts x) /= Just False

    test "TopSort of a cyclic graph" $ \x ys -> not (null ys) ==>
        topSort (ts $ x + circuit (nubOrd ys)) == Nothing

    test "TopSort idempotence" $ \x ->
        (topSort . ts . path =<< topSort (ts x)) == (topSort $ ts x)

    test "TopSort homomorphism" $ \x y ->
        ts x + ts y == ts (x + y) && ts x * ts y == ts (x * y)

    let t x = transpose (foldBasic x) :: G
    test "Transpose self-inverse" $ \x ->
        t (t x) == x

    test "Transpose antihomomorphism" $ \x y ->
        t x + t y == t (x + y) && t x * t y == t (y * x)

    test "EdgeList of fromEdgeList" $ \xs ->
        nubOrd (sort xs) == edgeList (foldBasic (fromEdgeList xs :: G))

    putStrLn "============ Reflexive graphs ============"
    test "Vertex self-loop" $ \x ->
        (vertex x :: R) == vertex x * vertex x

    putStrLn "============ Undirected graphs ============"
    test "Connect commutativity" $ \(x :: U) y ->
        x * y == y * x

    putStrLn "============ Partial Orders ============"
    test "Closure" $ mapSize (min 20) $ \(x :: P) y z -> y /= empty ==>
        x * y * z == x * y + y * z

    test "Path equals clique" $ mapSize (min 20) $ \xs ->
        path xs == (clique xs :: P)
