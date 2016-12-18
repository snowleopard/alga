{-# LANGUAGE ScopedTypeVariables #-}
import Data.Foldable
import Test.QuickCheck

import Algebra.Graph
import Algebra.Graph.Basic

type G = Basic Int
type R = Reflexive Int
type U = Undirected Int
type P = PartialOrder Int

test :: Testable a => String -> a -> IO ()
test str p = putStr (str ++ ": ") >> quickCheck p

main :: IO ()
main = do
    putStrLn "============ Directed graphs ============"

    test "Overlay identity" $ \(x :: G) ->
        x + empty == x

    test "Overlay commutativity" $ \(x :: G) y ->
        x + y == y + x

    test "Overlay associativity" $ \(x :: G) y z ->
        x + (y + z) == (x + y) + z

    test "Overlay idempotence" $ \(x :: G) ->
        x + x == x

    test "Connect identity" $ \(x :: G) ->
        x * empty == x && empty * x == x

    test "Overlay associativity" $ \(x :: G) y z ->
        x * (y * z) == (x * y) * z

    test "Distributivity" $ \(x :: G) y z ->
        x * (y + z) == x * y + x * z && (x + y) * z == x * z + y * z

    test "Decomposition" $ \(x :: G) y z ->
        x * y * z == x * y + x * z + y * z

    test "Absorption" $ \(x :: G) y ->
        x + x * y == x * y && y + x * y == x * y

    test "Connect saturation" $ \(x :: G) ->
        x * x == x * x * x

    test "Lower bound" $ \(x :: G) ->
        empty `isSubgraphOf` x

    test "Upper bound" $ \(x :: G) ->
        x `isSubgraphOf` (vertices (toList x) * vertices (toList x))

    test "Overlay order" $ \(x :: G) y ->
        x `isSubgraphOf` (x + y)

    test "Connect order" $ \(x :: G) y ->
        x `isSubgraphOf` (x * y) && y `isSubgraphOf` (x * y)

    test "Overlay-connect order" $ \(x :: G) y ->
        (x + y) `isSubgraphOf` (x * y)

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

    test "Induce subgraph" $ \(xs :: [Int]) (y :: G) ->
        induce (`elem` xs) y `isSubgraphOf` y

    let i x as = induce (`elem` (as :: [Int])) x
    test "Induce commutativity" $ \(x :: G) as bs ->
        x `i` as `i` bs == x `i` bs `i` as

    test "Induce idempotence" $ \(x :: G) as ->
        x `i` as `i` as == x `i` as

    test "Remove single vertex" $ \x ->
        removeVertex x (vertex x) == (empty :: G)

    test "Transpose inverse" $ \(x :: G) ->
        transpose (transpose $ foldBasic x) == x

    putStrLn "============ Reflexive graphs ============"
    test "Vertex self-loop" $ \x ->
        (vertex x :: R) == vertex x * vertex x

    putStrLn "============ Undirected graphs ============"
    test "Connect commutativity" $ \(x :: U) y ->
        x * y == y * x

    putStrLn "============ Partial Orders ============"
    test "Closure" $ mapSize (min 20) $ \(x :: P) y z ->
        y == empty || x * y * z == x * y + y * z

    test "Path equals clique" $ mapSize (min 20) $ \xs ->
        path xs == (clique xs :: P)
