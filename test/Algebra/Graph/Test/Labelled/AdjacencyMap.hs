-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Labelled.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Labelled.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Labelled.AdjacencyMap (
    -- * Testsuite
    testLabelledAdjacencyMap
    ) where

import Data.Monoid

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Internal
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

t :: Testsuite
t = testsuite "Labelled.AdjacencyMap." (empty :: LAI)

type LAI = AdjacencyMap Any Int
type LAS = AdjacencyMap (Sum Int) Int
type LAD = AdjacencyMap (Distance Int) Int

testLabelledAdjacencyMap :: IO ()
testLabelledAdjacencyMap = do
    putStrLn "\n============ Labelled.AdjacencyMap ============"
    test "Consistency of arbitraryLabelledAdjacencyMap" $ \(m :: LAI) ->
        consistent m

    test "Consistency of fromAdjacencyMaps" $ \xs ->
        consistent (fromAdjacencyMaps xs :: LAI)

    testEmpty  t
    testVertex t

    putStrLn "\n============ Labelled.AdjacencyMap.edge ============"
    test "edge e    x y              == connect e (vertex x) (vertex y)" $ \(e :: Sum Int) (x :: Int) y ->
          edge e    x y              == connect e (vertex x) (vertex y)

    test "edge zero x y              == vertices [x,y]" $ \(x :: Int) y ->
          edge (zero :: Sum Int) x y == vertices [x,y]

    test "hasEdge   x y (edge e x y) == (e /= mempty)" $ \(e :: Sum Int) (x :: Int) y ->
          hasEdge   x y (edge e x y) == (e /= mempty)

    test "edgeLabel x y (edge e x y) == e" $ \(e :: Sum Int) (x :: Int) y ->
          edgeLabel x y (edge e x y) == e

    test "edgeCount     (edge e x y) == if e == mempty then 0 else 1" $ \(e :: Sum Int) (x :: Int) y ->
          edgeCount     (edge e x y) == if e == mempty then 0 else 1

    test "vertexCount   (edge e 1 1) == 1" $ \(e :: Sum Int) ->
          vertexCount   (edge e 1 (1 :: Int)) == 1

    test "vertexCount   (edge e 1 2) == 2" $ \(e :: Sum Int) ->
          vertexCount   (edge e 1 (2 :: Int)) == 2

    test "x -<e>- y                  == edge e x y" $ \(e :: Sum Int) (x :: Int) y ->
          x -<e>- y                  == edge e x y

    testOverlay t

    putStrLn ""
    test "overlay (edge e x y) (edge f x y) == edge (e <+> f) x y" $ \(e :: Sum Int) f (x :: Int) y ->
          overlay (edge e x y) (edge f x y) == edge (e <+> f) x y

    putStrLn ""
    test "transitiveClosure (overlay (edge e 1 2) (edge f 2 3)) == overlays [edge e 1 2, edge f 2 3, edge (e <.> f) 1 3]" $ \(e :: Distance Int) f ->
          transitiveClosure (overlay (edge e 1 2) (edge f 2 3)) == overlays [edge e 1 2, edge f 2 3, edge (e <.> f) 1 (3 :: Int)]


    putStrLn "\n============ Labelled.AdjacencyMap.connect ============"
    test "isEmpty     (connect e x y) == isEmpty   x   && isEmpty   y" $ sizeLimit $ \(e :: Sum Int) (x :: LAS) y ->
          isEmpty     (connect e x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect e x y) == hasVertex z x || hasVertex z y" $ sizeLimit $ \(e :: Sum Int) (x :: LAS) y z ->
          hasVertex z (connect e x y) ==(hasVertex z x || hasVertex z y)

    test "vertexCount (connect e x y) >= vertexCount x" $ sizeLimit $ \(e :: Sum Int) (x :: LAS) y ->
          vertexCount (connect e x y) >= vertexCount x

    test "vertexCount (connect e x y) <= vertexCount x + vertexCount y" $ sizeLimit $ \(e :: Sum Int) (x :: LAS) y ->
          vertexCount (connect e x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect e x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ sizeLimit $ \(e :: Sum Int) (x :: LAS) y ->
          edgeCount   (connect e x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect e 1 2) == 2" $ \(e :: Any) ->
          vertexCount (connect e 1 (2 :: LAI)) == 2

    test "edgeCount   (connect e 1 2) == if e == zero then 0 else 1" $ \(e :: Any) ->
          edgeCount   (connect e 1 (2 :: LAI)) == if e == zero then 0 else 1

    testVertices t

    putStrLn "\n============ Labelled.AdjacencyMap.edges ============"
    test "edges []        == empty" $
          edges []        == (empty :: LAS)

    test "edges [(e,x,y)] == edge e x y" $ \(e :: Sum Int) (x :: Int) y ->
          edges [(e,x,y)] == edge e x y

    test "edges           == overlays . map (\\(e, x, y) -> edge e x y)" $ \(es :: [(Sum Int, Int, Int)]) ->
          edges es        ==(overlays . map (\(e, x, y) -> edge e x y)) es

    testOverlays t

    putStrLn "\n============ Labelled.AdjacencyMap.isSubgraphOf ============"
    test "isSubgraphOf empty      x     ==  True" $ \(x :: LAS) ->
          isSubgraphOf empty      x     ==  True

    test "isSubgraphOf (vertex x) empty ==  False" $ \(x :: Int) ->
          isSubgraphOf (vertex x)(empty :: LAS)==  False

    test "isSubgraphOf x y              ==> x <= y" $ \(x :: LAD) z ->
        let y = x + z -- Make sure we hit the precondition
        in isSubgraphOf x y             ==> x <= y

    putStrLn "\n============ Labelled.AdjacencyMap.isEmpty ============"
    test "isEmpty empty                         == True" $
          isEmpty empty                         == True

    test "isEmpty (overlay empty empty)         == True" $
          isEmpty (overlay empty empty :: LAS)  == True

    test "isEmpty (vertex x)                    == False" $ \(x :: Int) ->
          isEmpty (vertex x)                    == False

    test "isEmpty (removeVertex x $ vertex x)   == True" $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x)   == True

    test "isEmpty (removeEdge x y $ edge e x y) == False" $ \(e :: Sum Int) (x :: Int) y ->
          isEmpty (removeEdge x y $ edge e x y) == False

    putStrLn "\n============ Labelled.AdjacencyMap.replaceEdge ============"
    test "replaceEdge e x y m                 == overlay (removeEdge x y m) (edge e x y)" $ \(e :: Sum Int) (x :: Int) (y :: Int) m ->
          replaceEdge e x y m                 == overlay (removeEdge x y m) (edge e x y)

    test "replaceEdge e x y (edge f x y)      == edge e x y" $ \(e :: Sum Int) (f :: Sum Int) (x :: Int) (y :: Int) ->
          replaceEdge e x y (edge f x y)      == edge e x y

    test "edgeLabel x y (replaceEdge e x y m) == e" $ \(e :: Sum Int) (x :: Int) (y :: Int) m ->
          edgeLabel x y (replaceEdge e x y m) == e
