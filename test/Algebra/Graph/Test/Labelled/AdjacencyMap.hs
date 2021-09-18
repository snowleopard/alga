{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Labelled.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2021
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

import Data.Monoid (Any, Sum (..))

import Algebra.Graph.Label
import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Test
import Algebra.Graph.Test.API (toIntAPI, labelledAdjacencyMapAPI)
import Algebra.Graph.Test.Generic
import Algebra.Graph.ToGraph (reachable)

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set

tPoly :: Testsuite (AdjacencyMap Any) Ord
tPoly = ("Labelled.AdjacencyMap.", labelledAdjacencyMapAPI)

t :: TestsuiteInt (AdjacencyMap Any)
t = fmap toIntAPI tPoly

type S = Sum Int
type D = Distance Int

type LAI = AdjacencyMap Any Int
type LAS = AdjacencyMap S   Int
type LAD = AdjacencyMap D   Int

testLabelledAdjacencyMap :: IO ()
testLabelledAdjacencyMap = do
    putStrLn "\n============ Labelled.AdjacencyMap.consistent ============"
    test "arbitraryLabelledAdjacencyMap" $ \x -> consistent (x           :: LAS)
    test "empty" $                      consistent (empty                :: LAS)
    test "vertex" $ \x               -> consistent (vertex x             :: LAS)
    test "edge" $ \e x y             -> consistent (edge e x y           :: LAS)
    test "overlay" $ \x y            -> consistent (overlay x y          :: LAS)
    test "connect" $ size10 $ \e x y -> consistent (connect e x y        :: LAS)
    test "vertices" $ \xs            -> consistent (vertices xs          :: LAS)
    test "edges" $ \es               -> consistent (edges es             :: LAS)
    test "overlays" $ size10 $ \xs   -> consistent (overlays xs          :: LAS)
    test "fromAdjacencyMaps" $ \xs   -> consistent (fromAdjacencyMaps xs :: LAS)
    test "removeVertex" $ \x y       -> consistent (removeVertex x y     :: LAS)
    test "removeEdge" $ \x y z       -> consistent (removeEdge x y z     :: LAS)
    test "replaceVertex" $ \x y z    -> consistent (replaceVertex x y z  :: LAS)
    test "replaceEdge" $ \e x y z    -> consistent (replaceEdge e x y z  :: LAS)
    test "transpose" $ \x            -> consistent (transpose x          :: LAS)
    test "gmap" $ \(apply -> f) x    -> consistent (gmap f (x :: LAS)    :: LAS)
    test "emap" $ \(apply -> f) x    -> consistent (emap (fmap f::S->S) x:: LAS)
    test "induce" $ \(apply -> p) x  -> consistent (induce p x           :: LAS)

    test "closure"           $ size10 $ \x -> consistent (closure           x :: LAD)
    test "reflexiveClosure"  $ size10 $ \x -> consistent (reflexiveClosure  x :: LAD)
    test "symmetricClosure"  $ size10 $ \x -> consistent (symmetricClosure  x :: LAD)
    test "transitiveClosure" $ size10 $ \x -> consistent (transitiveClosure x :: LAD)

    testEmpty  t
    testVertex t

    putStrLn "\n============ Labelled.AdjacencyMap.edge ============"
    test "edge e    x y              == connect e (vertex x) (vertex y)" $ \(e :: S) (x :: Int) y ->
          edge e    x y              == connect e (vertex x) (vertex y)

    test "edge zero x y              == vertices [x,y]" $ \(x :: Int) y ->
          edge (zero :: S) x y       == vertices [x,y]

    test "hasEdge   x y (edge e x y) == (e /= mempty)" $ \(e :: S) (x :: Int) y ->
          hasEdge   x y (edge e x y) == (e /= mempty)

    test "edgeLabel x y (edge e x y) == e" $ \(e :: S) (x :: Int) y ->
          edgeLabel x y (edge e x y) == e

    test "edgeCount     (edge e x y) == if e == mempty then 0 else 1" $ \(e :: S) (x :: Int) y ->
          edgeCount     (edge e x y) == if e == mempty then 0 else 1

    test "vertexCount   (edge e 1 1) == 1" $ \(e :: S) ->
          vertexCount   (edge e 1 (1 :: Int)) == 1

    test "vertexCount   (edge e 1 2) == 2" $ \(e :: S) ->
          vertexCount   (edge e 1 (2 :: Int)) == 2

    test "x -<e>- y                  == edge e x y" $ \(e :: S) (x :: Int) y ->
          x -<e>- y                  == edge e x y

    testOverlay t

    putStrLn ""
    test "edgeLabel x y $ overlay (edge e x y) (edge zero x y) == e" $ \(e :: S) (x :: Int) y ->
          edgeLabel x y (overlay (edge e x y) (edge zero x y)) == e

    test "edgeLabel x y $ overlay (edge e x y) (edge f    x y) == e <+> f" $ \(e :: S) f (x :: Int) y ->
          edgeLabel x y (overlay (edge e x y) (edge f    x y)) == e <+> f

    putStrLn ""
    test "edgeLabel 1 3 $ transitiveClosure (overlay (edge e 1 2) (edge one 2 3)) == e" $ \(e :: D) ->
          edgeLabel 1 3 (transitiveClosure (overlay (edge e 1 2) (edge one 2 (3 :: Int)))) == e

    test "edgeLabel 1 3 $ transitiveClosure (overlay (edge e 1 2) (edge f   2 3)) == e <.> f" $ \(e :: D) f ->
          edgeLabel 1 3 (transitiveClosure (overlay (edge e 1 2) (edge f   2 (3 :: Int))))== e <.> f

    putStrLn "\n============ Labelled.AdjacencyMap.connect ============"
    test "isEmpty     (connect e x y) == isEmpty   x   && isEmpty   y" $ size10 $ \(e :: S) (x :: LAS) y ->
          isEmpty     (connect e x y) ==(isEmpty   x   && isEmpty   y)

    test "hasVertex z (connect e x y) == hasVertex z x || hasVertex z y" $ size10 $ \(e :: S) (x :: LAS) y z ->
          hasVertex z (connect e x y) ==(hasVertex z x || hasVertex z y)

    test "vertexCount (connect e x y) >= vertexCount x" $ size10 $ \(e :: S) (x :: LAS) y ->
          vertexCount (connect e x y) >= vertexCount x

    test "vertexCount (connect e x y) <= vertexCount x + vertexCount y" $ size10 $ \(e :: S) (x :: LAS) y ->
          vertexCount (connect e x y) <= vertexCount x + vertexCount y

    test "edgeCount   (connect e x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y" $ size10 $ \(e :: S) (x :: LAS) y ->
          edgeCount   (connect e x y) <= vertexCount x * vertexCount y + edgeCount x + edgeCount y

    test "vertexCount (connect e 1 2) == 2" $ \(e :: Any) ->
          vertexCount (connect e 1 (2 :: LAI)) == 2

    test "edgeCount   (connect e 1 2) == if e == zero then 0 else 1" $ \(e :: Any) ->
          edgeCount   (connect e 1 (2 :: LAI)) == if e == zero then 0 else 1

    testVertices t

    putStrLn "\n============ Labelled.AdjacencyMap.edges ============"
    test "edges []        == empty" $
          edges []        == (empty :: LAS)

    test "edges [(e,x,y)] == edge e x y" $ \(e :: S) (x :: Int) y ->
          edges [(e,x,y)] == edge e x y

    test "edges           == overlays . map (\\(e, x, y) -> edge e x y)" $ \(es :: [(S, Int, Int)]) ->
          edges es        ==(overlays . map (\(e, x, y) -> edge e x y)) es

    testOverlays t

    putStrLn "\n============ Labelled.AdjacencyMap.fromAdjacencyMaps ============"
    test "fromAdjacencyMaps []                                  == empty" $
          fromAdjacencyMaps []                                  == (empty :: LAS)

    test "fromAdjacencyMaps [(x, Map.empty)]                    == vertex x" $ \(x :: Int) ->
          fromAdjacencyMaps [(x, Map.empty)]                    == (vertex x :: LAS)

    test "fromAdjacencyMaps [(x, Map.singleton y e)]            == if e == zero then vertices [x,y] else edge e x y" $ \(e :: S) (x :: Int) y ->
          fromAdjacencyMaps [(x, Map.singleton y e)]            == if e == zero then vertices [x,y] else edge e x y

    test "overlay (fromAdjacencyMaps xs) (fromAdjacencyMaps ys) == fromAdjacencyMaps (xs ++ ys)" $ \xs ys ->
          overlay (fromAdjacencyMaps xs) (fromAdjacencyMaps ys) == (fromAdjacencyMaps (xs ++ ys) :: LAS)

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

    test "isEmpty (removeEdge x y $ edge e x y) == False" $ \(e :: S) (x :: Int) y ->
          isEmpty (removeEdge x y $ edge e x y) == False

    testHasVertex t

    putStrLn "\n============ Labelled.AdjacencyMap.hasEdge ============"
    test "hasEdge x y empty            == False" $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge e x y)     == (e /= zero)" $ \(e :: S) (x :: Int) y ->
          hasEdge x y (edge e x y)     == (e /= zero)

    test "hasEdge x y . removeEdge x y == const False" $ \x y (z :: LAS) ->
         (hasEdge x y . removeEdge x y) z == const False z

    test "hasEdge x y                  == not . null . filter (\\(_,ex,ey) -> ex == x && ey == y) . edgeList" $ \x y (z :: LAS) -> do
        (_, u, v) <- elements ((zero, x, y) : edgeList z)
        return $ hasEdge u v z         == (not . null . filter (\(_,ex,ey) -> ex == u && ey == v) . edgeList) z

    putStrLn "\n============ Labelled.AdjacencyMap.edgeLabel ============"
    test "edgeLabel x y empty         == zero" $ \(x :: Int) y ->
          edgeLabel x y empty         == (zero :: S)

    test "edgeLabel x y (vertex z)    == zero" $ \(x :: Int) y z ->
          edgeLabel x y (vertex z)    == (zero :: S)

    test "edgeLabel x y (edge e x y)  == e" $ \(e :: S) (x :: Int) y ->
          edgeLabel x y (edge e x y)  == e

    test "edgeLabel s t (overlay x y) == edgeLabel s t x + edgeLabel s t y" $ \(x :: LAS) y -> do
        z <- arbitrary
        s <- elements ([z] ++ vertexList x ++ vertexList y)
        t <- elements ([z] ++ vertexList x ++ vertexList y)
        return $ edgeLabel s t (overlay x y) == edgeLabel s t x + edgeLabel s t y

    testVertexCount t

    putStrLn "\n============ Labelled.AdjacencyMap.edgeCount ============"
    test "edgeCount empty        == 0" $
          edgeCount empty        == 0

    test "edgeCount (vertex x)   == 0" $ \(x :: Int) ->
          edgeCount (vertex x)   == 0

    test "edgeCount (edge e x y) == if e == zero then 0 else 1" $ \(e :: S) (x :: Int) y ->
          edgeCount (edge e x y) == if e == zero then 0 else 1

    test "edgeCount              == length . edgeList" $ \(x :: LAS) ->
          edgeCount x            == (length . edgeList) x

    testVertexList t

    putStrLn "\n============ Labelled.AdjacencyMap.edgeList ============"
    test "edgeList empty        == []" $
          edgeList (empty :: LAS) == []

    test "edgeList (vertex x)   == []" $ \(x :: Int) ->
          edgeList (vertex x :: LAS) == []

    test "edgeList (edge e x y) == if e == zero then [] else [(e,x,y)]" $ \(e :: S) (x :: Int) y ->
          edgeList (edge e x y) == if e == zero then [] else [(e,x,y)]

    testVertexSet t

    putStrLn "\n============ Labelled.AdjacencyMap.edgeSet ============"
    test "edgeSet empty        == Set.empty" $
          edgeSet (empty :: LAS) == Set.empty

    test "edgeSet (vertex x)   == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x :: LAS) == Set.empty

    test "edgeSet (edge e x y) == if e == zero then Set.empty else Set.singleton (e,x,y)" $ \(e :: S) (x :: Int) y ->
          edgeSet (edge e x y) == if e == zero then Set.empty else Set.singleton (e,x,y)

    putStrLn "\n============ Labelled.AdjacencyMap.preSet ============"
    test "preSet x empty        == Set.empty" $ \x ->
          preSet x (empty :: LAS) == Set.empty

    test "preSet x (vertex x)   == Set.empty" $ \x ->
          preSet x (vertex x :: LAS) == Set.empty

    test "preSet 1 (edge e 1 2) == Set.empty" $ \e ->
          preSet 1 (edge e 1 2 :: LAS) == Set.empty

    test "preSet y (edge e x y) == if e == zero then Set.empty else Set.fromList [x]" $ \(e :: S) (x :: Int) y ->
          preSet y (edge e x y) == if e == zero then Set.empty else Set.fromList [x]

    putStrLn "\n============ Labelled.AdjacencyMap.postSet ============"
    test "postSet x empty        == Set.empty" $ \x ->
          postSet x (empty :: LAS) == Set.empty

    test "postSet x (vertex x)   == Set.empty" $ \x ->
          postSet x (vertex x :: LAS) == Set.empty

    test "postSet x (edge e x y) == if e == zero then Set.empty else Set.fromList [y]" $ \(e :: S) (x :: Int) y ->
          postSet x (edge e x y) == if e == zero then Set.empty else Set.fromList [y]

    test "postSet 2 (edge e 1 2) == Set.empty" $ \e ->
          postSet 2 (edge e 1 2 :: LAS) == Set.empty

    putStrLn "\n============ Labelled.AdjacencyMap.skeleton ============"
    test "hasEdge x y == hasEdge x y . skeleton" $ \x y (z :: LAS) ->
          hasEdge x y z == (AM.hasEdge x y . skeleton) z

    putStrLn "\n============ Labelled.AdjacencyMap.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \x ->
          removeVertex x (vertex x)       == (empty :: LAS)

    test "removeVertex 1 (vertex 2)       == vertex 2" $
          removeVertex 1 (vertex 2)       == (vertex 2 :: LAS)

    test "removeVertex x (edge e x x)     == empty" $ \(e :: S) (x :: Int) ->
          removeVertex x (edge e x x)     == empty

    test "removeVertex 1 (edge e 1 2)     == vertex 2" $ \(e :: S) ->
          removeVertex 1 (edge e 1 2)     == vertex (2 :: Int)

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (y :: LAS) ->
         (removeVertex x . removeVertex x) y == removeVertex x y

    putStrLn "\n============ Labelled.AdjacencyMap.removeEdge ============"
    test "removeEdge x y (edge e x y)     == vertices [x,y]" $ \(e :: S) (x :: Int) y ->
          removeEdge x y (edge e x y)     == vertices [x,y]

    test "removeEdge x y . removeEdge x y == removeEdge x y" $ \x y (z :: LAS) ->
         (removeEdge x y . removeEdge x y) z == removeEdge x y z

    test "removeEdge x y . removeVertex x == removeVertex x" $ \x y (z :: LAS) ->
         (removeEdge x y . removeVertex x) z == removeVertex x z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2" $
          removeEdge 1 1 (1 * 1 * 2 * 2)  == (1 * 2 * 2 :: LAD)

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2" $
          removeEdge 1 2 (1 * 1 * 2 * 2)  == (1 * 1 + 2 * 2 :: LAD)

    putStrLn "\n============ Labelled.AdjacencyMap.replaceVertex ============"
    test "replaceVertex x x            == id" $ \x y ->
          replaceVertex x x y          == (y :: LAS)

    test "replaceVertex x y (vertex x) == vertex y" $ \x y ->
          replaceVertex x y (vertex x) == (vertex y :: LAS)

    test "replaceVertex x y            == gmap (\\v -> if v == x then y else v)" $ \x y (z :: LAS) ->
          replaceVertex x y z          == gmap (\v -> if v == x then y else v) z

    putStrLn "\n============ Labelled.AdjacencyMap.replaceEdge ============"
    test "replaceEdge e x y z                 == overlay (removeEdge x y z) (edge e x y)" $ \(e :: S) (x :: Int) y z ->
          replaceEdge e x y z                 == overlay (removeEdge x y z) (edge e x y)

    test "replaceEdge e x y (edge f x y)      == edge e x y" $ \(e :: S) f (x :: Int) y ->
          replaceEdge e x y (edge f x y)      == edge e x y

    test "edgeLabel x y (replaceEdge e x y z) == e" $ \(e :: S) (x :: Int) y z ->
          edgeLabel x y (replaceEdge e x y z) == e

    putStrLn "\n============ Labelled.AdjacencyMap.transpose ============"
    test "transpose empty        == empty" $
          transpose empty        == (empty :: LAS)

    test "transpose (vertex x)   == vertex x" $ \x ->
          transpose (vertex x)   == (vertex x :: LAS)

    test "transpose (edge e x y) == edge e y x" $ \e x y ->
          transpose (edge e x y) == (edge e y x :: LAS)

    test "transpose . transpose  == id" $ size10 $ \x ->
         (transpose . transpose) x == (x :: LAS)

    putStrLn "\n============ Labelled.AdjacencyMap.gmap ============"
    test "gmap f empty        == empty" $ \(apply -> f) ->
          gmap f (empty :: LAS) == (empty :: LAS)

    test "gmap f (vertex x)   == vertex (f x)" $ \(apply -> f) x ->
          gmap f (vertex x :: LAS) == (vertex (f x) :: LAS)

    test "gmap f (edge e x y) == edge e (f x) (f y)" $ \(apply -> f) e x y ->
          gmap f (edge e x y :: LAS) == (edge e (f x) (f y) :: LAS)

    test "gmap id             == id" $ \x ->
          gmap id x           == (x :: LAS)

    test "gmap f . gmap g     == gmap (f . g)" $ \(apply -> f) (apply -> g) x ->
         ((gmap f :: LAS -> LAS) . gmap g) (x :: LAS)  == gmap (f . g) x

    -- TODO: We only test homomorphisms @h@ on @Sum Int@, which all happen to be
    -- just linear transformations: @h = (k*)@ for some @k :: Int@. These tests
    -- are therefore rather weak and do not cover the ruch space of possible
    -- monoid homomorphisms. How can we improve this?
    putStrLn "\n============ Labelled.AdjacencyMap.emap ============"
    test "emap h empty           == empty" $ \(k :: S) ->
        let h = (k*)
        in emap h empty          == (empty :: LAS)

    test "emap h (vertex x)      == vertex x" $ \(k :: S) x ->
        let h = (k*)
        in emap h (vertex x)     == (vertex x :: LAS)

    test "emap h (edge e x y)    == edge (h e) x y" $ \(k :: S) e x y ->
        let h = (k*)
        in emap h (edge e x y)   == (edge (h e) x y :: LAS)

    test "emap h (overlay x y)   == overlay (emap h x) (emap h y)" $ \(k :: S) x y ->
        let h = (k*)
        in emap h (overlay x y)  == (overlay (emap h x) (emap h y) :: LAS)

    test "emap h (connect e x y) == connect (h e) (emap h x) (emap h y)" $ \(k :: S) (e :: S) x y ->
        let h = (k*)
        in emap h (connect e x y) == (connect (h e) (emap h x) (emap h y) :: LAS)

    test "emap id                == id" $ \x ->
          emap id x              == (id x :: LAS)

    test "emap g . emap h        == emap (g . h)" $ \(k :: S) (l :: S) x ->
        let h = (k*)
            g = (l*)
        in (emap g . emap h) x   == (emap (g . h) x :: LAS)

    testInduce t
    testInduceJust tPoly

    putStrLn "\n============ Labelled.AdjacencyMap.closure ============"
    test "closure empty         == empty" $
          closure empty         == (empty :: LAD)

    test "closure (vertex x)    == edge one x x" $ \x ->
          closure (vertex x)    == (edge one x x :: LAD)

    test "closure (edge e x x)  == edge one x x" $ \e x ->
          closure (edge e x x)  == (edge one x x :: LAD)

    test "closure (edge e x y)  == edges [(one,x,x), (e,x,y), (one,y,y)]" $ \e x y ->
          closure (edge e x y)  == (edges [(one,x,x), (e,x,y), (one,y,y)] :: LAD)

    test "closure               == reflexiveClosure . transitiveClosure" $ size10 $ \x ->
          closure (x :: LAD)    == (reflexiveClosure . transitiveClosure) x

    test "closure               == transitiveClosure . reflexiveClosure" $ size10 $ \x ->
          closure (x :: LAD)    == (transitiveClosure . reflexiveClosure) x

    test "closure . closure     == closure" $ size10 $ \x ->
         (closure . closure) x  == closure (x :: LAD)

    test "postSet x (closure y) == Set.fromList (reachable x y)" $ size10 $ \(x :: Int) (y :: LAD) ->
          postSet x (closure y) == Set.fromList (reachable x y)

    putStrLn "\n============ Labelled.AdjacencyMap.reflexiveClosure ============"
    test "reflexiveClosure empty              == empty" $
          reflexiveClosure empty              == (empty :: LAD)

    test "reflexiveClosure (vertex x)         == edge one x x" $ \x ->
          reflexiveClosure (vertex x)         == (edge one x x :: LAD)

    test "reflexiveClosure (edge e x x)       == edge one x x" $ \e x ->
          reflexiveClosure (edge e x x)       == (edge one x x :: LAD)

    test "reflexiveClosure (edge e x y)       == edges [(one,x,x), (e,x,y), (one,y,y)]" $ \e x y ->
          reflexiveClosure (edge e x y)       == (edges [(one,x,x), (e,x,y), (one,y,y)] :: LAD)

    test "reflexiveClosure . reflexiveClosure == reflexiveClosure" $ size10 $ \x ->
         (reflexiveClosure . reflexiveClosure) x == reflexiveClosure (x :: LAD)

    putStrLn "\n============ Labelled.AdjacencyMap.symmetricClosure ============"
    test "symmetricClosure empty              == empty" $
          symmetricClosure empty              == (empty :: LAD)

    test "symmetricClosure (vertex x)         == vertex x" $ \x ->
          symmetricClosure (vertex x)         == (vertex x :: LAD)

    test "symmetricClosure (edge e x y)       == edges [(e,x,y), (e,y,x)]" $ \e x y ->
          symmetricClosure (edge e x y)       == (edges [(e,x,y), (e,y,x)] :: LAD)

    test "symmetricClosure x                  == overlay x (transpose x)" $ \x ->
          symmetricClosure x                  == (overlay x (transpose x) :: LAD)

    test "symmetricClosure . symmetricClosure == symmetricClosure" $ size10 $ \x ->
         (symmetricClosure . symmetricClosure) x == symmetricClosure (x :: LAD)

    putStrLn "\n============ Labelled.AdjacencyMap.transitiveClosure ============"
    test "transitiveClosure empty               == empty" $
          transitiveClosure empty               == (empty :: LAD)

    test "transitiveClosure (vertex x)          == vertex x" $ \x ->
          transitiveClosure (vertex x)          == (vertex x :: LAD)

    test "transitiveClosure (edge e x y)        == edge e x y" $ \e x y ->
          transitiveClosure (edge e x y)        == (edge e x y :: LAD)

    test "transitiveClosure . transitiveClosure == transitiveClosure" $ size10 $ \x ->
         (transitiveClosure . transitiveClosure) x == transitiveClosure (x :: LAD)
