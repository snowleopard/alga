-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2022
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph" and polymorphic functions defined in
-- "Algebra.Graph.HigherKinded.Class".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Graph (
    -- * Testsuite
    testGraph
    ) where

import Data.Either

import Algebra.Graph
import Algebra.Graph.Test
import Algebra.Graph.Test.API (toIntAPI, graphAPI)
import Algebra.Graph.Test.Generic
import Algebra.Graph.ToGraph (reachable)

import qualified Data.Graph as KL

tPoly :: Testsuite Graph Ord
tPoly = ("Graph.", graphAPI)

t :: TestsuiteInt Graph
t = fmap toIntAPI tPoly

type G = Graph Int

testGraph :: IO ()
testGraph = do
    putStrLn "\n============ Graph ============"
    test "Axioms of graphs"   (axioms   @G)
    test "Theorems of graphs" (theorems @G)

    testBasicPrimitives t
    testIsSubgraphOf    t
    testToGraph         t
    testSize            t
    testGraphFamilies   t
    testTransformations t
    testInduceJust      tPoly

    ----------------------------------------------------------------
    -- Generic relational composition tests, plus an additional one
    testCompose         t
    test "size (compose x y)                        <= edgeCount x + edgeCount y + 1" $ \(x :: G) y ->
          size (compose x y)                        <= edgeCount x + edgeCount y + 1
    ----------------------------------------------------------------

    putStrLn "\n============ Graph.(===) ============"
    test "    x === x         == True" $ \(x :: G) ->
             (x === x)        == True

    test "    x === x + empty == False" $ \(x :: G) ->
             (x === x + empty)== False

    test "x + y === x + y     == True" $ \(x :: G) y ->
         (x + y === x + y)    == True

    test "1 + 2 === 2 + 1     == False" $
         (1 + 2 === 2 + (1 :: G)) == False

    test "x + y === x * y     == False" $ \(x :: G) y ->
         (x + y === x * y)    == False


    testMesh        tPoly
    testTorus       tPoly
    testDeBruijn    tPoly
    testSplitVertex t
    testBind        t
    testSimplify    t
    testBox         tPoly

    putStrLn "\n============ Graph.sparsify ============"
    test "sort . reachable x       == sort . rights . reachable (Right x) . sparsify" $ \x (y :: G) ->
         (sort . reachable x) y    == (sort . rights . reachable (Right x) . sparsify) y

    test "vertexCount (sparsify x) <= vertexCount x + size x + 1" $ \(x :: G) ->
          vertexCount (sparsify x) <= vertexCount x + size x + 1

    test "edgeCount   (sparsify x) <= 3 * size x" $ \(x :: G) ->
          edgeCount   (sparsify x) <= 3 * size x

    test "size        (sparsify x) <= 3 * size x" $ \(x :: G) ->
          size        (sparsify x) <= 3 * size x

    putStrLn "\n============ Graph.sparsifyKL ============"
    test "sort . reachable k                 == sort . filter (<= n) . flip reachable k . sparsifyKL n" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        k  <- choose (1, n)
        es <- listOf pairs
        let x = vertices [1..n] `overlay` edges es
        return $ (sort . reachable k) x == (sort . filter (<= n) . flip KL.reachable k . sparsifyKL n) x

    test "length (vertices $ sparsifyKL n x) <= vertexCount x + size x + 1" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        es <- listOf pairs
        let x = vertices [1..n] `overlay` edges es
        return $ length (KL.vertices $ sparsifyKL n x) <= vertexCount x + size x + 1

    test "length (edges    $ sparsifyKL n x) <= 3 * size x" $ \(Positive n) -> do
        let pairs = (,) <$> choose (1, n) <*> choose (1, n)
        es <- listOf pairs
        let x = vertices [1..n] `overlay` edges es
        return $ length (KL.edges $ sparsifyKL n x) <= 3 * size x

    putStrLn "\n============ Graph.context ============"
    test "context (const False) x                   == Nothing" $ \x ->
          context (const False) (x :: G)            == Nothing

    test "context (== 1)        (edge 1 2)          == Just (Context [   ] [2  ])" $
          context (== 1)        (edge 1 2 :: G)     == Just (Context [   ] [2  ])

    test "context (== 2)        (edge 1 2)          == Just (Context [1  ] [   ])" $
          context (== 2)        (edge 1 2 :: G)     == Just (Context [1  ] [   ])

    test "context (const True ) (edge 1 2)          == Just (Context [1  ] [2  ])" $
          context (const True ) (edge 1 2 :: G)     == Just (Context [1  ] [2  ])

    test "context (== 4)        (3 * 1 * 4 * 1 * 5) == Just (Context [3,1] [1,5])" $
          context (== 4)        (3 * 1 * 4 * 1 * 5 :: G) == Just (Context [3,1] [1,5])

    putStrLn "\n============ Graph.buildg ============"
    test "buildg (\\e _ _ _ -> e)                                     == empty" $
          buildg (\e _ _ _ -> e)                                      == (empty :: G)

    test "buildg (\\_ v _ _ -> v x)                                   == vertex x" $ \(x :: Int) ->
          buildg (\_ v _ _ -> v x)                                    == vertex x

    test "buildg (\\e v o c -> o (foldg e v o c x) (foldg e v o c y)) == overlay x y" $ \(x :: G) y ->
          buildg (\e v o c -> o (foldg e v o c x) (foldg e v o c y))  == overlay x y

    test "buildg (\\e v o c -> c (foldg e v o c x) (foldg e v o c y)) == connect x y" $ \(x :: G) y ->
          buildg (\e v o c -> c (foldg e v o c x) (foldg e v o c y))  == connect x y

    test "buildg (\\e v o _ -> foldr o e (map v xs))                  == vertices xs" $ \(xs :: [Int]) ->
          buildg (\e v o _ -> foldr o e (map v xs))                   == vertices xs

    test "buildg (\\e v o c -> foldg e v o (flip c) g)                == transpose g" $ \(g :: G) ->
          buildg (\e v o c -> foldg e v o (flip c) g)                 == transpose g
