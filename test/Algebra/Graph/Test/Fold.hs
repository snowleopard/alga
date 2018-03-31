-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Fold
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Fold" and polymorphic functions defined in
-- "Algebra.Graph.Class".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Fold (
    -- * Testsuite
    testFold
  ) where

import Algebra.Graph.Fold
import Algebra.Graph.Test
import Algebra.Graph.Test.Generic

t :: Testsuite
t = testsuite "Fold." (empty :: Fold Int)

h :: HTestsuite
h = hTestsuite "Fold." (empty :: Fold Int)

type F  = Fold Int

testFold :: IO ()
testFold = do
    putStrLn "\n============ Fold ============"
    test "Axioms of graphs" (axioms :: GraphTestsuite F)

    testShow            t
    testBasicPrimitives t
    testToGraph         h
    testIsSubgraphOf    t
    testSize            t
    testProperties      t
    testGraphFamilies   t
    testTransformations t

    putStrLn "\n============ Fold.mesh ============"
    test "mesh xs     []   == empty" $ \xs ->
          mesh xs     []   == (empty :: Fold (Int, Int))

    test "mesh []     ys   == empty" $ \ys ->
          mesh []     ys   == (empty :: Fold (Int, Int))

    test "mesh [x]    [y]  == vertex (x, y)" $ \(x :: Int) (y :: Int) ->
          mesh [x]    [y]  == (vertex (x, y) :: Fold (Int, Int))

    test "mesh xs     ys   == box (path xs) (path ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          mesh xs     ys   == (box (path xs) (path ys) :: Fold (Int, Int))

    test "mesh [1..3] \"ab\" == <correct result>" $
         (mesh [1..3]  "ab" :: Fold (Int, Char)) == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                                          , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3,'b')) ]

    putStrLn "\n============ Fold.torus ============"
    test "torus xs    []   == empty" $ \xs ->
          torus xs    []   == (empty :: Fold (Int, Int))

    test "torus []    ys   == empty" $ \ys ->
          torus []    ys   == (empty :: Fold (Int, Int))

    test "torus [x]   [y]  == edge (x, y) (x, y)" $ \(x :: Int) (y :: Int) ->
          torus [x]   [y]  == (edge (x, y) (x, y) :: Fold (Int, Int))

    test "torus xs    ys   == box (circuit xs) (circuit ys)" $ \(xs :: [Int]) (ys :: [Int]) ->
          torus xs    ys   == (box (circuit xs) (circuit ys) :: Fold (Int, Int))

    test "torus [1,2] \"ab\" == <correct result>" $
         (torus [1,2]  "ab" :: Fold (Int, Char)) == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                                          , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2,'a')) ]

    putStrLn "\n============ Fold.deBruijn ============"
    test "          deBruijn 0 xs               == edge [] []" $ \(xs :: [Int]) ->
                    deBruijn 0 xs               ==(edge [] [] :: Fold [Int])

    test "n > 0 ==> deBruijn n []               == empty" $ \n ->
          n > 0 ==> deBruijn n []               == (empty :: Fold [Int])

    test "          deBruijn 1 [0,1]            == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
                    deBruijn 1 [0,1]            ==(edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ] :: Fold [Int])

    test "          deBruijn 2 \"0\"              == edge \"00\" \"00\"" $
                    deBruijn 2 "0"              ==(edge "00" "00" :: Fold String)

    test "          deBruijn 2 \"01\"             == <correct result>" $
                    deBruijn 2 "01"             ==(edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                                         , ("10","00"), ("10","01"), ("11","10"), ("11","11") ] :: Fold String)

    test "          transpose   (deBruijn n xs) == gmap reverse $ deBruijn n xs" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
                    transpose   (deBruijn n xs) == ((gmap reverse $ deBruijn n xs) :: Fold [Int])

    test "          vertexCount (deBruijn n xs) == (length $ nub xs)^n" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
                    vertexCount (deBruijn n xs) == (length $ nubOrd xs)^n

    test "n > 0 ==> edgeCount   (deBruijn n xs) == (length $ nub xs)^(n + 1)" $ mapSize (min 5) $ \(NonNegative n) (xs :: [Int]) ->
          n > 0 ==> edgeCount   (deBruijn n xs) == (length $ nubOrd xs)^(n + 1)

    testSplitVertex t
    testBind        t
    testSimplify    t

    putStrLn "\n============ Fold.box ============"
    let unit = fmap $ \(a, ()) -> a
        comm = fmap $ \(a,  b) -> (b, a)
    test "box x y               ~~ box y x" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          comm (box x y)        == (box y x :: Fold (Int, Int))

    test "box x (overlay y z)   == overlay (box x y) (box x z)" $ mapSize (min 10) $ \(x :: F) (y :: F) z ->
          box x (overlay y z)   == (overlay (box x y) (box x z) :: Fold (Int, Int))

    test "box x (vertex ())     ~~ x" $ mapSize (min 10) $ \(x :: F) ->
     unit(box x (vertex ()))    == x

    test "box x empty           ~~ empty" $ mapSize (min 10) $ \(x :: F) ->
     unit(box x empty)          == empty

    let assoc = fmap $ \(a, (b, c)) -> ((a, b), c)
    test "box x (box y z)       ~~ box (box x y) z" $ mapSize (min 10) $ \(x :: F) (y :: F) (z :: F) ->
      assoc (box x (box y z))   == (box (box x y) z :: Fold ((Int, Int), Int))

    test "transpose   (box x y) == box (transpose x) (transpose y)" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          transpose   (box x y) == (box (transpose x) (transpose y) :: Fold (Int, Int))

    test "vertexCount (box x y) == vertexCount x * vertexCount y" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          vertexCount (box x y) == vertexCount x * vertexCount y

    test "edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y" $ mapSize (min 10) $ \(x :: F) (y :: F) ->
          edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y
