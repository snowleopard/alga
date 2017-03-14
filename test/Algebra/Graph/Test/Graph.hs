{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Graph
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for 'Graph' and polymorphic functions defined in "Algebra.Graph".
--
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Graph (
    -- * Testsuite
    testGraph
  ) where

import Algebra.Graph
import Algebra.Graph.Test
import Data.Set (Set)
import Data.IntSet (IntSet)
import Prelude hiding ((==))

import Algebra.Graph.Data as Data hiding (foldg, hasEdge, removeEdge)

import qualified Prelude ((==))
import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type G = Data.Graph Int

testGraph :: IO ()
testGraph = do
    putStrLn "\n============ Graph ============"
    quickCheck (axioms   :: GraphTestsuite G)
    quickCheck (theorems :: GraphTestsuite G)

    let (==) :: G -> G -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ vertices ============"

    test "vertices []  == empty   " $
          vertices []  == empty
    test "vertices [x] == vertex x" $ \x ->
          vertices [x] == vertex x

    putStrLn "\n============ overlays ============"

    test "overlays []    == empty      " $
          overlays []    == empty
    test "overlays [x]   == x          " $ \x ->
          overlays [x]   == x
    test "overlays [x,y] == overlay x y" $ \x y ->
          overlays [x,y] == overlay x y

    putStrLn "\n============ connects ============"

    test "connects []    == empty      " $
          connects []    == empty
    test "connects [x]   == x          " $ \x ->
          connects [x]   == x
    test "connects [x,y] == connect x y" $ \x y ->
          connects [x,y] == connect x y

    putStrLn "\n============ edge ============"

    test "edge x y == connect (vertex x) (vertex y)" $ \x y ->
          edge x y == connect (vertex x) (vertex y)

    putStrLn "\n============ edges ============"

    test "edges []      == empty   " $
          edges []      == empty

    test "edges [(x,y)] == edge x y" $ \x y ->
          edges [(x,y)] == edge x y

    putStrLn "\n============ graph ============"

    test "graph []  []      == empty   " $
          graph []  []      == empty

    test "graph [x] []      == vertex x" $ \x ->
          graph [x] []      == vertex x

    test "graph []  [(x,y)] == edge x y" $ \x y ->
          graph []  [(x,y)] == edge x y

    putStrLn "\n============ path ============"

    test "path []    == empty   " $
          path []    == empty

    test "path [x]   == vertex x" $ \x ->
          path [x]   == vertex x

    test "path [x,y] == edge x y" $ \x y ->
          path [x,y] == edge x y

    putStrLn "\n============ circuit ============"

    test "circuit []    == empty               " $
          circuit []    == empty

    test "circuit [x]   == edge x x            " $ \x ->
          circuit [x]   == edge x x

    test "circuit [x,y] == edges [(x,y), (y,x)]" $ \x y ->
          circuit [x,y] == edges [(x,y), (y,x)]

    putStrLn "\n============ clique ============"

    test "clique []      == empty                      " $
          clique []      == empty

    test "clique [x]     == vertex x                   " $ \x ->
          clique [x]     == vertex x

    test "clique [x,y]   == edge x y                   " $ \x y ->
          clique [x,y]   == edge x y

    test "clique [x,y,z] == edges [(x,y), (x,z), (y,z)]" $ \x y z ->
          clique [x,y,z] == edges [(x,y), (x,z), (y,z)]

    putStrLn "\n============ biclique ============"

    test "biclique []      []      == empty                                     " $
          biclique []      []      == empty

    test "biclique [x]     []      == vertex x                                  " $ \x ->
          biclique [x]     []      == vertex x

    test "biclique []      [y]     == vertex y                                  " $ \y ->
          biclique []      [y]     == vertex y

    test "biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]" $ \x1 x2 y1 y2 ->
          biclique [x1,x2] [y1,y2] == edges [(x1,y1), (x1,y2), (x2,y1), (x2,y2)]

    putStrLn "\n============ star ============"

    test "star x []    == vertex x            " $ \x ->
          star x []    == vertex x

    test "star x [y]   == edge x y            " $ \x y ->
          star x [y]   == edge x y

    test "star x [y,z] == edges [(x,y), (x,z)]" $ \x y z ->
          star x [y,z] == edges [(x,y), (x,z)]

    putStrLn "\n============ transpose ============"

    test "transpose empty         == empty   " $
          transpose empty         == empty

    test "transpose (vertex x)    == vertex x" $ \x ->
          transpose (vertex x)    == vertex x

    test "transpose (edge x y)    == edge y x" $ \x y ->
          transpose (edge x y)    == edge y x

    test "transpose (transpose x) == x       " $ \y -> let x = fromGraph y in
          transpose (transpose x) == gmap id x

    let (==) :: Eq a => a -> a -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ foldg ============"

    test "foldg []   return        (++) (++) x == toList x " $ \(y :: G) -> let x = fromGraph y in
          foldg []   return        (++) (++) x == toList x

    test "foldg 0    (const 1)     (+)  (+)  x == length x " $ \(y :: G) -> let x = fromGraph y in
          foldg 0    (const 1)     (+)  (+)  x == length x

    test "foldg True (const False) (&&) (&&) x == isEmpty x" $ \(y :: G) -> let x = fromGraph y in
          foldg True (const False) (&&) (&&) x == isEmpty x

    let (==) :: Bool -> Bool -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ isSubgraphOf ============"

    test "isSubgraphOf empty x                     == True " $ \(x :: G) ->
          isSubgraphOf empty x                     == True

    test "isSubgraphOf (vertex x) empty            == False" $ \x ->
          isSubgraphOf (vertex x) (empty :: G)     == False

    test "isSubgraphOf x (overlay x y)             == True " $ \(x :: G) y ->
          isSubgraphOf x (overlay x y)             == True

    test "isSubgraphOf (overlay x y) (connect x y) == True " $ \(x :: G) y ->
          isSubgraphOf (overlay x y) (connect x y) == True

    putStrLn "\n============ isEmpty ============"

    test "isEmpty empty                       == True " $
          isEmpty empty                       == True

    test "isEmpty (vertex x)                  == False" $ \(x :: Int) ->
          isEmpty (vertex x)                  == False

    test "isEmpty (removeVertex x $ vertex x) == True " $ \x ->
          isEmpty (removeVertex x $(vertex x :: Fold Int)) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ hasVertex ============"

    test "hasVertex x empty              == False        " $ \(x :: Int) ->
          hasVertex x empty              == False

    test "hasVertex x (vertex x)         == True         " $ \(x :: Int) ->
          hasVertex x (vertex x)         == True

    test "hasVertex x (removeVertex x y) == const False y" $ \x (z :: G) -> let y = fromGraph z in
          hasVertex x (removeVertex x y) == const False y

    putStrLn "\n============ hasEdge ============"

    test "hasEdge x y empty              == False        " $ \(x :: Int) y ->
          hasEdge x y empty              == False

    test "hasEdge x y (vertex z)         == False        " $ \(x :: Int) y z ->
          hasEdge x y (vertex z)         == False

    test "hasEdge x y (edge x y)         == True         " $ \(x :: Int) y ->
          hasEdge x y (edge x y)         == True

    test "hasEdge x y (removeEdge x y z) == const False z" $ \(x :: Int) y g -> let z = fromGraph g in
          hasEdge x y (removeEdge x y z) == const False z

    let (==) :: Set Int -> Set Int -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ toSet ============"

    test "toSet empty         == Set.empty      " $
          toSet (empty :: Fold Int) == Set.empty

    test "toSet (vertex x)    == Set.singleton x" $ \x ->
          toSet (vertex x :: Fold Int) == Set.singleton x

    test "toSet (vertices xs) == Set.fromList xs" $ \xs ->
          toSet (vertices xs :: Fold Int) == Set.fromList xs

    test "toSet (clique xs)   == Set.fromList xs" $ \xs ->
          toSet (clique xs :: Fold Int) == Set.fromList xs

    let (==) :: IntSet -> IntSet -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ toIntSet ============"

    test "toIntSet empty         == IntSet.empty      " $
          toIntSet (empty :: Fold Int) == IntSet.empty

    test "toIntSet (vertex x)    == IntSet.singleton x" $ \x ->
          toIntSet (vertex x :: Fold Int) == IntSet.singleton x

    test "toIntSet (vertices xs) == IntSet.fromList xs" $ \xs ->
          toIntSet (vertices xs :: Fold Int) == IntSet.fromList xs

    test "toIntSet (clique xs)   == IntSet.fromList xs" $ \xs ->
          toIntSet (clique xs :: Fold Int) == IntSet.fromList xs

    let (==) :: Data.Graph (Int, Char) -> Data.Graph (Int, Char) -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ mesh ============"

    test "mesh xs     []   == empty                  " $ \xs ->
          mesh xs     []   == empty

    test "mesh []     ys   == empty                  " $ \ys ->
          mesh []     ys   == empty

    test "mesh [x]    [y]  == vertex (x, y)          " $ \x y ->
          mesh [x]    [y]  == vertex (x, y)

    test "mesh xs     ys   == box (path xs) (path ys)" $ \xs ys ->
          mesh xs     ys   == box (path xs) (path ys)

    test ("mesh [1..3] \"ab\" == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))\n" ++
         "                          , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3,'b')) ]") $
         mesh [1..3] "ab"  == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(2,'b')), ((2,'a'),(2,'b'))
                                    , ((2,'a'),(3,'a')), ((2,'b'),(3,'b')), ((3,'a'),(3,'b')) ]

    putStrLn "\n============ torus ============"

    test "torus xs     []   == empty                        " $ \xs ->
          torus xs     []   == empty

    test "torus []     ys   == empty                        " $ \ys ->
          torus []     ys   == empty

    test "torus [x]    [y]  == edge (x, y) (x, y)           " $ \x y ->
          torus [x]    [y]  == edge (x, y) (x, y)

    test "torus xs     ys   == box (circuit xs) (circuit ys)" $ \xs ys ->
          torus xs     ys   == box (circuit xs) (circuit ys)

    test ("torus [1..2] \"ab\" == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))\n" ++
         "                           , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2,'a')) ]") $
         torus [1..2] "ab"  == edges [ ((1,'a'),(1,'b')), ((1,'a'),(2,'a')), ((1,'b'),(1,'a')), ((1,'b'),(2,'b'))
                                     , ((2,'a'),(1,'a')), ((2,'a'),(2,'b')), ((2,'b'),(1,'b')), ((2,'b'),(2,'a')) ]

    let (==) :: Data.Graph [Int] -> Data.Graph [Int] -> Bool
        (==) = (Prelude.==)
    putStrLn "\n============ deBruijn ============"
    test "deBruijn k []    == empty" $ \k ->
          deBruijn k []    == empty

    test "deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
          deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]

    let (==) :: Data.Graph String -> Data.Graph String -> Bool
        (==) = (Prelude.==)
    test "deBruijn 2 \"0\"   == edge \"00\" \"00\"" $
          deBruijn 2 "0"   == edge "00" "00"

    test ("deBruijn 2 \"01\"  == edges [ (\"00\",\"00\"), (\"00\",\"01\"), (\"01\",\"10\"), (\"01\",\"11\")\n" ++
          "                          , (\"10\",\"00\"), (\"10\",\"01\"), (\"11\",\"10\"), (\"11\",\"11\") ]") $
          deBruijn 2 "01"  == edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                    , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]

