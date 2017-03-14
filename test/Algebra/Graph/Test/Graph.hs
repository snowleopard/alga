{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
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
import Test.QuickCheck.Function

import Algebra.Graph.Data as Data hiding (foldg, hasEdge, removeEdge)

import qualified Prelude     as P
import qualified Data.Set    as Set
import qualified Data.IntSet as IntSet

type G  = Data.Graph Int
type II = Int -> Int
type IB = Int -> Bool
type IG = Int -> G

testGraph :: IO ()
testGraph = do
    putStrLn "\n============ Graph ============"
    quickCheck (axioms   :: GraphTestsuite G)
    quickCheck (theorems :: GraphTestsuite G)

    let (==) :: G -> G -> Bool
        (==) = (P.==)
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

    putStrLn "\n============ simplify ============"

    test "simplify x                        == x                            " $ \y -> let x = fromGraph y in
          simplify x                        == gmap id x

    test "1 + 1 :: Graph Int                == Overlay (Vertex 1) (Vertex 1)" $
         (1 + 1)                            == Overlay (Vertex 1) (Vertex 1)

    test "simplify (1 + 1) :: Graph Int     == Vertex 1                     " $
          simplify (1 + 1)                  == Vertex 1

    test "1 * 1 * 1 :: Graph Int            == Connect (Connect (Vertex 1) (Vertex 1)) (Vertex 1)" $
         (1 * 1 * 1)                        == Connect (Connect (Vertex 1) (Vertex 1)) (Vertex 1)

    test "simplify (1 * 1 * 1) :: Graph Int == Connect (Vertex 1) (Vertex 1)" $
          simplify (1 * 1 * 1)              == Connect (Vertex 1) (Vertex 1)

    putStrLn "\n============ gmap ============"

    test "gmap f empty      == empty           " $ \(apply -> f :: II) ->
          gmap f empty      == empty

    test "gmap f (vertex x) == vertex (f x)    " $ \(apply -> f :: II) x ->
          gmap f (vertex x) == vertex (f x)

    test "gmap f (edge x y) == edge (f x) (f y)" $ \(apply -> f :: II) x y ->
          gmap f (edge x y) == edge (f x) (f y)

    test "gmap id           == id              " $ \y -> let x = fromGraph y in
          gmap id x         == y

    test "gmap f . gmap g   == gmap (f . g)    " $ \(apply -> f :: II) (apply -> g :: II) y -> let x = fromGraph y in
         (gmap f . gmap g) x== gmap (f . g) x

    putStrLn "\n============ replaceVertex ============"

    test "replaceVertex x x            == id                    " $ \x z -> let y = fromGraph z in
          replaceVertex x x y          == gmap id y

    test "replaceVertex x y (vertex x) == vertex y              " $ \x y ->
          replaceVertex x y (vertex x) == vertex y

    test "replaceVertex x y            == mergeVertices (== x) y" $ \x y k -> let z = fromGraph k in
          replaceVertex x y z          == mergeVertices (P.== x) y (gmap id z)

    putStrLn "\n============ mergeVertices ============"

    test "mergeVertices (const False) x    == id               " $ \x z -> let y = fromGraph z in
          mergeVertices (const False) x y  == gmap id y

    test "mergeVertices (== x) y           == replaceVertex x y" $ \x y k -> let z = fromGraph k in
          mergeVertices (P.== x) y z       == replaceVertex x y z

    test "mergeVertices even 1 (0 * 2)     == 1 * 1            " $
          mergeVertices even 1 (0 * 2)     == (1 * 1)

    test "mergeVertices odd  1 (3 + 4 * 5) == 4 * 1            " $
          mergeVertices odd  1 (3 + 4 * 5) == (4 * 1)

    putStrLn "\n============ bind ============"

    test "bind empty f         == empty                       " $ \(apply -> f :: IG) ->
          bind empty f         == empty

    test "bind (vertex x) f    == f x                         " $ \(apply -> f :: IG) x ->
          bind (vertex x) f    == f x

    test "bind (edge x y) f    == connect (f x) (f y)         " $ \(apply -> f :: IG) x y ->
          bind (edge x y) f    == connect (f x) (f y)

    test "bind (vertices xs) f == overlays (map f xs)         " $ mapSize (min 10) $ \xs (apply -> f :: IG) ->
          bind (vertices xs) f == overlays (map f xs)

    test "bind x (const empty) == empty                       " $ \(y :: G) -> let x = fromGraph y in
          bind x (const empty) == empty

    test "bind x vertex        == x                           " $ \(y :: G) -> let x = fromGraph y in
          bind x vertex        == gmap id x

    test "bind (bind x f) g    == bind x (\\y -> bind (f y) g)" $ mapSize (min 10) $ \(z :: G) (apply -> f :: IG) (apply -> g :: IG) -> let x = fromGraph z in
          bind (fromGraph $ bind x f) g == bind x (\y -> bind (fromGraph $ f y) g)

    putStrLn "\n============ removeVertex ============"

    test "removeVertex x (vertex x)       == empty         " $ \x ->
          removeVertex x (vertex x)       == empty

    test "removeVertex x . removeVertex x == removeVertex x" $ \x (z :: G) -> let y = fromGraph z in
         (removeVertex x . removeVertex x)y==removeVertex x y

    putStrLn "\n============ splitVertex ============"

    test "splitVertex x []                   == removeVertex x   " $ \x z -> let y = fromGraph z in
         (splitVertex x []) y                == removeVertex x y

    test "splitVertex x [x]                  == id               " $ \x z -> let y = fromGraph z in
         (splitVertex x [x]) y               == gmap id y

    test "splitVertex x [y]                  == replaceVertex x y" $ \x y k -> let z = fromGraph k in
         (splitVertex x [y]) z               == replaceVertex x y z

    test "splitVertex 1 [0, 1] $ 1 * (2 + 3) == (0 + 1) * (2 + 3)" $
         (splitVertex 1 [0, 1] $ 1 * (2 + 3))==((0 + 1) * (2 + 3))

    putStrLn "\n============ removeEdge ============"

    test "removeEdge x y (edge x y)       == vertices [x, y]" $ \x y ->
          removeEdge x y (edge x y)       == vertices [x, y]

    test "removeEdge x y . removeEdge x y == removeEdge x y " $ \x y k -> let z = fromGraph k in
         (removeEdge x y . removeEdge x y)z==removeEdge x y z

    test "removeEdge x y . removeVertex x == removeVertex x " $ \x y k -> let z = fromGraph k in
         (removeEdge x y . removeVertex x)z==removeVertex x z

    test "removeEdge 1 1 (1 * 1 * 2 * 2)  == 1 * 2 * 2      " $
          removeEdge 1 1 (1 * 1 * 2 * 2)  ==(1 * 2 * 2)

    test "removeEdge 1 2 (1 * 1 * 2 * 2)  == 1 * 1 + 2 * 2  " $
          removeEdge 1 2 (1 * 1 * 2 * 2)  ==(1 * 1 + 2 * 2)

    putStrLn "\n============ induce ============"

    test "induce (const True)  x      == x                        " $ \(y :: G) -> let x = fromGraph y in
          induce (const True)  x      == gmap id x

    test "induce (const False) x      == empty                    " $ \(y :: G) -> let x = fromGraph y in
          induce (const False) x      == empty

    test "induce (/= x)               == removeVertex x           " $ \x (z :: G) -> let y = fromGraph z in
          induce (/= x) y             == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p :: IB) (apply -> q :: IB) (z :: G) -> let y = fromGraph z in
         (induce p . induce q) y      == induce (\x -> p x && q x) y

    let (==) :: Eq a => a -> a -> Bool
        (==) = (P.==)
    test "isSubgraphOf (induce p x) x == True                     " $ \(apply -> p :: IB) (x :: G) ->
          isSubgraphOf (induce p $ fromGraph x) x == True

    putStrLn "\n============ foldg ============"

    test "foldg []   return        (++) (++) == toList " $ \(y :: G) -> let x = fromGraph y in
          foldg []   return        (++) (++)x== toList x

    test "foldg 0    (const 1)     (+)  (+)  == length " $ \(y :: G) -> let x = fromGraph y in
          foldg 0    (const 1)     (+)  (+)x == length x

    test "foldg True (const False) (&&) (&&) == isEmpty" $ \(y :: G) -> let x = fromGraph y in
          foldg True (const False) (&&) (&&)x== isEmpty x

    let (==) :: Bool -> Bool -> Bool
        (==) = (P.==)
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

    test "isEmpty (removeVertex x $ vertex x) == True " $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x) == True

    test "isEmpty (removeEdge x y $ edge x y) == False" $ \(x :: Int) y ->
          isEmpty (removeEdge x y $ edge x y) == False

    putStrLn "\n============ hasVertex ============"

    test "hasVertex x empty            == False      " $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex x)       == True       " $ \(x :: Int) ->
          hasVertex x (vertex x)       == True

    test "hasVertex x . removeVertex x == const False" $ \x (z :: G) -> let y = fromGraph z in
          hasVertex x (removeVertex x y)==const False y

    putStrLn "\n============ hasEdge ============"

    test "hasEdge x y empty            == False      " $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False      " $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge x y (edge x y)       == True       " $ \(x :: Int) y ->
          hasEdge x y (edge x y)       == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y g -> let z = fromGraph g in
          hasEdge x y (removeEdge x y z)==const False z

    let (==) :: Set Int -> Set Int -> Bool
        (==) = (P.==)
    putStrLn "\n============ toSet ============"

    test "toSet empty         == Set.empty      " $
          toSet empty         == (Set.empty :: Set Int)

    test "toSet (vertex x)    == Set.singleton x" $ \(x :: Int) ->
          toSet (vertex x)    == Set.singleton x

    test "toSet (vertices xs) == Set.fromList xs" $ \(xs :: [Int]) ->
          toSet (vertices xs) == Set.fromList xs

    test "toSet (clique xs)   == Set.fromList xs" $ \(xs :: [Int]) ->
          toSet (clique xs)   == Set.fromList xs

    let (==) :: IntSet -> IntSet -> Bool
        (==) = (P.==)
    putStrLn "\n============ toIntSet ============"

    test "toIntSet empty         == IntSet.empty      " $
          toIntSet empty         == IntSet.empty

    test "toIntSet (vertex x)    == IntSet.singleton x" $ \x ->
          toIntSet (vertex x)    == IntSet.singleton x

    test "toIntSet (vertices xs) == IntSet.fromList xs" $ \xs ->
          toIntSet (vertices xs) == IntSet.fromList xs

    test "toIntSet (clique xs)   == IntSet.fromList xs" $ \xs ->
          toIntSet (clique xs)   == IntSet.fromList xs

    let (==) :: Data.Graph (Int, Char) -> Data.Graph (Int, Char) -> Bool
        (==) = (P.==)
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
        (==) = (P.==)
    putStrLn "\n============ deBruijn ============"
    test "deBruijn k []    == empty" $ \k ->
          deBruijn k []    == empty

    test "deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]" $
          deBruijn 1 [0,1] == edges [ ([0],[0]), ([0],[1]), ([1],[0]), ([1],[1]) ]

    let (==) :: Data.Graph String -> Data.Graph String -> Bool
        (==) = (P.==)
    test "deBruijn 2 \"0\"   == edge \"00\" \"00\"" $
          deBruijn 2 "0"   == edge "00" "00"

    test ("deBruijn 2 \"01\"  == edges [ (\"00\",\"00\"), (\"00\",\"01\"), (\"01\",\"10\"), (\"01\",\"11\")\n" ++
          "                          , (\"10\",\"00\"), (\"10\",\"01\"), (\"11\",\"10\"), (\"11\",\"11\") ]") $
          deBruijn 2 "01"  == edges [ ("00","00"), ("00","01"), ("01","10"), ("01","11")
                                    , ("10","00"), ("10","01"), ("11","10"), ("11","11") ]

