{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Acyclic.AdjacencyMap
-- Copyright  : (c) Andrey Mokhov 2016-2021
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Acyclic.AdjacencyMap".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Acyclic.AdjacencyMap (testAcyclicAdjacencyMap) where

import Algebra.Graph.Acyclic.AdjacencyMap
import Algebra.Graph.Test hiding (shrink)

import Data.Bifunctor
import Data.Tuple

import qualified Algebra.Graph.AdjacencyMap           as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.NonEmpty.AdjacencyMap  as NonEmpty
import qualified Data.Set                             as Set
import qualified GHC.Exts                             as Exts

type AAI = AdjacencyMap Int
type AI  = AM.AdjacencyMap Int

-- TODO: Switch to using generic tests.
testAcyclicAdjacencyMap :: IO ()
testAcyclicAdjacencyMap = do
    putStrLn "\n============ Acyclic.AdjacencyMap.Show ============"
    test "show empty                == \"empty\"" $
          show (empty :: AAI)       == "empty"

    test "show (shrink 1)           == \"vertex 1\"" $
          show (shrink 1 :: AAI)    == "vertex 1"

    test "show (shrink $ 1 + 2)     == \"vertices [1,2]\"" $
          show (shrink $ 1 + 2 :: AAI) == "vertices [1,2]"

    test "show (shrink $ 1 * 2)     == \"(fromJust . toAcyclic) (edge 1 2)\"" $
          show (shrink $ 1 * 2 :: AAI) == "(fromJust . toAcyclic) (edge 1 2)"

    test "show (shrink $ 1 * 2 * 3) == \"(fromJust . toAcyclic) (edges [(1,2),(1,3),(2,3)])\"" $
          show (shrink $ 1 * 2 * 3 :: AAI) == "(fromJust . toAcyclic) (edges [(1,2),(1,3),(2,3)])"

    test "show (shrink $ 1 * 2 + 3) == \"(fromJust . toAcyclic) (overlay (vertex 3) (edge 1 2))\"" $
          show (shrink $ 1 * 2 + 3 :: AAI) == "(fromJust . toAcyclic) (overlay (vertex 3) (edge 1 2))"

    putStrLn "\n============ Acyclic.AdjacencyMap.fromAcyclic ============"
    test "fromAcyclic empty                == empty" $
          fromAcyclic (empty :: AAI)       == AM.empty

    test "fromAcyclic . vertex             == vertex" $ \(x :: Int) ->
         (fromAcyclic . vertex) x          == AM.vertex x

    test "fromAcyclic (shrink $ 1 * 3 * 2) == star 1 [2,3]" $
          fromAcyclic (shrink $ 1 * 3 + 2) == 1 * 3 + (2 :: AI)

    test "vertexCount . fromAcyclic        == vertexCount" $ \(x :: AAI) ->
         (AM.vertexCount . fromAcyclic) x  == vertexCount x

    test "edgeCount   . fromAcyclic        == edgeCount" $ \(x :: AAI) ->
         (AM.edgeCount . fromAcyclic) x    == edgeCount x

    test "isAcyclic   . fromAcyclic        == const True" $ \(x :: AAI) ->
         (AM.isAcyclic . fromAcyclic) x    == const True x

    putStrLn "\n============ Acyclic.AdjacencyMap.empty ============"
    test "isEmpty     empty == True" $
          isEmpty     (empty :: AAI) == True

    test "hasVertex x empty == False" $ \x ->
          hasVertex x (empty :: AAI) == False

    test "vertexCount empty == 0" $
          vertexCount (empty :: AAI) == 0

    test "edgeCount   empty == 0" $
          edgeCount   (empty :: AAI) == 0

    putStrLn "\n============ Acyclic.AdjacencyMap.vertex ============"
    test "isEmpty     (vertex x) == False" $ \(x :: Int) ->
          isEmpty     (vertex x) == False

    test "hasVertex x (vertex y) == (x == y)" $ \(x :: Int) y ->
          hasVertex x (vertex y) == (x == y)

    test "vertexCount (vertex x) == 1" $ \(x :: Int) ->
          vertexCount (vertex x) == 1

    test "edgeCount   (vertex x) == 0" $ \(x :: Int) ->
          edgeCount   (vertex x) == 0

    putStrLn "\n============ Acyclic.AdjacencyMap.vertices ============"
    test "vertices []            == empty" $
          vertices []            == (empty :: AAI)

    test "vertices [x]           == vertex x" $ \(x :: Int) ->
          vertices [x]           == vertex x

    test "hasVertex x . vertices == elem x" $ \(x :: Int) xs ->
         (hasVertex x . vertices) xs == elem x xs

    test "vertexCount . vertices == length . nub" $ \(xs :: [Int]) ->
         (vertexCount . vertices) xs == (length . nubOrd) xs

    test "vertexSet   . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet   . vertices) xs == Set.fromList xs

    putStrLn "\n============ Acyclic.AdjacencyMap.union ============"
    test "vertexSet (union x y) == <correct result>" $ \(x :: AAI) (y :: AAI) ->
          vertexSet (union x y) == Set.unions [ Set.map Left  (vertexSet x)
                                              , Set.map Right (vertexSet y) ]

    test "edgeSet   (union x y) == <correct result>" $ \(x :: AAI) (y :: AAI) ->
          edgeSet   (union x y) == Set.unions [ Set.map (bimap Left  Left ) (edgeSet x)
                                              , Set.map (bimap Right Right) (edgeSet y) ]

    putStrLn "\n============ Acyclic.AdjacencyMap.join ============"
    test "vertexSet (join x y) == <correct result>" $ \(x :: AAI) (y :: AAI) ->
          vertexSet (join x y) == Set.unions [ Set.map Left  (vertexSet x)
                                             , Set.map Right (vertexSet y) ]

    test "edgeSet   (join x y) == <correct result>" $ \(x :: AAI) (y :: AAI) ->
          edgeSet   (join x y) == Set.unions
            [ Set.map (bimap Left  Left ) (edgeSet x)
            , Set.map (bimap Right Right) (edgeSet y)
            , Set.map (bimap Left  Right) (Set.cartesianProduct (vertexSet x) (vertexSet y)) ]

    putStrLn "\n============ Acyclic.AdjacencyMap.isSubgraphOf ============"
    test "isSubgraphOf empty        x          ==  True" $ \(x :: AAI) ->
          isSubgraphOf empty        x          ==  True

    test "isSubgraphOf (vertex x)   empty      ==  False" $ \(x :: Int) ->
          isSubgraphOf (vertex x)   empty      ==  False

    test "isSubgraphOf (induce p x) x          ==  True" $ \(x :: AAI) (apply -> p) ->
          isSubgraphOf (induce p x) x          ==  True

    test "isSubgraphOf x (transitiveClosure x) ==  True" $ \(x :: AAI) ->
          isSubgraphOf x (transitiveClosure x) ==  True

    test "isSubgraphOf x y                     ==> x <= y" $ \(x :: AAI) z ->
        let connect x y = shrink $ fromAcyclic x + fromAcyclic y
            -- TODO: Make the precondition stronger
            y = connect x (vertices z) -- Make sure we hit the precondition
        in isSubgraphOf x y                    ==> x <= y

    putStrLn "\n============ Acyclic.AdjacencyMap.isEmpty ============"
    test "isEmpty empty                             == True" $
          isEmpty (empty :: AAI)                    == True

    test "isEmpty (vertex x)                        == False" $ \(x :: Int) ->
          isEmpty (vertex x)                        == False

    test "isEmpty (removeVertex x $ vertex x)       == True" $ \(x :: Int) ->
          isEmpty (removeVertex x $ vertex x)       == True

    test "isEmpty (removeEdge 1 2 $ shrink $ 1 * 2) == False" $
          isEmpty (removeEdge 1 2 $ shrink $ 1 * 2 :: AAI) == False

    putStrLn "\n============ Acyclic.AdjacencyMap.hasVertex ============"
    test "hasVertex x empty            == False" $ \(x :: Int) ->
          hasVertex x empty            == False

    test "hasVertex x (vertex y)       == (x == y)" $ \(x :: Int) y ->
          hasVertex x (vertex y)       == (x == y)

    test "hasVertex x . removeVertex x == const False" $ \(x :: Int) y ->
         (hasVertex x . removeVertex x) y == const False y

    putStrLn "\n============ Acyclic.AdjacencyMap.hasEdge ============"
    test "hasEdge x y empty            == False" $ \(x :: Int) y ->
          hasEdge x y empty            == False

    test "hasEdge x y (vertex z)       == False" $ \(x :: Int) y z ->
          hasEdge x y (vertex z)       == False

    test "hasEdge 1 2 (shrink $ 1 * 2) == True" $
          hasEdge 1 2 (shrink $ 1 * 2 :: AAI)    == True

    test "hasEdge x y . removeEdge x y == const False" $ \(x :: Int) y z ->
         (hasEdge x y . removeEdge x y) z == const False z

    test "hasEdge x y                  == elem (x,y) . edgeList" $ \(x :: Int) y z -> do
        (u, v) <- elements ((x, y) : edgeList z)
        return $ hasEdge u v z         == elem (u, v) (edgeList z)

    putStrLn "\n============ Acyclic.AdjacencyMap.vertexCount ============"
    test "vertexCount empty             ==  0" $
          vertexCount (empty :: AAI)    ==  0

    test "vertexCount (vertex x)        ==  1" $ \(x :: Int) ->
          vertexCount (vertex x)        ==  1

    test "vertexCount                   ==  length . vertexList" $ \(x :: AAI) ->
          vertexCount x                 ==  (length . vertexList) x

    test "vertexCount x < vertexCount y ==> x < y" $ \(x :: AAI) y ->
        if vertexCount x < vertexCount y
        then property (x < y)
        else (vertexCount x > vertexCount y ==> x > y)

    putStrLn "\n============ Acyclic.AdjacencyMap.edgeCount ============"
    test "edgeCount empty            == 0" $
          edgeCount (empty :: AAI)   == 0

    test "edgeCount (vertex x)       == 0" $ \(x :: Int) ->
          edgeCount (vertex x)       == 0

    test "edgeCount (shrink $ 1 * 2) == 1" $
          edgeCount (shrink $ 1 * 2 :: AAI) == 1

    test "edgeCount                  == length . edgeList" $ \(x :: AAI) ->
          edgeCount x                == (length . edgeList) x

    putStrLn "\n============ Acyclic.AdjacencyMap.vertexList ============"
    test "vertexList empty      == []" $
          vertexList (empty :: AAI) == []

    test "vertexList (vertex x) == [x]" $ \(x :: Int) ->
          vertexList (vertex x) == [x]

    test "vertexList . vertices == nub . sort" $ \(xs :: [Int]) ->
         (vertexList . vertices) xs == (nubOrd . sort) xs

    putStrLn "\n============ Acyclic.AdjacencyMap.edgeList ============"
    test "edgeList empty            == []" $
          edgeList (empty :: AAI)   == []

    test "edgeList (vertex x)       == []" $ \(x :: Int) ->
          edgeList (vertex x)       == []

    test "edgeList (shrink $ 2 * 1) == [(2,1)]" $
          edgeList (shrink $ 2 * 1 :: AAI) == [(2,1)]

    test "edgeList . transpose      == sort . map swap . edgeList" $ \(x :: AAI) ->
         (edgeList . transpose) x   == (sort . map swap . edgeList) x

    putStrLn "\n============ Acyclic.AdjacencyMap.adjacencyList ============"
    test "adjacencyList empty            == []" $
          adjacencyList (empty :: AAI)   == []

    test "adjacencyList (vertex x)       == [(x, [])]" $ \(x :: Int) ->
          adjacencyList (vertex x)       == [(x, [])]

    test "adjacencyList (shrink $ 1 * 2) == [(1, [2]), (2, [])]" $
          adjacencyList (shrink $ 1 * 2 :: AAI) == [(1, [2]), (2, [])]

    putStrLn "\n============ Acyclic.AdjacencyMap.vertexSet ============"
    test "vertexSet empty      == Set.empty" $
          vertexSet (empty :: AAI) == Set.empty

    test "vertexSet . vertex   == Set.singleton" $ \(x :: Int) ->
         (vertexSet . vertex) x == Set.singleton x

    test "vertexSet . vertices == Set.fromList" $ \(xs :: [Int]) ->
         (vertexSet . vertices) xs == Set.fromList xs

    putStrLn "\n============ Acyclic.AdjacencyMap.edgeSet ============"
    test "edgeSet empty            == Set.empty" $
          edgeSet (empty :: AAI)   == Set.empty

    test "edgeSet (vertex x)       == Set.empty" $ \(x :: Int) ->
          edgeSet (vertex x)       == Set.empty

    test "edgeSet (shrink $ 1 * 2) == Set.singleton (1,2)" $
          edgeSet (shrink $ 1 * 2 :: AAI) == Set.singleton (1,2)

    putStrLn "\n============ Acyclic.AdjacencyMap.preSet ============"
    test "preSet x empty            == Set.empty" $ \(x :: Int) ->
          preSet x empty            == Set.empty

    test "preSet x (vertex x)       == Set.empty" $ \(x :: Int) ->
          preSet x (vertex x)       == Set.empty

    test "preSet 1 (shrink $ 1 * 2) == Set.empty" $
          preSet 1 (shrink $ 1 * 2 :: AAI) == Set.empty

    test "preSet 2 (shrink $ 1 * 2) == Set.fromList [1]" $
          preSet 2 (shrink $ 1 * 2 :: AAI) == Set.fromList [1]

    test "Set.member x . preSet x   == const False" $ \(x :: Int) y ->
         (Set.member x . preSet x) y == const False y

    putStrLn "\n============ Acyclic.AdjacencyMap.postSet ============"
    test "postSet x empty            == Set.empty" $ \(x :: Int) ->
          postSet x empty            == Set.empty

    test "postSet x (vertex x)       == Set.empty" $ \(x :: Int) ->
          postSet x (vertex x)       == Set.empty

    test "postSet 1 (shrink $ 1 * 2) == Set.fromList [2]" $
          postSet 1 (shrink $ 1 * 2 :: AAI) == Set.fromList [2]

    test "postSet 2 (shrink $ 1 * 2) == Set.empty" $
          postSet 2 (shrink $ 1 * 2 :: AAI) == Set.empty

    test "Set.member x . postSet x   == const False" $ \(x :: Int) y ->
         (Set.member x . postSet x) y == const False y

    putStrLn "\n============ Acyclic.AdjacencyMap.removeVertex ============"
    test "removeVertex x (vertex x)       == empty" $ \(x :: Int) ->
          removeVertex x (vertex x)       == empty

    test "removeVertex 1 (vertex 2)       == vertex 2" $
          removeVertex 1 (vertex 2 :: AAI) == vertex 2

    test "removeVertex 1 (shrink $ 1 * 2) == vertex 2" $
          removeVertex 1 (shrink $ 1 * 2 :: AAI) == vertex 2

    test "removeVertex x . removeVertex x == removeVertex x" $ \(x :: Int) y ->
         (removeVertex x . removeVertex x) y == removeVertex x y

    putStrLn "\n============ Acyclic.AdjacencyMap.removeEdge ============"
    test "removeEdge 1 2 (shrink $ 1 * 2)     == vertices [1,2]" $
          removeEdge 1 2 (shrink $ 1 * 2 :: AAI) == vertices [1,2]

    test "removeEdge x y . removeEdge x y     == removeEdge x y" $ \(x :: Int) y z ->
         (removeEdge x y . removeEdge x y) z  == removeEdge x y z

    test "removeEdge x y . removeVertex x     == removeVertex x" $ \(x :: Int) y z ->
         (removeEdge x y . removeVertex x) z  == removeVertex x z

    test "removeEdge 1 2 (shrink $ 1 * 2 * 3) == shrink ((1 + 2) * 3)" $
          removeEdge 1 2 (shrink $ 1 * 2 * 3 :: AAI) == shrink ((1 + 2) * 3)

    putStrLn "\n============ Acyclic.AdjacencyMap.transpose ============"
    test "transpose empty       == empty" $
          transpose (empty :: AAI) == empty

    test "transpose (vertex x)  == vertex x" $ \(x :: Int) ->
          transpose (vertex x)  == vertex x

    test "transpose . transpose == id" $ size10 $ \(x :: AAI) ->
         (transpose . transpose) x == id x

    test "edgeList . transpose  == sort . map swap . edgeList" $ \(x :: AAI) ->
         (edgeList . transpose) x  == (sort . map swap . edgeList) x

    putStrLn "\n============ Acyclic.AdjacencyMap.induce ============"
    test "induce (const True ) x      == x" $ \(x :: AAI) ->
          induce (const True ) x      == x

    test "induce (const False) x      == empty" $ \(x :: AAI) ->
          induce (const False) x      == empty

    test "induce (/= x)               == removeVertex x" $ \x (y :: AAI) ->
          induce (/= x) y             == removeVertex x y

    test "induce p . induce q         == induce (\\x -> p x && q x)" $ \(apply -> p) (apply -> q) (y :: AAI) ->
         (induce p . induce q) y      == induce (\x -> p x && q x) y

    test "isSubgraphOf (induce p x) x == True" $ \(apply -> p) (x :: AAI) ->
          isSubgraphOf (induce p x) x == True

    putStrLn "\n============ Acyclic.AdjacencyMap.induceJust ============"
    test "induceJust (vertex Nothing) == empty" $
          induceJust (vertex Nothing) == (empty :: AAI)

    test "induceJust . vertex . Just  == vertex" $ \(x :: Int) ->
         (induceJust . vertex . Just) x == vertex x

    putStrLn "\n============ Acyclic.AdjacencyMap.box ============"
    test "edgeList (box (shrink $ 1 * 2) (shrink $ 10 * 20)) == <correct result>\n" $
          edgeList (box (shrink $ 1 * 2) (shrink $ 10 * 20)) == [ ((1,10), (1,20))
                                                                , ((1,10), (2,10))
                                                                , ((1,20), (2,20))
                                                                , ((2,10), (2 :: Int,20 :: Int)) ]

    let gmap f = shrink . AM.gmap f . fromAcyclic
        unit = gmap $ \(a :: Int, ()      ) -> a
        comm = gmap $ \(a :: Int, b :: Int) -> (b, a)
    test "box x y               ~~ box y x" $ size10 $ \x y ->
          comm (box x y)        == box y x

    test "box x (vertex ())     ~~ x" $ size10 $ \x ->
     unit(box x (vertex ()))    == (x `asTypeOf` empty)

    test "box x empty           ~~ empty" $ size10 $ \x ->
     unit(box x empty)          == empty

    let assoc = gmap $ \(a :: Int, (b :: Int, c :: Int)) -> ((a, b), c)
    test "box x (box y z)       ~~ box (box x y) z" $ size10 $ \x y z ->
      assoc (box x (box y z))   == box (box x y) z

    test "transpose   (box x y) == box (transpose x) (transpose y)" $ size10 $ \(x :: AAI) (y :: AAI) ->
          transpose   (box x y) == box (transpose x) (transpose y)

    test "vertexCount (box x y) == vertexCount x * vertexCount y" $ size10 $ \(x :: AAI) (y :: AAI) ->
          vertexCount (box x y) == vertexCount x * vertexCount y

    test "edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y" $ size10 $ \(x :: AAI) (y :: AAI) ->
          edgeCount   (box x y) <= vertexCount x * edgeCount y + edgeCount x * vertexCount y

    putStrLn "\n============ Acyclic.AdjacencyMap.transitiveClosure ============"
    test "transitiveClosure empty                    == empty" $
          transitiveClosure empty                    == (empty :: AAI)

    test "transitiveClosure (vertex x)               == vertex x" $ \(x :: Int) ->
          transitiveClosure (vertex x)               == vertex x

    test "transitiveClosure (shrink $ 1 * 2 + 2 * 3) == shrink (1 * 2 + 1 * 3 + 2 * 3)" $
          transitiveClosure (shrink $ 1 * 2 + 2 * 3  :: AAI) == shrink (1 * 2 + 1 * 3 + 2 * 3)

    test "transitiveClosure . transitiveClosure      == transitiveClosure" $ \(x :: AAI) ->
         (transitiveClosure . transitiveClosure) x   == transitiveClosure x

    putStrLn "\n============ Acyclic.AdjacencyMap.topSort ============"
    test "topSort empty                          == []" $
          topSort (empty :: AAI)                 == []

    test "topSort (vertex x)                     == [x]" $ \(x :: Int) ->
          topSort (vertex x)                     == [x]

    test "topSort (shrink $ 1 * (2 + 4) + 3 * 4) == [1, 2, 3, 4]" $
          topSort (shrink $ 1 * (2 + 4) + 3 * 4) == [1, 2, 3, 4 :: Int]

    test "topSort (join x y)                     == fmap Left (topSort x) ++ fmap Right (topSort y)" $ \(x :: AAI) (y :: AAI) ->
          topSort (join x y)                     == fmap Left (topSort x) ++ fmap Right (topSort y)

    test "Right . topSort                        == AM.topSort . fromAcyclic" $ \(x :: AAI) ->
          Right (topSort x)                      == AM.topSort (fromAcyclic x)

    putStrLn "\n============ Acyclic.AdjacencyMap.scc ============"
    test "           scc empty               == empty" $
                     scc (AM.empty :: AI)    == empty

    test "           scc (vertex x)          == vertex (NonEmpty.vertex x)" $ \(x :: Int) ->
                     scc (AM.vertex x)       == vertex (NonEmpty.vertex x)

    test "           scc (edge 1 1)          == vertex (NonEmpty.edge 1 1)" $
                     scc (AM.edge 1 1 :: AI) == vertex (NonEmpty.edge 1 1)

    test "edgeList $ scc (edge 1 2)          == [ (NonEmpty.vertex 1, NonEmpty.vertex 2) ]" $
          edgeList (scc (AM.edge 1 2 :: AI)) == [ (NonEmpty.vertex 1, NonEmpty.vertex 2) ]

    test "edgeList $ scc (3 * 1 * 4 * 1 * 5) == <correct result>" $
          edgeList (scc (3 * 1 * 4 * 1 * 5)) == [ (NonEmpty.vertex 3, NonEmpty.vertex (5 :: Int))
                                                , (NonEmpty.vertex 3, NonEmpty.clique1 (Exts.fromList [1,4,1]))
                                                , (NonEmpty.clique1 (Exts.fromList [1,4,1]), NonEmpty.vertex 5) ]

    putStrLn "\n============ Acyclic.AdjacencyMap.toAcyclic ============"
    test "toAcyclic (path    [1,2,3]) == Just (shrink $ 1 * 2 + 2 * 3)" $
          toAcyclic (AM.path [1,2,3]) == Just (shrink $ 1 * 2 + 2 * 3 :: AAI)

    test "toAcyclic (clique  [3,2,1]) == Just (transpose (shrink $ 1 * 2 * 3))" $
          toAcyclic (AM.clique [3,2,1]) == Just (transpose (shrink $ 1 * 2 * 3 :: AAI))

    test "toAcyclic (circuit [1,2,3]) == Nothing" $
          toAcyclic (AM.circuit [1,2,3 :: Int]) == Nothing

    test "toAcyclic . fromAcyclic     == Just" $ \(x :: AAI) ->
         (toAcyclic . fromAcyclic) x  == Just x

    putStrLn "\n============ Acyclic.AdjacencyMap.toAcyclicOrd ============"
    test "toAcyclicOrd empty       == empty" $
          toAcyclicOrd AM.empty    == (empty :: AAI)

    test "toAcyclicOrd . vertex    == vertex" $ \(x :: Int) ->
         (toAcyclicOrd . AM.vertex) x == vertex x

    test "toAcyclicOrd (1 + 2)     == shrink (1 + 2)" $
          toAcyclicOrd (1 + 2)     == (shrink $ 1 + 2 :: AAI)

    test "toAcyclicOrd (1 * 2)     == shrink (1 * 2)" $
          toAcyclicOrd (1 * 2)     == (shrink $ 1 * 2 :: AAI)

    test "toAcyclicOrd (2 * 1)     == shrink (1 + 2)" $
          toAcyclicOrd (2 * 1)     == (shrink $ 1 + 2 :: AAI)

    test "toAcyclicOrd (1 * 2 * 1) == shrink (1 * 2)" $
          toAcyclicOrd (1 * 2 * 1) == (shrink $ 1 * 2 :: AAI)

    test "toAcyclicOrd (1 * 2 * 3) == shrink (1 * 2 * 3)" $
          toAcyclicOrd (1 * 2 * 3) == (shrink $ 1 * 2 * 3 :: AAI)


    putStrLn "\n============ Acyclic.AdjacencyMap.shrink ============"
    test "shrink . AM.vertex   == vertex" $ \x ->
          (shrink . AM.vertex) x == (vertex x :: AAI)

    test "shrink . AM.vertices == vertices" $ \x ->
          (shrink . AM.vertices) x == (vertices x :: AAI)

    test "shrink . fromAcyclic == id" $ \(x :: AAI) ->
          (shrink . fromAcyclic) x == id x

    putStrLn "\n============ Acyclic.AdjacencyMap.consistent ============"
    test "Arbitrary"         $ \(x :: AAI)            -> consistent x
    test "empty"             $                           consistent (empty :: AAI)
    test "vertex"            $ \(x :: Int)            -> consistent (vertex x)
    test "vertices"          $ \(xs :: [Int])         -> consistent (vertices xs)
    test "union"             $ \(x :: AAI) (y :: AAI) -> consistent (union x y)
    test "join"              $ \(x :: AAI) (y :: AAI) -> consistent (join x y)
    test "transpose"         $ \(x :: AAI)            -> consistent (transpose x)
    test "box"      $ size10 $ \(x :: AAI) (y :: AAI) -> consistent (box x y)
    test "transitiveClosure" $ \(x :: AAI)            -> consistent (transitiveClosure x)
    test "scc"               $ \(x :: AI)             -> consistent (scc x)
    test "toAcyclic"         $ \(x :: AI)             -> fmap consistent (toAcyclic x) /= Just False
    test "toAcyclicOrd"      $ \(x :: AI)             -> consistent (toAcyclicOrd x)
