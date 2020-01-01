{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.Algorithm where

import Algebra.Graph.Label
import Data.Map.Strict (Map, (!))
import Algebra.Graph.Labelled.AdjacencyMap

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- TODO: Improve documentation for 'dijkstra'.
-- TODO: Improve performance
-- | A generic Dijkstra algorithm that relaxes the list of edges
-- based on the 'Dioid'. 
--
-- The heap (min vs max) for Dijkstra is decided based on '<+>' 
--
-- We assume two things,
--
-- 1. The underlying semiring is selective i.e. the 
-- operation '<+>' always selects one of its arguments.
--
-- 2. The underlying semiring has an optimisation criterion.
-- 
-- Assuming the above, the Heap type is chosen depending on '<+>':
-- If '<+>' acts like a 'min' then we use a Min Heap else we use a 
-- Max Heap.
--
-- The algorithm is best suited to work with the 'Optimum' data type.
--
-- If the edge type is 'Distance' 'Int':
-- @
-- '<+>'  == 'min'
-- '<.>'  == '+'
-- 'one'  == 0
-- 'zero' == 'distance' 'infinity'
-- @
-- @
-- dijkstra ('vertex' 'a') 'a' == 'Map.fromList' [('a', 'one')]
-- dijkstra ('vertex' 'a') 'z' == 'Map.fromList' [('a', 'zero')]
-- dijkstra ('edge' x 'a' 'b') 'a' == 'Map.fromList' [('a', 'one'), ('b', x)]
-- dijkstra ('edge' x 'a' 'b') 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- dijkstra ('vertices' ['a', 'b']) 'a' == 'Map.fromList' [('a', 'one'), ('b', 'zero')]
-- dijkstra ('vertices' ['a', 'b']) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- dijkstra ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'a' == 'Map.fromList' [('a', 'one'), ('b', 3), ('c', 4)]
-- dijkstra ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero'), ('c', 'zero')]
-- @
--
-- If the edge type is 'Capacity' 'Int':
-- @
-- '<+>'  == 'max'
-- '<.>'  == 'min'
-- 'one'  == 'distance' 'infinity'
-- 'zero' == 0
-- @
-- @
-- dijkstra ('vertex' 'a') 'a' == 'Map.fromList' [('a', 'one')]
-- dijkstra ('vertex' 'a') 'z' == 'Map.fromList' [('a', 'zero')]
-- dijkstra ('edge' x 'a' 'b') 'a' == 'Map.fromList' [('a', 'one'), ('b', x)]
-- dijkstra ('edge' x 'a' 'b') 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- dijkstra ('vertices' ['a', 'b']) 'a' == 'Map.fromList' [('a', 'one'), ('b', 'zero')]
-- dijkstra ('vertices' ['a', 'b']) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- dijkstra ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'a' == 'Map.fromList' [('a', 'one'), ('b', 3), ('c', 5)]
-- dijkstra ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero'), ('c', 'zero')]
-- @
dijkstra :: (Ord a, Ord e, Dioid e) => AdjacencyMap e a -> a -> Map a e
dijkstra = dijkstra' zero one
  where
    dijkstra' :: (Ord a, Ord e, Dioid e) => e -> e -> AdjacencyMap e a -> a -> Map a e
    dijkstra' z o wam src = maybe zm (snd . processG) jsm
      where
        am = adjacencyMap wam
        zm = Map.map (const zero) am
        im = Map.insert src one zm
        is = Set.singleton (one, src)
        jsm = (is, im) <$ Map.lookup src zm
        view
            | o <+> z == o =
                if o < z
                    then Set.minView
                    else Set.maxView
            | o <+> z == z =
                if o < z
                    then Set.maxView
                    else Set.minView
            | otherwise = Set.minView
        processG sm@(s, _) = processS (view s) sm
        processS Nothing sm = sm
        processS (Just ((_, v1), s)) (_, m) = processG $ relaxV v1 (s, m)
        relaxV v1 sm =
            let eL = map (\(v2, e) -> (e, v1, v2)) . Map.toList $ am ! v1
             in foldr relaxE sm eL
        relaxE (e, v1, v2) (s, m) =
            let n = ((m ! v1) <.> e) <+> (m ! v2)
             in (Set.insert (n, v2) s, Map.insert v2 n m)

-- TODO: Improve documentation for bellmanFord
-- TODO: Improve performance
-- TODO: safely change 'vL' to 'tail vL' in processL
-- | A generic Bellman-Ford algorithm that relaxes the list of edges
-- based on the 'Dioid'. 
--
-- We assume two things,
--
-- 1. The underlying semiring is selective i.e. the 
-- operation '<+>' always selects one of its arguments.
--
-- 2. The underlying semiring has an optimisation criterion.
-- 
-- The algorithm is best suited to work with the 'Optimum' data type.
--
-- If the edge type is 'Distance' 'Int':
-- @
-- '<+>'  == 'min'
-- '<.>'  == '+'
-- 'one'  == 0
-- 'zero' == 'distance' 'infinity'
-- @
-- @
-- bellmanFord ('vertex' 'a') 'a' == 'Map.fromList' [('a', 'one')]
-- bellmanFord ('vertex' 'a') 'z' == 'Map.fromList' [('a', 'zero')]
-- bellmanFord ('edge' x 'a' 'b') 'a' == 'Map.fromList' [('a', 'one'), ('b', x)]
-- bellmanFord ('edge' x 'a' 'b') 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- bellmanFord ('vertices' ['a', 'b']) 'a' == 'Map.fromList' [('a', 'one'), ('b', 'zero')]
-- bellmanFord ('vertices' ['a', 'b']) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- bellmanFord ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'a' == 'Map.fromList' [('a', 'one'), ('b', 3), ('c', 4)]
-- bellmanFord ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero'), ('c', 'zero')]
-- @
--
-- If the edge type is 'Capacity' 'Int':
-- @
-- '<+>'  == 'max'
-- '<.>'  == 'min'
-- 'one'  == 'distance' 'infinity'
-- 'zero' == 0
-- @
-- @
-- bellmanFord ('vertex' 'a') 'a' == 'Map.fromList' [('a', 'one')]
-- bellmanFord ('vertex' 'a') 'z' == 'Map.fromList' [('a', 'zero')]
-- bellmanFord ('edge' x 'a' 'b') 'a' == 'Map.fromList' [('a', 'one'), ('b', x)]
-- bellmanFord ('edge' x 'a' 'b') 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- bellmanFord ('vertices' ['a', 'b']) 'a' == 'Map.fromList' [('a', 'one'), ('b', 'zero')]
-- bellmanFord ('vertices' ['a', 'b']) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero')]
-- bellmanFord ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'a' == 'Map.fromList' [('a', 'one'), ('b', 3), ('c', 5)]
-- bellmanFord ('edges' [(5, 'a', 'c'), (3, 'a', 'b'), (1, 'b', 'c')]) 'z' == 'Map.fromList' [('a', 'zero'), ('b', 'zero'), ('c', 'zero')]
-- @
bellmanFord :: (Ord a, Dioid e) => AdjacencyMap e a -> a -> Map a e
bellmanFord wam src = maybe zm processL jim
  where
    am = adjacencyMap wam
    zm = Map.map (const zero) am
    vL = Map.keys am
    jim = Map.insert src one zm <$ Map.lookup src zm
    processL m = foldr (const processR) m vL
    processR m = foldr relaxV m vL
    relaxV v1 m =
        let eL = map (\(v2, e) -> (e, v1, v2)) . Map.toList $ am ! v1
         in foldr relaxE m eL
    relaxE (e, v1, v2) m =
        let n = ((m ! v1) <.> e) <+> (m ! v2)
         in Map.adjust (const n) v2 m

-- TODO: Improve documentation for floydWarshall
-- TODO: Improve performance
-- TODO: Use a strict fold
-- A generic Floyd-Warshall algorithm that finds all pair optimum path
-- based on the 'Dioid'.
--
-- We assume two things,
--
-- 1. The underlying semiring is selective i.e. the 
-- operation '<+>' always selects one of its arguments.
--
-- 2. The underlying semiring has an optimisation criterion.
-- 
-- The algorithm is best suited to work with the 'Optimum' data type.
--
-- The algorithm returns a 2 dimentional 'Map' with the signature
-- 'Map' a ('Map' a e). Assuming @g :: 'AdjacencyMap' ('Distance' 'Int') 'Int'),
-- if @floydWarshall g == m@ then @m '!' x '!' y@ is the distance between @x@ and @y@. 
-- @
-- forall vertex v in g. floydWarshall g ! v == 'dijkstra' g v
-- forall vertex v in g. floydWarshall g ! v == 'bellmanFord' g v
-- @
floydWarshall :: (Ord a, Dioid e) => AdjacencyMap e a -> Map a (Map a e)
floydWarshall wam = relax0 im
  where
    am = adjacencyMap wam
    zm = Map.map (const $ Map.map (const zero) am) am
    em = Map.unionWith Map.union am zm
    im = Map.mapWithKey (Map.adjust (const one)) em
    vL = Map.keys am
    relax0 m = foldr relax1 m vL
    relax1 i m = foldr (relax2 i) m vL
    relax2 i j m = foldr (relax3 i j) m vL
    relax3 i j k m =
        let n = (m ! i ! j) <+> ((m ! i ! k) <.> (m ! k ! j))
         in Map.adjust (Map.adjust (const n) j) i m

