{-# LANGUAGE OverloadedLists, FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.Example.Automaton
-- Copyright  : (c) Andrey Mokhov 2016-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- __Alga__ is a library for algebraic construction and manipulation of graphs
-- in Haskell. See <https://github.com/snowleopard/alga-paper this paper> for the
-- motivation behind the library, the underlying theory, and implementation details.
--
-- This module contains a simple example of using edge-labelled graphs defined
-- in the module "Algebra.Graph.Labelled" for working with finite automata.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.Example.Automaton where

import Data.Map (Map)
import qualified Data.Map as Map

import Algebra.Graph.Labelled
import Algebra.Graph.ToGraph

-- | The alphabet of actions for ordering coffee or tea.
data Alphabet = Coffee -- ^ Order coffee
              | Tea    -- ^ Order tea
              | Cancel -- ^ Cancel payment or order
              | Pay    -- ^ Pay for the order
              deriving (Bounded, Enum, Eq, Ord, Show)

-- | The state of the order.
data State = Choice   -- ^ Choosing what to order
           | Payment  -- ^ Making the payment
           | Complete -- ^ The order is complete
           deriving (Bounded, Enum, Eq, Ord, Show)

-- | An example automaton for ordering coffee or tea.
--
-- @
-- order = 'edges' [ ('Choice' , ['Coffee', 'Tea'], 'Payment' )
--               , ('Choice' , ['Cancel'     ], 'Complete')
--               , ('Payment', ['Cancel'     ], 'Choice'  )
--               , ('Payment', ['Pay'        ], 'Complete') ]
-- @
order :: Automaton Alphabet State
order = edges [ (Choice , [Coffee, Tea], Payment )
              , (Choice , [Cancel     ], Complete)
              , (Payment, [Cancel     ], Choice  )
              , (Payment, [Pay        ], Complete) ]

-- | The map of 'State' reachability.
--
-- @
-- reachability = Map.fromList $ map (\s -> (s, 'reachable' s order)) ['Choice' ..]
-- @
reachability :: Map State [State]
reachability = Map.fromList $ map (\s -> (s, reachable s order)) [Choice ..]
