{-# LANGUAGE OverloadedLists, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.Example.Automaton
-- Copyright  : (c) Andrey Mokhov 2016-2025
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

import Control.Arrow ((&&&))
import Data.Map    (Map)
import Data.Monoid (Any (..))

import Algebra.Graph.Label
import Algebra.Graph.Labelled
import Algebra.Graph.ToGraph

import qualified Data.Map as Map

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

-- TODO: Add an illustration.
-- | An example automaton for ordering coffee or tea.
--
-- @
-- coffeeTeaAutomaton = 'overlays' [ 'Choice'  '-<'['Coffee', 'Tea']'>-' 'Payment'
--                               , 'Payment' '-<'['Pay'        ]'>-' 'Complete'
--                               , 'Choice'  '-<'['Cancel'     ]'>-' 'Complete'
--                               , 'Payment' '-<'['Cancel'     ]'>-' 'Choice' ]
-- @
coffeeTeaAutomaton :: Automaton Alphabet State
coffeeTeaAutomaton = overlays [ Choice  -<[Coffee, Tea]>- Payment
                              , Payment -<[Pay        ]>- Complete
                              , Choice  -<[Cancel     ]>- Complete
                              , Payment -<[Cancel     ]>- Choice ]

-- | The map of 'State' reachability.
--
-- @
-- reachability = Map.'Map.fromList' $ map ('id' '&&&' 'reachable' skeleton) ['Choice' ..]
--   where
--     skeleton = emap (Any . not . 'isZero') coffeeTeaAutomaton
-- @
--
-- Or, when evaluated:
--
-- @
-- reachability = Map.'Map.fromList' [ ('Choice'  , ['Choice'  , 'Payment', 'Complete'])
--                             , ('Payment' , ['Payment' , 'Choice' , 'Complete'])
--                             , ('Complete', ['Complete'                   ]) ]
-- @
reachability :: Map State [State]
reachability = Map.fromList $ map (id &&& reachable skeleton) [Choice ..]
  where
    skeleton :: Graph Any State
    skeleton = emap (Any . not . isZero) coffeeTeaAutomaton
