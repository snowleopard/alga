{-# LANGUAGE CPP, OverloadedLists, TypeFamilies #-}
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

import Algebra.Graph.Label
import Algebra.Graph.Labelled
import Algebra.Graph.ToGraph

#if !MIN_VERSION_base(4,8,0)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts

instance Ord a => IsList (Set a) where
    type Item (Set a) = a
    fromList = Set.fromList
    toList   = Set.toList
#endif

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
-- order = 'overlays' [ 'Choice'  '-<'['Coffee', 'Tea']'>-' 'Payment'
--                  , 'Choice'  '-<'['Cancel'     ]'>-' 'Complete'
--                  , 'Payment' '-<'['Cancel'     ]'>-' 'Choice'
--                  , 'Payment' '-<'['Pay'        ]'>-' 'Complete' ]
-- @
order :: Automaton Alphabet State
order = overlays [ Choice  -<[Coffee, Tea]>- Payment
                 , Choice  -<[Cancel     ]>- Complete
                 , Payment -<[Cancel     ]>- Choice
                 , Payment -<[Pay        ]>- Complete ]

order2 :: Graph (RE Alphabet) State
order2 = overlays [ Choice  -<Var Coffee >- Payment
                  , Choice  -<Var Tea    >- Payment
                  , Choice  -<Var Cancel >- Complete
                  , Payment -<Var Cancel >- Choice
                  , Payment -<Var Pay    >- Complete ]

-- | The map of 'State' reachability.
--
-- @
-- reachability = Map.'Map.fromList' $ map (\s -> (s, 'reachable' s 'order')) ['Choice' ..]
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
reachability = Map.fromList $ map (\s -> (s, reachable s order)) [Choice ..]
