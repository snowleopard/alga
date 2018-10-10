{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.Example.Network
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
-- in the module "Algebra.Graph.Labelled" for working with networks, i.e. graphs
-- whose edges are labelled with distances.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.Example.Network where

import Algebra.Graph.Labelled

-- | Our example networks have /cities/ as vertices.
data City = Aberdeen
          | Edinburgh
          | Glasgow
          | London
          | Newcastle
          deriving (Bounded, Enum, Eq, Ord, Show)

-- | For simplicity we measure /journey times/ in integer number of minutes.
type JourneyTime = Int

-- | A part of the EastCoast train network between 'Aberdeen' and 'London'.
--
-- @
-- eastCoast = 'overlays' [ 'Aberdeen'  '-<'&#49;50'>-' 'Edinburgh'
--                      , 'Edinburgh' '-<' 90'>-' 'Newcastle'
--                      , 'Newcastle' '-<'&#49;70'>-' 'London' ]
-- @
eastCoast :: Network JourneyTime City
eastCoast = overlays [ Aberdeen  -<150>- Edinburgh
                     , Edinburgh -< 90>- Newcastle
                     , Newcastle -<170>- London ]

-- | A part of the ScotRail train network between 'Aberdeen' and 'Glasgow'.
--
-- @
-- scotRail = 'overlays' [ 'Aberdeen'  '-<'&#49;40'>-' 'Edinburgh'
--                     , 'Edinburgh' '-<' 50'>-' 'Glasgow'
--                     , 'Edinburgh' '-<' 70'>-' 'Glasgow' ]
-- @
scotRail :: Network JourneyTime City
scotRail = overlays [ Aberdeen  -<140>- Edinburgh
                    , Edinburgh -< 50>- Glasgow
                    , Edinburgh -< 70>- Glasgow ]

-- TODO: Add an illustration.
-- | An example train network.
--
-- @
-- network = 'overlay' 'eastCost' 'scotRail'
-- @
network :: Network JourneyTime City
network = overlay eastCost scotRail
