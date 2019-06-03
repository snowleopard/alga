{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Labelled.AdjdacencyMap.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2019
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : unstable
--
-- This module exposes the implementation of edge-labelled adjacency maps. The
-- API is unstable and unsafe, and is exposed only for documentation. You should
-- use the non-internal module "Algebra.Graph.Labelled.AdjdacencyMap" instead.
-----------------------------------------------------------------------------
module Algebra.Graph.Labelled.AdjacencyMap.Internal (
    -- * Labelled adjacency map implementation
    AdjacencyMap (..), consistent
    ) where

import Data.Map.Strict (Map)
import Data.Monoid (Sum (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Algebra.Graph.Label

