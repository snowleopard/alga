{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Internal
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Internal".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Internal (
    -- * Testsuite
    testInternal
  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (pure)
import Data.Semigroup

import Algebra.Graph.Internal
import Algebra.Graph.Test

testInternal :: IO ()
testInternal = do
    putStrLn "\n============ Internal.List ============"
    test "pure 1 <> pure 4 == [1, 4]" $
          pure 1 <> pure 4 == ([1, 4] :: List Int)
