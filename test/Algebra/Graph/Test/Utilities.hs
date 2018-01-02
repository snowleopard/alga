{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Utilities
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Utilities".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Utilities (
    -- * Testsuite
    testUtilities
  ) where

import Prelude
import Data.Monoid

import Algebra.Graph.Utilities
import Algebra.Graph.Test

testUtilities :: IO ()
testUtilities = do
    putStrLn "\n============ Utilities.List ============"
    test "pure \"al\" <> pure \"ga\"          == [\"al\", \"ga\"]" $
          pure "al" <> pure "ga"          == (["al", "ga"] :: List String)
