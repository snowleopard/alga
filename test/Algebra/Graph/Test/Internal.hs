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

import Prelude
import Data.Monoid

import Algebra.Graph.Internal
import Algebra.Graph.Test

testInternal :: IO ()
testInternal = do
    putStrLn "\n============ Internal.List ============"
    test "pure \"al\" <> pure \"ga\"          == [\"al\", \"ga\"]" $
          pure "al" <> pure "ga"          == (["al", "ga"] :: List String)
