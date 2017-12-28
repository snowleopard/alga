{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
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
    putStrLn "\n============ Utilities.literal ============"
    test "literal \"Hello, \" <> literal \"World!\" == literal \"Hello, World!\"" $
          literal "Hello, " <> literal "World!" == literal ("Hello, World!" :: String)

    test "literal \"I am just a string literal\"  == \"I am just a string literal\"" $
          literal "I am just a string literal"  == ("I am just a string literal" :: Doc String)

    test "literal mempty                        == mempty" $
          literal mempty                        == (mempty :: Doc String)

    test "literal \"al\" <> literal \"ga\"          == [\"al\", \"ga\"]" $
          literal "al" <> literal "ga"          == (["al", "ga"] :: Doc String)

    test "render . literal                      == id" $ \(x :: String) ->
         (render . literal) x                   == x

    test "literal . render                      == id" $ \(xs :: [String]) -> let x = mconcat (map literal xs) in
         (literal . render) x                   == x

    putStrLn "\n============ Utilities.render ============"
    test "render (literal \"al\" <> literal \"ga\") == \"alga\"" $
          render (literal "al" <> literal "ga") == ("alga" :: String)

    test "render mempty                         == mempty" $
          render mempty                         == (mempty :: Doc String)
