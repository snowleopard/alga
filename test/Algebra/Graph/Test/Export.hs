{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Algebra.Graph.Test.Export
-- Copyright  : (c) Andrey Mokhov 2016-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Testsuite for "Algebra.Graph.Export" and "Algebra.Graph.Export.Dot".
-----------------------------------------------------------------------------
module Algebra.Graph.Test.Export (
    -- * Testsuite
    testExport
  ) where

import Prelude hiding (unlines)
import Data.Monoid

import Algebra.Graph.Test
import Algebra.Graph.Export

testExport :: IO ()
testExport = do
    putStrLn "\n============ Export.literal ============"
    test "literal \"Hello, \" <> literal \"World!\" == literal \"Hello, World!\"" $
          literal "Hello, " <> literal "World!" == literal ("Hello, World!" :: String)

    test "literal \"I am just a string literal\"  == \"I am just a string literal\"" $
          literal "I am just a string literal"  == ("I am just a string literal" :: Doc String)

    test "literal mempty                        == mempty" $
          literal mempty                        == (mempty :: Doc String)

    test "export . literal                      == id" $ \(x :: String) ->
         (export . literal) x                   == x

    test "literal . export                      == id" $ \(xs :: [String]) -> let x = mconcat (map literal xs) in
         (literal . export) x                   == x

    putStrLn "\n============ Export.export ============"
    test "export (literal \"al\" <> literal \"ga\") == \"alga\"" $
          export (literal "al" <> literal "ga") == ("alga" :: String)

    test "export mempty                         == mempty" $
          export mempty                         == (mempty :: Doc String)

    putStrLn "\n============ Export.brackets ============"
    test "brackets \"i\"    == \"[i]\"" $
          brackets "i"    == ("[i]" :: Doc String)

    test "brackets mempty == \"[]\"" $
          brackets mempty == ("[]" :: Doc String)

    putStrLn "\n============ Export.doubleQuotes ============"
    test "doubleQuotes \"/path/with spaces\"   == \"\\\"/path/with spaces\\\"\"" $
          doubleQuotes "/path/with spaces"    == ("\"/path/with spaces\"" :: Doc String)

    test "doubleQuotes (doubleQuotes mempty) == \"\\\"\\\"\\\"\\\"\"" $
          doubleQuotes (doubleQuotes mempty) == ("\"\"\"\"" :: Doc String)

    putStrLn "\n============ Export.indent ============"
    test "indent 0        == id" $ \(x :: String) ->
         (indent 0) (literal x) == literal x

    test "indent 1 mempty == \" \"" $
          indent 1 mempty == (" " :: Doc String)

    putStrLn "\n============ Export.indent ============"
    test "unlines []                    == mempty" $
          unlines []                    == (mempty :: Doc String)

    test "unlines [mempty]              == \"\\n\"" $
          unlines [mempty]              == ("\n" :: Doc String)

    test "unlines [\"title\", \"subtitle\"] == \"title\\nsubtitle\\n\"" $
          unlines ["title",    "subtitle" ] == ("title\nsubtitle\n" :: Doc String)
