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

import Prelude ()
import Prelude.Compat

import Data.Semigroup

import Algebra.Graph (Graph, circuit)
import Algebra.Graph.Export hiding (unlines)
import Algebra.Graph.Export.Dot (Attribute (..))
import Algebra.Graph.Test

import qualified Algebra.Graph.Export     as E
import qualified Algebra.Graph.Export.Dot as ED

testExport :: IO ()
testExport = do
    putStrLn "\n============ Export.literal ============"
    test "literal \"Hello, \" <> literal \"World!\" == literal \"Hello, World!\"" $
          literal "Hello, " <> literal "World!" == literal ("Hello, World!" :: String)

    test "literal \"I am just a string literal\"  == \"I am just a string literal\"" $
          literal "I am just a string literal"  == ("I am just a string literal" :: Doc String)

    test "literal mempty                        == mempty" $
          literal mempty                        == (mempty :: Doc String)

    test "render . literal                      == id" $ \(x :: String) ->
         (render . literal) x                   == x

    test "literal . render                      == id" $ \(xs :: [String]) -> let x = mconcat (map literal xs) in
         (literal . render) x                   == x

    putStrLn "\n============ Export.render ============"
    test "render (literal \"al\" <> literal \"ga\") == \"alga\"" $
          render (literal "al" <> literal "ga") == ("alga" :: String)

    test "render mempty                         == mempty" $
          render mempty                         == (mempty :: Doc String)

    putStrLn "\n============ Export.<+> ============"
    test "x <+> mempty         == x" $ \(x :: Doc String) ->
          x <+> mempty         == x

    test "mempty <+> x         == x" $ \(x :: Doc String) ->
          mempty <+> x         == x

    test "x <+> (y <+> z)      == (x <+> y) <+> z" $ \(x :: Doc String) y z ->
          x <+> (y <+> z)      == (x <+> y) <+> z

    test "\"name\" <+> \"surname\" == \"name surname\"" $
          "name" <+> "surname" == ("name surname" :: Doc String)

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
          indent 0 (literal x) == literal x

    test "indent 1 mempty == \" \"" $
          indent 1 mempty == (" " :: Doc String)

    putStrLn "\n============ Export.unlines ============"
    test "unlines []                    == mempty" $
        E.unlines []                    == (mempty :: Doc String)

    test "unlines [mempty]              == \"\\n\"" $
        E.unlines [mempty]              == ("\n" :: Doc String)

    test "unlines [\"title\", \"subtitle\"] == \"title\\nsubtitle\\n\"" $
        E.unlines ["title",    "subtitle" ] == ("title\nsubtitle\n" :: Doc String)

    putStrLn "\n============ Export.export ============"
    let vDoc x   = literal (show x) <> "\n"
        eDoc x y = literal (show x) <> " -> " <> literal (show y) <> "\n"
    test "render $ export vDoc eDoc (1 + 2 * (3 + 4) :: Graph Int)" $
         (render (export vDoc eDoc (1 + 2 * (3 + 4) :: Graph Int)) :: String) ==
            unlines [ "1"
                    , "2"
                    , "3"
                    , "4"
                    , "2 -> 3"
                    , "2 -> 4" ]

    putStrLn "\n============ Export.Dot.export ============"
    let style = ED.Style
            { ED.graphName               = "Example"
            , ED.preamble                = "  // This is an example\n"
            , ED.graphAttributes         = ["label" := "Example", "labelloc" := "top"]
            , ED.defaultVertexAttributes = ["shape" := "circle"]
            , ED.defaultEdgeAttributes   = mempty
            , ED.vertexName              = \x   -> "v" ++ show x
            , ED.vertexAttributes        = \x   -> ["color" := "blue"   | odd x      ]
            , ED.edgeAttributes          = \x y -> ["style" := "dashed" | odd (x * y)] }
    test "export style (1 * 2 + 3 * 4 * 5 :: Graph Int)" $
        (ED.export style (1 * 2 + 3 * 4 * 5 :: Graph Int) :: String) ==
            unlines [ "digraph Example"
                    , "{"
                    , "  // This is an example"
                    , ""
                    , "  graph [label=\"Example\" labelloc=\"top\"]"
                    , "  node [shape=\"circle\"]"
                    , "  \"v1\" [color=\"blue\"]"
                    , "  \"v2\""
                    , "  \"v3\" [color=\"blue\"]"
                    , "  \"v4\""
                    , "  \"v5\" [color=\"blue\"]"
                    , "  \"v1\" -> \"v2\""
                    , "  \"v3\" -> \"v4\""
                    , "  \"v3\" -> \"v5\" [style=\"dashed\"]"
                    , "  \"v4\" -> \"v5\""
                    , "}" ]

    putStrLn "\n============ Export.Dot.exportAsIs ============"
    test "exportAsIs (circuit [\"a\", \"b\", \"c\"] :: Graph String)" $
        (ED.exportAsIs (circuit ["a", "b", "c"] :: Graph String) :: String) ==
            unlines [ "digraph"
                    , "{"
                    , "  \"a\""
                    , "  \"b\""
                    , "  \"c\""
                    , "  \"a\" -> \"b\""
                    , "  \"b\" -> \"c\""
                    , "  \"c\" -> \"a\""
                    , "}" ]

    putStrLn "\n============ Export.Dot.exportViaShow ============"
    test "exportViaShow (1 + 2 * (3 + 4) :: Graph Int)" $
        (ED.exportViaShow (1 + 2 * (3 + 4) :: Graph Int) :: String) ==
            unlines [ "digraph"
                    , "{"
                    , "  \"1\""
                    , "  \"2\""
                    , "  \"3\""
                    , "  \"4\""
                    , "  \"2\" -> \"3\""
                    , "  \"2\" -> \"4\""
                    , "}" ]
