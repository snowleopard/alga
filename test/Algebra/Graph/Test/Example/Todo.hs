{-# LANGUAGE OverloadedStrings #-}

module Algebra.Graph.Test.Example.Todo (
    testTodo
    ) where

import Algebra.Graph.Class
import Algebra.Graph.Test
import Algebra.Graph.Example.Todo

testTodo :: IO ()
testTodo = do
    putStrLn "\n============ Example.Todo (Holiday) ============"
    test "A todo list is semantically Maybe [a]" $
        todo ("presents" :: Todo String) == Just ["presents"]

    test "The overlay operator (+) adds non-dependent items to the todo list" $
        todo ("coat" + "presents" :: Todo String) == Just ["coat", "presents"]

    test "The connect operator (*) adds dependency between items" $
        let
            shopping :: Todo String = "presents" + "coat" + "scarf"
            holiday :: Todo String = shopping * "pack" * "travel"
        in todo (holiday + "scarf" * "coat")
            == Just ["presents","scarf","coat", "pack","travel"]

    test "Contradictory constraints make the todo list impossible to schedule" $
        let
            shopping :: Todo String = "presents" + "coat" + "scarf"
            holiday :: Todo String = shopping * "pack" * "travel"
        in todo (holiday + "travel" * "presents") == Nothing

    test "Introduce item priority to schedule the todo list" $
        let
            shopping :: Todo String
            shopping = "presents" + "coat" + low "phone wife" * "scarf"
            holiday :: Todo String
            holiday = shopping * "pack" * "travel" + "scarf" * "coat"
        in todo holiday
            == Just ["presents","phone wife","scarf","coat","pack","travel"]

    test "Custom connect operators pull/repel arguments during scheduling" $
        let
            shopping :: Todo String
            shopping = "presents" + "coat" + "phone wife" ~*~ "scarf"
            holiday :: Todo String
            holiday = shopping * "pack" * "travel" + "scarf" * "coat"
        in todo holiday
            == Just ["presents","phone wife","scarf","coat","pack","travel"]

    putStrLn "\n============ Example.Todo (Commandline) ============"
    test "The pull connect operator maintains command line semantics" $
        let
            cmdl :: Todo String
            cmdl = "gcc" * ("-c" ~*~ "src.c" + "-o" ~*~ "src.o")
        in todo cmdl == Just ["gcc","-c","src.c","-o","src.o"]

    test "Swapping flags are allowed by the commutative overlay opeartor" $
        let
            cmdl1 :: Todo String
            cmdl1 = "gcc" * ("-c" ~*~ "src.c" + "-o" ~*~ "src.o")
            cmdl2 :: Todo String
            cmdl2 = "gcc" * ("-o" ~*~ "src.o" + "-c" ~*~ "src.c")
        in cmdl1 == cmdl2

    test "The usual connect operator breaks semantics" $
        let
            cmdl :: Todo String
            cmdl = "gcc" * ("-c" * "src.c" + "-o" * "src.o")
        in
            todo cmdl == Just ["gcc","-c","-o","src.c","src.o"]

    test "Transform command lines by adding optimisation flag" $
        let
            cmdl :: Todo String
            cmdl = "gcc" * ("-c" ~*~ "src.c" + "-o" ~*~ "src.o")
            optimise :: Int -> Todo String -> Todo String
            optimise level = (* flag)
                where flag = vertex $ "-O" ++ show level
        in todo (optimise 2 cmdl) ==
            Just ["gcc","-c","src.c","-o","src.o","-O2"]
