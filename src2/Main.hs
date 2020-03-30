module Main where

import Base
import Parser (parse)
import Compiler (compile)
import System.Process (system)
import System.Directory (removeFile)
import Data.Time.Clock.System (getSystemTime, systemSeconds)

main = do
  system $ "mkdir -p /tmp/moa"
  -- primitives
  test "0" "0"
  test "-1" "(-1)"
  test "true" "true"
  test "false" "false"
  test "" "\"\""
  test "a" "\"a\""
  -- containers
  test "2" "[1 3].count"
  test "3" $ code [
      "vec2:"
    , "  x int"
    , "  y int"
    , "vec2(1 2).x + vec2(1 2).y"]
  -- enum and branch
  test "3" $ code [
      "ab:"
    , "| a"
    , "| b"
    , "f x = x"
    , "| a -> 1"
    , "| b -> 2"
    , "f(a) + f(b)"]
  -- define and call
  test "2" $ code [
      "inc x = x + 1"
    , "inc(1)"]
  -- operations
  test "1" "1 + 2 * 3 / (4 - 2) % 3"
  test "true" "true && (false || true)"
  test "true" "1 == 1 && 1 != 2"
  test "false" "1 == 2 || 1 != 1"
  test "true" "1 >= 1 && 1<= 1"
  test "false" "1 > 1 || 1 < 1"
  -- monad pure
  test "3" $ code [
      "add x y ="
    , "  z = x + y"
    , "  z"
    , "add(1 2)"]
  -- monad do
  test "2" $ code [
      "mut x ="
    , "  x += 1"
    , "  x"
    , "mut(1)"]
  -- monad opt
  test "1" "ok(1)"
  test "1" "ok(1).or(2)"
  test "message" "err(\"message\")"
  test "1" "err(\"message\").or(1)"
  test "5" $ code [
      "f v = v"
    , "| ok -> 2"
    , "| err -> 3"
    , "go ="
    , "  v1 <- f(ok(1))"
    , "  v2 <- f(err(\"msg\"))"
    , "  v1 + v2"
    , "go"
    ]
  -- monad try
  test "failed" $ code [
      "f ="
    , "  err(\"failed\")"
    , "  ok(1)"
    , "f"
    ]
  test "2" $ code [
      "f ="
    , "  err(\"failed\").or(1)"
    , "  ok(2)"
    , "f"
    ]
  test "failed" $ code [
      "f ="
    , "  v <- err(\"failed\")"
    , "  v"
    , "f"
    ]
  test "3" $ code [
      "f ="
    , "  v <- err(\"failed\").or(3)"
    , "  v"
    , "f"
    ]
  -- private
  test "3" $ code [
      "f = go:"
    , "  go = a + b"
    , "  a = 1"
    , "  b = 2"
    , "f"
    ]
  test "3" $ code [
      "f x = go:"
    , "  n int"
    , "  f1 ="
    , "    n += 1"
    , "  f2 ="
    , "    n += 2"
    , "  iif cond a b = cond"
    , "  | true -> a"
    , "  | false -> b"
    , "  go ="
    , "    v <- iif(x f1 f2)"
    , "    v"
    , "g = go:"
    , "  go ="
    , "    a <- f(true)"
    , "    b <- f(false)"
    , "    a + b"
    , "g"
    ]
  putStrLn "done"

code xs = string_join "\n" xs

test :: String -> String -> IO ()
test expect input = go
  where
    go = do
      with_rb
      --with_js
    name = safe_name input

    with_rb = do
      let input_with_main = replace_last (\x -> "main = " ++ x) (reverse $ lines input) []
      (ast, ruby, stdout) <- eval_with_ruby expect input_with_main name
      test_eq expect stdout ast input ruby

    with_js = do
      compiler <- readFile "v1.moa"
      let moa = compiler ++ "\n" ++ "main = compile(" ++ show input ++ ")"
      (_, _, js) <- eval_with_ruby expect input name
      let full_js = "main = () => (" ++ js ++ ")\nprocess.stdout.write(String(main()))"
      stdout <- exec "node" full_js input ".js"
      test_eq expect stdout Void input js

    safe_name name = go
      where
        go = take 30 $ fst $ foldr glue ("", ' ') name
        glue c (acc, prev) = let
          c2 = safe c prev
          acc2 = if c2 == '*' then acc else c2 : acc
          in (acc2, c2)
        safe c prev
          | elem c "abcdefghijklmnopqrstuvxwyz0123456789" = c
          | prev == '_' || prev == '*' = '*'
          | otherwise = '_'

    replace_last :: (String -> String) -> [String] -> [String] -> String
    replace_last f [last] acc = (unlines $ reverse acc) ++ f last
    replace_last f (x:xs) acc = replace_last f (x : acc) xs

    test_eq expect stdout ast input code = if expect == stdout
      then putChar '.'
      else putStrLn $ unlines [
          "x"
        , "- expect: " ++ show expect
        , "-   fact: " ++ show stdout ++ " :: " ++ show ast
        , "-    src: " ++ input
        , "-   code: " ++ code
        ]

    eval_with_ruby :: String -> String -> String -> IO (AST, String, String)
    eval_with_ruby expect input name = let
      ast = case parse input of
        Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ expect
        Just (ast, s) -> if pos s == len s
          then ast
          else error $ "Parser error\n- remaining: " ++ (drop (pos s) (src s)) ++ "\n- src: " ++ input
      ruby = compile ast
      in do
        stdout <- exec "ruby" ruby input (name ++ ".rb")
        return (ast, ruby, stdout)

    exec command body unique name = do
      let exe_path = "/tmp/moa/" ++ name
      let out_path = "/tmp/moa/stdout_" ++ name
      writeFile exe_path body
      system $ command ++ " " ++ exe_path ++ " > " ++ out_path
      ret <- readFile out_path
      removeFile out_path
      return ret
