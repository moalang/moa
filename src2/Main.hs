module Main where

import Base
import Parser (parse)
import Compiler (compile)
import System.Process (system)

main = do
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
    ]
  -- monad try
  test "failed" $ code [
      "f ="
    , "  err(\"failed\")"
    , "  ok(1)"
    ]
  test "2" $ code [
      "f ="
    , "  err(\"failed\").or(1)"
    , "  ok(2)"
    ]
  test "failed" $ code [
      "f ="
    , "  v <- err(\"failed\")"
    , "  v"
    ]
  test "3" $ code [
      "f ="
    , "  v <- err(\"failed\").or(3)"
    , "  v"
    ]
  -- private
  test "3" $ code [
      "f = go:"
    , "  go = a + b"
    , "  a = 1"
    , "  b = 2"
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
    ]
  putStrLn "done"

code xs = string_join "\n" xs

test :: String -> String -> IO ()
test expect input = do
  test_on_haskell expect input
  test_on_v1 expect input

test_on_haskell :: String -> String -> IO ()
test_on_haskell expect input = do
  let ast = parse_or_fail input expect
  let ruby = compile ast
  stdout <- eval ruby
  if expect == stdout
  then putChar '.'
  else putStrLn $ unlines [
      "x"
    , "- expect: " ++ show expect
    , "-   fact: " ++ show stdout ++ " :: " ++ show ast
    , "-    src: " ++ input
    , "-   ruby: " ++ ruby
    ]

test_on_v1 expect input = do
  return ()
  --v1_src <- readFile "v1.moa"
  --test_on_haskell expect v1_src

parse_or_fail input expect = case parse input of
  Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ expect
  Just (ast, s) -> if pos s == len s
    then ast
    else error $ "Parser error\n- remaining: " ++ (drop (pos s) (src s)) ++ "\n- src: " ++ input

eval :: String -> IO String
eval ruby = do
  system $ "mkdir -p /tmp/moa"
  writeFile "/tmp/moa/moa.rb" ruby
  system $ "(cd /tmp/moa/ && ruby moa.rb > stdout.txt)"
  readFile "/tmp/moa/stdout.txt"
