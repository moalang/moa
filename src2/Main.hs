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
  test "1" $ code ["x = 1", "x"]
  test "2" $ code ["inc x = x + 1", "inc(1)"]
  test "3" $ code ["vector2:", "  x int", "  y int", "v = vector2(1 2)", "v.x + v.y"]
  test "node(leaf(1) node(leaf(2) leaf(3)))" $ code ["tree a | leaf a | node tree tree", "node(leaf(1) node(leaf(2) leaf(3)))"]
  test "2" $ code ["add x y =", "  z = x + y", "  z", "add(1, 2)"]
  -- operations
  test "5" "2 + 3"
  test "-1" "2 - 3"
  test "6" "2 * 3"
  test "0" "2 / 3"
  test "2" "2 % 3"
  test "9" "2 + 3 + 4"
  test "14" "2 + (3 * 4)"
  test "20" "(2 + 3) * 4"
  test "true" "true && true"
  test "false" "false && true"
  test "true" "true || true"
  test "true" "false || true"
  test "true" "1 == 1"
  test "false" "1 == 2"
  test "false" "1 != 1"
  test "true" "1 != 2"
  test "true" "1 >= 1"
  test "false" "1 > 1"
  test "true" "1 <= 1"
  test "false" "1 < 1"
  -- mutable
  putStrLn "done"

eval :: String -> IO String
eval ruby = do
  system $ "mkdir -p /tmp/moa"
  writeFile "/tmp/moa/moa.rb" ruby
  system $ "(cd /tmp/moa/ && ruby moa.rb > stdout.txt)"
  readFile "/tmp/moa/stdout.txt"

test :: String -> String -> IO ()
test expect input = go
  where
    go = do
      case parse input of
        Nothing -> error $ "Parser error " ++ input ++ " expect: " ++ expect
        Just (x, s) -> if pos s == len s
          then run x
          else error $ "Parser error\n- remaining: " ++ (drop (pos s) (src s)) ++ "\n- src: " ++ input
    run ast = do
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

code xs = string_join "\n" xs
