module Main where

import Base
import Parser (parse)
import Compiler (compile)
import System.Process (system)
import System.Directory (removeFile)
import Data.Time.Clock.System (getSystemTime, systemSeconds)

main = go
  where
    go = do
      prepare "/tmp/moa/moa"
      mapM_ (\(i, (expect, input)) -> run_test i expect input) $ zip [0..] tests
      putStrLn "done"

tests = go1
  where
    t expect input = (expect, input)
    code xs = string_join "\n" xs
    go2 = [
        t "5" $ code [
            "f = go:"
          , "  list array(int)"
          , "  go ="
          , "    list.push(1)"
          , "    list.push(4)"
          , "    list.sum"
          , "f"
        ]
        ]
    go1 = [
      -- primitives
        t "0" "0"
      , t "-1" "(-1)"
      , t "true" "true"
      , t "false" "false"
      , t "" "``"
      , t "a" "`a`"
      -- containers
      , t "2" "[1 2].count"
      , t "3" $ code [
            "vec2:"
          , "  x int"
          , "  y int"
          , "vec2(1 2).x + vec2(1 2).y"]
      -- enum and branch
      , t "3" $ code [
            "ab|"
          , "  a"
          , "  b"
          , "f x = x"
          , "| a -> 1"
          , "| b -> 2"
          , "f(a) + f(b)"]
      -- define and call
      , t "2" $ code [
            "inc x = x + 1"
          , "inc(1)"]
      , t "3" "(x => x + 1)(2)"
      , t "5" $ code [
            "id x = x"
          , "id((n => n + 2))(3)"
          ]
      -- core methods
      , t "true" "`hello`.has(`e`)"
      , t "false" "`hello`.has(`z`)"
      -- operations
      , t "1" "1 + 2 * 3 / (4 - 2) % 3"
      , t "ab" "`a` ++ `b`"
      , t "true" "true && (false || true)"
      , t "true" "1 == 1 && 1 != 2"
      , t "false" "1 == 2 || 1 != 1"
      , t "true" "1 >= 1 && 1<= 1"
      , t "false" "1 > 1 || 1 < 1"
      -- monad pure
      , t "3" $ code [
            "add x y ="
          , "  z = x + y"
          , "  z"
          , "add(1 2)"]
      -- monad do
      , t "2" $ code [
            "mut x ="
          , "  x += 1"
          , "  x"
          , "mut(1)"]
      -- monad opt
      , t "1" "ok(1)"
      , t "1" "ok(1).or(2)"
      , t "message" "err(`message`)"
      , t "1" "err(`message`).or(1)"
      , t "5" $ code [
            "f v = v"
          , "| ok -> 2"
          , "| err -> 3"
          , "go ="
          , "  v1 <- f(ok(1))"
          , "  v2 <- f(err(`msg`))"
          , "  v1 + v2"
          , "go"
          ]
      -- monad try
      , t "failed" $ code [
            "f ="
          , "  err(`failed`)"
          , "  ok(1)"
          , "f"
          ]
      , t "2" $ code [
            "f ="
          , "  err(`failed`).or(1)"
          , "  ok(2)"
          , "f"
          ]
      , t "failed" $ code [
            "f ="
          , "  v <- err(`failed`)"
          , "  v"
          , "f"
          ]
      , t "3" $ code [
            "f ="
          , "  v <- err(`failed`).or(3)"
          , "  v"
          , "f"
          ]
      -- private functions
      , t "3" $ code [
            "f = go:"
          , "  go = a + b"
          , "  a = 1"
          , "  b = 2"
          , "f"
          ]
      , t "4" $ code [
            "f x = go:"
          , "  n int"
          , "  f1 ="
          , "    n += 1"
          , "  f2 ="
          , "    n += 3"
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
      -- mutation
      , t "5" $ code [
            "f = go:"
          , "  list array(int)"
          , "  go ="
          , "    list.push(1)"
          , "    list.push(4)"
          , "    list.sum"
          , "f"
        ]
      ]

run_test i expect input = do
  let moa_path = "/tmp/moa/" ++ show i ++ ".moa"
  let js_path = "/tmp/moa/" ++ show i ++ ".js"
  let ret_path = "/tmp/moa/" ++ show i ++ ".txt"
  let a = lines input
  let n = length a - 1
  let test_code = unlines $ (take n a) ++ ["test = " ++ (a !! n)]
  writeFile moa_path test_code
  let cmd1 = "/usr/bin/ruby /tmp/moa/moa < " ++ moa_path ++ " > " ++ js_path
  let cmd2 = "echo \"\n\nprocess.stdout.write(String(__effect(test())))\" >> " ++ js_path
  let cmd3 = "node -r ./vm.js " ++ js_path ++ " > " ++ ret_path
  system $ cmd1 ++ ";" ++ cmd2 ++ ";" ++ cmd3
  stdout <- readFile ret_path
  if expect == stdout
    then putChar '.'
    else error $ unlines [
        "Failed `" ++ input ++ "`"
      , "- expect: " ++ show expect
      , "-   fact: " ++ show stdout
      , "-   ruby: " ++ "ruby /tmp/moa/moa < " ++ moa_path
      , "-     js: " ++ "node -r ./vm.js " ++ js_path
      ]

prepare exe_path = go
  where
    go = do
      system $ "mkdir -p /tmp/moa"
      system $ "cp vm.js /tmp/moa/"
      moa <- readFile "v1.moa"
      vm <- readFile "vm.rb"
      writeFile exe_path (ruby moa vm)
      system $ "chmod 0755 " ++ exe_path
    ruby moa vm = let
      ast = case parse moa of
        Nothing -> error $ "Parser error " ++ moa
        Just (ast, s) -> if pos s == len s
          then ast
          else error $ "Parser error\n- remaining: " ++ (take 100 $ drop (pos s) (src s)) ++ " ...\n- src: " ++ take 100 moa ++ " ..."
      in unlines ["#!/usr/bin/ruby", vm, compile ast]
