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
      system $ "mkdir -p /tmp/moa"
      run "ruby" test_rb
      run "  js" test_js
      putStrLn "done"
    run title f = do
      putStr title
      mapM_ (\(i, (expect, input)) -> f i expect input) $ zip [0..] tests
      putStrLn ""

tests = go
  where
    t expect input = (expect, input)
    code xs = string_join "\n" xs
    go = [
      -- primitives
        t "0" "0"
      , t "-1" "(-1)"
      , t "true" "true"
      , t "false" "false"
      , t "" "``"
      , t "a" "`a`"
      -- containers
      , t "2" "[1 3].count"
      , t "3" $ code [
            "vec2:"
          , "  x int"
          , "  y int"
          , "vec2(1 2).x + vec2(1 2).y"]
      -- enum and branch
      , t "3" $ code [
            "ab:"
          , "| a"
          , "| b"
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
      , t "ab" "\"a\" ++ \"b\""
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
      , t "message" "err(\"message\")"
      , t "1" "err(\"message\").or(1)"
      , t "5" $ code [
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
      , t "failed" $ code [
            "f ="
          , "  err(\"failed\")"
          , "  ok(1)"
          , "f"
          ]
      , t "2" $ code [
            "f ="
          , "  err(\"failed\").or(1)"
          , "  ok(2)"
          , "f"
          ]
      , t "failed" $ code [
            "f ="
          , "  v <- err(\"failed\")"
          , "  v"
          , "f"
          ]
      , t "3" $ code [
            "f ="
          , "  v <- err(\"failed\").or(3)"
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
      , t "3" $ code [
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
      -- mutation
      , t "3" $ code [
            "f = go:"
          , "  list array(int)"
          , "  go ="
          , "    list.push(1)"
          , "    list.push(2)"
          , "    list.sum"
          , "f"
        ]
      ]

test_rb i expect input = do
  let name = "rb_" ++ show i ++ "_" ++ map safe expect
  let input_with_main = replace_last (\x -> "main = " ++ x) (reverse $ lines input) []
  (ast, ruby, stdout) <- eval_with_ruby expect input_with_main name
  assert_eq expect stdout ast input ruby

test_js i expect input = do
  let name = "js_" ++ show i ++ "_" ++ map safe expect
  compiler <- readFile "v1.moa"
  let moa = compiler ++ "\n" ++ "main = compile(" ++ show input ++ ")"
  (ast, _, js) <- eval_with_ruby expect moa name
  let full_js = "main = () => (" ++ js ++ ")\nprocess.stdout.write(String(main()))"
  stdout <- exec "node" full_js input (name ++ ".js")
  assert_eq expect stdout ast input js

safe c = if elem c "abcdefghijklmnopqrstuvxwyz0123456789" then c else '_'

replace_last :: (String -> String) -> [String] -> [String] -> String
replace_last f [last] acc = (unlines $ reverse acc) ++ f last
replace_last f (x:xs) acc = replace_last f (x : acc) xs

assert_eq expect stdout ast input code = if expect == stdout
  then putChar '.'
  else error $ unlines [
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
