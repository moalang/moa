module Compiler (compile) where

import Base
import Debug.Trace (trace)

compile :: AST -> String
compile top = unlines [
                "def main"
              , eval top
              , "end"
              , "print main"
              ]

eval ast = case ast of
  Void -> "()"
  I64 v -> show v
  Bool b -> if b then "true" else "false"
  String s -> show s
  Def name args body -> eval_def name args body
  Call name argv -> "(" ++ eval_call name argv ++ ")"
  Stmt lines -> unlines $ map eval lines
  Struct name args methods -> eval_struct name args methods
eval_call name argv = if elem name all_parse_ops
  then eval_op2 name argv
  else eval_call_func name argv
eval_op2 ":=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 "=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 op [l, r] = eval l ++ " " ++ op ++ " " ++ eval r
eval_call_func name [] = name
eval_call_func name argv = name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_def name args body = unlines [
                            "def " ++ name ++ " " ++ (cjoin args)
                          , eval body
                          , "end"
                          ]
eval_struct name args methods = unlines [
                            "def " ++ name ++ " " ++ (cjoin args)
                          , "Struct.new " ++ (cjoin $ map (\x -> ":" ++ x) args) ++ " do"
                          , unlines $ map eval methods
                          , "end.new(" ++ (cjoin args) ++ ")"
                          , "end"
                          ]


cjoin xs = go [] xs
  where
    go acc [] = foldr (++) "" $ drop 1 $ reverse acc
    go acc (x:xs) = go (x : "," : acc) xs
--  | Call String [AST]
--  | Struct String [(String, AST)]
--  | Stmt [AST]
