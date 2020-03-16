module Compiler (compile) where

import Base
import Debug.Trace (trace)

compile :: AST -> String
compile top = std ++ unlines [
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
  Enum name enums -> eval_enum name enums
  Branch target conds -> eval_branch target conds
eval_call name argv = if elem name all_parse_ops
  then eval_op2 name argv
  else eval_call_func name argv
eval_op2 ":=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 "=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 op [l, r] = eval l ++ " " ++ op ++ " " ++ eval r
eval_call_func name [] = name
eval_call_func name argv = name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_def name args body = unlines [
                            "define_method(:" ++ name ++ ") do |" ++ (cjoin args) ++ "|"
                          , eval body
                          , "end"
                          ]
eval_struct name args methods = unlines [
                            "def " ++ name ++ " " ++ (cjoin args)
                          , "Struct.new " ++ (cjoin $ map (\x -> ":" ++ x) ("_tag" : args)) ++ " do"
                          , unlines $ map eval methods
                          , "end.new(" ++ (cjoin $ (':' : name) : args) ++ ")"
                          , "end"
                          ]
eval_enum name enums = unlines $ map to_ruby enums
  where
    to_ruby (tag, fields) = eval_struct tag fields []
eval_branch target conds = "moa_branch(" ++ eval target ++ ", " ++
                           "[" ++ (cjoin $ map to_ruby conds) ++ "]" ++
                           ")"
  where
    to_ruby (cond, body) = case cond of
      TypeMatcher t -> "[:" ++ t ++ "," ++ eval body ++ "]"
      ValueMatcher t -> "[" ++ eval t ++ "," ++ eval body ++ "]"

cjoin xs = go [] xs
  where
    go acc [] = foldr (++) "" $ drop 1 $ reverse acc
    go acc (x:xs) = go (x : "," : acc) xs

std = unlines [
        "def moa_branch(target, conds)"
      , "  conds.each do |(cond, body)|"
      , "    return body if moa_branch_eq(target, cond)"
      , "  end"
      , "  raise Exception.new('Unexpected branch ' + target.inspect + ' ' + conds.inspect)"
      , "end"
      , "def moa_branch_eq(target, cond)"
      , "  if cond.instance_of?(Symbol)"
      , "    target._tag == cond"
      , "  else"
      , "    target == cond"
      , "  end"
      , "end"
      ]
