module Compiler (compile) where

import Base
import Debug.Trace (trace)

compile :: AST -> String
compile top = unlines [
                eval top
              , "main.run!"
              ]

eval ast = case ast of
  -- simple value
  Void -> "()"
  I64 v -> show v
  Bool b -> if b then "true" else "false"
  String s -> quote_string s
  -- container
  Array a -> "[" ++ (cjoin $ map eval a) ++ "]"
  Struct name args methods -> eval_struct name args methods
  Enum name enums -> eval_enum name enums
  -- variable
  Var name kind -> eval_var name kind
  -- define and call
  Def name args body -> eval_def name args body
  Call name argv -> eval_call name argv
  Apply ast argv -> eval_apply ast argv
  Method obj name argv -> eval_method obj name argv
  -- parenthesis
  Parenthesis exp -> "(" ++ eval exp ++ ")"
  -- statement
  Stmt lines -> unlines $ local_variables lines : map eval_line lines
  -- branch
  Branch target conds -> eval_branch target conds
eval_var name kind = name ++ " = " ++ to_value(kind)
  where
    to_value "int" = "0"
    to_value "string" = ""
    to_value "array" = "[]"
    to_value "array(int)" = "[]"
    to_value "array(string)" = "[]"
    to_value "array(bool)" = "[]"
eval_call name argv = go
  where
    go = if elem name all_parse_ops
      then run_op2 name argv
      else run_func name argv
    run_op2 "<-" [Call name [], r] = name ++ " = (" ++ eval r ++ ").run!"
    run_op2 ":=" [Call name [], r] = name ++ " = " ++ eval r
    run_op2 op@(_:"=") [Call name [], r] = name ++ " " ++ op ++ " " ++ eval r
    run_op2 "++" [l, r] = run_op2 "+" [l, r]
    run_op2 op [l, r] = eval l ++ " " ++ op ++ " " ++ eval r
    run_func name [] = name ++ ".__call"
    run_func name argv = name ++ ".__call(" ++ (cjoin $ map eval argv) ++ ")"

eval_apply (Def name args body) argv = (unlines $ zipWith (\k v -> k ++ " = " ++ eval v) args argv) ++ eval body
eval_apply (Call _ [def]) argv = eval_apply def argv
eval_apply x argv = error $ show x ++ "\n" ++ show argv
eval_method obj name argv = eval obj ++ "." ++ name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_line (Def name args body) = eval_def name args body
eval_line s = "_v = (" ++ (eval s) ++ ").run!\nif _v.err? then return _v else _v end"
eval_def name args body = go body
  where
    go (Stmt lines) = unlines [
        name ++ " = lambda do |" ++ (cjoin args) ++ "|"
      , "  MoaStmt.new(lambda do"
      , local_variables lines
      , unlines $ map (eval_line . call) lines
      , "  end)"
      , "end"
      ]
    go _ = unlines [
        name ++ " = lambda do |" ++ (cjoin args) ++ "|"
      , eval $ call body
      , "end"
      ]
    call c@(Call _ []) = c
    call (Stmt lines) = Stmt $ map call lines
    call (Branch target conds) = Branch (call target) (map (\(c, v) -> (c, (call v))) conds)
    call x = x
eval_struct name args methods = unlines [
                            name ++ " = lambda do |" ++ (cjoin args) ++ "|"
                          , "  Struct.new " ++ (cjoin $ map (\x -> ":" ++ x) ("_tag" : args)) ++ " do"
                          , unlines $ map eval methods
                          , "  end.new(" ++ (cjoin $ (':' : name) : args) ++ ")"
                          , "end"
                          ]
eval_enum name enums = unlines $ map to_ruby enums
  where
    to_ruby (tag, fields) = eval_struct tag fields []
eval_branch target conds = "moa_branch(" ++ eval target ++ ", " ++
                           "[" ++ (join ",\n  " $ map to_ruby conds) ++ "]" ++
                           ")"
  where
    to_ruby (cond, body) = case cond of
      TypeMatcher t -> "[" ++ (to_sym t) ++ ", proc { " ++ eval body ++ " }]"
      ValueMatcher t -> "[" ++ eval t ++ ", proc { " ++ eval body ++ " }]"
    to_sym "true" = "true"
    to_sym "false" = "false"
    to_sym x = ':' : x

local_variables lines = (join "" (map local_variable lines)) ++ " nil"
local_variable (Def id _ _) = id ++ " = "
local_variable _ = ""
cjoin xs = join "," xs
join glue xs = go [] xs
  where
    go acc [] = foldr (++) "" $ drop 1 $ reverse acc
    go acc (x:xs) = go (x : glue : acc) xs
