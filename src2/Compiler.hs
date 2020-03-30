module Compiler (compile) where

import Base
import Debug.Trace (trace)

compile :: AST -> String
compile top = std ++ unlines [
                "def _main"
              , eval top
              , "end"
              , "print _main.run!"
              ]

eval ast = case ast of
  -- simple value
  Void -> "()"
  I64 v -> show v
  Bool b -> if b then "true" else "false"
  String s -> show s
  -- container
  Array a -> '[' : (cjoin $ map eval a) ++ "]"
  Struct name args methods -> eval_struct name args methods
  Enum name enums -> eval_enum name enums
  -- variable
  Var name kind -> eval_var name kind
  -- define and call
  Def name args body -> eval_def name args body
  Call name argv -> eval_call name argv
  Method obj name argv -> eval_method obj name argv
  -- parenthesis
  Parenthesis exp -> "(" ++ eval exp ++ ")"
  -- statement
  Stmt lines -> unlines $ map eval_line lines
  -- branch
  Branch target conds -> eval_branch target conds
eval_var name kind = name ++ " = " ++ to_value(kind)
  where
    to_value "int" = "0"
    to_value "string" = ""
    to_value "array" = "[]"
eval_call name argv = if elem name all_parse_ops
  then eval_op2 name argv
  else eval_call_func name argv
eval_line s@(Def _ _ _) = eval s
eval_line s = "_v = (" ++ (eval s) ++ ").run!; if _v.err? then return _v else _v end"
eval_op2 "<-" [Call name [], r] = name ++ " = (" ++ eval r ++ ").run!"
eval_op2 ":=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 "=" [Call name [], r] = name ++ " = " ++ eval r
eval_op2 "++" [l, r] = eval_op2 "+" [l, r]
eval_op2 op [l, r] = eval l ++ " " ++ op ++ " " ++ eval r
eval_call_func name [] = name
eval_call_func name argv = name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_method obj name argv = eval obj ++ "." ++ name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_def name [] (Stmt lines) = unlines [
                            "_method(:" ++ name ++ ") do"
                          , "  lambda do"
                          , unlines $ map eval_line lines
                          , "  end"
                          , "end"
                          ]
eval_def name args body = unlines [
                            "_method(:" ++ name ++ ") do |" ++ (cjoin args) ++ "|"
                          , eval body
                          , "end"
                          ]
eval_struct name args methods = unlines [
                            "def " ++ name ++ " " ++ (cjoin args)
                          , "  Struct.new " ++ (cjoin $ map (\x -> ":" ++ x) ("_tag" : args)) ++ " do"
                          , unlines $ map eval methods
                          , "  end.new(" ++ (cjoin $ (':' : name) : args) ++ ")"
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
      TypeMatcher t -> "[" ++ (to_sym t) ++ ", proc { " ++ eval body ++ " }]"
      ValueMatcher t -> "[" ++ eval t ++ ", proc { " ++ eval body ++ " }]"
    to_sym "true" = "true"
    to_sym "false" = "false"
    to_sym x = ':' : x

cjoin xs = join "," xs
join glue xs = go [] xs
  where
    go acc [] = foldr (++) "" $ drop 1 $ reverse acc
    go acc (x:xs) = go (x : glue : acc) xs

std = unlines [
        "def moa_branch(target, conds)"
      , "  conds.each do |(cond, body)|"
      , "    return body if moa_branch_eq(target, cond)"
      , "  end"
      , "  raise Exception.new('Unexpected branch ' + target.inspect + ' ' + conds.inspect)"
      , "end"
      , "def moa_branch_eq(target, cond)"
      , "  is_moa_error = target.instance_of?(MoaError)"
      , "  return target == cond unless cond.instance_of?(Symbol)"
      , "  return !is_moa_error if cond == :ok"
      , "  return is_moa_error if cond == :err"
      , "  return target._tag == cond"
      , "end"
      , "def ok(v)"
      , "  v"
      , "end"
      , "class MoaError"
      , "  def initialize(message)"
      , "    @m = message"
      , "  end"
      , "  def to_s"
      , "    @m"
      , "  end"
      , "  def or(v)"
      , "    v"
      , "  end"
      , "end"
      , "def _method(name, &body)"
      , "  method(define_method(name, body))"
      , "end"
      , "def err(s)"
      , "  MoaError.new(s)"
      , "end"
      , "class Object"
      , "  def or(_)"
      , "    self"
      , "  end"
      , "  def err?"
      , "    instance_of?(MoaError)"
      , "  end"
      , "  def run!"
      , "    instance_of?(Proc) || instance_of?(Method) ? self.call.run! : self"
      , "  end"
      , "  def to_s"
      , "    self.to_s"
      , "  end"
      , "  def to_i"
      , "    self.to_int"
      , "  end"
      , "end"
      ]
