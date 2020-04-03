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
  Apply ast argv -> eval_apply ast argv
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
    run_op2 "=" [Call name [], r] = name ++ " = " ++ eval r
    run_op2 "++" [l, r] = run_op2 "+" [l, r]
    run_op2 op [l, r] = eval l ++ " " ++ op ++ " " ++ eval r
    run_func name [] = name
    run_func name argv = name ++ "(" ++ (cjoin $ map eval argv) ++ ")"

eval_apply (Def name args body) argv = (unlines $ zipWith (\k v -> k ++ " = " ++ eval v) args argv) ++ eval body
eval_apply (Call _ [def]) argv = eval_apply def argv
eval_apply x argv = error $ show x ++ "\n" ++ show argv
eval_method obj name argv = eval obj ++ "." ++ name ++ "(" ++ (cjoin $ map eval argv) ++ ")"
eval_line (Def name args body) = eval_def name args body
eval_line s = "_v = (" ++ (eval s) ++ ").run!\nif _v.err? then return _v else _v end"
eval_def name args body = go body
  where
    go (Stmt lines) = unlines [
        "_method(:" ++ name ++ ") do |" ++ (cjoin args) ++ "|"
      , "  lambda do"
      , unlines $ map (eval_line . call) lines
      , "  end"
      , "end"
      ]
    go _ = unlines [
      "_method(:" ++ name ++ ") do |" ++ (cjoin args) ++ "|"
      , eval $ call body
      , "end"
      ]
    call c@(Call _ []) = c
    call (Call fname argv)
      | elem fname args = Call (fname ++ ".call") argv
      | otherwise = Call fname argv
    call (Stmt lines) = Stmt $ map call lines
    call (Branch target conds) = Branch (call target) (map (\(c, v) -> (c, (call v))) conds)
    call x = x
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
      , "  target = target.__call"
      , "  conds.each do |(cond, body)|"
      , "    return body.__call if moa_branch_eq(target, cond)"
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
      , "class String"
      , "  def has(x)"
      , "    include?(x)"
      , "  end"
      , "end"
      , "class Proc"
      , "  def or(x)"
      , "    p = self"
      , "    lambda { |*args| p.call(*args).or(x) }"
      , "  end"
      , "end"
      , "class Object"
      , "  def or(_)"
      , "    self"
      , "  end"
      , "  def err?"
      , "    instance_of?(MoaError)"
      , "  end"
      , "  def __call"
      , "    instance_of?(Proc) ? self.call.__call : self"
      , "  end"
      , "  def run!"
      , "    v = __call"
      , "    v.instance_of?(Method) ? v.call.__call : v"
      , "  end"
      , "  def to_s"
      , "    self.to_s"
      , "  end"
      , "  def to_i"
      , "    self.to_int"
      , "  end"
      , "end"
      ]
