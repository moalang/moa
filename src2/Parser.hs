module Parser where

import Base
import Data.Foldable (foldrM)
import Debug.Trace (trace)
import Control.Applicative ((<|>))
import Control.Monad (guard)

--( Parse AST )----------------------------------------------------------------
parse :: String -> Maybe (AST, Source)
parse s = runParser parse_top $ Source s 0 (length s) 0
parse_top = do
  stmt <- parse_top_stmt
  read_separator
  return stmt
parse_top_stmt = Stmt <$> sep_by1 parse_line read_br1
parse_line = parse_type <|> parse_func <|> parse_exp
parse_type = do
  kind <- string "enum " <|> string "struct "
  name <- read_id
  many (read_spaces1 >> read_type) -- drop type info
  string ":\n"
  case kind of
    "enum " -> parse_enum name
    "struct " -> parse_struct name
parse_enum name = go
  where
    go = do
      tags <- sep_by1 tag br
      return $ Enum name tags
    tag = do
      string "  "
      name <- read_id
      fields <- sep_by (parse_field) (char ',')
      return (name, fields)
parse_struct name = block $ do
  fields <- sep_by1 (indent parse_field) br
  methods <- sep_by (indent parse_func) br
  return $ Struct name fields methods
parse_field = do
  read_spaces
  name <- read_id
  read_spaces1
  read_type -- drop type info
  return name
parse_func :: Parser AST
parse_func = go
  where
    go = do
      name <- read_id
      args <- many (read_spaces1 >> read_id)
      read_spaces
      char '='
      read_spaces
      body <- parse_stmt <|> parse_exp
      funcs <- parse_func_private <|> (return [])
      return $ Def name args (merge_body body funcs)
    parse_func_private = block $ do
      char ':'
      many1 $ br >> indent (parse_func <|> parse_var)
    merge_body body [] = body
    merge_body (Stmt lines) funcs = Stmt $ funcs ++ lines
    merge_body body funcs = Stmt $ funcs ++ [body]
parse_var :: Parser AST
parse_var = do
  name <- read_id
  read_spaces1
  type_ <- read_type
  check_or_eof (== '\n')
  return $ Var name type_
parse_stmt = block $ Stmt <$> many1 (br >> indent parse_line)
parse_exp :: Parser AST
parse_exp = go
  where
    go = do
      read_spaces
      exp <- body
      branch exp
    body = do
      left <- parse_unit
      (exp_op_remaining left) <|> (return left)
    branch exp = do
      conds <- many cond
      return $ if conds == []
        then exp
        else Branch exp conds
    cond = do
      br
      read_spaces
      string "| "
      m <- (TypeMatcher <$> read_id) <|> (ValueMatcher <$> body)
      string " -> "
      b <- body
      return (m, b)
    exp_op_remaining left = do
      read_spaces
      op <- read_op
      read_spaces
      right <- body
      return $ Call op [left, right]

parse_unit :: Parser AST
parse_unit = go
  where
    go = do
      u <- unit
      remain u
    unit = lambda <|> parenthesis <|> parse_array <|> parse_string <|> parse_int <|> parse_bool <|> parse_call
    remain u = remain_dot u <|> remain_apply u
    remain_dot u = do
      char '.'
      name <- read_ref
      argv <- parse_argv
      let obj = Method u name argv
      remain obj <|> return obj
    remain_apply u = do
      argv <- parse_argv
      return $ if length argv == 0 then u else Apply u argv
    lambda = do
      char '('
      arg <- read_id
      string " => "
      exp <- parse_exp
      char ')'
      return $ Def "_" [arg] exp
    parenthesis = between_char '(' ')' (Parenthesis <$> op1_or_exp)
    op1_or_exp = op1 <|> parse_exp
    op1 = do
      char '-'
      x <- parse_exp
      return $ Call "*" [I64 (-1), x]
    parse_array = Array <$> between_char '[' ']' (many parse_exp)
    parse_string = String <$> (
                       between_char '"' '"' (many $ satisfy (\c -> c /= '"'))
                   <|> between_char '`' '`' (many $ satisfy (\c -> c /= '`'))
                   )
    parse_int = do
      s <- many1 (satisfy ((flip elem) "0123456789"))
      return $ I64 (read s :: Int)
    parse_bool = do
      s <- string "true" <|> string "false"
      return $ Bool (s == "true")
    parse_call = do
      name <- read_id
      argv <- parse_argv
      return $ Call name argv

parse_argv = between_char '(' ')' (sep_by parse_unit read_spaces1) <|> (return [])

--( Read String )---------------------------------------------------------------
read_op :: Parser String
read_op = op2 <|> op1
  where
    op1 = satisfy ((flip elem) "+-*/%") >>= \c -> return [c]
    op2 = try_op2 all_parse_ops
    try_op2 [] = Parser $ \_ -> Nothing
    try_op2 (x:xs) = (string x) <|> try_op2 xs
read_id :: Parser String
read_id = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_0123456789")
  return $ xs ++ ys
read_type :: Parser String
read_type = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_0123456789( )")
  return $ xs ++ ys
read_ref :: Parser String
read_ref = do
  xs <- many1 $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_")
  ys <- many $ satisfy ((flip elem) "abcdefghijklmnopqrstuvwxyz_.0123456789")
  return $ xs ++ ys
read_spaces :: Parser String
read_spaces = many $ satisfy ((flip elem) "\t ")
read_spaces1 :: Parser String
read_spaces1 = many1 $ satisfy ((flip elem) "\t ")
read_separator :: Parser String
read_separator = many $ satisfy ((flip elem) "\n\t ")
read_br1 :: Parser ()
read_br1 = do
  read_spaces
  br
  many $ satisfy ((flip elem) "\n\t ")
  return ()

--( Parser combinators )--------------------------------------------------------
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> do
  guard $ pos s < len s
  let c = (src s) !! (pos s)
  guard $ f c
  return (c, s { pos = 1 + pos s })
check_or_eof :: (Char -> Bool) -> Parser ()
check_or_eof f = Parser $ \s ->
  if (pos s >= len s) || f ((src s) !! (pos s))
  then return ((), s)
  else Nothing
many :: Parser a -> Parser [a]
many f = many_acc f []
many1 :: Parser a -> Parser [a]
many1 f = do
  x <- f
  xs <- many f
  return $ x : xs
many_acc :: Parser a -> [a] -> Parser [a]
many_acc f acc = (do
  x <- f
  many_acc f (x : acc)
  ) <|> (return $ reverse acc)
sep_by :: Parser a -> Parser b -> Parser [a]
sep_by body sep = (sep_by1 body sep) <|> return []
sep_by1 :: Parser a -> Parser b -> Parser [a]
sep_by1 body sep = do
  x <- body
  xs <- many (sep >> body)
  return $ x : xs
char :: Char -> Parser Char
char c = satisfy (== c)
string :: String -> Parser String
string target = Parser $ \s -> do
  guard $ target == take (length target) (drop (pos s) (src s))
  return (target, s { pos = length target + pos s })
between_char l r c = do
  char l
  v <- c <|> (die $ "failed in between on the center " ++ [l])
  char r <|> (die $ "faield in between on the right " ++ [l])
  return $ v

-- const
br = char '\n'

-- indent
block f = do
  Parser $ \s -> return (nest s, s { nest = 1 + nest s })
  n <- Parser $ \s -> return (nest s, s)
  v <- f
  Parser $ \s -> return (v, s { nest = -1 + nest s })
indent f = do
  n <- Parser $ \s -> return (nest s, s)
  string $ (take (n * 2) $ repeat ' ')
  v <- f
  check_or_eof (== '\n')
  return v

-- debug
die msg = debug "" >> error msg
debug msg = Parser $ \s -> trace (msg ++ "\nremain:" ++ (show $ drop (pos s) (src s))) (return ((), s))
