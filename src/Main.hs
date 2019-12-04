module Main where

import Debug.Trace (trace)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (unless, guard)
import Control.Monad.State (StateT, runStateT, lift, get, put)
import System.Environment (getArgs)
import System.IO (isEOF)

-- Entry point
main = do
  args <- getArgs
  case args of
    ["test"] -> test
    (expr:_) -> putStrLn $ run expr
    _ -> repl

repl :: IO ()
repl = do
  putStr "> "
  v <- isEOF
  unless v $ do
    cmd <- getLine
    case cmd of
      "quit" -> return ()
      "exit" -> return ()
      "q" -> return ()
      _ -> do
        putStrLn $ run cmd
        repl

test :: IO ()
test = go
  where
    go = do
      read "true"
      read "false"
      read "1"
      read "123"
      read "\"hello\""
      read "a -> 1"
      read "[]"
      read "[\"hello\"]"
      read "[true false]"
      read "[1 2 3]"
      read "vector2: x int, y int"
      read "bool: true | false"
      read "name"
      read "func(1 true)"
      read "f = 1; f"
      read "vector1: x int; vector1(1)"
      stmt "5" "2 + 3"
      stmt "-1" "2 - 3"
      stmt "6" "2 * 3"
      stmt "2" "7 // 3"
      stmt "1" "a = 1; a"
      stmt "1" "a = 1; b = a; b"
      stmt "3" "add a b = a + b; add(1 2)"
      putStrLn "done"
    read expr = putStr $ eq expr (to_string $ parse expr)
    stmt expect expr = putStr $ eq expect (run expr)
    eq a b = if a == b then "." else (b)

run :: String -> String
run = to_string . eval . parse

-- Parser and Evaluator
type ENV = [(String, AST)]
type ATTRS = [(String, String)]
data AST = Void
  | Int Int
  | String String
  | Bool Bool
  | Func [String] AST -- captures, arguments, body
  | Closure ENV AST -- captures, body
  | Array [AST]
  | Def String AST
  | Struct ATTRS
  | Enum [(String, ATTRS)]
  | Op2 String AST AST
  | Ref String
  | Call AST [AST]
  | Seq [AST]
  deriving (Show)

string_join glue [] = ""
string_join glue xs = drop (length glue) $ foldr (\l r -> r ++ glue ++ l) "" (reverse xs)

to_string (Int n) = show n
to_string (String s) = show s
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (Func args body) = (string_join "," args) ++ " -> " ++ (to_string body)
to_string (Closure env body) = (string_join "," (map fst env)) ++ " -> " ++ (to_string body)
to_string (Array xs) = "[" ++ string_join " " (map to_string xs) ++ "]"
to_string (Def name x@(Struct _)) = name ++ ": " ++ to_string x
to_string (Def name x@(Enum _)) = name ++ ": " ++ to_string x
to_string (Def name x) = name ++ " = " ++ to_string x
to_string (Struct kts) = kts_string kts
to_string (Enum xs) = string_join " | " (map (\(k, kts) -> squash_strings [k, kts_string kts]) xs)
to_string (Op2 op l r) = squash_strings [to_string l, op, to_string r]
to_string (Ref name) = name
to_string (Call self args) = to_string self ++ "(" ++ (squash_strings $ map to_string args) ++ ")"
to_string (Seq xs) = string_join "; " $ map to_string xs
to_string e = error $ show e
kts_string kts = string_join ", " $ map (\(k, t) -> squash_strings [k, t]) kts
squash_strings :: [String] -> String
squash_strings [] = ""
squash_strings ("":zs) = squash_strings zs
squash_strings (" ":zs) = squash_strings zs
squash_strings (x:"":zs) = squash_strings (x : zs)
squash_strings (x:" ":zs) = squash_strings (x : zs)
squash_strings (x:y:zs) = x ++ " " ++ y ++ (squash_strings zs)
squash_strings [x] = x

-- Parser
data Source = Source { src :: String, pos :: Int } deriving Show
type Parser a = StateT Source Maybe a

parse :: String -> AST
parse input = go
  where
    go = case runStateT parse_top (Source input 0) of
      Nothing -> error $ "parse error: " ++ input
      Just (ast, s) -> case length (src s) == pos s of
        True -> ast
        False -> error $ "Remaining: " ++ drop (pos s) (src s)
    parse_top :: Parser AST
    parse_top = make_seq <$> sepBy (read_br) parse_line
    parse_line = parse_def `or` parse_exp
    parse_def = do
      id <- read_id
      args <- read_args
      mark <- read_any ":="
      body <- case mark of
        ':' -> parse_struct `or` parse_enum `or` (die "body in parse_def")
        '=' -> parse_exp
      return $ Def id (make_func args body)
    parse_struct = Struct <$> read_kvs1
    parse_enum = Enum <$> read_enums1
    parse_exp = do
      l <- parse_unit
      parse_op2 l `or` (return l)
    parse_op2 left = do
      op <- read_op
      right <- parse_exp
      return $ Op2 op left right
    parse_unit = parse_bool `or`
                 parse_int `or`
                 parse_string `or`
                 parse_array `or`
                 parse_closure `or`
                 parse_ref_or_call
    parse_ref_or_call = do
      id <- read_id
      args <- (between '(' ')' (many parse_unit)) `or` (return [])
      return $ make_call id args
    parse_bool = Bool <$> fmap (== "true") (read_strings ["true", "false"])
    parse_int = Int <$> fmap read read_int
    parse_string = String <$> between '"' '"' (many $ satisfy (/= '"'))
    parse_array = Array <$> between '[' ']' (many $ parse_unit)
    parse_closure = do
      args <- read_args
      read_string "->"
      body <- parse_exp
      return $ Func args body

    make_func [] body = body
    make_func args body = Func args body
    make_call id [] = Ref id
    make_call id args = Call (Ref id) args
    make_seq [x] = x
    make_seq xs = Seq xs

    read_enums1 = sepBy2 (read_char '|') read_enum
    read_enum = do
      k <- read_id
      v <- read_kvs
      return (k, v)
    read_kvs = sepBy (read_char ',') read_kv
    read_kvs1 = sepBy1 (read_char ',') read_kv
    read_kv = do
      k <- read_id
      v <- read_type
      return (k, v)
    read_args = many read_id
    read_type = read_id
    read_id = lex $ many1 $ get_any "abcdefghijklmnopqrstuvwxyz0123456789_"
    read_int = lex $ many1 $ get_any "0123456789"
    read_strings (x:xs) = foldr or (read_string x) (map read_string xs)
    read_string s = lex $ mapM_ (\x -> satisfy (== x)) s >> return s
    read_char c = lex $ satisfy (== c)
    read_op = read_strings ["+", "-", "*", "//"]
    read_br = read_strings [";", ",", "\n"]
    read_any s = lex $ get_any s
    get_any s = satisfy (\x -> elem x s)

    or l r = do
      s <- get
      l <|> (put s >> r)

    dump :: Parser ()
    dump = do
      s <- get
      trace (show s ++ " @ " ++ (show $ drop (pos s) (src s))) (return ())

    lex f = (many $ satisfy (== ' ')) >> f
    indent f = do
      read_string "  "
      v <- f
      read_string "\n"
      return v
    sepBy sep f = (sepBy1 sep f) `or` (return [])
    sepBy1 sep f = do
      x <- f
      xs <- many (sep >> f)
      return $ x : xs
    sepBy2 sep f = do
      x <- f
      xs <- many1 (sep >> f)
      return $ x : xs
    between l r m = do
      satisfy (== l)
      v <- m `or` (die $ "missing body in " ++ show l ++ show r)
      satisfy (== r) `or` (die $ "Does not close " ++ show r)
      return v
    many f = many_acc f []
    many1 f = do
      x <- f
      xs <- many f
      return $ x : xs
    many_acc f acc = (f >>= \x -> many_acc f (x : acc)) `or` (return $ reverse acc)
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = do
      s <- get
      guard $ (pos s) < (length $ src s)
      let c = (src s) !! (pos s)
      guard $ f c
      put (s { pos = (pos s) + 1 })
      return c
    die message = dump >> error message

-- Evaluator
eval :: AST -> AST
eval x = top [] x
  where
    top :: ENV -> AST -> AST
    top env (Seq xs) = go env xs
      where
        go :: ENV -> [AST] -> AST
        go env [] = snd $ head env
        go env ((Def name body):ys) = go ((name, top env body) : env) ys
        go env (y:ys) = go (("_", top env y) : env) ys
    top env x = go x
      where
        go (Ref name) = case lookup name env of
          Just x -> x
          Nothing -> error $ "not found " ++ name ++ " in " ++ string_join ", " (map fst env)
        go (Call target argv) = case go target of
          (Func args body) -> top (zip args $ map go argv) body
        go (Op2 op left right) = case (op, go left, go right) of
          ("+", (Int l), (Int r)) -> Int $ l + r
          ("-", (Int l), (Int r)) -> Int $ l - r
          ("*", (Int l), (Int r)) -> Int $ l * r
          ("//", (Int l), (Int r)) -> Int $ l `div` r
        go x = x

--eval (Op2 op l r) =
--eval (Ref name) =
--eval (Call AST args) =
--eval call@(Call _ _) = error $ "Invalid call " ++ show a
--  | Int Int
--  | String String
--  | Bool Bool
--  | Func [String] AST -- captures, arguments, body
--  | Closure ENV AST -- captures, body
--  | Array [AST]
--  | Def String AST
--  | Struct ATTRS
--  | Enum [(String, ATTRS)]
--  | Op2 String AST AST
--  | Ref String

-- TODO
--   x REPL
--   - for parser
--     x bool
--     x int
--     (skip) float
--     x string
--     x closure (func)
--     x array
--     (skip) dictionary
--     x struct
--     x enum
--     (skip) flow
--     x sequence
--     - branch
--     x binary operators
--     - embedded functions
--   - for evaluator
--     - bool
--     - int
--     (skip) float
--     - string
--     - closure
--     - array
--     - dictionary
--     - struct
--     - enum
--     - flow
--     - sequence
--     - branch
--     - binary operators
--     - embedded functions
