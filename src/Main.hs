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
    ["repl"] -> repl
    ["test"] -> test
    ["compile"] -> compile
    _ -> help

help :: IO ()
help = do
  putStrLn "Usage: runghc Main.hs [command]"
  putStrLn "The commands are:"
  putStrLn "\trepl\tstart repl"
  putStrLn "\ttest\ttest itself"
  putStrLn "\tgo\tcompile to go"

compile :: IO ()
compile = do
  src <- getContents
  let code = src ++ "compile(" ++ show src ++ ")"
  let (Seq list1) = parse code
  let list2 = list1 ++ [Apply (Ref "compile") [String src]]
  case eval (Seq list2) of
    (String s) -> putStrLn s
    x -> print x

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
      read "f = 1; 2"
      read "f = 1\ng = f\ng"
      read "f = 1\ng = f; f\ng"
      read "f = \"hello\""
      read "add a b = a + b"
      read "vector1: x int\nvector1(1)"
      read "table: values tuple(string int).array"
      read "counter: count int, incr = count += 1"
      read "a.b"
      read "a.b.c(1).d(2 e.f(3))"
      read $ string_join "\n| " ["num", "1 = a", "2 = b", "_ = c"]
      stmt "5" "2 + 3"
      stmt "-1" "2 - 3"
      stmt "6" "2 * 3"
      stmt "2" "7 // 3"
      stmt "2" "a = 1; 2"
      stmt "3" "add a b = a + b\nadd(1 2)"
      stmt "1" "v = 1\nf x = x\nf(v)"
      stmt "[1 2 3]" "[1 1+1 3]"
      stmt "3" "vector2: x int, y int\nv = vector2(1 2)\nv.x + v.y"
      stmt "3" "vector2: x int, y int, sum = x + y\nvector2(1 2).sum"
      stmt "3" "vector2: x int\n  y int\n  sum = x + y\nvector2(1 2).sum"
      stmt "ab.a" "ab: a | b\nab.a"
      stmt "ab.b" "ab: a | b\nab.b"
      stmt "ab.b" "ab:\n| a\n| b\nab.b"
      stmt "10" "a = 1\na\n| 1 = 10\n| _ = 20"
      stmt "20" "a = 2\na\n| 1 = 10\n| _ = 20"
      stmt "3" "counter: count 0, incr = count += 1, twice = incr; incr\ncounter(1).twice"
      putStrLn "done"
    read expr = eq expr (parse expr) expr
    stmt expect expr = eq expect (eval $ parse expr) expr
    eq a b src = putStrLn $ if a == to_string b then "ok: " ++ oneline a "" else "EXPECT: " ++ a ++ "\nFACT  : " ++ to_string b ++ "\nAST   : " ++ (show b) ++ "\nSRC   : " ++ (src)
    oneline "" acc = reverse acc
    oneline ('\n':xs) acc = oneline xs ('n' : '\\' : acc)
    oneline (x:xs) acc = oneline xs (x : acc)

run :: String -> String
run = to_string . eval . parse

-- Parser and Evaluator
data Type = Type { tname :: String, args :: [Type] } deriving (Show, Eq)
type Env = [(String, AST)]
type Attrs = [(String, Type)]
type Branches = [(AST, AST)]
data AST = Void
  | Int Int
  | String String
  | Bool Bool
  | Func [String] AST -- captures, arguments, body
  | Array [AST]
  | Def String AST
  | Class String Attrs Env -- type name, attributes, methods
  | Instance String Env -- type name, attributes + methods
  | Enum String Env -- type name, members
  | Op2 String AST AST
  | Ref String
  | Member AST String
  | Apply AST [AST]
  | Seq [AST]
  | Fork AST Branches -- target, branches
  deriving (Show, Eq)

string_join glue [] = ""
string_join glue xs = drop (length glue) $ foldr (\l r -> r ++ glue ++ l) "" (reverse xs)

to_string x = go x
  where
    go (Int n) = show n
    go (String s) = show s
    go (Bool True) = "true"
    go (Bool False) = "false"
    go (Func args body) = (string_join "," args) ++ " -> " ++ (go body)
    go (Array xs) = "[" ++ string_join " " (map go xs) ++ "]"
    go (Def name x@(Class _ _ _)) = name ++ ": " ++ def_string x
    go (Def name x@(Enum _ attrs)) = name ++ ": " ++ enum_string attrs
    go (Def name x@(Func args body)) = name ++ " " ++ (string_join " " args) ++ " = " ++ to_string body
    go (Def name x) = name ++ " = " ++ go x
    go (Class name _ _) = name
    go (Enum name _) = name
    go (Op2 op l r) = go l ++ " " ++ op ++ " " ++ go r
    go (Ref id) = id
    go (Member ast member) = go ast ++ "." ++ member
    go (Apply self args) = go self ++ "(" ++ (squash_strings $ map go args) ++ ")"
    go (Seq xs) = seq_join xs "" ""
      where
        seq_join [] _ acc = acc
        seq_join (x@(Def name ast):xs) glue acc = seq_join xs "\n" (acc ++ glue ++ to_string x)
        seq_join (x:xs) glue acc = seq_join xs "; " (acc ++ glue ++ to_string x)
    go (Instance name []) = name
    go (Instance name xs) = name ++ "(" ++ (env_string xs) ++ ")"
    go (Fork target branches) = (go target) ++ foldr show_branch "" branches
      where
        show_branch (cond, body) acc = "\n| " ++ go cond ++ " = " ++ go body ++ acc
    go e = error $ show e
    attrs_string attrs = string_join ", " $ (map (\(k, t) -> squash_strings [k, type_string t]) attrs)
    attrs_methods methods = squash_strings $ (map (\(k, m) -> squash_strings [", " ++ k, "= ", go m]) methods)
    def_string (Class _ attrs methods) = attrs_string attrs ++ attrs_methods methods
    enum_string xs = string_join " | " (map (\(k, x) -> squash_strings [k, def_string x]) xs)
    env_string env = string_join ", " $ map (\(k, v) -> squash_strings [k, go v]) env
    type_string t = case args t of
      [] -> tname t
      [x] -> type_string x ++ "." ++ tname t
      types -> tname t ++ "(" ++ string_join " " (map type_string types) ++ ")"
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
    parse_top = between spaces spaces parse_lines
      where
        parse_lines = make_seq <$> sepBy (read_br) parse_line
        parse_line = parse_def `or` parse_exp_or_fork
        parse_def = do
          id <- read_id
          args <- read_args
          mark <- read_any ":="
          body <- case mark of
            '=' -> parse_seq
            ':' -> (parse_enum id) `or` (parse_class id) `or` (die $ "invalid definition in parse_def for " ++ id)
          return $ Def id (make_func args body)
        parse_class name = do
          attrs <- read_attrs1
          methods <- read_methods
          return $ Class name attrs methods
        parse_enum name = Enum name <$> read_enums1 (name ++ ".")
        parse_exp_or_fork = do
          exp <- parse_exp
          (parse_fork exp) `or` (return exp)
        parse_fork exp = Fork exp <$> many1 parse_branch
        parse_branch = do
          satisfy (== '\n')
          satisfy (== '|')
          cond <- parse_unit
          read_char '='
          body <- parse_seq
          return (cond, body)
    parse_seq = make_seq <$> sepBy1 (read_char ';') parse_exp
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
                 parse_apply
      where
        parse_apply :: Parser AST
        parse_apply = parse_ref >>= go
          where
            go :: AST -> Parser AST
            go node = option node $ _go node
            _go :: AST -> Parser AST
            _go node = do
              mark <- satisfy $ \x -> elem x ".("
              case mark of
                '.' -> (Member node <$> get_id) >>= go
                '(' -> do
                  args <- many parse_exp
                  read_char ')'
                  go $ make_apply node args
        parse_ref = Ref <$> read_id
        parse_bool = Bool <$> fmap (== "true") (read_strings ["true", "false"])
        parse_int = Int <$> fmap read read_int
        parse_string = String <$> between (read_char '"') (char '"') (many $ satisfy (/= '"'))
        parse_array = Array <$> between (read_char '[') (char ']') (many parse_exp)
        parse_closure = do
          args <- read_args
          read_string "->"
          body <- parse_exp
          return $ Func args body

    make_func [] body = body
    make_func args body = Func args body
    make_apply node [] = node
    make_apply node args = Apply node args
    make_seq [x] = x
    make_seq xs = Seq xs

    read_enums1 prefix = go
      where
        go = enum_lines `or` enum_line
        enum_lines = many1 (read_string "\n|" >> read_enum prefix)
        enum_line = sepBy2 (read_char '|') $ read_enum prefix
        read_enum prefix = do
          k <- read_id
          v <- read_attrs
          return (k, Class (prefix ++ k) v [])
    read_attrs = sepBy read_sep_member read_kv
    read_attrs1 = sepBy1 read_sep_member read_kv
    read_kv = do
      k <- read_id
      v <- read_type
      return (k, v)
    read_methods = many $ do
      read_sep_member
      id <- read_id
      args <- read_args
      read_char '='
      body <- parse_seq
      return (id, make_func args body)
    read_args = many read_id
    read_type = read_type_nest []
    read_type_nest vargs = do
      id <- read_id
      args <- option [] $ between (char '(') (read_char ')') $ sepBy (many1 $ char ' ') read_type
      let t = Type id (vargs ++ args)
      option t $ do
          char '.'
          read_type_nest [t]
    read_id = lex get_id
    read_ids1 = sepBy1 (satisfy (== '.')) read_id
    read_int = lex $ many1 $ get_any "0123456789"
    read_strings (x:xs) = foldr or (read_string x) (map read_string xs)
    read_string s = lex $ mapM_ (\x -> satisfy (== x)) s >> return s
    read_char c = lex $ satisfy (== c)
    read_op = read_strings [
              "==", "!=", ">=", "<=", ">", "<",
              "+=", "-=", "*=", "//=",
              "+", "-", "*", "//"]
    read_sep_member = read_strings [",", "\n  "]
    read_br = read_strings [";", ",", "\n"]
    read_any s = lex $ get_any s
    get_any s = satisfy (\x -> elem x s)
    get_id = many1 $ get_any "abcdefghijklmnopqrstuvwxyz0123456789_"

    option alt main = main `or` (return alt)
    or l r = do
      s <- get
      l <|> (put s >> r)

    lex f = (many $ satisfy (== ' ')) >> f
    indent f = do
      read_string "  "
      v <- f
      read_string "\n"
      return v
    spaces = many $ read_any "\n\t "
    sepBy sep f = (sepBy1 sep f) `or` (return [])
    sepBy1 sep f = do
      x <- f
      xs <- many (sep >> f)
      return $ x : xs
    sepBy2 sep f = do
      x <- f
      xs <- many1 (sep >> f)
      return $ x : xs
    char c = satisfy (== c)
    between l r m = do
      l
      v <- m -- `or` (die $ "missing body in " ++ show l ++ show r)
      r `or` (die $ "Does not close in between")
      return v
    many1 f = do
      x <- f
      xs <- many f
      return $ x : xs
    many f = go []
      where
        go acc = (next acc) `or` (return $ reverse acc)
        next acc = do
          x <- f
          go (x : acc)
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = do
      s <- get
      guard $ (pos s) < (length $ src s)
      let c = (src s) !! (pos s)
      guard $ f c
      put (s { pos = (pos s) + 1 })
      return c
    see :: Parser String
    see = do
      s <- get
      return $ if (pos s) < (length $ src s)
        then [(src s) !! (pos s)]
        else ""
    die message = trace message (return ()) >> dump >> error message
    dump :: Parser ()
    dump = do
      s <- get
      trace ("die: " ++ show s ++ " @ " ++ (show $ drop (pos s) (src s))) (return ())


-- Evaluator
eval :: AST -> AST
eval root = go [] root
  where
    go :: Env -> AST -> AST
    go env (Seq xs) = run_seq env xs
      where
        run_seq :: Env -> [AST] -> AST
        run_seq env [] = snd $ head env
        run_seq env ((Def name body):ys) = run_seq ((name, go env body) : env) ys
        run_seq env (y:ys) = case go env y of
          (Def name body) -> run_seq ((name, body) : env) ys
          body -> run_seq (("_", body): env) ys
    go env (Def _ body) = go env body
    go env (Ref name) = go env $ find name env
    go env (Member target name) = case go env target of
      (Instance _ env2) -> bind (env2 ++ env) name env2
      (Enum _ env2) -> bind (env2 ++ env) name env2
    go env (Apply target raw_argv) = go_apply
      where
        argv = map (go env) raw_argv
        go_apply = case go env target of
          (Func args body) -> go ((zip args $ map (go env) argv) ++ env) body
          (Class name attrs methods) -> Instance name ((zip (map fst attrs) argv) ++ methods)
    go env (Array xs) = Array $ map (go env) xs
    go env (Op2 op left right) = case (op, go env left, go env right) of
      ("+=", (Int l), (Int r)) -> update left (Int $ l + r)
      ("-=", (Int l), (Int r)) -> update left (Int $ l - r)
      ("*=", (Int l), (Int r)) -> update left (Int $ l * r)
      ("//=", (Int l), (Int r)) -> update left (Int $ l `div` r)
      ("+", (Int l), (Int r)) -> Int $ l + r
      ("-", (Int l), (Int r)) -> Int $ l - r
      ("*", (Int l), (Int r)) -> Int $ l * r
      ("//", (Int l), (Int r)) -> Int $ l `div` r
      x -> error $ "op2: " ++ show x
    go env (Fork raw_target branches) = match branches
      where
        target = go env raw_target
        match [] = error $ "Does not match target=" ++ show target ++ " branches=" ++ show branches
        match (((Ref "_"), body):_) = body
        match ((cond, body):xs) = if target == cond then body else match xs
    go _ x = x
    update (Ref name) ast = Def name ast
    find :: String -> Env -> AST
    find k kvs = case lookup k kvs of
      Nothing -> error $ "not found " ++ k ++ " in " ++ string_join ", " (map fst kvs)
      Just x -> x
    bind env k kvs = case go env $ find k kvs of
      (Func args body) -> Func args $ Apply (Func (map fst env) body) (map snd env)
      x -> x
