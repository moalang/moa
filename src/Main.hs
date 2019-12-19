module Main where

import Debug.Trace (trace)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (unless, guard)
import Control.Monad.State (StateT, runStateT, lift, get, put, modify)
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
  let (Eff list1) = parse src
  let list2 = list1 ++ [Apply (Ref "compile") [String src]]
  case eval (Eff list2) of
    (String s) -> putStrLn s
    x -> print x
  putStrLn "// done"

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
      read "mix: a int, add b = a + b\nmix(1).add(2)"
      read $ join_string "\n| " ["num", "1 = a", "2 = b", "_ = c"]
      stmt "()" "()"
      stmt "5" "2 + 3"
      stmt "-1" "2 - 3"
      stmt "6" "2 * 3"
      stmt "2" "7 // 3"
      stmt "2" "a = 1; 2"
      stmt "2" "3 - (2 - 1)"
      stmt "3" "add a b = a + b\nadd(1 2)"
      stmt "3" "add a b =\n  c = a + b\n  c\nadd(1 2)"
      stmt "1" "v = 1\nf x = x\nf(v)"
      stmt "[1 2 3]" "[1 1+1 3]"
      stmt "3" "vector2: x int, y int\nv = vector2(1 2)\nv.x + v.y"
      stmt "3" "vector2: x int, y int, sum = x + y\nvector2(1 2).sum"
      stmt "3" "vector2:\n  x int\n  y int\n  sum = x + y\nvector2(1 2).sum"
      stmt "ab.a" "ab: a | b\nab.a"
      stmt "ab.b" "ab: a | b\nab.b"
      stmt "ab.b" "ab:\n| a\n| b\nab.b"
      stmt "10" "a = 1\na\n| 1 = 10\n| _ = 20"
      stmt "20" "a = 2\na\n| 1 = 10\n| _ = 20"
      stmt "3" "counter: count int, incr = count += 1, twice = incr; incr\ncounter(1).twice"
      stmt "5" "counter: count int, incr = count += 1, twice = a <- incr; b <- incr; a + b\ncounter(1).twice"
      stmt "14" "counter:\n  count int\n  incr a = count += a\n  twice =\n    b <- incr(1)\n    c <- incr(1)\n    b + c\n  quad =\n    d <- twice\n    e <- twice\n    d + e\ncounter(1).quad"
      stmt "[\"1\" \"2\"]" "\"12\".to_array"
      stmt "3" "\"1\".to_int + 2"
      stmt "\"0\"" "0.to_string"
      stmt "[1 2]" "[1] ++ [2]"
      stmt "[1 2]" "f x = x < 2\n| x + 1\n| false.guard(0)\nloop x acc = y <- f(x)\n| acc\n| loop(y acc ++ [y])\nloop(0 [])"
      stmt "3" "s: n 0, inc = n += 1, b1 = b2; b2, b2 = v <- inc\n| 9\n| n\ns(1).b1"
      stmt "3" "s: n 0, inc = n += 1, calc = twice(inc)\ntwice f = log.debug(f); f; f\ns(1).calc"
      -- build-in functions
      stmt "1" "if(true 1 2)"
      stmt "2" "if(false 1 2)"
      stmt "()" "true.guard(1)"
      stmt "\"b\"" "\"ab\"(1)"
      stmt "\"b\"" "\"ab\"(1)"
      stmt "Error: 0" "false.guard(0)"
      stmt "Error: out of index" "\"\"(0)"
      stmt "true" "[1].include(1)"
      stmt "false" "[1].include(2)"
      stmt "\"a,b\"" "[\"a\" \"b\"].join(\",\")"
      stmt "()" "log.debug(1 true)"
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
type Env = [(String, AST)]
type Branches = [(AST, AST)]
data AST = Void
  | Int Int
  | String String
  | Bool Bool
  | Func [String] AST -- captures, arguments, body
  | Array [AST]
  | Def String AST
  | Class String Env Env -- type name, attributes, methods
  | Instance String Env -- type name, attributes + methods
  | Enum String Env -- type name, members
  | Op2 String AST AST
  | Ref String
  | Member AST String [AST] -- target, name, arguments
  | Apply AST [AST]
  | Eff [AST]
  | Fork AST Branches -- target, branches
  | Update Env AST
  | Type AST
  | Error String
  deriving (Show, Eq)

join_string glue [] = ""
join_string glue xs = drop (length glue) $ foldr (\l r -> r ++ glue ++ l) "" (reverse xs)

to_string x = go x
  where
    go (Int n) = show n
    go (String s) = show s
    go (Bool True) = "true"
    go (Bool False) = "false"
    go (Func args body) = (join_string "," args) ++ " -> " ++ (go body)
    go (Array xs) = "[" ++ join_string " " (map go xs) ++ "]"
    go (Def name x@(Class _ _ _)) = name ++ ": " ++ def_string x
    go (Def name x@(Enum _ attrs)) = name ++ ": " ++ enum_string attrs
    go (Def name x@(Func args body)) = name ++ " " ++ (join_string " " args) ++ " = " ++ to_string body
    go (Def name (Type x)) = name ++ " " ++ go x
    go (Def name x) = name ++ " = " ++ go x
    go (Class name _ _) = name
    go (Enum name _) = name
    go (Op2 op l r) = go l ++ " " ++ op ++ " " ++ go r
    go (Ref id) = id
    go (Member ast member []) = go ast ++ "." ++ member
    go (Member ast member args) = go ast ++ "." ++ member ++ "(" ++ (join_string " " $ map go args) ++ ")"
    go (Apply self args) = go self ++ "(" ++ (join_string " " $ map go args) ++ ")"
    go (Eff xs) = join_eff xs "" ""
    go (Instance name []) = name
    go (Instance name xs) = name ++ "(" ++ (env_string xs) ++ ")"
    go (Void) = "()"
    go (Error message) = "Error: " ++ message
    go (Fork target branches) = (go target) ++ foldr show_branch "" branches
    go e = error $ show e
    join_eff [] _ acc = acc
    join_eff (x@(Def name ast):xs) glue acc = join_eff xs "\n" (acc ++ glue ++ to_string x)
    join_eff (x:xs) glue acc = join_eff xs "; " (acc ++ glue ++ to_string x)
    show_branch (cond, body) acc = "\n| " ++ go cond ++ " = " ++ go body ++ acc
    def_string (Class _ attrs methods) = env_string $ attrs ++ methods
    enum_string xs = join_string " | " (map (\(k, x) -> k ++ def_string x) xs)
    env_string env = join_string ", " $ map (\(k, v) -> go $ Def k v) env
    type_string (Type x) = to_string x
    type_string x = to_string x

-- Parser
data Source = Source { src :: String, pos :: Int, depth :: Int } deriving Show
type Parser a = StateT Source Maybe a

parse :: String -> AST
parse input = go
  where
    go = case runStateT parse_top (Source input 0 0) of
      Nothing -> error $ "parse error: " ++ input
      Just (ast, s) -> case length (src s) == pos s of
        True -> ast
        False -> error $ unlines [
            "\n--(parse failed)----------------------------"
          , "Expect   : " ++ (show $ length (src s))
          , "Fact     : " ++ (show $ pos s)
          , "Remaining: " ++ drop (pos s) (src s)
          , "--------------------------------------------"
          ]
    parse_top :: Parser AST
    parse_top = between spaces spaces parse_lines
    parse_lines = make_eff <$> sepBy (read_br) parse_line
    parse_line = parse_def `or` parse_exp_or_fork
    parse_def = do
      id <- read_id
      args <- read_args
      mark <- read_any ":="
      body <- indent $ case mark of
        '=' -> parse_body
        ':' -> (parse_enum id) `or` (parse_class id) `or` (die $ "invalid definition in parse_def for " ++ id)
      return $ Def id (make_func args body)
    parse_class name = do
      (attrs, methods) <- read_attrs_and_methods
      return $ Class name attrs methods
    parse_enum name = Enum name <$> read_enums1 (name ++ ".")
    parse_exp_or_fork = do
      exp <- parse_exp
      (parse_fork exp) `or` (return exp)
    parse_fork exp = Fork exp <$> (many1 parse_switch) `or` (parse_branch exp)
    parse_branch (Op2 "<-" _ _) = parse_branch_errors
    parse_branch _ = parse_branch_bools
    parse_branch_bools = do
      a <- read_string "\n| " >> parse_exp
      b <- read_string "\n| " >> parse_exp
      return [(Bool True, a), (Bool False, b)]
    parse_branch_errors = do
      a <- read_string "\n| " >> parse_exp
      b <- read_string "\n| " >> parse_exp
      return [(Error "_", a), (Ref "_", b)]
    parse_switch = do
      read_string "\n| "
      cond <- parse_unit
      read_char '='
      body <- indent parse_eff
      return (cond, body)
    parse_body = (parse_exp >>= parse_fork) `or` parse_eff
    parse_eff = make_eff <$> (
                  (sepBy1 (read_char ';') parse_exp) `or`
                  (many1 (read_indent >> parse_line))
                  )
    parse_exp = do
      l <- parse_unit
      parse_op2 l `or` (return l)
    parse_op2 left = do
      op <- read_op
      right <- parse_exp
      return $ Op2 op left right
    parse_unit = parse_value >>= parse_apply
      where
        parse_value = parse_void `or`
                      parse_parentheses `or`
                      parse_bool `or`
                      parse_int `or`
                      parse_string `or`
                      parse_array `or`
                      parse_closure `or`
                      parse_ref
        parse_void = read_string "()" >> return Void
        parse_parentheses = guard_prev (\x -> elem x " =") >> between (read_char '(') (read_char ')') parse_exp
        parse_apply node = option node $ parse_follow node
        parse_follow node = do
          mark <- satisfy $ \x -> elem x ".("
          case mark of
            '.' -> do
              id <- get_id
              argv <- option [] $ between (char '(') (read_char ')') $ many parse_exp
              parse_apply $ Member node id argv
            '(' -> do
              args <- many parse_exp
              read_char ')'
              parse_apply $ make_apply node args
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
    make_eff [x] = x
    make_eff xs = Eff xs

    read_enums1 prefix = go
      where
        go = enum_lines `or` enum_line
        enum_lines = many1 (read_string "\n|" >> read_enum prefix)
        enum_line = sepBy2 (read_char '|') $ read_enum prefix
        read_enum prefix = do
          id <- read_id
          (attrs, method) <- read_attrs_and_methods
          return (id, Class (prefix ++ id) attrs method)
    read_attrs_and_methods = go
      where
        go = fmap (split [] []) read_members
        split acc1 acc2 [] = (reverse acc1, reverse acc2)
        split acc1 acc2 (x@(_, Type _):xs) = split (x : acc1) acc2 xs
        split acc1 acc2 (x:xs) = split acc1 (x : acc2) xs
        read_members = (read_indent >> sepBy read_indent read_member) `or` sepBy (read_char ',') read_member
        read_member = read_member_method `or` read_member_attr
        read_member_method = do
          id <- read_id
          args <- read_args
          read_char '='
          body <- indent parse_body
          return (id, make_func args body)
        read_member_attr = do
          id <- read_id
          t <- Type <$> parse_unit
          return (id, t)
    read_args = many read_id
    read_id = lex get_id
    read_ids1 = sepBy1 (satisfy (== '.')) read_id
    read_int = lex $ many1 $ get_any "0123456789"
    read_strings (x:xs) = foldl or (read_string x) (map read_string xs)
    read_string s = lex $ mapM_ (\x -> satisfy (== x)) s >> return s
    read_char c = lex $ satisfy (== c)
    read_op = read_strings [
              "<-",
              "==", "!=", ">=", "<=", ">", "<",
              "+=", "-=", "*=", "//=",
              "++",
              "+", "-", "*", "//"]
    read_br = read_strings [";", ",", "\n"]
    read_any s = lex $ get_any s
    read_indent = do
      s <- get
      let sp = take (2 * depth s) $ repeat ' '
      read_string $ "\n" ++ sp

    get_any s = satisfy (\x -> elem x s)
    get_id = many1 $ get_any "abcdefghijklmnopqrstuvwxyz0123456789_"

    option alt main = main `or` (return alt)
    or l r = do
      s <- get
      l <|> (put s >> r)
    lex f = (many $ satisfy (== ' ')) >> f
    indent :: Parser a -> Parser a
    indent f = do
      modify $ \s -> s { depth = depth s + 1 }
      ret <- f
      modify $ \s -> s { depth = depth s - 1 }
      return ret
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
    guard_prev :: (Char -> Bool) -> Parser ()
    guard_prev f = do
      s <- get
      let c = (src s) !! (max 0 (pos s - 2))
      let b = f c
      guard b
    see :: Parser String
    see = do
      s <- get
      return $ if (pos s) < (length $ src s)
        then [(src s) !! (pos s)]
        else ""
    tracer :: Show a => a -> Parser ()
    tracer mark = do
      s <- get
      trace ("TRACER: " ++ (show mark) ++ " " ++ drop (pos s) (src s)) (return ())
      return ()
    die message = trace message (return ()) >> dump >> error message
    dump :: Parser ()
    dump = do
      s <- get
      trace ("die: " ++ show s ++ " @ " ++ (show $ drop (pos s) (src s))) (return ())


-- Evaluator
eval :: AST -> AST
eval root = unwrap $ go [] root
  where
    unwrap x = case x of
      (Update _ body) -> body
      (Def _ body) -> body
      (Eff body) -> error $ show body
      _ -> x
    go :: Env -> AST -> AST
    go env (Op2 "<-" (Ref name) right) = case go env right of
      x@(Error _) -> Update [(name, x)] x
      (Update diff x) -> Update ((name, x) : diff) x
      x -> Update [(name, x)] x
    go env (Def _ body) = go env body
    go env (Ref name) = go env $ find name env
    go env (Member (Ref "log") name argv) = trace ("- " ++ name ++ ": " ++ (show $ go_argv env argv)) Void
    go env (Member target name argv) = run_member env (go env target) name (go_argv env argv)
    go env (Apply (Ref "if") [a, b, c]) = buildin_if env (go env a) b c
    go env (Apply target argv) = run_apply env (go env target) argv
    go env (Array xs) = Array $ map (go env) xs
    go env (Op2 "+=" l@(Ref name) r) = update name $ run_op2 env "+" l r
    go env (Op2 "-=" l@(Ref name) r) = update name $ run_op2 env "-" l r
    go env (Op2 "*=" l@(Ref name) r) = update name $ run_op2 env "*" l r
    go env (Op2 "//=" l@(Ref name) r) = update name $ run_op2 env "//" l r
    go env (Op2 op l r) = run_op2 env op l r
    go env (Fork raw_target branches) = run_fork env (go env raw_target) branches
    go env (Eff xs) = run_eff env [] xs
    go _ x = x
    go_argv env xs = map (go_pure env) xs
    go_pure env x@(Eff _) = x
    go_pure env x = go env x
    run_eff :: Env -> Env -> [AST] -> AST
    run_eff env [] [] = snd $ head env
    run_eff env eff [] = Update eff (snd $ head env)
    run_eff env eff ((Def name body):ys) = run_eff ((name, go env body) : env) eff ys
    run_eff env eff ((Op2 "<-" (Ref name) right):ys) = case go env right of
      (Update diff body) -> run_eff ((name, body) : diff ++ env) (diff ++ eff) ys
      body               -> run_eff ((name, body) : env) eff ys
    run_eff env eff ((Op2 "<-" l r):ys) = error "Invalid operation"
    run_eff env eff (y:ys) = case go env y of
      (Update diff body) -> run_eff (("_", body) : diff ++ env) (diff ++ eff) ys
      body -> run_eff (("_", body): env) eff ys
    run_fork env (Update diff1 body) branches = case run_fork (diff1 ++ env) body branches of
      (Update diff2 body) -> Update (diff1 ++ diff2) body
      body -> Update diff1 body
    run_fork env target branches = match target branches
      where
        match _ [] = error $ "Does not match target=" ++ show target ++ " branches=" ++ show branches
        match _ (((Ref "_"), body):_) = go env body
        match (Error _) (((Error _), body):_) = go env body
        match _ ((cond, body):xs) = if target == cond then go env body else match target xs
    run_member env (String s) "to_array" [] = Array $ map (\x -> String [x]) s
    run_member env (String s) "to_int" [] = Int (read s :: Int)
    run_member env (Int s) "to_string" [] = String (show s)
    run_member env (Array xs) "include" [x] = Bool (elem x $ map (go env) xs)
    run_member env (Array xs) "join" [(String x)] = String (buildin_join x xs)
    run_member env (Instance _ env2) name argv = run_apply (env2 ++ env) (find name env2) argv
    run_member env (Enum _ env2) name argv = run_apply (env2 ++ env) (find name env2) argv
    run_member env (Func args body) _ argv = go ((zip args $ go_argv env argv) ++ env) body
    run_member env (Bool True) "guard" [_] = Void
    run_member env (Bool False) "guard" [x] = Error $ to_string x
    run_member env x name argv = error $ "Unexpected member " ++ name ++ " of " ++ show x ++ " with " ++ show argv
    run_apply env target raw_argv = let argv = go_argv env raw_argv in case (target, argv) of
      ((Func args body), _) -> go ((zip args argv) ++ env) body
      ((Class name attrs methods), _) -> Instance name ((zip (map fst attrs) argv) ++ methods)
      ((String s), [Int x]) -> if x < length s then String $ [s !! x] else Error "out of index"
      (x, []) -> go env x
      x -> error $ "Unexpected applying " ++ show x
    run_op2 :: Env -> String -> AST -> AST -> AST
    run_op2 env op left right = case (op, go env left, go env right) of
      ("++", (Array l), (Array r)) -> Array $ l ++ r
      ("+", (Int l), (Int r)) -> Int $ l + r
      ("-", (Int l), (Int r)) -> Int $ l - r
      ("*", (Int l), (Int r)) -> Int $ l * r
      ("//", (Int l), (Int r)) -> Int $ l `div` r
      (">", (Int l), (Int r)) -> Bool $ l > r
      (">=", (Int l), (Int r)) -> Bool $ l >= r
      ("<", (Int l), (Int r)) -> Bool $ l < r
      ("<=", (Int l), (Int r)) -> Bool $ l <= r
      ("==", (Int l), (Int r)) -> Bool $ l == r
      x -> error $ "op2: " ++ show x
    update name body = Update [(name, body)] body
    find :: String -> Env -> AST
    find k kvs = case lookup k kvs of
      Nothing -> error $ "not found " ++ k ++ " in " ++ join_string ", " (map fst kvs)
      Just x -> x
    buildin_if env (Bool True) x _ = go env x
    buildin_if env (Bool False) _ x = go env x
    buildin_if env x _ _ = error $ "Invalid argument " ++ show x ++ " in build-in `if`"
    buildin_join glue params = join_string glue $ filter_string params
    filter_string [] = []
    filter_string ((String x):xs) = x : filter_string xs
    filter_string ((x:_)) = error $ "Not string type " ++ show x
