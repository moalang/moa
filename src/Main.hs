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
  let (Seq list1) = parse src
  let list2 = list1 ++ [Apply (Ref "compile") [String src]]
  case eval (Seq list2) of
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
      read "counter: count var(int), incr = count += 1"
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
      stmt "3" "counter: count var(int), incr = count += 1, twice = incr; incr\ncounter(1).twice"
      stmt "5" "counter: count var(int), incr = count += 1, twice = a <- incr; b <- incr; a + b\ncounter(1).twice"
      stmt "14" "counter:\n  count var(int)\n  incr a = count += a\n  twice =\n    b <- incr(1)\n    c <- incr(1)\n    b + c\n  quad =\n    d <- twice\n    e <- twice\n    d + e\ncounter(1).quad"
      stmt "[\"1\" \"2\"]" "\"12\".to_array"
      stmt "3" "\"1\".to_int + 2"
      stmt "\"0\"" "0.to_string"
      stmt "[1 2]" "[1] ++ [2]"
      stmt "[1 2]" "f x = x < 2\n| x + 1\n| false.guard(0)\nloop x acc = y <- f(x)\n| acc\n| loop(y acc ++ [y])\nloop(0 [])"
      stmt "3" "s: n var(0), inc = n += 1, b1 = b2; b2, b2 = v <- inc\n| 9\n| n\ns(1).b1"
      stmt "3" "s: n var(0), inc = n += 1, calc = twice(inc)\ntwice f = f; f\ns(1).calc"
      stmt "Error: 1" "f = false.guard(1)"
      stmt "Error: 1" "f = false.guard(1); 1"
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
    eq a b src = putStrLn $ if a == to_string b
      then "ok: " ++ oneline a ""
      else "EXPECT: " ++ a ++
           "\nFACT  : " ++ to_string b ++ " # " ++ (show b) ++
           "\nAST   : " ++ (show $ parse src) ++
           "\nSRC   : " ++ (src)
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
  | Assign String AST -- name <- exp
  | Modify String AST -- name := exp
  | Seq [AST]         -- exp; exp
  | Eff AST           -- exp could mutate variables
  | Fork AST Branches -- target, branches
  | Update Env AST    -- contains mutated variables
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
    go (Modify name body) = name ++ " := " ++ go body
    go (Member ast member []) = go ast ++ "." ++ member
    go (Member ast member args) = go ast ++ "." ++ member ++ "(" ++ (join_string " " $ map go args) ++ ")"
    go (Apply self args) = go self ++ "(" ++ (join_string " " $ map go args) ++ ")"
    go (Seq xs) = join_exps xs "" ""
    go (Eff x) = to_string x
    go (Instance name []) = name
    go (Instance name xs) = name ++ "(" ++ (env_string xs) ++ ")"
    go (Void) = "()"
    go (Error message) = "Error: " ++ message
    go (Fork target branches) = (go target) ++ foldr show_branch "" branches
    go e = error $ show e
    join_exps [] _ acc = acc
    join_exps (x@(Def name ast):xs) glue acc = join_exps xs "\n" (acc ++ glue ++ to_string x)
    join_exps (x:xs) glue acc = join_exps xs "; " (acc ++ glue ++ to_string x)
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
    parse_lines = make_seq <$> sepBy (read_br) parse_line
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
      return $ make_class name attrs methods
    parse_enum name = Enum name <$> read_enums1 (name ++ ".")
    parse_exp_or_fork = do
      exp <- parse_exp
      (parse_fork exp) `or` (return exp)
    parse_fork exp = Fork exp <$> (many1 parse_switch) `or` (parse_branch exp)
    parse_branch (Assign _ _) = parse_branch_errors
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
    parse_eff = make_seq <$> (
                  (sepBy1 (read_char ';') parse_exp) `or`
                  (many1 (read_indent >> parse_line))
                  )
    parse_exp = parse_assign `or` parse_op
    parse_assign = do
      id <- read_id
      read_string "<-"
      Assign id <$> parse_op
    parse_op = do
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
    make_seq [x] = x
    make_seq xs = Seq xs
    make_class name attrs methods = Class name attrs (if any is_variable attrs
      then map (\(k, v) -> (k, Eff v)) methods
      else methods)
    is_variable (_, Type (Apply (Ref "var") _)) = True
    is_variable x = False

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
eval root = unwrap $ unify [] root
  where
    unwrap x = case x of
      (Update _ body) -> unwrap body
      (Def _ body) -> unwrap body
      (Seq body) -> error $ show body
      (Eff body) -> unwrap $ unify [] body
      _ -> x

unify env value = switch value
  where
    switch value = case value of
      (Def _ body) -> switch body
      (Ref name) -> switch $ find name env
      (Member (Ref "log") name argv) -> trace ("- " ++ name ++ ": " ++ (show $ map switch argv)) Void
      (Member target name argv) -> run_member (switch target) name (map switch argv)
      (Apply (Ref "if") [a, b, c]) -> buildin_if (switch a) b c
      (Apply target argv) -> run_apply [] (switch target) argv
      (Array xs) -> Array $ map switch xs
      (Fork raw_target branches) -> run_fork env (switch raw_target) branches
      (Seq xs) -> run_seq [] [] xs
      (Op2 "+=" l@(Ref name) r) -> Modify name $ Op2 "+" l r
      (Op2 "-=" l@(Ref name) r) -> Modify name $ Op2 "-" l r
      (Op2 "*=" l@(Ref name) r) -> Modify name $ Op2 "*" l r
      (Op2 "//=" l@(Ref name) r) -> Modify name $ Op2 "//" l r
      (Op2 op l r) -> run_op2 op l r
      (Assign name right) -> case switch right of
        x@(Error _) -> Update [(name, x)] x
        (Update diff x) -> Update ((name, x) : diff) x
        x -> Update [(name, x)] x
      x -> x
    run_seq :: Env -> Env -> [AST] -> AST
    run_seq local eff [] = Update eff (snd $ head local)
    run_seq local eff (value:remain) = go value
      where
        go (Def name body)                 = run_seq ((name, u body) : local) eff remain
        go (Fork (Assign name x) branches) = next name $ run_fork scope (effect x) branches
        go (Assign name right)             = next name $ effect right
        go x                               = next "_" $ effect x
        effect x = case u x of
          (Eff (Seq xs)) -> run_seq local eff xs
          (Eff x) -> run_seq local eff [x]
          x -> x
        next name value = case value of
          x@(Update _ (Error _)) -> x
          x@(Error _)            -> Update (eff ++ local) x
          (Update diff body)     -> run_seq ((name, u body) : local) (diff ++ eff) remain
          (Modify field body)    -> run_seq ((name, u body) : local) ((field, u body) : eff) remain
          body                   -> run_seq ((name, u body) : local) eff remain
        scope = eff ++ local ++ env
        u x = unify scope x
    run_fork env (Update diff1 body) branches = case run_fork (diff1 ++ env) body branches of
      (Update diff2 body) -> Update (diff1 ++ diff2) body
      body -> Update diff1 body
    run_fork env target branches = match target branches
      where
        match _ [] = error $ "Does not match target=" ++ show target ++ " branches=" ++ show branches
        match _ (((Ref "_"), body):_) = unify env body
        match (Error _) (((Error _), body):_) = unify env body
        match _ ((cond, body):xs) = if target == cond then unify env body else match target xs
    run_member target name argv = go target name argv
      where
        go (String s) "to_array" [] = Array $ map (\x -> String [x]) s
        go (String s) "to_int" [] = Int (read s :: Int)
        go (Int s) "to_string" [] = String (show s)
        go (Array xs) "include" [x] = Bool (elem x $ map switch xs)
        go (Array xs) "join" [(String x)] = String (buildin_join x xs)
        go (Instance _ env2) name argv = run_apply env2 (find name env2) argv
        go (Enum _ env2) name argv = run_apply env2 (find name env2) argv
        go (Func args body) _ argv = unify ((zip args $ map switch argv) ++ env) body
        go (Bool True) "guard" [_] = Void
        go (Bool False) "guard" [x] = Error $ to_string x
        go x name argv = error $ "Unexpected member " ++ name ++ " of " ++ show x ++ " with " ++ show argv
    run_apply extra target raw_argv = go target argv
      where
        argv = map switch raw_argv
        go (Func args body) _ = unify ((zip args argv) ++ extra ++ env) body
        go (Class name attrs methods) _ = Instance name ((zip (map fst attrs) argv) ++ methods)
        go (String s) [Int x] = if x < length s then String $ [s !! x] else Error "out of index"
        go (Eff (Func args body)) _ = Eff (Seq $ (zipWith Def args argv) ++ [body])
        go (Eff (Seq s)) [] = Eff (Seq $ map (\(k, v) -> Def k v) extra ++ s)
        go (Eff x) [] = Eff (Seq $ map (\(k, v) -> Def k v) extra ++ [x])
        go (Eff _) _ = error "Invalid eff pattern"
        go x [] = unify (extra ++ env) x
        x = error $ "Unexpected applying " ++ show target
    run_op2 op left right = go op (switch left) (switch right)
      where
        go "++" (Array l) (Array r) = Array $ l ++ r
        go "+"  (Int l)   (Int r)   = Int $ l + r
        go "-"  (Int l)   (Int r)   = Int $ l - r
        go "*"  (Int l)   (Int r)   = Int $ l * r
        go "//" (Int l)   (Int r)   = Int $ l `div` r
        go ">"  (Int l)   (Int r)   = Bool $ l > r
        go ">=" (Int l)   (Int r)   = Bool $ l >= r
        go "<"  (Int l)   (Int r)   = Bool $ l < r
        go "<=" (Int l)   (Int r)   = Bool $ l <= r
        go "==" (Int l)   (Int r)   = Bool $ l == r
        go _    _         _         = error $ "op2: " ++ show op
    find :: String -> Env -> AST
    find k kvs = case lookup k kvs of
      Nothing -> error $ "not found " ++ k ++ " in " ++ join_string ", " (map fst kvs)
      Just x -> x
    buildin_if (Bool True) x _ = switch x
    buildin_if (Bool False) _ x = switch x
    buildin_if x _ _ = error $ "Invalid argument " ++ show x ++ " in build-in `if`"
    buildin_join glue params = join_string glue $ filter_string params
    filter_string [] = []
    filter_string ((String x):xs) = x : filter_string xs
    filter_string ((x:_)) = error $ "Not string type " ++ show x

debug x = trace ("# " ++ show x) x
debug1 x y = trace ("# " ++ show x ++ "\n- " ++ show y) y
debug2 x y z = trace ("# " ++ show x ++ "\n- " ++ show y ++ "\n- " ++ show z) z
