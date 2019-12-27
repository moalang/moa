module Main where

import Debug.Trace (trace)
import Control.Applicative ((<|>), (<$>))
import Control.Monad (unless, guard)
import Control.Monad.State (StateT, runStateT, lift, get, put, modify)
import System.Environment (getArgs)
import System.IO (isEOF)

-- Entry point
main = do
  -- value(4)
  test "1" "1"
  test "hello world" "\"hello world\""
  test "true" "true"
  test "false" "false"
  -- exp(8)
  test "3" "1 + 2"
  test "-1" "1 - 2"
  test "6" "2 * 3"
  test "4" "9 / 2"
  test "1" "a = 1; a"
  test "2" "inc a = a + 1; inc(1)"
  test "6" "add a b = a + b; add(1 2 + 3)"
  test "3" "c = 1; b n = n + c; a = b(2); a"
  test "2" "a = 1; incr = a += 1; incr; a"
  test "1" "true\n| 1\n| 2"
  test "2" "false\n| 1\n| 2"
  test "true" "1\n| 1 = true\n| 2 = false"
  test "false" "2\n| 1 = true\n| 2 = false"
  test "false" "3\n| 1 = true\n| _ = false"
  -- container(3)
  test "1" "[1 2 3](0)"
  test "2" "s class: n int\ns(2).n"
  test "3" "ab enum:\n  a x int\n  b y int\nab.a(3).x"
  test "4" "ab enum:\n  a x int\n  b y int\nab.b(4).y"
  -- error(2)
  test "error: divide by zero" "1 / 0"
  test "2" "1 / 0 | 2"
  putStrLn "done"

test expect src = go
  where
    ast = parse src
    ret = eval ast
    fact = to_string ret
    fill s = take 30 (s ++ repeat ' ')
    go = putStrLn $ if expect == fact
      then "ok: " ++ fill src ++ " == " ++ fact
      else "FAIL    : " ++ src ++
           "\n| EXPECT: " ++ expect ++
           "\n| FACT  : " ++ fact ++ " # " ++ show ret ++
           "\n| AST   : " ++ show ast

-- Parser and Evaluator
type Env = [(String, AST)]
data AST = Void
  -- value(4)
  | Int Int
  | String String
  | Bool Bool
  | Func [String] AST -- captures, arguments, body
  -- exp(8)
  | Ref String
  | Member AST String
  | Op2 String AST AST
  | Apply AST [AST]
  | Fork AST [(AST, AST)] -- target, branches
  | Seq [AST]
  | Def String AST
  | Update String AST
  -- container(4)
  | Array [AST]
  | Struct String Env
  | Class String [String]
  | Enum String Env
  -- error(2)
  | Catch AST AST
  | Error String
  deriving (Show, Eq)

ops_calculate = [
  "==", "!=", ">=", "<=", ">", "<",
  "++",
  "+", "-", "*", "/"]
ops_update = [":=", "+=", "-=", "*=", "/="]

to_string (Int n) = show n
to_string (String s) = s
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (Array xs) = join_string " " $ map to_string xs
to_string (Error m) = "error: " ++ m
to_string x = show x
join_string glue [] = ""
join_string glue xs = drop (length glue) $ foldr (\l r -> r ++ glue ++ l) "" (reverse xs)

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
    parse_top = parse_seq
    parse_seq = Seq <$> sepBy1 read_br parse_eff
    parse_eff = parse_def `or` parse_update `or` parse_exp_or_fork
    parse_def = fmap (\(k, v) -> Def k v) read_def
    parse_update = do
      id <- read_id
      op <- read_strings ops_update
      body <- parse_exp_or_fork
      return $ Update id $ case op of
        ":=" -> body
        "+=" -> Op2 "+" (Ref id) body
        "-=" -> Op2 "-" (Ref id) body
        "*=" -> Op2 "*" (Ref id) body
        "/=" -> Op2 "/" (Ref id) body
    parse_exp_or_fork = parse_exp >>= parse_fork
    parse_exp = do
      l <- parse_unit
      parse_next l
    parse_next l = option l $ read_strings ("(" : "." : " | " : ops_calculate) >>= go
      where
        go "(" = do
          argv <- many parse_exp
          read_string ")"
          parse_next $ Apply l argv
        go "." = do
          id <- read_id
          parse_next $ Member l id
        go " | " = do
          alt <- parse_exp
          parse_next $ Catch l alt
        go op = do
          r <- parse_exp
          parse_next $ Op2 op l r
    parse_unit = (parse_int `or`
                  parse_str `or`
                  parse_array `or`
                  parse_ref)
    parse_int = Int <$> fmap read read_int
    parse_str = String <$> read_between "\"" "\"" (many $ satisfy (/= '"'))
    parse_array = Array <$> between (read_string "[") (read_string "]") (many parse_exp)
    parse_ref = do
      id <- read_id
      return $ case id of
        "true" -> Bool True
        "false" -> Bool False
        _ -> Ref id
    parse_fork unit = option unit (fork_eq `or` fork_bool `or` fork_catch)
      where
        fork_eq = Fork unit <$> (many1 guard_eq)
        fork_bool = Fork unit <$> guard_exps
        fork_catch = Catch unit <$> guard_catch
        guard_eq = do
          read_string "\n| "
          cond <- parse_exp
          read_string "="
          body <- parse_exp
          return (cond, body)
        guard_exps = do
          t <- guard_exp (Bool True)
          f <- guard_exp (Bool False)
          return [t, f]
        guard_exp cond = do
          read_string "\n| "
          x <- parse_exp
          return (cond, x)
        guard_catch = read_string "|" >> parse_exp
    parse_enum id = Enum id <$> read_enums
    parse_class id = Class id <$> read_props
    read_enums = many1 (read_string "\n  " >> read_enum)
    read_enum :: Parser (String, AST)
    read_enum = do
      id <- read_id
      props <- read_props
      return (id, Class id props)
    read_props = sepBy (read_string ",") read_prop
    read_prop = do
      id <- read_id
      read_type -- drop type information
      return id
    read_def = do
      id <- read_id
      args <- read_args
      mark <- read_strings ["=", ":"]
      case mark of
        "=" -> do
          body <- parse_eff
          return (id, if length args == 0 then body else (Func args body))
        ":" -> do
          x <- parse_enum id `or` parse_class id
          return (id, x)
    read_args = many read_id
    read_id = lex get_id
    read_ids1 = sepBy1 (satisfy (== '.')) read_id
    read_type = lex $ many1 $ satisfy (\x -> not $ elem x " \t\n")
    read_int = lex $ many1 $ get_any "0123456789"
    read_strings (x:xs) = foldl or (read_string x) (map read_string xs)
    read_string s = lex $ get_string s
    read_between l r m = between (read_string l) (get_string r) m
    read_char c = lex $ satisfy (== c)
    read_op = read_strings ops_calculate
    read_br = read_strings [";", "\n"]
    read_any s = lex $ get_any s
    read_sep = read_string "," `or` read_indent
    read_indent = do
      s <- get
      let sp = take (2 * depth s) $ repeat ' '
      read_string $ "\n" ++ sp

    get_any s = satisfy (\x -> elem x s)
    get_string s = mapM_ (\x -> satisfy (== x)) s >> return s
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
data Scope = Scope { local :: Env, change :: Env } deriving (Show)
type Runner a = StateT Scope (Either String) a
eval :: AST -> AST
eval root = top root
  where
    top exp = case runStateT (eval_top exp) (Scope [] []) of
      Left x -> Error x
      Right (x, s) -> x
    eval_top (Seq exps) = run exps Void
    eval_top exp = run [exp] Void
    run :: [AST] -> AST -> Runner AST
    run [] ret = return $ ret
    run (x:xs) _ = go x >>= run xs
    update :: String -> AST -> Runner AST
    update k v = do { modify $ \s -> s { change = (k, v) : change s }; return v }
    append :: String -> AST -> Runner AST
    append k v = do { modify $ \s -> s { local = (k, v) : local s }; return v }
    call kv body = do
      s <- get
      modify $ \s -> s { local = kv ++ local s }
      r <- go body
      modify $ \s -> s { local = local s }
      return r
    go :: AST -> Runner AST
    -- value(4)
    -- Int
    -- String
    -- Bool
    -- Func
    -- exp(8)
    go (Ref name) = find name
    go (Member ast name) = go ast >>= member name
    go (Op2 op left right) = run_op2 op left right
    go (Apply target []) = go target
    go (Apply target argv) = apply target argv
    go (Fork target branches) = fork target branches
    go (Seq list) = error "TBD seq"
    go (Def name exp) = append name exp
    go (Update name exp) = go exp >>= update name
    -- container(3)
    go (Array xs) = Array <$> mapM go xs
    -- Struct
    -- Enum
    -- error(2)
    go (Catch l r) = (go l) >>= catch r
    go v = append "" v
    member name (Struct _ xs) = look name xs
    member name (Enum _ xs) = look name xs
    catch alt (Error _) = go alt
    catch _ x = return x
    fork target branches = go target >>= match branches
      where
        match ((cond, exp):xs) x = if eq cond x then return exp else match xs x
        match [] x = error $ "unmatch " ++ show x ++ foldr (\x acc -> acc ++ "\n| " ++ show x) "" branches
        eq (Ref "_") _ = True
        eq x y = x == y
    apply target argv = do
      x <- go target
      xs <- mapM go argv
      case (x, xs) of
        ((Func args exp), _) -> call (zip args argv) exp
        ((Class name props), _) -> return $ Struct name (zip props argv)
        ((Array ys), [Int n]) -> return $ ys !! n
        _ -> error $ "unkown apply target " ++ show x
    run_op2 op left right = do
      l <- go left
      r <- go right
      append "" $ op2 op l r
    op2 "+" (Int l) (Int r) = Int $ l + r
    op2 "-" (Int l) (Int r) = Int $ l - r
    op2 "*" (Int l) (Int r) = Int $ l * r
    op2 "/" (Int l) (Int 0) = Error "divide by zero"
    op2 "/" (Int l) (Int r) = Int $ l `div` r
    op2 op l r = error $ "unknown operator " ++ op ++
                             "\n| " ++ show l ++
                             "\n| " ++ show r
    find :: String -> Runner AST
    find name = do
      s <- get
      look name (change s ++ local s)
    look x xs = case lookup x xs of
      Just x -> go x
      Nothing -> error $ "Not found " ++ x ++ " in " ++ show (map fst xs)
    dump :: Runner ()
    dump = do
      s <- get
      trace (show s) (return ())

debug x = trace ("- " ++ show x) x
debug1 x y = trace ("- " ++ show x ++ "\n| " ++ show y) y
debug2 x y z = trace ("- " ++ show x ++ "\n| " ++ show y ++ "\n| " ++ show z) z
