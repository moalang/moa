module Base where

import Debug.Trace (trace)
import Control.Applicative

-- Definitions
all_exec_ops = [":=", "++", "+", "-", "*", "/", "%", ">=", ">", "<=", "<", "!=", "==", "&&", "||"]
all_parse_ops = ["<-", "+=", "-=", "*=", "/=", "%="] ++ all_exec_ops

type Env = [(String, AST)]

data AST =
-- simple value
    Void
  | I64 Int
  | Bool Bool
  | String String
  | Array [AST]
-- container
  | Struct String [String] [AST]
  | Enum String [(String, [String])]
-- variable
  | Var String String
-- define and call
  | Def String [String] AST
  | Call String [AST]
  | Apply AST [AST]
  | Method AST String [AST]
-- parenthesis
  | Parenthesis AST
-- statement
  | Stmt [AST]
-- branch
  | Branch AST [(Matcher, AST)]
  deriving (Show, Eq)

data Matcher =
    TypeMatcher String
  | ValueMatcher AST
  deriving (Show, Eq)

-- Pparser
data Source = Source { src :: String, pos :: Int, len :: Int, nest :: Int } deriving (Show)

data Parser a = Parser { runParser :: Source -> Maybe (a, Source) }

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap (\(a, ss) -> (f a, ss)) (runParser p s)

instance Applicative Parser where
  pure v = Parser $ \s -> Just (v, s)
  l <*> r = Parser $ \s ->
            case runParser l s of
              Just (f, s') -> case runParser r s' of
                Just (v, s'') -> Just (f v, s'')
                _ -> Nothing
              _ -> Nothing

instance Monad Parser where
  return = pure
  l >>= f = Parser $ \s -> case runParser l s of
    Just (a, ss) -> runParser (f a) ss
    _ -> Nothing

instance MonadFail Parser where
  fail _ = Parser $ \s -> Nothing

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  l <|> r = Parser $ \s ->
            case runParser l s of
              Just x@(_, _) -> return x
              Nothing -> runParser r s

-- utility
to_string Void = "_"
to_string (I64 x) = show x
to_string (Bool True) = "true"
to_string (Bool False) = "false"
to_string (String s) = s
to_string (Call name []) = name
to_string (Call name argv) = name ++ "(" ++ (string_join " " $ map to_string argv) ++ ")"
to_string (Struct name _ _) = name
string_join glue [] = ""
string_join glue [x] = x
string_join glue (x:xs) = x ++ glue ++ (string_join glue xs)
quote_string x = go x []
  where
    go ('\\':x:xs) acc = go xs $ x : '\\' : acc
    go ('"':xs) acc = go xs $ '"' : '\\' : acc
    go (x:xs) acc = go xs $ x : acc
    go [] acc = '"' : (reverse ('"' : acc))
