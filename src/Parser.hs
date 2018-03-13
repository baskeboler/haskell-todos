module Parser where

import           Command
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Lib


newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream"
    _           -> error "Parser error"

tryParser :: Parser a -> String -> Maybe a
tryParser m s =
  case parse m s of
    [(res, [])] -> Just res
    _           -> Nothing

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])
instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])
instance Monad Parser where
  return = unit
  (>>=) = bind
instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (const [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

characterItem :: Parser Char
characterItem = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = characterItem `bind` \c ->
  if p c then unit c else Parser (const [])

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
    where rest a = (do  f <- op
                        b <- p
                        rest (f a b)) <|>  return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string []     = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a}

reserved :: String -> Parser String
reserved s = token $ string s

spaces :: Parser String
spaces = many $ oneOf " \r\n"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

listCmd :: Parser Command
listCmd = reserved "list" >> return ListTodos

getTodoCmd :: Parser Command
getTodoCmd = do
  reserved "get"
  todoId <- number
  return (GetTodo todoId)

toggleCmd :: Parser Command
toggleCmd = do
  reserved "toggle"
  todoId <- number
  return (ToggleTodo todoId)
rmTodoCmd :: Parser Command
rmTodoCmd = do
  reserved "rm"
  todoId <- number
  return $ RemoveTodo todoId

addTodoCmd :: Parser Command
addTodoCmd = do
  reserved "add"
  char '\"'
  todoText <- many $ satisfy $ \s -> s /= '\"'
  char '\"'
  return (AddTodo todoText)

quitCmd :: Parser Command
quitCmd = reserved "quit" >> return Quit

commandParser :: Parser Command
commandParser = listCmd
  <|> getTodoCmd
  <|> rmTodoCmd
  <|> toggleCmd
  <|> addTodoCmd
  <|> quitCmd

type Env = Todos

run :: String -> Command
run = runParser commandParser

safeRun :: String -> Maybe Command
safeRun = tryParser commandParser

doRun :: String -> IO Command
doRun s = do
  return $ run s
-- todosState :: State Todos String
doSafeRun :: String -> IO (Maybe Command)
doSafeRun s = do
  return $ safeRun s
