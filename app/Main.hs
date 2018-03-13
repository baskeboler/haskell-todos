
module Main where

import           Command
import           Lib
import           Parser
import           System.IO
import Database
shouldContinue :: Maybe Command -> Bool
shouldContinue (Just Quit) = False
shouldContinue _           = True

loop :: Todos -> IO Todos
loop todos = do
  prompt
  a <- getLine
  c <- doSafeRun a
  (c1, t) <- doSafeEvalCommand c todos
  if shouldContinue c1 then loop t
  else return t

prompt :: IO ()
prompt = do
  putStr "todo-app > "
  hFlush stdout
  return ()

main :: IO ()
main = do
  t <- loadTodosFromDb
  todos <- loop t
  saveTodos todos
  return ()
