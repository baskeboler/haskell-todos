module Command where

import           Lib

data Command = ListTodos
  | GetTodo  Int
  | RemoveTodo Int
  | ToggleTodo  Int
  | AddTodo  String
  | Quit
  deriving (Eq, Show)

evalCommand :: Command -> Todos -> (Command,Todos)
evalCommand cmd@ListTodos todos = (cmd, todos)
evalCommand cmd@(GetTodo i) todos = (cmd, todos)
evalCommand cmd@(RemoveTodo i) todos = (cmd, removeTodo i todos)
evalCommand cmd@(AddTodo txt) todos = (cmd, addTodo txt todos)
evalCommand cmd@(ToggleTodo i) todos = ( cmd, todos')
  where todos' = toggleTodo i todos
evalCommand cmd@Quit todos = ( cmd, todos)

doEvalCommand :: Command -> Todos -> IO (Command, Todos)
doEvalCommand c todos = do
  printResults c todos
  return $ evalCommand c todos

doSafeEvalCommand :: Maybe Command -> Todos -> IO(Maybe Command, Todos)
doSafeEvalCommand Nothing todos = do
  putStrLn "Invalid command"
  return (Nothing, todos)

doSafeEvalCommand (Just cmd) todos = do
  let (c, t) = evalCommand cmd todos
  printResults cmd todos
  return (Just c, t)

printResults :: Command -> Todos -> IO()
printResults (GetTodo i) todos = do
  putStrLn $ showMaybeTodo $ getTodo todos i
  return ()
printResults ListTodos todos = do
  putStrLn $ listTodos todos
  return ()
printResults _ _ = putStrLn "OK." >> return ()

showMaybeTodo :: Maybe Todo -> String
showMaybeTodo Nothing  = "Not found"
showMaybeTodo (Just t) = showTodo t

showTodo :: Todo -> String
showTodo  t@(CompletedTodo td) = show (getId t) ++ " [completed] " ++ text td
showTodo  t@(PendingTodo td)   = show (getId t) ++ " [pending] " ++ text td

listTodos :: Todos -> String
listTodos todos = concatMap ((++ "\n") . showTodo) $ reverse $ items todos
