module Lib where
import Data.DateTime

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data TodoData = TodoData
            {
              id   :: Int ,
              text :: String
            } deriving (Show, Eq)

data Todo = PendingTodo TodoData | CompletedTodo TodoData deriving (Show, Eq)

data TimedTodo = TimedTodo DateTime Todo

getId :: Todo -> Int
getId (PendingTodo (TodoData todoId _))   = todoId
getId (CompletedTodo (TodoData todoId _)) = todoId

getText :: Todo -> String
getText (PendingTodo (TodoData _ text))   = text
getText (CompletedTodo (TodoData _ text)) = text

toggle :: Todo -> Todo
toggle (CompletedTodo a) = PendingTodo a
toggle (PendingTodo a)   = CompletedTodo a


data Todos = Todos
  {
  nextId :: Int,
  items  :: [Todo]
       } deriving (Show, Eq)

newTodos :: Todos
newTodos = Todos 0 []

addTodo :: String -> Todos -> Todos
addTodo txt todos = Todos newId newTodos
  where newId = nextId todos + 1
        newTodos = newTodo : items todos
        newTodo = PendingTodo $ TodoData (nextId todos) txt

getPending :: Todos -> [Todo]
getPending t = filter pending (items t)
  where pending (PendingTodo _) = True
        pending _               = False

containsTodo :: Todos -> Int -> Bool
containsTodo todos todoId = length todosFilterResult == 1
  where todosFilterResult = filter (\t -> getId t == todoId) (items todos)

getTodo :: Todos -> Int -> Maybe Todo
getTodo todos todoId
  | containsTodo todos todoId = Just $ head $ filter (\t -> getId t == todoId) (items todos)
  | otherwise = Nothing

emptyTodos :: Todos
emptyTodos = newTodos

toggleTodo :: Int -> Todos -> Todos
toggleTodo toggleId (Todos nextId' items') =
  Todos nextId' (map
    (\t -> if getId t == toggleId then toggle t else t)
    items')
removeTodo :: Int -> Todos -> Todos
removeTodo tId (Todos nextId' items') =
  Todos nextId' $ filter (\t-> getId t /= tId) items'

testTodos :: Todos
testTodos = addTodo "Mirar el documental de Yugoslavia" $
  addTodo "Hacer la commida" $
  addTodo "Hacer los deberes" $
  addTodo "Hacer la cama" emptyTodos
