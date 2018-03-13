{-# LANGUAGE OverloadedStrings #-}
module Database where

import           Control.Applicative
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Lib
data TestField = TestField Int T.Text Int deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField _id str completed) = toRow (_id, str, completed)

toTodo :: TestField -> Todo
toTodo (TestField id_ txt 0) = PendingTodo (TodoData id_ (T.unpack txt))
toTodo (TestField id_ txt 1) = CompletedTodo (TodoData id_ (T.unpack txt))

fromTodo :: Todo -> TestField
fromTodo (PendingTodo (TodoData id_ txt))   = TestField id_ (T.pack txt) 0
fromTodo (CompletedTodo (TodoData id_ txt)) = TestField id_ (T.pack txt) 1

dbFile :: String
dbFile = "todos.db"

createStmt :: Query
createStmt = "CREATE TABLE IF NOT EXISTS todos(id INTEGER PRIMARY KEY, txt TEXT, completed INTEGER DEFAULT 0)"

insertStmt :: Query
insertStmt = "insert into todos(txt) values (?)"

insertTodo :: Query
insertTodo = "insert into todos(id, txt, completed) values (?,?,?)"
selectQuery :: Query
selectQuery = "select * from todos"

main2 :: IO()
main2 = do
  conn <- open dbFile
  execute_ conn createStmt
  execute conn insertStmt (Only ("tarea numero 1" :: String))
  execute conn insertStmt (Only ("tarea numero 2" :: String))
  execute conn insertStmt (Only ("tarea numero 3" :: String))
  execute conn insertStmt (Only ("tarea numero 4" :: String))
  rowId <- lastInsertRowId conn
  r <- query_ conn  selectQuery :: IO [TestField]
  mapM_ (print . toTodo) r
  close conn

loadTodosFromDb :: IO Todos
loadTodosFromDb = do
  conn <- open dbFile
  r <- query_ conn selectQuery :: IO [TestField]
  close conn
  let items = map toTodo r
  return $ Todos (succ $ resolveNextId items) items

clearDb :: IO()
clearDb = do
  conn <- open dbFile
  execute_ conn "delete from todos"
  close conn

saveTodos :: Todos -> IO()
saveTodos todos = do
  clearDb
  conn <- open dbFile
  executeMany conn insertTodo $ map fromTodo $ items todos
  close conn

resolveNextId :: [Todo] -> Int
resolveNextId = foldr (max . getId) 0
