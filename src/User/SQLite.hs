module User.SQLite where

import           ClassyPrelude
import           Common.SQLite                  (SQLite, withConnection)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.BCrypt
import qualified Data.ByteString.Char8          as C
import           Data.Maybe
import           Database.SQLite.Simple         hiding (withConnection)
import           Database.SQLite.Simple.FromRow
import           User.Types

instance FromRow User where
  fromRow = User <$> field
                <*> field
                <*> field
                <*> field

instance ToRow User where
  toRow (User id e p a) = toRow (id, e, p, a)

migrate :: SQLite m => m ()
migrate = withConnection $ \conn -> liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, email TEXT NOT NULL, password TEXT NOT NULL, age INTEGER NOT NULL)"

list :: SQLite m => m [User]
list = withConnection $ \conn -> liftIO $ query_ conn "SELECT * FROM users"

create :: (SQLite m) => CreateUser -> m User
create (CreateUser email pass age) = withConnection $ \conn -> liftIO $ do
  hashPassword <- maybe (error "hashing failed") C.unpack <$> hashPasswordUsingPolicy slowerBcryptHashingPolicy (C.pack pass)
  execute conn "INSERT INTO users (email, password, age) VALUES (?, ?, ?)" (email, hashPassword, age)
  ident <- lastInsertRowId conn
  return $ User (fromIntegral ident) email hashPassword age

find :: (SQLite m) => Int -> m (Maybe User)
find ident = withConnection $ \conn -> liftIO $ do
  user <- query conn "SELECT * FROM users WHERE id = ?" $ Only ident
  return $ listToMaybe user

findByEmail :: (SQLite m) => String -> m (Maybe User)
findByEmail email = withConnection $ \conn -> liftIO $ do
  user <- query conn "SELECT * FROM users WHERE email = ?" $ Only email
  return $ listToMaybe user

checkCredentials :: SQLite m => String -> String -> m Bool
checkCredentials email pass = withConnection $ \conn -> do
  let p = C.pack
  maybeUser <- findByEmail email
  return $ case maybeUser of
    (Just u) -> validatePassword (p $ userPasswordHash u) (p pass)
    Nothing  -> False
