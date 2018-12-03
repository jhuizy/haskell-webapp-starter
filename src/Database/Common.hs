module Database.Common where

import           ClassyPrelude
import           Database.SQLite.Simple

class (Monad m) => HasConnection m where
  getConnection :: m Connection

instance HasConnection IO where
  getConnection = open ""

withConnection :: (Connection -> IO a) -> IO a
withConnection = bracket initConn closeConn
  where
    initConn = open "test.db"
    closeConn = close


