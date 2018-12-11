module Common.SQLite where

import           ClassyPrelude
import           Database.SQLite.Simple

data SQLiteEnv = SQLiteEnv
  { filename :: String
  }

type SQLite m = (MonadUnliftIO m, MonadReader SQLiteEnv m)

withConnection :: (SQLite m) => (Connection -> m a) -> m a
withConnection = bracket initConn closeConn
  where
    initConn = ask >>= liftIO . open . filename
    closeConn = liftIO . close


