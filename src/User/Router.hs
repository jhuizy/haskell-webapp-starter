module User.Router
  ( UserService(..)
  , routes
  ) where

import           ClassyPrelude
import           Common.Types
import qualified Data.Text.Lazy   as L
import           User.Types       (CreateUser, User)
import           Web.Scotty.Trans

class (Monad m) => UserService m where
  listUsers :: m [User]
  createUser :: CreateUser -> m (Maybe User)
  getUserById :: Int -> m (Maybe User)

routes :: (MonadIO m, UserService m) => ScottyT APIError m ()
routes = do
  get "/users" $ do
    users <- lift listUsers
    text . L.pack . show $ users
