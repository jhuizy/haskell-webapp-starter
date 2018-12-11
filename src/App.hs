module App where

import           ClassyPrelude
import           Common.Types
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State                  (lift, liftM)
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import qualified User.Router                          as UserRouter
import qualified User.SQLite                          as UserSQLite
import           User.Types
import           Web.Scotty.Trans

newtype ScottyM a = ScottyM { runApp :: ScottyT APIError IO a } deriving (Functor, Applicative, Monad)

errorHandler :: APIError -> ActionM ()
errorHandler (NotFound i) = do
  status status404
  text "Not Found"
errorHandler (BadRequest s) = do
  status status400
  text $ L.pack s
errorHandler (UnhandledError s) = do
  liftIO . putStrLn $ T.pack s
  status status500
  text "Something went wrong..."

main :: IO ()
main = scottyT 3000 id (runApp app)

instance UserRouter.UserService ScottyM where
  listUsers = UserSQLite.list
  createUser = UserSQLite.create
  getUserById = UserSQLite.find

app :: ScottyM ()
app = ScottyM $ do

  defaultHandler errorHandler

  middleware logStdout

  UserRouter.routes
