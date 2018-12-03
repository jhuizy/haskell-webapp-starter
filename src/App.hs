module App where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State                  (lift, liftM)
import           Data.String                          (fromString)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L
import           Database.Common                      (withConnection)
import qualified Database.User                        as DB
import           HTTP.Common                          (APIError (..), ActionM)
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import qualified Service.User                         as HTTP
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

app :: ScottyM ()
app = ScottyM $ do

  defaultHandler errorHandler

  middleware logStdout

  get "/users/:id" $ do
    id <- param "id"
    user <- liftIO $ withConnection $ \conn -> DB.find conn id
    text . L.pack . show $ HTTP.fromUser <$> user

