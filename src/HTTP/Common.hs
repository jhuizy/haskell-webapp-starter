module HTTP.Common
  ( ActionM
  , APIError (..)
  , parseAndValidateJSON
  , opts
  ) where

import           ClassyPrelude
import           Data.Aeson
import           Web.Scotty.Trans

type ActionM = ActionT APIError IO

data APIError =
  NotFound Int
  | BadRequest String
  | UnhandledError String
  deriving (Show, Eq)

instance ScottyError APIError where
  stringError = UnhandledError
  showError = fromString . show

parseAndValidateJSON :: (FromJSON a) => ActionM a
parseAndValidateJSON = jsonData `rescue` (\_ -> raise $ BadRequest "Invalid JSON")

opts i = defaultOptions { fieldLabelModifier = drop i }
