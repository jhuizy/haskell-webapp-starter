module Common.Types
  ( APIError(..)
  , (<$$>)
  , ActionM
  ) where

import           ClassyPrelude
import qualified Data.Text.Lazy   as L
import           Web.Scotty.Trans

data APIError =
  NotFound Int
  | BadRequest String
  | UnhandledError String
  deriving (Show, Eq)

type ActionM = ActionT APIError IO

instance ScottyError APIError where
  stringError = UnhandledError
  showError = L.pack . show

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap
