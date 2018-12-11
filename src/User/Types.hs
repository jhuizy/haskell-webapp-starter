module User.Types
  ( User (..)
  , CreateUser (..)
  ) where

import           ClassyPrelude

data User = User
  { userId           :: Int
  , userEmail        :: String
  , userPasswordHash :: String
  , userAge          :: Int
  } deriving (Show, Eq)

data CreateUser = CreateUser
  { createUserEmail    :: String
  , createUserPassword :: String
  , createUserAge      :: Int
  } deriving (Show, Eq)
