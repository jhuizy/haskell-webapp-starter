{-# LANGUAGE TemplateHaskell #-}

module Service.User
  ( CreateUserRequest
  , GetUserResponse
  , LoginRequest
  , UserService
  , fromUser
  ) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State
import           Data.Maybe
import qualified Database.User       as DB
import           GHC.Generics
import           System.Random

data CreateUserRequest = CreateUserRequest
  { _curEmail    :: String
  , _curPassword :: String
  , _curAge      :: Int
  } deriving (Show, Generic)

data GetUserResponse = GetUserResponse
  { _gurId    :: Int
  , _gurEmail :: String
  , _gurAge   :: Int
  } deriving (Show, Generic)

data LoginRequest = LoginRequest
    { _lrEmail    :: String
    , _lrPassword :: String
    } deriving (Show, Generic)

makeLenses ''CreateUserRequest
makeLenses ''GetUserResponse
makeLenses ''LoginRequest

fromUser :: DB.User -> GetUserResponse
fromUser (DB.User id email _ age) = GetUserResponse id email age

class (Monad m) => UserService m where
  getUsers :: m [GetUserResponse]
  getUserById :: m GetUserResponse

