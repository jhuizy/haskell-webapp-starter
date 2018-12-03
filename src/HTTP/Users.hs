{-# LANGUAGE TemplateHaskell #-}

module HTTP.Users where

import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import           GHC.Generics
import           HTTP.Common
import           Service.User
import           System.Random

deriveJSON (opts 4) ''CreateUserRequest
deriveJSON (opts 4) ''GetUserResponse
deriveJSON (opts 3) ''LoginRequest

