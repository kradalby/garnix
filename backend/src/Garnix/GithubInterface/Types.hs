module Garnix.GithubInterface.Types (GhRole (..), UserOrgMembership (..)) where

import Data.Aeson (withObject, withText, (.:))
import Garnix.Prelude
import Garnix.Types hiding (Admin)

data GhRole = Admin | Other Text
  deriving stock (Show, Eq)

instance FromJSON GhRole where
  parseJSON = withText "GhRole" $ \role -> pure $ case role of
    "admin" -> Admin
    other -> Other other

data UserOrgMembership = UserOrgMembership
  { organizationName :: RepoOwner,
    role :: GhRole
  }
  deriving stock (Show, Eq)

instance FromJSON UserOrgMembership where
  parseJSON = withObject "UserOrgMembership" $ \v -> do
    org <- v .: "organization"
    name <- withObject "UserOrgMembership.organization" (.: "login") org
    role <- parseJSON =<< (v .: "role")
    pure $ UserOrgMembership name role
