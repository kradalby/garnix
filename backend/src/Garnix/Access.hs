module Garnix.Access
  ( Access (..),
    getBuildWithAccess,
    getRunWithAccess,
    hasAccessTo,
    hasAccessToRepo,
  )
where

import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types as Types
import GitHub.Data.Id (Id (Id))

data Access = Read | Cancel

getRunWithAccess :: Access -> Maybe User -> RunId -> M Run
getRunWithAccess access user' runId = do
  let accessCheck = case access of
        Read -> hasAccessTo
        Cancel -> canCancelBuild
  run' <- DB.getRun runId
  run <- case run' of
    Just run -> pure run
    Nothing -> throw (NoSuchRun runId)
  installationId <- getGarnixInstallationId (run ^. repoUser) (run ^. repoName)
  iAuth <- case installationId of
    Nothing -> throw $ OtherError "Failed to look up installation auth"
    Just id -> getInstallation (Id $ fromInteger id)
  repoPublicity <- getRepoPublicity iAuth (run ^. repoUser) (run ^. repoName)
  hasAccess <- accessCheck user' repoPublicity (run ^. reqUser) (run ^. repoUser) (run ^. repoName)
  when (not hasAccess) $ throw (NoSuchRun runId)
  pure run

getBuildWithAccess :: Access -> Maybe User -> BuildId -> M Build
getBuildWithAccess access user' buildId = do
  let accessCheck = case access of
        Read -> hasAccessTo
        Cancel -> canCancelBuild
  build <- DB.getBuild buildId
  hasAccess <- accessCheck user' (build ^. repoIsPublic) (build ^. reqUser) (build ^. repoUser) (build ^. repoName)
  when (not hasAccess) $ throw (NoSuchBuild buildId)
  pure build

hasAccessTo :: Maybe User -> RepoPublicity -> ForgeLogin -> RepoOwner -> RepoName -> M Bool
hasAccessTo user' repoIsPublic reqUser owner name
  | user' ^? _Just . githubLogin == Just reqUser = pure True
  | otherwise = hasAccessToRepo user' repoIsPublic owner name

hasAccessToRepo :: Maybe User -> RepoPublicity -> RepoOwner -> RepoName -> M Bool
hasAccessToRepo user' repoIsPublic owner name
  | isRepoPublic repoIsPublic = pure True
  | user' ^? _Just . subscriptionType == Just Admin = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators owner name
        case collaborators of
          RepoNotFound -> pure False
          Collaborators collaborators' -> pure $ (user ^. githubLogin) `elem` collaborators'

getCollaborators :: RepoOwner -> RepoName -> M Collaborators
getCollaborators owner repo = do
  installationId <- getGarnixInstallationId owner repo
  case installationId of
    Nothing -> pure RepoNotFound
    Just id -> do
      iAuth <- getInstallation (Id $ fromInteger id)
      getRepoCollaborators iAuth owner repo

canCancelBuild :: Maybe User -> RepoPublicity -> ForgeLogin -> RepoOwner -> RepoName -> M Bool
canCancelBuild user' _ reqUser owner name
  | user' ^? _Just . subscriptionType == Just Admin = pure True
  | user' ^? _Just . githubLogin == Just reqUser = pure True
  | otherwise = case user' of
      Nothing -> pure False
      Just user -> do
        collaborators <- getCollaborators owner name
        case collaborators of
          RepoNotFound -> pure False
          Collaborators collaborators' -> pure $ (user ^. githubLogin) `elem` collaborators'
