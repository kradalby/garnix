module Garnix.API.Commits where

import Garnix.API.Runs (RunSummary, toRunSummary)
import Garnix.Access (hasAccessTo, hasAccessToRepo)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import GitHub.Data.Id (Id (..))
import Servant.Auth.Server

data CommitAPI route = CommitAPI
  { _commitAPIgetCommitsForRepo :: route :- "repo" :> Capture "owner" RepoOwner :> Capture "repo" RepoName :> Get '[JSON] ListCommits,
    _commitAPIgetCommitsForUser :: route :- Get '[JSON] ListCommits,
    _commitAPIgetSingleCommit :: route :- Capture "commit" CommitHash :> Get '[JSON] GetCommit
  }
  deriving (Generic)

commitAPI :: AuthResult AuthJwtPayload -> CommitAPI (AsServerT M)
commitAPI (Authenticated ((^. #user) -> user')) =
  CommitAPI
    { _commitAPIgetCommitsForRepo = getCommitsForRepo (Just user'),
      _commitAPIgetCommitsForUser = getCommitsForUser user',
      _commitAPIgetSingleCommit = getSingleCommit (Just user')
    }
commitAPI _ =
  CommitAPI
    { _commitAPIgetCommitsForRepo = getCommitsForRepo Nothing,
      _commitAPIgetCommitsForUser = throw Unauthorized,
      _commitAPIgetSingleCommit = getSingleCommit Nothing
    }

data ListCommits = ListCommits
  { _listCommitsCommits :: [CommitSummary]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ListCommits where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data GetCommit = GetCommit
  { _getCommitSummary :: CommitSummary,
    _getCommitBuilds :: [Build],
    _getCommitRuns :: [RunSummary]
  }
  deriving (Eq, Show, Generic)

instance ToJSON GetCommit where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getCommitsForRepo :: (HasCallStack) => Maybe User -> RepoOwner -> RepoName -> M ListCommits
getCommitsForRepo user repoOwner repoName = do
  installationId <- getGarnixInstallationId repoOwner repoName
  iAuth <- case installationId of
    Nothing -> throw $ NoSuchRepo {_owner = repoOwner, _name = repoName}
    Just id -> getInstallation (Id $ fromInteger id)
  repoPublicity <- getRepoPublicity iAuth repoOwner repoName
  hasAccess <- hasAccessToRepo user repoPublicity repoOwner repoName
  when (not hasAccess) $ throw NoSuchRepo {_owner = repoOwner, _name = repoName}
  ListCommits <$> DB.getCommitsByOwnerAndRepo repoOwner repoName

getCommitsForUser :: User -> M ListCommits
getCommitsForUser user = do
  commits <- DB.getCommitsForReqUser user
  pure $ ListCommits {_listCommitsCommits = commits}

getSingleCommit :: Maybe User -> CommitHash -> M GetCommit
getSingleCommit user' commit = do
  summary <- DB.getCommitSummary commit
  hasAccess <- hasAccessTo user' (summary ^. repoIsPublic) (summary ^. reqUser) (summary ^. repoOwner) (summary ^. repoName)
  when (not hasAccess) $ throw (NoSuchCommit commit)
  result <- DB.getBuildsAndRunsByCommit (summary ^. repoOwner) (summary ^. repoName) commit
  pure $ case result of
    CommitEvaluating -> GetCommit summary [] []
    CommitEvaluated _ builds runs ->
      GetCommit
        summary
        (filter (\b -> b ^. packageType /= TypeOverall) builds)
        (map toRunSummary runs)
