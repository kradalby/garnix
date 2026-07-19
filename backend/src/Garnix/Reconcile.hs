module Garnix.Reconcile (reconcileOrphanedBuilds) where

import Control.Lens
import Data.Map.Strict qualified as Map
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import GitHub.Data.Id (Id (..))

-- | Startup reconciler — run once, before webhooks are served. After a
-- crash/restart the previous process's build threads are gone, but their rows
-- are still @status IS NULL@ and their GitHub check runs still show "in
-- progress", which on the head commit blocks a PR on a forever-spinning
-- required check. This:
--
--   1. closes each orphan's GitHub check run (best-effort), then
--   2. marks every orphan @cancelled@ in the DB (authoritative; always runs).
--
-- The GitHub step is bounded (only in-flight builds, capped by the build pool)
-- and one-shot, so it is not the crash-loop re-dispatch that triggers GitHub's
-- secondary rate limit. It is nonetheless best-effort: any failure (auth, rate
-- limit, API down) is swallowed so it can never block startup or the DB reset.
-- Returns the number of builds reset.
reconcileOrphanedBuilds :: M Int
reconcileOrphanedBuilds = do
  orphans <- DB.getOrphanedBuilds
  let byRepo =
        Map.toList
          $ Map.fromListWith (<>) [((b ^. repoUser, b ^. repoName), [b]) | b <- orphans]
  forM_ byRepo $ \((owner, name), builds) ->
    ignoringAllErrors $ do
      getGarnixInstallationId owner name >>= \case
        Nothing -> pure ()
        Just instId -> do
          iAuth <- getInstallation (Id (fromInteger instId))
          token <- getAccessToken iAuth
          let repoInfo = RepoInfo iAuth token owner name
          forM_ builds $ \b ->
            forM_ (b ^. githubRunId) $ \runId ->
              ignoringAllErrors $ updateBuildReport runId (cancelledReport b) repoInfo
  DB.abortOrphanedBuilds

cancelledReport :: Build -> GhRunReport
cancelledReport b =
  let name = reportNameForBuild b
   in GhRunReport
        { _ghRunReportName = name,
          _ghRunReportCommit = b ^. gitCommit,
          _ghRunReportUrl = Nothing,
          _ghRunReportStatus = RunReportStatusCancelled,
          _ghRunReportTitle = name,
          _ghRunReportSummary = name <> " cancelled (garnix restarted with this build in flight)",
          _ghRunReportLogs = RawLogs ""
        }
