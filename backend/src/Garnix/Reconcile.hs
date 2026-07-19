module Garnix.Reconcile (reconcileOrphanedBuilds) where

import Control.Lens
import Data.Map.Strict qualified as Map
import Garnix.Build.Reporting (reportNameForBuild)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.Concurrency (forkM)
import Garnix.Prelude
import Garnix.Types
import GitHub.Data.Id (Id (..))

-- | Startup reconciler — run once, before webhooks are served.
--
-- After a crash/restart the previous process's build threads are gone but their
-- rows are still @status IS NULL@. This clears them all in a single fast
-- @UPDATE@ ('DB.abortOrphanedBuilds') and returns the count. Running pre-Warp
-- keeps the guarantee that no live build can be caught.
--
-- Separately it closes the GitHub check runs of *recent* orphans so a PR does
-- not hang on a forever-spinning required check. That work is:
--
--   * bounded to recent, check-bearing orphans ('DB.getRecentOrphanedChecks') —
--     the historical backlog of superseded orphans is harmless, and closing all
--     of it would be a GitHub API burst (the 403 secondary-rate-limit storm);
--   * run in the background ('forkM') so it never blocks startup / @notifyReady@
--     (closing hundreds of checks synchronously here would time out the unit);
--   * best-effort ('ignoringAllErrors') so a GitHub failure is swallowed.
--
-- It operates on rows captured before the cancel, so it cannot race new builds.
reconcileOrphanedBuilds :: M Int
reconcileOrphanedBuilds = do
  recent <- DB.getRecentOrphanedChecks
  n <- DB.abortOrphanedBuilds
  unless (null recent) $ forkM $ closeChecks recent
  pure n

closeChecks :: [Build] -> M ()
closeChecks builds =
  forM_ (Map.toList byRepo) $ \((owner, name), bs) ->
    ignoringAllErrors
      $ getGarnixInstallationId owner name
      >>= \case
        Nothing -> pure ()
        Just instId -> do
          iAuth <- getInstallation (Id (fromInteger instId))
          token <- getAccessToken iAuth
          let repoInfo = RepoInfo iAuth token owner name
          forM_ bs $ \b ->
            forM_ (b ^. githubRunId) $ \runId ->
              ignoringAllErrors $ updateBuildReport runId (cancelledReport b) repoInfo
  where
    byRepo = Map.fromListWith (<>) [((b ^. repoUser, b ^. repoName), [b]) | b <- builds]

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
