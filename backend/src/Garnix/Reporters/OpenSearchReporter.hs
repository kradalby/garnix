module Garnix.Reporters.OpenSearchReporter
  ( openSearchReporter,
  )
where

import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Garnix.UserLogs

openSearchReporter :: Reporter
openSearchReporter =
  Reporter
    { createNewRun = \reportType -> do
        pure $ case reportType of
          MetaCheck -> do
            RunReporter
              { reportLogs = \_ -> pure (),
                reportComplete = \_ -> pure ()
              }
          ReportRun run -> do
            RunReporter
              { reportLogs = \logLine -> do
                  storeRunLogLine run logLine,
                reportComplete = \runReportStatus -> do
                  let status = case runReportStatus of
                        RunReportStatusSuccess -> Just Success
                        RunReportStatusInProgress -> Nothing
                        RunReportStatusFailure -> Just Failure
                        RunReportStatusTimeout -> Just Timeout
                        RunReportStatusCancelled -> Just Cancelled
                  DB.setRunStatus (run ^. id) status
              }
          ReportBuild _name build -> do
            RunReporter
              { reportLogs = \logLine -> do
                  storeBuildLogLine build logLine,
                reportComplete = \_runReportStatus -> do
                  pure ()
              }
    }
