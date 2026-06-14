module Garnix.Reporters.GithubReporter (mkGithubReporter) where

import Control.Concurrent.Lifted (modifyMVar_, newMVar, readMVar)
import Control.Debounce
import Data.Text qualified as T
import Garnix.BuildLogs.Types (LogLine (..))
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Network.URI (URI, parseRelativeReference)

mkGithubReporter :: RepoInfo -> CommitHash -> Reporter
mkGithubReporter repoInfo commit =
  Reporter
    { createNewRun = \reportType -> do
        let name = reportName reportType
        url <- getRelativeUrl reportType
        -- Set the check run's external_id to our own build id, so a provider-native
        -- rerun maps back to a build without us storing the forge's run id.
        let externalId = case reportType of
              ReportBuild _ build -> Just $ build ^. id . to getBuildId . re hashIdText
              _ -> Nothing
        let initialReport = mkReport name url commit "" RunReportStatusInProgress externalId
        ghRunId <- newBuildReport repoInfo initialReport
        logsMVar <- newMVar (RunReportStatusInProgress, Nothing)
        lastSentLogsMVar <- newMVar Nothing
        let addNewline t = if "\n" `T.isSuffixOf` t then t else t <> "\n"
        let appendLogs status logs = do
              modifyMVar_ logsMVar $ \(curStatus, curLogs) -> do
                pure (fromMaybe curStatus status, Just $ mconcat $ map addNewline $ catMaybes [curLogs, logs])
        let sendLogs = do
              (status, logs) <- readMVar logsMVar
              lastSent <- readMVar lastSentLogsMVar
              when (lastSent /= Just (status, logs)) $ do
                modifyMVar_ lastSentLogsMVar $ const $ pure $ Just (status, logs)
                let report = mkReport name url commit (fromMaybe "" logs) status externalId
                void $ ignoringAllErrors $ updateBuildReport ghRunId report repoInfo
        debouncedSendLogs <- do
          debounceDuration <- view #githubLogDebounceDuration
          if debounceDuration == emptyDuration
            then pure sendLogs
            else do
              env <- ask
              liftIO
                <$> liftIO
                  ( mkDebounce
                      defaultDebounceSettings
                        { debounceAction = void $ runM env sendLogs,
                          debounceFreq = toMicroseconds debounceDuration,
                          debounceEdge = trailingEdge
                        }
                  )
        pure
          $ RunReporter
            { reportLogs = \(LogLine package _phase log) -> do
                appendLogs Nothing $ Just $ prefixLogLineWithPackageName package log
                debouncedSendLogs,
              reportComplete = \status -> do
                appendLogs (Just status) Nothing
                sendLogs
            }
    }

prefixLogLineWithPackageName :: Maybe PackageName -> Text -> Text
prefixLogLineWithPackageName mPkgName logLine = prefix <> logLine
  where
    prefix = maybe "" ((<> "> ") . getPackageName) mPkgName

getRelativeUrl :: ReportType -> M (Maybe URI)
getRelativeUrl buildOrRun = do
  let path = case buildOrRun of
        ReportBuild _name build -> Just $ "/build/" <> build ^. id . to getBuildId . re hashIdText
        ReportRun run -> Just $ "/run/" <> run ^. id . to getRunId . re hashIdText
        MetaCheck -> Nothing
  case path of
    Nothing -> pure Nothing
    Just path -> do
      case parseRelativeReference (cs path) of
        Just uri -> pure $ Just uri
        Nothing -> throw $ OtherError $ "Failed to parse build URI from " <> path

mkReport :: Text -> Maybe URI -> CommitHash -> Text -> RunReportStatus -> Maybe Text -> GhRunReport
mkReport name url commit logs reportStatus externalId =
  GhRunReport
    { _ghRunReportName = name,
      _ghRunReportCommit = commit,
      _ghRunReportUrl = show <$> url,
      _ghRunReportStatus = reportStatus,
      _ghRunReportTitle = name,
      _ghRunReportSummary = getReportSummary $ checkRunSummary name reportStatus,
      _ghRunReportLogs = RawLogs logs,
      _ghRunReportExternalId = externalId
    }

checkRunSummary :: Text -> RunReportStatus -> ReportSummary
checkRunSummary runName = \case
  RunReportStatusInProgress -> ReportSummary runName
  RunReportStatusSuccess -> ReportSummary $ runName <> " succeeded"
  RunReportStatusFailure -> ReportSummary $ runName <> " failed"
  RunReportStatusTimeout -> ReportSummary $ runName <> " timed out"
  RunReportStatusCancelled -> ReportSummary $ runName <> " cancelled"
