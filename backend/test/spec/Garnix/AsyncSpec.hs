module Garnix.AsyncSpec (spec) where

import Cradle
import Data.Char (isSpace)
import Data.IORef
import Data.Text (strip)
import Data.Text.IO qualified as T
import Garnix.Async
import Garnix.Duration
import Garnix.Prelude
import Garnix.TestHelpers
import NeatInterpolation (trimming)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcess)
import Test.Hspec

spec :: Spec
spec = do
  describe "timeout" $ do
    it "runs for (approximately) the specified number of milliseconds" $ do
      (action, counterReader) <- makeMilliCounter 1000000
      result <- timeout (fromSeconds @Double 0.1) action
      result `shouldBe` Nothing
      approxSecsElapsed <- counterReader
      approxSecsElapsed `shouldSatisfy` (> 5)
      approxSecsElapsed `shouldSatisfy` (< 15)

    it "kills the action on timeout" $ do
      (action, counterReader) <- makeMilliCounter 1000000
      _ <- timeout (fromSeconds @Double 0.1) action
      elapsedNow <- counterReader
      threadDelay (fromSeconds @Double 0.02)
      elapsedLater <- counterReader
      elapsedLater `shouldBe` elapsedNow

    it "returns the result if action completes on time" $ do
      (action, _) <- makeMilliCounter 10
      result <- timeout (fromSeconds @Double 0.1) action
      result `shouldBe` Just ()

    it "kills processes in the thread" $ do
      bashPath <- cs . dropWhileEnd isSpace <$> readProcess "which" ["bash"] ""
      -- We start a long-lived process, and check that it's PID doesn't exist
      -- past the timeout
      withScript
        [trimming|
          #! $bashPath
          echo $$$ > $$1
          exec sleep 1000
        |]
        $ \scriptFile -> withSystemTempFile "garnix-test" $ \outFile hdl -> do
          result <- timeout (fromSeconds @Int 1) $ run_ $ cmd (cs scriptFile) & addArgs [outFile]
          result `shouldBe` Nothing
          procId <- T.hGetContents hdl
          strip procId `shouldNotBe` ""
          result <-
            run
              $ cmd "ps"
              & addArgs ["-p", strip procId]
              & silenceStdout
          result `shouldBe` (ExitFailure 1, StderrRaw "")

    it "kills even processes that ignore signals" $ do
      bashPath <- cs . dropWhileEnd isSpace <$> readProcess "which" ["bash"] ""
      withScript
        [trimming|
          #! $bashPath
          trap "sleep 10" EXIT
          echo $$$ > $$1
          exec sleep 10
        |]
        $ \scriptFile -> withSystemTempFile "garnix-test" $ \outFile hdl -> do
          result <- timeout (fromSeconds @Int 1) $ run_ $ cmd (cs scriptFile) & addArgs [outFile]
          result `shouldBe` Nothing
          procId <- T.hGetContents hdl
          strip procId `shouldNotBe` ""
          result <-
            run
              $ cmd "ps"
              & addArgs ["-p", strip procId]
              & silenceStdout
          result `shouldBe` (ExitFailure 1, StderrRaw "")

makeMilliCounter :: Int -> IO (IO (), IO Int)
makeMilliCounter endAt = do
  ref <- newIORef 0
  let go n
        | n <= 0 = return ()
        | otherwise = do
            threadDelay (fromSeconds @Double 0.01)
            modifyIORef ref succ
            go (n - 1000)
  return (go endAt, readIORef ref)
