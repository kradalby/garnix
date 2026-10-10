module Garnix.SandboxSpec
  ( spec,
  )
where

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Concurrent.Async.Lifted qualified as Async
import Control.Monad.Trans.Control (liftBaseOp_)
import Cradle
import Data.String.Interpolate
import Data.Text qualified as T
import Garnix.Async (timeout)
import Garnix.Build.Helpers (withInternalCacheToken)
import Garnix.DB qualified as DB
import Garnix.Duration (fromSeconds)
import Garnix.NixConfig (addNixConfigEnvironment, nixConfDefaults)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.TestHelpers (waitFor)
import Garnix.TestHelpers.Monad
import Garnix.Types
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp
import System.Random
import Test.HUnit (assertFailure)
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = inM $ do
  describe "runInNixSandbox" $ do
    it "only gives access to selected paths from /etc" $ do
      let allowedEtcFiles =
            [ "hostname",
              "localtime",
              "nix",
              "nixos",
              "nsswitch.conf",
              "resolv.conf",
              "ssl",
              "static",
              "zoneinfo"
            ]
      StdoutTrimmed dirs <-
        failIfErr
          $ cmd "ls"
          & addArgs ["/etc/" :: String]
          & pure
          & inNixSandbox [] Nothing
      T.words dirs \\ allowedEtcFiles `shouldBeM` []

    it "hides the pre-existing /tmp" $ do
      randomName <- liftIO (abs <$> randomIO :: IO Int)
      let filename = "/tmp/" <> show randomName
      () <- liftIO . run $ cmd "touch" & addArgs [filename]
      StdoutTrimmed withoutSandbox <-
        failIfErr
          . liftIO
          . run
          $ cmd "ls"
          & addArgs ["/tmp/" :: String]
      T.lines withoutSandbox `shouldContainM` [show randomName]
      StdoutTrimmed withSandbox <-
        failIfErr
          $ cmd "ls"
          & addArgs ["/tmp/" :: String]
          & pure
          & inNixSandbox [] Nothing
      T.lines withSandbox `shouldNotContainM` [show randomName]

    it "works with basic nix eval" $ do
      StdoutTrimmed out <-
        failIfErr
          $ cmd "nix"
          & addArgs
            [ "eval" :: String,
              "--expr",
              "1 + 2"
            ]
          & nixConfDefaults
          & pure
          & inNixSandbox [] Nothing
      out `shouldBeM` "3"

    it "allows eval for flake files that have fetchers" $ do
      withSystemTempDirectory "garnix-test" $ \dir -> do
        let flake =
              [i|
                {
                  outputs = inputs : {
                    foo = builtins.fetchGit {
                      url = "https://github.com/garnix-io/garnix-lib";
                      rev = "0573417fc462b0eeed5d762c8fe08093afb35a3d";
                    };
                  };
                }
              |]
        liftIO $ writeFile (dir <> "/flake.nix") flake
        StdoutTrimmed out <-
          failIfErr
            $ cmd "nix"
            & setWorkingDir dir
            & addArgs
              [ "eval" :: String,
                ".#foo",
                "--apply",
                "x : x.lastModifiedDate"
              ]
            & nixConfDefaults
            & pure
            & inNixSandbox [] Nothing
        out `shouldBeM` "\"20250130113659\""

    it "mounts the extra read-only paths" $ do
      withSystemTempDirectory "garnix-test" $ \dir -> do
        liftIO $ writeFile (dir <> "/foo") ""
        StdoutTrimmed out <-
          failIfErr
            $ cmd "ls"
            & addArgs [dir]
            & pure
            & inNixSandbox [(dir, ReadOnly)] Nothing
        out `shouldBeM` "foo"
        exit <-
          cmd "touch"
            & addArgs [dir <> "/bar"]
            & silenceStderr
            & pure
            & inNixSandbox [(dir, ReadOnly)] Nothing
        exit `shouldBeM` ExitFailure 1

    it "creates an empty $HOME" $ do
      StdoutTrimmed out <-
        failIfErr
          $ cmd "sh"
          & addArgs ["-c", "ls -l $HOME" :: String]
          & pure
          & inNixSandbox [] Nothing
      out `shouldBeM` "total 0"

    it "can create cache home with just a nix directory" $ do
      StdoutTrimmed out <-
        failIfErr
          $ cmd "sh"
          & addArgs ["-c", "find $XDG_CACHE_HOME" :: String]
          & pure
          & inNixSandbox [] Nothing
      (sort . lines . cs $ out) `shouldBeM` ["/home/nix-runner/.cache", "/home/nix-runner/.cache/nix", "/home/nix-runner/.cache/nix/gitv3"]

    it "passes through the given cache home as $XDG_CACHE_HOME" $ do
      withSystemTempDirectory "cache-home" $ \cacheHome -> do
        liftIO $ writeFile (cacheHome </> "file") ""
        StdoutTrimmed out <-
          failIfErr
            $ cmd "sh"
            & addArgs ["-c", "find $XDG_CACHE_HOME" :: String]
            & pure
            & inNixSandbox [] (Just cacheHome)
        (sort . lines . cs $ out) `shouldBeM` ["/home/nix-runner/.cache", "/home/nix-runner/.cache/file", "/home/nix-runner/.cache/nix", "/home/nix-runner/.cache/nix/gitv3"]

    it "clears the environment" $ do
      liftBaseOp_ (withModifiedEnvironment [("SOME_SECRET", "psst")]) $ do
        StdoutTrimmed out <-
          failIfErr
            $ cmd "sh"
            & addArgs ["-c", "echo $SOME_SECRET" :: String]
            & pure
            & inNixSandbox [] Nothing
        out `shouldBeM` ""

    it "allows passing environment set with cradle through" $ do
      StdoutTrimmed out <-
        failIfErr
          $ cmd "sh"
          & addArgs ["-c", "echo $FOO" :: String]
          & modifyEnvVar "FOO" (const $ Just "bar")
          & pure
          & inNixSandbox [] Nothing
      out `shouldBeM` "bar"

    it "does not mount any netrc files if not specified" $ do
      () <-
        failIfErr
          $ cmd "sh"
          & addArgs ["-c", "test ! -e $(nix --extra-experimental-features \"nix-command\" config show | grep netrc-file | awk '{ print $3 }')" :: String]
          & pure
          & inNixSandbox [] Nothing
      pure ()

    it "mounts in netrc file path if specified" $ do
      let dummyUser = GhLogin "huhu"
      token <- DB.getUserInternalToken dummyUser
      withInternalCacheToken dummyUser $ do
        nixConfig <- view #userNixConfig
        StdoutTrimmed out <-
          failIfErr
            $ cmd "sh"
            & addArgs ["-c", "cat $(nix config show | grep netrc-file | awk '{ print $3 }')" :: String]
            & addNixConfigEnvironment nixConfig
            & pure
            & inNixSandbox [] Nothing
        out
          `shouldBeM` (T.strip . T.unlines)
            [ "machine cache.garnix.io",
              "login " <> cs (getGhLogin dummyUser),
              "password " <> cs (getInternalCacheToken token)
            ]

    it "allows modifying $PATH and $NIX_CONFIG" $ do
      StdoutRaw out <-
        failIfErr
          $ cmd "sh"
          & addArgs ["-c", "echo $PATH ; echo $NIX_CONFIG" :: String]
          & modifyEnvVar "PATH" (const $ Just "/bin:/some-path")
          & modifyEnvVar "NIX_CONFIG" (const $ Just "some-nix-config")
          & pure
          & inNixSandbox [] Nothing
      out `shouldBeM` "/bin:/some-path\nsome-nix-config\n"

    it "has access to make TLS requests in the sandbox" $ do
      () <-
        failIfErr
          $ cmd "curl"
          & addArgs ["https://garnix.io" :: String]
          & silenceStdout
          & pure
          & inNixSandbox [] Nothing
      pure ()

    it "mounts `~/.config/nix/nix.conf`, if it exists (to avoid github rate limiting in tests on the action-runner)" $ do
      liftBaseOp (withSystemTempDirectory "garnix") $ \home -> do
        liftBaseOp_ (withModifiedEnvironment [("HOME", home)]) $ do
          liftIO $ do
            createDirectoryIfMissing True (home </> ".config/nix")
            writeFile (home </> ".config/nix/nix.conf") ""
          StdoutTrimmed out <-
            failIfErr
              $ cmd "sh"
              & addArgs ["-c", "cd ~/.config/nix ; ls" :: String]
              & silenceStdout
              & pure
              & inNixSandbox [] Nothing
          out `shouldBeM` "nix.conf"

    -- bwrap's --die-with-parent is tied to the OS thread that forked it, and one
    -- capability makes the RTS retire surplus worker threads often. Forked from
    -- an ordinary thread, a few of these die with -9 in most runs.
    -- ponytail: probabilistic; a miss is a false pass, never a false failure.
    it "is not killed when the RTS retires the worker thread that forked it" $ do
      caps <- liftIO getNumCapabilities
      let churn = forever (run (cmd "true") :: IO (ExitCode, StdoutRaw, StderrRaw))
      codes <-
        bracket_ (liftIO $ setNumCapabilities 1) (liftIO $ setNumCapabilities caps)
          $ bracket (liftIO $ replicateM 32 (Async.async churn)) (liftIO . mapM_ Async.cancel)
          $ \_ -> forConcurrently [1 .. 40 :: Int] $ \_ -> do
            (code, StdoutRaw _, StderrRaw _) <-
              cmd "sleep" & addArgs ["5" :: String] & pure & inNixSandbox [] Nothing
            pure code
      filter (/= ExitSuccess) codes `shouldBeM` []

    -- The fork happens on a bound thread; cancelling the caller has to reach
    -- it, or a build timeout would leave the sandbox running.
    it "tears the sandbox down when the caller times out" $ do
      marker :: Text <- liftIO $ ("30." <>) . show <$> randomRIO (100000 :: Int, 999999)
      let running = do
            StdoutRaw ps <- run (cmd "ps" & addArgs ["-eo", "args" :: String])
            pure $ length $ filter (marker `T.isInfixOf`) $ T.lines $ cs ps
      result :: Maybe ExitCode <-
        timeout (fromSeconds @Int 1) $ cmd "sleep" & addArgs [marker] & pure & inNixSandbox [] Nothing
      result `shouldBeM` Nothing
      waitFor (fromSeconds @Int 5) $ running `shouldReturnM` 0

failIfErr :: (MonadIO m, HasCallStack) => m (output, StderrRaw, ExitCode) -> m output
failIfErr action = do
  (output, StderrRaw err, code) <- action
  case code of
    ExitSuccess -> pure output
    ExitFailure e -> do
      liftIO
        . assertFailure
        $ "Command exited with unexpected failure. Exit code: "
        <> cs (show e)
        <> "\nStderr: \n"
        <> cs err
