module Garnix.DeploySpec (spec) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Lens
import Cradle
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Text.IO qualified as T
import Data.Time.Calendar as Time
import Data.Time.Clock (UTCTime (..))
import Data.Tuple.Extra ((&&&))
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.GhWebhooks (ghWebhookPullRequest)
import Garnix.Build (buildFlake)
import Garnix.Build.Checkout qualified as Build.Checkout
import Garnix.Build.Helpers (withPrivateNixXdgCache)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (setExtraUsageLimits)
import Garnix.Entitlements qualified as Entitlements
import Garnix.Hosting.Deploy
import Garnix.Hosting.ServerPool (sshArgsFor)
import Garnix.Hosting.ServerPool.Types (ServerTier (..))
import Garnix.Monad
import Garnix.Monad.Async (emptyPromise, resolve)
import Garnix.MonetaryCost
import Garnix.Orchestrator qualified as Orchestrator
import Garnix.Prelude hiding (head)
import Garnix.GithubInterface (githubRepoInfo)
import Garnix.TestHelpers.GithubInterface.Internal (fakeRepoInfo)
import Garnix.TestHelpers hiding (shouldReturn)
import Garnix.TestHelpers.Common
import Garnix.TestHelpers.Deprecated qualified as Deprecated
import Garnix.TestHelpers.HetznerMock
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.Reporter (TestReport (..), withTestReporter_)
import Garnix.TestHelpers.ServerPool
import Garnix.Types hiding (context)
import Garnix.YamlConfig (DeploySection (OnPullRequest), GarnixConfig, ServerSection (ServerSection), serverSection)
import GitHub.Data.Id qualified as Github.Data
import GitHub.Data.Webhooks.Events (CheckSuiteEvent (..), EventHasRepo (..), PullRequestEvent, senderOfEvent)
import GitHub.Data.Webhooks.Payload (HookCheckSuite (..), HookRepository (..), whUserLogin)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit (assertFailure)
import Test.Hspec hiding (shouldThrow)

spec :: Spec
spec = do
  describe "rolloutNewServerVersion @slow"
    $ before truncateDB
    $ after_ stopActiveServers
    $ around_ Deprecated.quietWhenPassing
    $ aroundAll_ withServerPool
    $ do
      it "deploys a new server" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          writeMatchingConfig branch (PackageName "default")
          servers <- rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          liftIO $ length servers `shouldBe` 1
          forM_ servers $ \server -> server `shouldHaveState` "running"
          case servers ^.. traversed . readyAt of
            res | any isNothing res -> liftIO $ assertFailure "expected ready flag to be set in returned serverInfo"
            _ -> pure ()

      it "does not deploy on any failing builds" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          writeMatchingConfig branch (PackageName "myHost")
          _ <- doABuild flakeWithFailingBuilds event repoInfo
          serverLogs <- getDeployLogsDB
          liftIO $ serverLogs `shouldBe` []

      it "does nothing if the branch doesn't match" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          withContext (defaultEvent & eventBranch ?~ "something-else") $ \_ branch ->
            writeMatchingConfig branch (PackageName "default")
          servers <- rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          liftIO $ length servers `shouldBe` 0

      it "does nothing if there are no servers to deploy" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          writeUnmatchingConfig
          servers <- rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          liftIO $ length servers `shouldBe` 0

      it "deletes old servers" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          writeMatchingConfig branch (PackageName "default")
          firstGenServers <- rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          void $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          forM_ firstGenServers assertNotExists

      context "persistence" $ do
        let commitInfo c = CommitInfo "owner" (RepoIsPublic True) (fakeRepoInfo "owner" "repo") (Just "branch") Nothing c
            flake = flakeWithPersistence True "db" "db" "local"
            yaml = Just $ getMultiConfig "branch" [PackageName "db"]
            branch = "branch"
            sshServer serverInfo args = do
              (ip, sshArgs) <- sshArgsFor serverInfo
              StdoutRaw stdout <- run $ cmd "ssh" & addArgs (sshArgs <> (("garnix@" <> ip) : args))
              pure stdout
        context "build" $ around_ Deprecated.addTestSecrets $ do
          it "build fails when persistence is enabled and name is empty" $ do
            (Left error) <- Deprecated.withMockRepo (cs $ flakeWithPersistence True "db" "" "local") yaml branch $ \_mockGithubRepo commit -> do
              addBranchHostingEntitlement
              withMockReturning #executeDeployPlanMock [] $ do
                resolve =<< buildFlake mempty (commitInfo commit)
                (_, _, plan1, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
                liftIO $ plan1 ^.. #toSpinUp . traverse . #build . persistenceName `shouldBe` [Just "db"]
            err error `shouldBe` DeploymentWantsNixosConfigurationsThatDontExist [PackageName "db"]

        context "planning" $ around_ Deprecated.addTestSecrets $ do
          it "ignores persistence names if persistence is not enabled" $ do
            result <- Deprecated.withMockRepo (cs $ flakeWithPersistence False "db" "db" "local") yaml branch $ \_mockGithubRepo commit -> do
              addBranchHostingEntitlement
              withMockReturning #executeDeployPlanMock [] $ do
                resolve =<< buildFlake mempty (commitInfo commit)
                (_, _, plan1, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
                liftIO $ plan1 ^.. #toSpinUp . traverse . #build . persistenceName `shouldBe` [Nothing]
            result `shouldBe` Right ()

          it "fails if persistence name is not a valid subdomain" $ do
            (Left error) <- Deprecated.withMockRepo (cs $ flakeWithPersistence True "db/invalid-name" "db" "local") yaml branch $ \_mockGithubRepo commit -> do
              addBranchHostingEntitlement
              withMockReturning #executeDeployPlanMock [] $ do
                resolve =<< buildFlake mempty (commitInfo commit)
            err error `shouldBe` NameIsNotValidSubdomain PersistenceNameSubdomain "db/invalid-name"

          it "correctly reads and stores the persistence name" $ do
            result <- Deprecated.withMockRepo flake yaml branch $ \_mockGithubRepo commit -> do
              addBranchHostingEntitlement
              withMockReturning #executeDeployPlanMock [] $ do
                resolve =<< buildFlake mempty (commitInfo commit)
                (_, _, plan1, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
                liftIO $ plan1 ^.. #toSpinUp . traverse . #build . persistenceName `shouldBe` [Just "db"]
            result `shouldBe` Right ()

          it "plans to redeploy to the same server" $ do
            result <- Deprecated.withMockRepo flake yaml branch $ \_mockGithubRepo commit -> do
              addBranchHostingEntitlement
              withMockReturning #executeDeployPlanMock [] $ do
                now <- liftIO getCurrentTime
                [existingBuild] <- createBuildsFor "owner" "repo" "branch" "prevcommit" [("db", Just "db")]
                void $ addTestServer $ \server ->
                  server
                    & configurationBuildId .~ (existingBuild ^. id)
                    & readyAt ?~ now
                    & endedAt .~ Nothing

                resolve =<< buildFlake mempty (commitInfo commit)

                (_, _, plan1, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock

                liftIO $ length (plan1 ^. #toSpinUp) `shouldBe` 0
                liftIO $ length (plan1 ^. #toSpinDown) `shouldBe` 0
                liftIO $ length (plan1 ^. #toRedeploy) `shouldBe` 1
            result `shouldBe` Right ()

        it "reuses servers @slow" $ Deprecated.addTestSecrets $ do
          result <- Deprecated.withMockRepo flake yaml branch $ \mockGithubRepo commit -> do
            addBranchHostingEntitlement

            resolve =<< buildFlake mempty (commitInfo commit)
            firstGenServer <- fromSingleton <$> getAllDbServers

            void $ sshServer firstGenServer ["sudo", "touch", "/hello"]

            liftIO $ writeFile (mockGithubRepo </> "flake.nix") (cs $ flakeWithPersistence True "db" "db" "second")
            commit2 <- commitAll mockGithubRepo
            resolve =<< buildFlake mempty (commitInfo commit2)
            secondGenServers <- getAllDbServers

            liftIO $ length secondGenServers `shouldBe` 1
            let secondGen = fromSingleton secondGenServers

            stdout <- sshServer secondGen ["sudo", "ls", "/hello"]

            liftIO $ do
              firstGenServer ^. configurationBuildId `shouldNotBe` secondGen ^. configurationBuildId
              firstGenServer ^. id `shouldBe` secondGen ^. id
              firstGenServer ^. ipv4Addr `shouldBe` secondGen ^. ipv4Addr
              stdout `shouldBe` "/hello\n"

          result `shouldBe` Right ()

      it "does not mark server as ready from a failed deployment" $ do
        let event = defaultEvent
        let packages = [PackageName "first", PackageName "second"]
        let sort' = sortOn (getHashId . getServerId . _serverInfoId)
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild (makeMultiFlake packages) event repoInfo
          writeMultiConfig branch packages
          void $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          firstGenServers <- sort' <$> getAllDbServers
          liftIO $ length firstGenServers `shouldBe` 2

          sync <- liftIO newEmptyMVar
          void
            $ try
            $ withMock #startServerMock (startServerAndFailOnAllExcept sync "first")
            $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          secondGenServers <- (\\ firstGenServers) . sort' <$> getAllDbServers

          liftIO $ fmap (^. endedAt) firstGenServers `shouldBe` [Nothing, Nothing]
          liftIO $ length secondGenServers `shouldBe` 1
          liftIO $ fmap (^. readyAt) secondGenServers `shouldBe` [Nothing]

      it "deletes servers from previous deploys that did not successfully initialize" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          writeMatchingConfig branch (PackageName "default")
          firstGenServers <-
            withMock
              #waitTillServerIsInitializedMock
              (const $ return False)
              $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
              `catchError` const (pure [])
          liftIO $ length firstGenServers `shouldBe` 1
          void $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          forM_ firstGenServers $ \server ->
            assertNotExists server

      it "does not delete servers from a different branch" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo1 <- doABuild simpleFlake event repoInfo
          writeMatchingConfig branch (PackageName "default")
          firstGenServers <- rolloutNewServerVersion mempty commitInfo1 (BranchDeployment branch)
          liftIO $ length firstGenServers `shouldBe` 1
          let event2 = defaultEvent & eventBranch ?~ "some-other-branch"
          withContext event2 $ \repoInfo branch -> do
            writeUnmatchingConfig
            commitInfo2 <- doABuild (simpleFlake' "Some other description") event2 repoInfo
            writeMatchingConfig branch (PackageName "default")
            secondGenServers <- rolloutNewServerVersion mempty commitInfo2 (BranchDeployment branch)
            liftIO $ length secondGenServers `shouldBe` 1
            forM_ firstGenServers $ \server ->
              server `shouldHaveState` "running"

      it "deploys the repo key to /var/garnix/keys/repo-key (only root readable)" $ do
        let event = defaultEvent
        runTestM $ withContext event $ \repoInfo branch -> do
          commitInfo <- doABuild simpleFlake event repoInfo
          writeMatchingConfig branch (PackageName "default")
          [server] <- rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
          (ip, sshArgs) <- sshArgsFor server
          StdoutRaw result <-
            run $ cmd "ssh"
              & addArgs
                (sshArgs <> ["root@" <> cs ip, "cat /var/garnix/keys/repo-key"])

          liftIO $ cs result `shouldStartWith` "AGE-SECRET-KEY"

      context "stopUnusedServers" $ do
        let commitInfo c = CommitInfo "owner" (RepoIsPublic True) (fakeRepoInfo "owner" "repo") (Just "branch") Nothing c
            flake = flakeWithPersistence True "db" "db" "local"
            yaml = Just $ onPullRequestConfig (PackageName "db")
            branch = "branch"
            runPrEvent ci = do
              resolve =<< buildFlake mempty ci
              void $ Build.Checkout.withCheckout ci $ rolloutNewServerVersion mempty ci (GhPrDeployment 1)

        it "stops servers that do not have a heartbeat" $ Deprecated.addTestSecrets $ do
          result <- Deprecated.withMockRepo flake yaml branch $ \_mockGithubRepo commit -> do
            let ci = commitInfo commit
            addPrHostingEntitlement

            runPrEvent ci
            before <- fromSingleton <$> getAllDbServers
            void
              $ DB.pgExec
                [pgSQL|
                  UPDATE servers
                    SET ready_at = (ready_at - interval '13 hours')
                |]
            stopUnusedServers
            after <- fromSingleton <$> getAllDbServers

            liftIO $ do
              before ^. endedAt `shouldBe` Nothing
              after ^. endedAt `shouldNotBe` Nothing
              before ^. id `shouldBe` after ^. id

          result `shouldBe` Right ()

        it "does not stop servers were started recently" $ Deprecated.addTestSecrets $ do
          result <- Deprecated.withMockRepo flake yaml branch $ \_mockGithubRepo commit -> do
            let ci = commitInfo commit
            addPrHostingEntitlement

            runPrEvent ci
            before <- fromSingleton <$> getAllDbServers
            stopUnusedServers
            after <- fromSingleton <$> getAllDbServers

            liftIO $ do
              before ^. endedAt `shouldBe` Nothing
              after ^. endedAt `shouldBe` Nothing
              before ^. id `shouldBe` after ^. id

          result `shouldBe` Right ()

        it "does not stop servers with a heartbeat" $ Deprecated.addTestSecrets $ do
          result <- Deprecated.withMockRepo flake yaml branch $ \_mockGithubRepo commit -> do
            let ci = commitInfo commit
            addPrHostingEntitlement

            runPrEvent ci
            before <- fromSingleton <$> getAllDbServers
            void
              $ DB.pgExec
                [pgSQL|
                  UPDATE servers
                    SET ready_at = (ready_at - interval '13 hours')
                    WHERE servers.id = ${before ^. id}
                |]
            hosts <- DB.getAllRunningHosts
            void $ DB.upsertHeartbeat $ fmap hostToDomainName hosts
            stopUnusedServers
            after <- fromSingleton <$> getAllDbServers

            liftIO $ do
              before ^. endedAt `shouldBe` Nothing
              after ^. endedAt `shouldNotBe` Nothing
              before ^. id `shouldBe` after ^. id

          result `shouldBe` Right ()

        it "does not stop branch servers" $ Deprecated.addTestSecrets $ do
          let branchYaml = Just $ getMultiConfig branch [PackageName "db"]
          result <- Deprecated.withMockRepo flake branchYaml branch $ \_mockGithubRepo commit -> do
            let ci = commitInfo commit
            addBranchHostingEntitlement

            resolve =<< buildFlake mempty ci
            before <- fromSingleton <$> getAllDbServers
            void
              $ DB.pgExec
                [pgSQL|
                  UPDATE servers
                    SET ready_at = (ready_at - interval '13 hours')
                    WHERE servers.id = ${before ^. id}
                |]
            stopUnusedServers
            after <- fromSingleton <$> getAllDbServers

            liftIO $ do
              before ^. endedAt `shouldBe` Nothing
              after ^. endedAt `shouldBe` Nothing
              before ^. id `shouldBe` after ^. id

          result `shouldBe` Right ()

      describe "deployment reporting" $ do
        it "stores deploy logs of failing deployments" $ do
          let event = defaultEvent
          runTestM $ withContext event $ \repoInfo branch -> do
            withMock #setupServerMock (\_ -> throw $ ProvisioningError "test error") $ do
              commitInfo <- doABuild simpleFlake event repoInfo
              writeMatchingConfig branch (PackageName "default")
              Left result <- try $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branch)
              liftIO $ err result `shouldBe` ProvisioningError "test error"
              [(_id, deployLogs)] <- getDeployLogsDB
              liftIO $ deployLogs `shouldBe` "Error provisioning server: test error\n"

        it "reports failed deployments to github" $ do
          let event = defaultEvent
          runTestM $ withContext event $ \repoInfo branch -> do
            withMock #setupServerMock (\_ -> throw $ ProvisioningError "test error") $ do
              commitInfo <- doABuild simpleFlake event repoInfo
              writeMatchingConfig branch (PackageName "default")
              result <- withTestReporter_ $ \reporter ->
                void $ try $ rolloutNewServerVersion reporter commitInfo (BranchDeployment branch)
              let (Just testReport) = result ^? ix "deployment default"
              (testReport ^. #success) `shouldBeM` Just False
              liftIO $ testReport ^. #logs . to cs `shouldStartWith` "Error provisioning server: test error"

        it "reports failed activations to github" $ do
          let event = defaultEvent
              testServerInfo =
                ServerInfo
                  { _serverInfoId = ServerId $ 1 ^. from hashIdInt,
                    _serverInfoHetznerServerId = HetznerServerId 20950838,
                    _serverInfoIpv4Addr = "<none>",
                    _serverInfoIpv6Addr = "<none>",
                    _serverInfoCreatedAt = error "not used",
                    _serverInfoEndedAt = Nothing,
                    _serverInfoConfigurationBuildId = BuildId $ 123 ^. from hashIdInt,
                    _serverInfoPullRequest = Nothing,
                    _serverInfoReadyAt = Nothing,
                    _serverInfoBuildPersistenceName = Nothing,
                    _serverInfoTier = def,
                    _serverInfoIsPrimary = False
                  }
              expectedError =
                ActivationError
                  ( testServerInfo
                      & ipv4Addr
                        .~ "12.34.56.78"
                      & ipv6Addr
                        .~ "0123:4567:89ab:cdef::/64"
                  )
                  "some stderr"
          runTestM $ withContext event $ \repoInfo branch -> do
            withMock #setupServerMock (\_ -> throw expectedError) $ do
              commitInfo <- doABuild simpleFlake event repoInfo
              writeMatchingConfig branch (PackageName "default")
              result <- withTestReporter_ $ \reporter -> do
                void $ try $ rolloutNewServerVersion reporter commitInfo (BranchDeployment branch)
              let (Just testReport) = result ^? ix "deployment default"
              (testReport ^. #success) `shouldBeM` Just False
              liftIO $ testReport ^. #logs . to cs `shouldStartWith` "Failed to activate server\nYou may be able to debug this by sshing into 12.34.56.78 or 0123:4567:89ab:cdef::/64\nStderr:\nsome stderr\n"

        it "reports successful deployments to github" $ do
          let event = defaultEvent
          runTestM $ withContext event $ \repoInfo branch -> do
            commitInfo <- doABuild simpleFlake event repoInfo
            writeMatchingConfig branch (PackageName "default")
            reports <- withTestReporter_ $ \reporter ->
              void $ try $ rolloutNewServerVersion reporter commitInfo (BranchDeployment branch)
            let logs' = cs $ (reports ! "deployment default") ^. #logs
            liftIO $ do
              logs' `shouldContain` "Server has been successfully deployed to: https://default.branch.repo.owner.garnix.me"
              logs' `shouldContain` "starting the following units:"

        it "includes activate script output on failures" $ do
          let event = defaultEvent
          runTestM $ withContext event $ \repoInfo branch -> do
            commitInfo <- doABuild flakeWithFailingActivation event repoInfo
            writeMatchingConfig branch (PackageName "default")
            reports <- withTestReporter_ $ \reporter -> do
              void $ try $ rolloutNewServerVersion reporter commitInfo (BranchDeployment branch)
            let logs' = cs $ (reports Map.! "deployment default") ^. #logs
            liftIO $ do
              logs' `shouldContain` "Failed to activate server"
              logs' `shouldContain` "activationFailure: command not found"

        it "report redeployment of persistent server" $ do
          let event = defaultEvent
          runTestM $ withContext event $ \repoInfo branch -> do
            let flake = flakeWithPersistenceAndConfig True "db" "db" "local"
            _ <- doABuild flake event repoInfo
            let flake2 = flakeWithPersistenceAndConfig True "db" "db" "local2"
            commitInfo <- doABuild flake2 event repoInfo
            let builds = DB.getBuildsByCommit (repoInfo ^. ghRepoOwner) (repoInfo ^. ghRepoName) (commitInfo ^. commit)
            build <- fromSingleton . filter (\p -> p ^. packageType == TypeNixosConfiguration) <$> builds
            secondGenServers <- getAllDbServers
            let serverInfo2 = fromSingleton secondGenServers
            reports <- withTestReporter_ $ \reporter -> do
              void $ redeployServer reporter commitInfo (BranchDeployment branch) serverInfo2 build
            let logs' = cs $ (reports Map.! "redeployment db") ^. #logs
            liftIO $ do
              logs' `shouldContain` "Server has been successfully redeployed to: https://db.branch.repo.owner.garnix.me"

  describe "branch deployments" $ inM $ beforeM_ (truncateDBM >> addBranchHostingEntitlement) $ do
    let user = "owner"
        name = "repo"
        commit = "aaaa"
        branchName = "branch"
    it "allows deploying 0 servers"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        result <- try $ deployNewServerFor "owner" "repo" "branch" "aaaaaa" []
        liftIO $ result `shouldBe` Right []

    it "allows deploying 1 server"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        let machineName = "test-machine"
        void $ deployNewServerFor user name branchName commit [(machineName, Nothing)]
        (_, _, plan, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
        liftIO $ length (plan ^. #toSpinUp) `shouldBe` 1

    it "blocks deploying when going over the limit"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        let machineNames = (\p -> (PackageName p, Nothing)) <$> ["first", "second", "third"]
        entitlementError <- try $ deployNewServerFor user name branchName commit machineNames
        liftIO
          $ first err entitlementError
          `shouldBe` Left
            ( EntitlementError
                $ "Deploying this would result in $15.00 extra monthly server cost above your plan. "
                <> "You have not configured any additional spending budget on deployed hosts. "
                <> "You can configure this spending limit on your account page at garnix.io.\n\n"
                <> "Here is a breakdown of what would be deployed by this commit and the associated costs:\n"
                <> "  i2x4 (x3) = $15.00 (2 included in plan)\n"
            )

    it "allows deploying one extra server given a custom limit"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        let machineNames = (\p -> (PackageName p, Nothing)) <$> ["first", "second", "third"]
        Entitlements.setExtraUsageLimits user (emptyUsageLimits & #hostingSpend .~ usd 16)
        void $ deployNewServerFor user name branchName commit machineNames
        (_, _, plan, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
        liftIO $ length (plan ^. #toSpinUp) `shouldBe` 3

    it "does not allow deploying extra servers if the limit is not set high enough"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        let machineNames = (\p -> (PackageName p, Nothing)) <$> ["first", "second", "third"]
        Entitlements.setExtraUsageLimits user (emptyUsageLimits & #hostingSpend .~ usd 14)
        entitlementError <- try $ deployNewServerFor user name branchName commit machineNames
        liftIO
          $ first err entitlementError
          `shouldBe` Left
            ( EntitlementError
                $ "Deploying this would result in $15.00 extra monthly server cost above your plan. "
                <> "However, you have configured a max spend of $14.00 per month on deployed hosts. "
                <> "You can configure this spending limit on your account page at garnix.io.\n\n"
                <> "Here is a breakdown of what would be deployed by this commit and the associated costs:\n"
                <> "  i2x4 (x3) = $15.00 (2 included in plan)\n"
            )

    it "does apply the hosting limit over all branches"
      $ withMockReturning #executeDeployPlanMock []
      $ do
        let anotherCommit = "bbbb"
            anotherBranch = "another-branch"
            machineNames = (\p -> (PackageName p, Nothing)) <$> ["first", "second"]
        now <- liftIO getCurrentTime
        existingBuild <-
          fromSingleton
            <$> createBuildsFor user name anotherBranch anotherCommit [("existing-test-machine", Nothing)]
        void $ addTestServer $ \server ->
          server
            & configurationBuildId .~ (existingBuild ^. id)
            & readyAt ?~ now
            & endedAt .~ Nothing

        entitlementError <- try $ deployNewServerFor user name branchName commit machineNames

        liftIO
          $ first err entitlementError
          `shouldBe` Left
            ( EntitlementError
                $ "Deploying this would result in $15.00 extra monthly server cost above your plan. "
                <> "You have not configured any additional spending budget on deployed hosts. "
                <> "You can configure this spending limit on your account page at garnix.io.\n\n"
                <> "Here is a breakdown of what would be deployed by this commit and the associated costs:\n"
                <> "  i2x4 (x3) = $15.00 (2 included in plan)\n"
            )

    it "allows specifying a primary domain deployment" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        let garnixYaml =
              unindent
                [i|
                  servers:
                    - configuration: foo
                      deployment:
                        type: on-branch
                        branch: #{getBranch branchName}
                        isPrimary: true
                    - configuration: bar
                      deployment:
                        type: on-branch
                        branch: #{getBranch branchName}
                |]
        dir <- view #workingDir
        liftIO $ T.writeFile (dir </> "garnix.yaml") $ cs garnixYaml
        void $ createBuildsFor user name branchName commit [("foo", Nothing), ("bar", Nothing)]
        iAuth <- getInstallation $ Github.Data.Id 42
        let repoInfo = githubRepoInfo iAuth (ForgeToken "test-token") user name
        let commitInfo = CommitInfo (getRepoOwner user) (RepoIsPublic True) repoInfo (Just branchName) Nothing commit
        void
          $ withPrivateNixXdgCache
          $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branchName)
        (_, _, plan, _) <- fromSingleton <$> getMockCalls #executeDeployPlanMock
        Set.fromList
          ( plan
              ^.. #toSpinUp
                . each
                . to ((^. #build . package) &&& (^. #domainIsPrimary))
          )
          `shouldBeM` Set.fromList [("foo", True), ("bar", False)]

  let wrap =
        inM
          . beforeM_ truncateDBM
          . aroundM_ (withTestEntitlement "test" (maximumPrDeploymentTime .~ fromMinutes @Int 2) "test-owner" . suppressLogsWhenPassing)
  describe "pull-request-deployments" $ wrap $ do
    let shouldHavePlan ::
          (HasCallStack) =>
          (Reporter, CommitInfo, DeployPlan, DeploymentType) ->
          (DeploymentType, [BuildId], [(CommitHash, PackageName)]) ->
          M ()
        shouldHavePlan (_, _, plan, deploymentType) expected =
          liftIO
            $ ( deploymentType,
                plan ^. #toSpinDown . to (fmap (^. configurationBuildId)),
                plan
                  ^. #toSpinUp
                    . to (fmap (\s -> (s ^. #build . gitCommit, s ^. #build . package)))
              )
            `shouldBe` expected
        planSpunUpBuildIds :: (a, b, DeployPlan, c) -> [BuildId]
        planSpunUpBuildIds (_, _, plan, _) = plan ^. #toSpinUp . to (fmap (^. #build . id))

    it "deploys servers for a PR with `on-pull-request`" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

    it "does not deploy when on-pull-request is not set" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ []
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [])

    it "does not deploy on wrong package type" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "Build starting"
            & packageType .~ TypeOverall
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-conoofig"
            & packageType .~ TypePackage
            & uploadedToCache ?~ True
        result <- try $ Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        liftIO $ first err result `shouldBe` Left (DeploymentWantsNixosConfigurationsThatDontExist [PackageName "test-nix-config"])

    it "works if there are other (non-nixosConfig) packages with the same name" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & packageType .~ TypePackage
            & uploadedToCache ?~ True
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & packageType .~ TypeNixosConfiguration
            & uploadedToCache ?~ True
        Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

    it "does not deploy from external forks" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        p <- emptyPromise
        withMockReturning #buildFlakeMock p $ do
          commit <-
            Deprecated.writeMockRemote "test-branch"
              $ def
              & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
          let prEvent =
                mkPullRequestEvent commit "test-branch" "other-owner/repo-fork" "owner/repo" testInstallationId
                  & number .~ 42
          _ <- testBuild $ \build ->
            build
              & fromPrEvent prEvent
              & package .~ "test-nix-config"
              & uploadedToCache ?~ True
          ghWebhookPullRequest prEvent >>= resolve
          calls <- getMockCalls #executeDeployPlanMock
          liftIO $ null calls `shouldBe` True

    it "does not deploy invalid subdomains" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "foo/bar" OnPullRequest]
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "foo/bar"
            & uploadedToCache ?~ True
        result <- try $ Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        liftIO $ first err result `shouldBe` Left (NameIsNotValidSubdomain PackageNameSubdomain "foo/bar")

    it "does deploy when (unused) branch name is not a valid subdomain" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "sh/some-feature"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        let prEvent =
              mkPullRequestEvent commit "sh/some-feature" "test-owner/test-repo" "test-owner/test-repo" testInstallationId
                & number .~ 42
        _ <- testBuild $ \build ->
          build
            & fromPrEvent prEvent
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        ghWebhookPullRequest prEvent >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

    it "deploys multiple servers" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "pkg-a" OnPullRequest, ServerSection "pkg-b" OnPullRequest]
        let prEvent =
              mkPullRequestEvent commit "test-branch" "test-owner/test-repo" "test-owner/test-repo" testInstallationId
                & number .~ 42
        _ <- testBuild $ \build ->
          build
            & fromPrEvent prEvent
            & package .~ "pkg-a"
            & uploadedToCache ?~ True
        _ <- testBuild $ \build ->
          build
            & fromPrEvent prEvent
            & package .~ "pkg-b"
            & uploadedToCache ?~ True
        ghWebhookPullRequest prEvent >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "pkg-a"), (commit, "pkg-b")])

    it "does not deploy any servers if a configuration fails" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _build <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & status ?~ Failure
            & uploadedToCache ?~ True
        promise <- Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42
        result <- try $ resolve promise
        liftIO $ first err result `shouldBe` Left (OtherError "test-nix-config failed")

    it "does not deploy any servers if a configuration times out" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _build <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & status ?~ Timeout
            & uploadedToCache ?~ True
        promise <- Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42
        result <- try $ resolve promise
        liftIO $ first err result `shouldBe` Left (OtherError "test-nix-config timed out")

    it "shuts down old servers from the same pull request" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commitA <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        let prEvent =
              mkPullRequestEvent commitA "test-branch" "test-owner/test-repo" "test-owner/test-repo" testInstallationId
                & number .~ 42
        buildA <- testBuild $ \build ->
          build
            & fromPrEvent prEvent
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        ghWebhookPullRequest prEvent >>= resolve
        void $ addTestServer $ \server ->
          server
            & configurationBuildId .~ (buildA ^. id)
            & pullRequest ?~ PullRequestId (fromIntegral $ prEvent ^. number)

        mockRemote <- view #workingDir
        liftIO $ writeFile (mockRemote </> "some-added-file") "foo"
        commitB <- commitAll mockRemote
        let prEvent =
              mkPullRequestEvent commitB "test-branch" "test-owner/test-repo" "test-owner/test-repo" testInstallationId
                & number .~ 42
        _ <- testBuild $ \build ->
          build
            & fromPrEvent prEvent
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        ghWebhookPullRequest prEvent >>= resolve
        [planA, planB] <- getMockCalls #executeDeployPlanMock
        planA `shouldHavePlan` (GhPrDeployment 42, [], [(commitA, "test-nix-config")])
        planB `shouldHavePlan` (GhPrDeployment 42, planSpunUpBuildIds planA, [(commitB, "test-nix-config")])

    it "does not shut down `on-branch` servers from the same branch" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        build <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        void $ addTestServer $ \server ->
          server
            & configurationBuildId .~ (build ^. id)
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

    it "aborts when the repoOwner doesn't have the pr hosting entitlement" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        void $ DB.pgExec [pgSQL| TRUNCATE repo_owner_has_product |]
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        _ <- testBuild $ \build ->
          build
            & fromPrEvent (mkPrEvent commit)
            & package .~ "test-nix-config"
            & uploadedToCache ?~ True
        iAuth <- getInstallation $ Github.Data.Id 42
        let repoInfo = githubRepoInfo iAuth (ForgeToken "test-token") "test-owner" "test-repo"
        let commitInfo = CommitInfo "test-owner" (RepoIsPublic True) repoInfo Nothing Nothing commit
        reports <- withTestReporter_ $ \testReporter -> do
          void $ try $ withPrivateNixXdgCache $ rolloutNewServerVersion testReporter commitInfo (GhPrDeployment 42)
        reports
          `shouldBeM` ( "deployment plan"
                          ~> TestReport
                            "Sorry, hosting is not allowed for test-owner."
                            (Just False)
                      )

    it "does not deploy if all PR deploy minutes are used up" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]

        now <- liftIO getCurrentTime
        result <-
          try
            $ deployBranchPRServers
              commit
              [ \server ->
                  server
                    & readyAt ?~ subTime (fromDays @Int 1) now
                    & endedAt .~ Nothing
              ]
        liftIO $ first err result `shouldBe` Left (EntitlementError "Sorry, you have exhausted all your PR deployment minutes.")
        calls <- getMockCalls #executeDeployPlanMock
        liftIO $ length calls `shouldBe` 0

    it "allows going over PR deploy minutes if extra usage minutes are configured" $ do
      setExtraUsageLimits "test-owner" (emptyUsageLimits & #prDeployTime .~ fromMinutes @Int 5)
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]

        now <- liftIO getCurrentTime
        let firstOfMonth = flip UTCTime 0 . (\(y, m, _d) -> Time.fromGregorian y m 1) . Time.toGregorian $ utctDay now
        deployBranchPRServers
          commit
          [ (readyAt ?~ firstOfMonth) . (endedAt ?~ addTime (fromMinutes @Int 5) firstOfMonth)
          ]
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

    it "correctly counts minutes from multiple servers" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]
        now <- liftIO getCurrentTime
        let firstOfMonth = flip UTCTime 0 . (\(y, m, _d) -> Time.fromGregorian y m 1) . Time.toGregorian $ utctDay now
        result <-
          try
            $ deployBranchPRServers
              commit
              [ \server1 ->
                  server1
                    & readyAt ?~ firstOfMonth
                    & endedAt ?~ addTime (fromSeconds @Int 90) firstOfMonth,
                \server2 ->
                  server2
                    & readyAt ?~ firstOfMonth
                    & endedAt ?~ addTime (fromSeconds @Int 90) firstOfMonth
              ]

        liftIO $ first err result `shouldBe` Left (EntitlementError "Sorry, you have exhausted all your PR deployment minutes.")
        calls <- getMockCalls #executeDeployPlanMock
        liftIO $ length calls `shouldBe` 0

    it "does not count minutes from previous months" $ do
      withMockReturning #executeDeployPlanMock [] $ do
        commit <-
          Deprecated.writeMockRemote "test-branch"
            $ def
            & serverSection .~ [ServerSection "test-nix-config" OnPullRequest]

        now <- liftIO getCurrentTime
        let firstOfMonth = flip UTCTime 0 . (\(y, m, _d) -> Time.fromGregorian y m 1) . Time.toGregorian $ utctDay now
        deployBranchPRServers
          commit
          [ \server ->
              server
                & readyAt ?~ subTime (fromDays @Int 1) firstOfMonth
                & endedAt ?~ addTime (fromMinutes @Int 1) firstOfMonth
          ]
        [plan] <- getMockCalls #executeDeployPlanMock
        plan `shouldHavePlan` (GhPrDeployment 42, [], [(commit, "test-nix-config")])

  describe "checkDeployPlan" $ inM $ beforeM_ truncateDBM $ do
    let user = "test-owner"
        name = "test-repo"
        branchName = "main"
        mkPlanWithTier tier build =
          DeployPlan
            { toSpinDown = [],
              toSpinUp = [ServerToSpinUp {serverTier = tier, build, domainIsPrimary = False}],
              toRedeploy = []
            }
        mkRepoInfo = do
          iAuth <- getInstallation $ Github.Data.Id 42
          pure $ githubRepoInfo iAuth (ForgeToken "test-token") user name

    it "allows larger servers with individual-v1 plan" $ do
      void
        $ DB.pgExec
          [pgSQL|
            INSERT INTO products
              (name, hosting, ci_minutes, larger_servers)
              VALUES ('test-individual', 10, 200000, true)
              ON CONFLICT DO NOTHING
          |]
      void
        $ DB.pgExec
          [pgSQL|
            INSERT INTO repo_owner_has_product
              (repo_owner, product)
              VALUES (${user}, 'test-individual')
          |]
      setExtraUsageLimits user (emptyUsageLimits & #hostingSpend .~ usd 100)
      build <- testBuild $ \b ->
        b
          & repoUser .~ user
          & repoName .~ name
          & branch ?~ branchName
          & status ?~ Success
          & packageType .~ TypeNixosConfiguration
          & uploadedToCache ?~ True
      repoInfo <- mkRepoInfo
      result <- try $ checkDeployPlan repoInfo (BranchDeployment branchName) (mkPlanWithTier I4x8 build)
      liftIO $ result `shouldBe` Right ()

    it "rejects larger servers with free-v1 plan" $ do
      void
        $ DB.pgExec
          [pgSQL|
            INSERT INTO products
              (name, hosting, ci_minutes, larger_servers)
              VALUES ('test-free', 10, 200000, false)
              ON CONFLICT DO NOTHING
          |]
      void
        $ DB.pgExec
          [pgSQL|
            INSERT INTO repo_owner_has_product
              (repo_owner, product)
              VALUES (${user}, 'test-free')
          |]
      setExtraUsageLimits user (emptyUsageLimits & #hostingSpend .~ usd 100)
      build <- testBuild $ \b ->
        b
          & repoUser .~ user
          & repoName .~ name
          & branch ?~ branchName
          & status ?~ Success
          & packageType .~ TypeNixosConfiguration
          & uploadedToCache ?~ True
      repoInfo <- mkRepoInfo
      result <- try $ checkDeployPlan repoInfo (BranchDeployment branchName) (mkPlanWithTier I4x8 build)
      liftIO $ first err result `shouldBe` Left (EntitlementError "Only server tier `i2x4` supported.")

-- * Helpers

truncateDB :: IO ()
truncateDB = do
  withSystemTempDirectory "truncateDB" $ \tmp -> do
    withTestEnvironment tmp $ void . flip runM truncateDBM

withContext :: CheckSuiteEvent -> (RepoInfo -> Branch -> M a) -> M a
withContext event action = do
  let (owner, name) = event ^. eventRepoName
  let Just branch = event ^. eventBranch
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO repo_owner_has_product
          (repo_owner, product)
          VALUES
          ('owner', 'hosting-beta')
          ON CONFLICT DO NOTHING
      |]
  iAuth <- getInstallation $ Github.Data.Id 42
  let repoInfo = githubRepoInfo iAuth (ForgeToken "test-token") owner name
  withPrivateNixXdgCache $ action repoInfo branch

doABuild :: Text -> CheckSuiteEvent -> RepoInfo -> M CommitInfo
doABuild flake event repoInfo = do
  _ <- Deprecated.writeMockRemote (fromJust $ event ^. eventBranch) (def :: GarnixConfig)
  dir <- view #workingDir
  liftIO $ T.writeFile (dir </> "flake.nix") flake
  commit <- commitAll dir
  event <- pure $ event & eventCommit .~ commit
  notifyOfCommit event
  pure
    $ CommitInfo
      { _commitInfoReqUser = ForgeLogin . whUserLogin $ senderOfEvent event,
        _commitInfoRepoPublicity = RepoIsPublic . not . whRepoIsPrivate $ repoForEvent event,
        _commitInfoRepoInfo = repoInfo,
        _commitInfoBranch = Branch <$> whCheckSuiteHeadBranch (evCheckSuiteCheckSuite event),
        _commitInfoPrFromFork = Nothing,
        _commitInfoCommit = commit
      }

getDeployLogsDB :: M [(ServerId, Text)]
getDeployLogsDB = do
  DB.pgQuery
    [pgSQL|!
      SELECT
        servers.id,
        servers.deploy_logs
      FROM servers
    |]

getAllDbServers :: M [ServerInfo]
getAllDbServers = do
  DB.pgQueryPrism
    _ServerInfo
    [pgSQL|!
    SELECT
      servers.id,
      servers.hetzner_id,
      servers.ipv4,
      servers.ipv6,
      servers.created_at,
      servers.ended_at,
      servers.configuration_build_id,
      servers.pull_request,
      servers.ready_at,
      builds.persistence_name,
      servers.server_tier,
      servers.is_primary
    FROM servers
    JOIN builds on servers.configuration_build_id = builds.id
    |]

assertNotExists :: (HasCallStack) => ServerInfo -> M ()
assertNotExists serverInfo = liftIO $ do
  let HetznerState st = hetznerState
  let hetznerId = serverInfo ^. hetznerServerId
  readMVar st >>= \m -> case Map.lookup hetznerId m of
    Nothing -> pure ()
    Just (tid, state, mvar) -> do
      actualState <- liftIO $ readMVar mvar
      assertFailure $ "assertNotExists: server exists: " <> cs (show (threadId tid, state, actualState))

shouldHaveState :: (HasCallStack) => ServerInfo -> Text -> M ()
shouldHaveState serverInfo expectedState = liftIO $ do
  let HetznerState st = hetznerState
  let hetznerId = serverInfo ^. hetznerServerId
  readMVar st >>= \m -> case Map.lookup hetznerId m of
    Nothing ->
      expectationFailure
        . cs
        $ "Expected to find a container with ID: "
        <> show (serverInfo ^. id)
    Just (_, _, mvar) -> do
      actualState <- liftIO $ readMVar mvar
      actualState `shouldBe` expectedState

simpleFlake :: Text
simpleFlake = simpleFlake' "A simple flake"

virtualisationModules :: Text
virtualisationModules =
  cs
    [i|
      "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
      {
        virtualisation.useNixStoreImage = true;
        virtualisation.writableStore = true;
      }
    |]

simpleFlake' :: Text -> Text
simpleFlake' description' =
  cs
    [i|
  {
    description = "#{description'}";
    # If you update this, update also places where it matches.
    # Search for INNER_NIXPKGS_MATCHES
    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";

    outputs = { self, nixpkgs }: {

      nixosConfigurations = {
        default = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            #{virtualisationModules}
            ({ pkgs, ... } : {
              boot.isContainer = true;
              services.openssh.enable = true;
              users.users.root = {
                openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2sZYF9l/ssO+uk5bdaZLskJKxNFbbJDd3cR1TR17KE1elmC4KQ7LOU3329JMyiDU73DlUHRG+1zhN9I6UNCJR8en7YDPWODw+1eKAFI1IQiYuuvp3rO9RnR5DYXxzGjEBuxxxOqLRCLmaWsP4nQ6kzmmWvIYZ9npNLCp1KN42EcCzlpUR4NOqxJr834vkqlgk3dnl00wYlLO5v4+t0l48SrcUL8EM7z/i0ivjT/15sl6PgNSgTGbB6eIWg9oLt76rhXpGvvccCp/atDb98+OXlPpDw90MgO0sGA8UyAFAKrpoLaNTPFyRrCBlHLIBlvgagNaYoq6DOGJVOGK227tJMiwDnhUyOirutYnIJ6MNdUGmq2bF7nX15uXGmGKfHf4TaShgMCcitlsrzVwuO/gdce1Y5TnJc/Wdbj3D8j95/41bBp6MyRlUK5gpT0R+NSX1hv0rL+eSa56REwfcZMrYWFr3Hpv7eq9VHAS0NBj+Hy5N9JCc+mvB7w2XufNoMkk= jkarni@janus"
                ];
              };
            })
          ];
        };
      };
    };
  }

|]

flakeWithFailingBuilds :: Text
flakeWithFailingBuilds =
  cs
    [i|
  {
    # If you update this, update also places where it matches.
    # Search for INNER_NIXPKGS_MATCHES
    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";

    outputs = { self, nixpkgs }: {
      packages.x86_64-linux.failing = derivation {
      };

      nixosConfigurations = {
        myHost = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            #{virtualisationModules}
            ({ pkgs, ... } : {
              boot.isContainer = true;
              services.openssh.enable = true;
              users.users.root = {
                openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2sZYF9l/ssO+uk5bdaZLskJKxNFbbJDd3cR1TR17KE1elmC4KQ7LOU3329JMyiDU73DlUHRG+1zhN9I6UNCJR8en7YDPWODw+1eKAFI1IQiYuuvp3rO9RnR5DYXxzGjEBuxxxOqLRCLmaWsP4nQ6kzmmWvIYZ9npNLCp1KN42EcCzlpUR4NOqxJr834vkqlgk3dnl00wYlLO5v4+t0l48SrcUL8EM7z/i0ivjT/15sl6PgNSgTGbB6eIWg9oLt76rhXpGvvccCp/atDb98+OXlPpDw90MgO0sGA8UyAFAKrpoLaNTPFyRrCBlHLIBlvgagNaYoq6DOGJVOGK227tJMiwDnhUyOirutYnIJ6MNdUGmq2bF7nX15uXGmGKfHf4TaShgMCcitlsrzVwuO/gdce1Y5TnJc/Wdbj3D8j95/41bBp6MyRlUK5gpT0R+NSX1hv0rL+eSa56REwfcZMrYWFr3Hpv7eq9VHAS0NBj+Hy5N9JCc+mvB7w2XufNoMkk= jkarni@janus"
                ];
              };
            })
          ];
        };
      };
    };
  }

|]

flakeWithFailingActivation :: Text
flakeWithFailingActivation =
  cs
    [i|
      {
        # If you update this, update also places where it matches.
        # Search for INNER_NIXPKGS_MATCHES
        inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";

        outputs = { self, nixpkgs }: {

          nixosConfigurations = {
            default = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [
                #{virtualisationModules}
                ({ pkgs, ... } : {
                  boot.isContainer = true;
                  system.activationScripts.activationFailure.text = ''
                    activationFailure
                  '';
                })
              ];
            };
          };
        };
      }
    |]

flakeWithPersistenceAndConfig :: Bool -> Text -> PackageName -> Text -> Text
flakeWithPersistenceAndConfig enable name (PackageName package) t =
  cs
    [i|
{
  inputs = {
    # If you update this, update also places where it matches.
    # Search for INNER_NIXPKGS_MATCHES
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";
    garnix.url = "github:garnix-io/garnix-lib";
  };

  outputs =
    { self, nixpkgs, garnix }: {
      garnix.config = {
        servers = [{
          configuration = "#{package}";
          deployment = {
            type = "on-branch";
            branch = "branch";
          };
        }];
      };

      nixosConfigurations."#{package}" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          #{virtualisationModules}
          garnix.nixosModules.garnix
          {
            config = {
              boot.isContainer = true;

              #{sshKey}

              # something that we can safely modify to get a different hash
              networking.hostName = "#{t}";

              garnix.server = {
                 enable = #{if enable then "true" :: String else "false"};
                 isVM = true;
                 persistence = {
                  enable = #{if enable then "true" :: String else "false"};
                  name = "#{name}";
                };
              };
            };
          }
        ];

      };
    };
}
    |]
  where
    sshKey :: String
    sshKey =
      if enable
        then
          [i|
              users.users.garnix.openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2sZYF9l/ssO+uk5bdaZLskJKxNFbbJDd3cR1TR17KE1elmC4KQ7LOU3329JMyiDU73DlUHRG+1zhN9I6UNCJR8en7YDPWODw+1eKAFI1IQiYuuvp3rO9RnR5DYXxzGjEBuxxxOqLRCLmaWsP4nQ6kzmmWvIYZ9npNLCp1KN42EcCzlpUR4NOqxJr834vkqlgk3dnl00wYlLO5v4+t0l48SrcUL8EM7z/i0ivjT/15sl6PgNSgTGbB6eIWg9oLt76rhXpGvvccCp/atDb98+OXlPpDw90MgO0sGA8UyAFAKrpoLaNTPFyRrCBlHLIBlvgagNaYoq6DOGJVOGK227tJMiwDnhUyOirutYnIJ6MNdUGmq2bF7nX15uXGmGKfHf4TaShgMCcitlsrzVwuO/gdce1Y5TnJc/Wdbj3D8j95/41bBp6MyRlUK5gpT0R+NSX1hv0rL+eSa56REwfcZMrYWFr3Hpv7eq9VHAS0NBj+Hy5N9JCc+mvB7w2XufNoMkk= jkarni@janus"
              ];
      |]
        else ""

flakeWithPersistence :: Bool -> Text -> PackageName -> Text -> Text
flakeWithPersistence enable name (PackageName package) t =
  cs
    [i|
{
  inputs = {
    # If you update this, update also places where it matches.
    # Search for INNER_NIXPKGS_MATCHES
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";
    garnix.url = "github:garnix-io/garnix-lib";
  };

  outputs =
    { self, nixpkgs, garnix }: {

      nixosConfigurations."#{package}" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          #{virtualisationModules}
          garnix.nixosModules.garnix
          {
            config = {
              boot.isContainer = true;

              #{sshKey}

              # something that we can safely modify to get a different hash
              networking.hostName = "#{t}";

              garnix.server = {
                 enable = #{if enable then "true" :: String else "false"};
                 isVM = true;
                 persistence = {
                  enable = #{if enable then "true" :: String else "false"};
                  name = "#{name}";
                };
              };
            };
          }
        ];

      };
    };
}
    |]
  where
    sshKey :: String
    sshKey =
      if enable
        then
          [i|
              users.users.garnix.openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2sZYF9l/ssO+uk5bdaZLskJKxNFbbJDd3cR1TR17KE1elmC4KQ7LOU3329JMyiDU73DlUHRG+1zhN9I6UNCJR8en7YDPWODw+1eKAFI1IQiYuuvp3rO9RnR5DYXxzGjEBuxxxOqLRCLmaWsP4nQ6kzmmWvIYZ9npNLCp1KN42EcCzlpUR4NOqxJr834vkqlgk3dnl00wYlLO5v4+t0l48SrcUL8EM7z/i0ivjT/15sl6PgNSgTGbB6eIWg9oLt76rhXpGvvccCp/atDb98+OXlPpDw90MgO0sGA8UyAFAKrpoLaNTPFyRrCBlHLIBlvgagNaYoq6DOGJVOGK227tJMiwDnhUyOirutYnIJ6MNdUGmq2bF7nX15uXGmGKfHf4TaShgMCcitlsrzVwuO/gdce1Y5TnJc/Wdbj3D8j95/41bBp6MyRlUK5gpT0R+NSX1hv0rL+eSa56REwfcZMrYWFr3Hpv7eq9VHAS0NBj+Hy5N9JCc+mvB7w2XufNoMkk= jkarni@janus"
              ];
      |]
        else ""

makeMultiFlake :: [PackageName] -> Text
makeMultiFlake packages = cs buildFile
  where
    buildFile :: String
    buildFile = start <> foldMap buildEntry packages <> end

    start :: String
    start =
      [i|
  {
    description = "simple description here";
    # If you update this, update also places where it matches.
    # Search for INNER_NIXPKGS_MATCHES
    inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11-small";

    outputs = { self, nixpkgs }: {

      nixosConfigurations = {
     |]

    end :: String
    end =
      [i|
      };
    };
  }
     |]

    buildEntry :: PackageName -> String
    buildEntry (PackageName pkg) =
      [i|
        #{pkg} = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            #{virtualisationModules}
            ({ pkgs, ... } : {
              boot.isContainer = true;
              services.openssh.enable = true;
              users.users.root = {
                openssh.authorizedKeys.keys = [
                   "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2sZYF9l/ssO+uk5bdaZLskJKxNFbbJDd3cR1TR17KE1elmC4KQ7LOU3329JMyiDU73DlUHRG+1zhN9I6UNCJR8en7YDPWODw+1eKAFI1IQiYuuvp3rO9RnR5DYXxzGjEBuxxxOqLRCLmaWsP4nQ6kzmmWvIYZ9npNLCp1KN42EcCzlpUR4NOqxJr834vkqlgk3dnl00wYlLO5v4+t0l48SrcUL8EM7z/i0ivjT/15sl6PgNSgTGbB6eIWg9oLt76rhXpGvvccCp/atDb98+OXlPpDw90MgO0sGA8UyAFAKrpoLaNTPFyRrCBlHLIBlvgagNaYoq6DOGJVOGK227tJMiwDnhUyOirutYnIJ6MNdUGmq2bF7nX15uXGmGKfHf4TaShgMCcitlsrzVwuO/gdce1Y5TnJc/Wdbj3D8j95/41bBp6MyRlUK5gpT0R+NSX1hv0rL+eSa56REwfcZMrYWFr3Hpv7eq9VHAS0NBj+Hy5N9JCc+mvB7w2XufNoMkk= jkarni@janus"
                ];
              };
            })
          ];
        };

       |]

writeMatchingConfig :: Branch -> PackageName -> M ()
writeMatchingConfig branch = writeMultiConfig branch . pure

writeUnmatchingConfig :: M ()
writeUnmatchingConfig = do
  dir <- view #workingDir
  liftIO $ T.writeFile (dir </> "garnix.yaml") (cs cfg)
  where
    cfg =
      unindent
        [i|
        servers: []

      |]

writeMultiConfig :: Branch -> [PackageName] -> M ()
writeMultiConfig branch packages = do
  dir <- view #workingDir
  liftIO $ T.writeFile (dir </> "garnix.yaml") (cs $ getMultiConfig branch packages)

onPullRequestConfig :: PackageName -> Text
onPullRequestConfig (PackageName pkg) =
  cs
    [i|
servers:
  - configuration: #{pkg}
    deployment:
      type: on-pull-request
  |]

getMultiConfig :: Branch -> [PackageName] -> Text
getMultiConfig (Branch branch) packages = cs buildFile
  where
    buildFile :: String
    buildFile =
      "servers:" <> case packages of
        [] -> " []"
        _ -> foldMap buildConfigEntry packages

    buildConfigEntry :: PackageName -> String
    buildConfigEntry (PackageName pkg) =
      [i|
  - configuration: #{pkg}
    deployment:
      type: on-branch
      branch: #{branch}
       |]

startServerAndFailOnAllExcept ::
  MVar () ->
  PackageName ->
  (Reporter, CommitInfo, DeploymentType, ServerToSpinUp) ->
  M ServerInfo
startServerAndFailOnAllExcept sync provision (reporter, commitInfo, deploymentType, serverToSpinUp) = do
  if serverToSpinUp ^. #build . package == provision
    then do
      result <- withUnmock #startServerMock $ startServer reporter commitInfo deploymentType serverToSpinUp
      liftIO $ putMVar sync ()
      pure result
    else do
      _ <- liftIO $ readMVar sync
      throw $ ProvisioningError "test error"

addBranchHostingEntitlement :: M ()
addBranchHostingEntitlement =
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO repo_owner_has_product
          (repo_owner, product)
          VALUES
          ('owner', 'hosting-beta')
      |]

addPrHostingEntitlement :: M ()
addPrHostingEntitlement = do
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO products
          (name, pr_hosting)
          VALUES ('pr-hosting-beta', 1000)
          ON CONFLICT DO NOTHING;
      |]
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO repo_owner_has_product
          (repo_owner, product)
          VALUES
          ('owner', 'pr-hosting-beta')
      |]

createBuildsFor :: RepoOwner -> RepoName -> Branch -> CommitHash -> [(PackageName, Maybe Text)] -> M [Build]
createBuildsFor user name branchName commit machines = do
  overallBuild <- testBuild $ \build ->
    build
      & repoUser .~ user
      & repoName .~ name
      & gitCommit .~ commit
      & branch ?~ branchName
      & status ?~ Success
      & packageType .~ TypeOverall
      & package .~ "overall package"

  forM machines $ \(machine, pname) -> do
    testBuild $ \_ ->
      overallBuild
        & packageType .~ TypeNixosConfiguration
        & package .~ machine
        & persistenceName .~ pname
        & uploadedToCache ?~ True

deployNewServerFor ::
  RepoOwner ->
  RepoName ->
  Branch ->
  CommitHash ->
  [(PackageName, Maybe Text)] ->
  M [ServerInfo]
deployNewServerFor user name branchName commit machineNames = do
  writeMultiConfig branchName $ fmap fst machineNames
  void $ createBuildsFor user name branchName commit machineNames
  iAuth <- getInstallation $ Github.Data.Id 42
  let repoInfo = githubRepoInfo iAuth (ForgeToken "test-token") user name
  let commitInfo = CommitInfo (getRepoOwner user) (RepoIsPublic True) repoInfo (Just branchName) Nothing commit
  withPrivateNixXdgCache
    $ rolloutNewServerVersion mempty commitInfo (BranchDeployment branchName)

mkPrEvent :: CommitHash -> PullRequestEvent
mkPrEvent commit =
  mkPullRequestEvent commit "test-branch" "test-owner/test-repo" "test-owner/test-repo" testInstallationId
    & number .~ 42

testInstallationId :: Int
testInstallationId = 123456

fromPrEvent :: PullRequestEvent -> Build -> Build
fromPrEvent prEvent build =
  build
    & branch ?~ Branch (prEvent ^. payload . head . ref)
    & packageType .~ TypeNixosConfiguration
    & repoUser .~ "test-owner"
    & repoName .~ "test-repo"
    & gitCommit .~ CommitHash (prEvent ^. payload . head . sha)
    & uploadedToCache ?~ True

mkCommitInfo :: CommitHash -> CommitInfo
mkCommitInfo commitHash =
  defaultCommitInfo
    & commit .~ commitHash
    & branch .~ Nothing
    & reqUser .~ "test-owner"
    & repoInfo . ghRepoOwner .~ "test-owner"
    & repoInfo . ghRepoName .~ "test-repo"

deployBranchPRServers :: CommitHash -> [ServerInfo -> ServerInfo] -> M ()
deployBranchPRServers commit servers = do
  build <- testBuild $ \build ->
    build
      & repoUser .~ "test-owner"
      & fromPrEvent (mkPrEvent commit)
      & package .~ "test-nix-config"
      & uploadedToCache ?~ True

  forM_ servers $ \f -> do
    addTestServer $ \server ->
      server
        & configurationBuildId .~ (build ^. id)
        & pullRequest ?~ 1
        & f

  Orchestrator.handlePullRequest mempty (mkCommitInfo commit) 42 >>= resolve
