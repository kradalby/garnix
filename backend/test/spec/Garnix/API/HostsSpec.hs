{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.HostsSpec (spec) where

import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.Hosts
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers.GithubInterface.Internal (fakeRepoInfo)
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types
import Network.Wreq.Lens
import Test.Hspec hiding (shouldReturn)
import Test.Hspec.Golden (defaultGolden)

spec :: Spec
spec = do
  inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
    describe "/api/hosts" $ do
      it "responds with 401 when logged out" $ withServer $ \testServer -> do
        result <- testServer.get "/api/hosts"
        result `shouldHaveStatusCode` 401

      it "responds with an empty array if the user has no servers" $ withServer $ \testServer -> do
        _user <- testServer.login
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO $ result ^?! responseBody . _Value `shouldBe` [aesonQQ| [] |]

      it "lists servers currently booting" $ withServer $ \testServer -> do
        user <- testServer.login
        serverInfo <- createSimpleServer user (readyAt .~ Nothing)
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
            [
              {
                id: #{serverInfo ^. id},
                ipv4: "<none>",
                type: { tag: "BranchDeployment", contents: "branch" },
                status: "Booting",
                repo_name: "repo",
                repo_owner: #{user ^. githubLogin},
                package_name: "package",
                commit: "baz",
                configuration_build_id: #{serverInfo ^. configurationBuildId},
                created_at: #{serverInfo ^. createdAt},
                deploy_logs: ""
              }
            ]
          |]

      it "lists servers currently running" $ withServer $ \testServer -> do
        user <- testServer.login
        serverInfo <- createSimpleServer user identity
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
            [
              {
                id: #{serverInfo ^. id},
                ipv4: "<none>",
                type: { tag: "BranchDeployment", contents: "branch" },
                status: "Online",
                repo_name: "repo",
                repo_owner: #{user ^. githubLogin},
                package_name: "package",
                commit: "baz",
                configuration_build_id: #{serverInfo ^. configurationBuildId},
                created_at: #{serverInfo ^. createdAt},
                deploy_logs: ""
              }
            ]
          |]

      it "lists servers that have ended" $ withServer $ \testServer -> do
        user <- testServer.login
        now <- liftIO getCurrentTime
        serverInfo <- createSimpleServer user (endedAt ?~ now)
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
            [
              {
                id: #{serverInfo ^. id},
                ipv4: "<none>",
                type: { tag: "BranchDeployment", contents: "branch" },
                status: "Ended",
                repo_name: "repo",
                repo_owner: #{user ^. githubLogin},
                package_name: "package",
                commit: "baz",
                configuration_build_id: #{serverInfo ^. configurationBuildId},
                created_at: #{serverInfo ^. createdAt},
                deploy_logs: ""
              }
            ]
          |]

      it "does not list servers that ended over 24 hours ago" $ withServer $ \testServer -> do
        user <- testServer.login
        oneDayAgo <- liftIO $ subTime (fromHours @Double 24.01) <$> getCurrentTime
        _serverInfo <- createSimpleServer user (endedAt ?~ oneDayAgo)
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ| [ ] |]

      it "lists servers for organizations you aren't an admin in" $ GH.withFakeGithubInterface $ \st -> withServer $ \testServer -> do
        let someGhOrg = "user-isnt-admin-in-this-org"
        user <- testServer.login
        GH.addOrgMembers st [UserOrgMembership someGhOrg (Other "user")]
        GH.mkRepo st someGhOrg "repo" identity
        serverInfo <-
          createServer
            someGhOrg
            (RepoName "repo")
            (Branch "branch")
            Nothing
            (PackageName "package")
            Nothing
            user
            identity
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
            [
              {
                id: #{serverInfo ^. id},
                ipv4: "<none>",
                type: { tag: "BranchDeployment", contents: "branch" },
                status: "Online",
                repo_name: "repo",
                repo_owner: #{someGhOrg},
                package_name: "package",
                commit: "baz",
                configuration_build_id: #{serverInfo ^. configurationBuildId},
                created_at: #{serverInfo ^. createdAt},
                deploy_logs: ""
              }
            ]
          |]

      it "lists PR deployments" $ withServer $ \testServer -> do
        user <- testServer.login
        serverInfo <-
          createServer
            (RepoOwner $ user ^. githubLogin)
            (RepoName "repo")
            (Branch "branch")
            (Just 42)
            (PackageName "package")
            Nothing
            user
            identity
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^?! responseBody . _Value
          `shouldBe` [aesonQQ|
            [
              {
                id: #{serverInfo ^. id},
                ipv4: "<none>",
                type: { tag: "GhPrDeployment", contents: 42 },
                status: "Online",
                repo_name: "repo",
                repo_owner: #{user ^. githubLogin},
                package_name: "package",
                commit: "baz",
                configuration_build_id: #{serverInfo ^. configurationBuildId},
                created_at: #{serverInfo ^. createdAt},
                deploy_logs: ""
              }
            ]
          |]

      it "lists servers in reverse order of creation" $ withServer $ \testServer -> do
        user <- testServer.login
        now <- liftIO getCurrentTime
        firstServer <- createSimpleServer user (createdAt .~ subTime (fromMinutes @Int 20) now)
        secondServer <- createSimpleServer user (endedAt ?~ subTime (fromMinutes @Int 20) now)
        result <- assert200 $ testServer.get "/api/hosts"
        liftIO
          $ result
          ^.. responseBody . _Array . traverse . key "id" . _Value
          `shouldBe` [ [aesonQQ| #{secondServer ^. id} |],
                       [aesonQQ| #{firstServer ^. id} |]
                     ]

    beforeM_ truncateDBM $ describe "heartbeat" $ do
      it "inserts posted heartbeats to the database" $ do
        void $ postHostsHeartbeat ["host1", "host2"]
        hosts <- DB.getRecentHeartbeats
        liftIO $ hosts `shouldBe` ["host1", "host2"]

      it "updates existing entries" $ do
        void
          $ DB.pgExec
            [pgSQL|
      INSERT INTO heartbeat (hostname, last_heartbeat)
        VALUES ('some_host', NOW() - interval '13 hours')
        |]
        initialHosts <- DB.getRecentHeartbeats
        void $ postHostsHeartbeat ["some_host"]
        postUpdateHosts <- DB.getRecentHeartbeats

        liftIO $ do
          initialHosts `shouldBe` []
          postUpdateHosts `shouldBe` ["some_host"]

    describe "getHostsForTraefik" $ do
      let skipMiddleware json = json & key "http" . atKey "middlewares" .~ Nothing
      it "correctly sets up the heartbeat middleware" $ do
        (fmap Aeson.Object . (^? key "http" . key "middlewares" . _Object) . toJSON <$> getHostsForTraefik)
          `shouldReturnM` Just
            [aesonQQ| {
            "heartbeatmiddleware": {
                "plugin": {
                  "heartbeatmiddleware": {
                    "reportEndpoint": "https://garnix.io/api/hosts/heartbeat"
                  }
                }
              }
          } |]
      it "provides an initially empty set of hosts" $ do
        (skipMiddleware . toJSON <$> getHostsForTraefik)
          `shouldReturnM` [aesonQQ| {
          "http": {
            "routers": {},
            "services": {}
          }
          } |]

      it "does not include servers which have not been initialized" $ do
        user <- testUser
        serverInfo <- createSimpleServer user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        DB.updateServerPostDeploy $ serverInfo & readyAt .~ Nothing
        hosts <- getHostsForTraefik
        skipMiddleware (toJSON hosts)
          `shouldBeM` [aesonQQ| {
          "http": { "routers": {}, "services": {} }
          } |]

      it "serves hosts marked as primary at the repo root domain" $ do
        user <- testUser
        void $ createSimpleServer user (isPrimary .~ True)
        hosts <- getHostsForTraefik
        let json = toJSON hosts
        let routers = json ^?! key "http" . key "routers" . _Object . to Aeson.toList
        let services = json ^?! key "http" . key "services" . _Object . to Aeson.keys
        routers
          <&> (\(k, v) -> (k, v ^. key "rule" . _String, v ^. key "service" . _String))
            `shouldBeM` [ ("package.branch.repo.user", "Host(`package.branch.repo.user.garnix.me`)", "package.branch.repo.user"),
                          ("repo.user", "Host(`repo.user.garnix.me`)", "package.branch.repo.user")
                        ]
        services `shouldBeM` ["package.branch.repo.user"]

      it "adds hosts when a new server is deployed" $ do
        user <- testUser
        void $ createSimpleServer user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        hosts <- getHostsForTraefik
        case hosts of
          HostList [_] _ -> pure ()
          h -> liftIO $ expectationFailure $ "Expected exactly one host. Got: " <> cs (show h)
        pure $ defaultGolden "HostSpec/adds-hosts-when-a-new-server-is-deployed" $ cs $ encodePretty hosts

      it "reflects the latest deployed versions" $ do
        user <- testUser
        initial <- createSimpleServer user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        void $ createSimpleServer user ((ipv4Addr .~ "11.111.111.2") . (ipv6Addr .~ "::"))
        markServerDead initial
        finalHosts <- getHostsForTraefik
        case finalHosts of
          HostList [_] _ -> pure ()
          h -> liftIO $ expectationFailure $ "Expected exactly one host. Got: " <> cs (show h)
        pure $ defaultGolden "HostSpec/reflects-the-latest-deployed-versions" $ cs $ encodePretty finalHosts

      it "supports exposing more than one configuration per repo" $ do
        user <- testUser
        void $ createServer (RepoOwner $ ForgeLogin "owner") (RepoName "repo") (Branch "main") Nothing (PackageName "nginx") Nothing user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        void $ createServer (RepoOwner $ ForgeLogin "owner") (RepoName "repo") (Branch "main") Nothing (PackageName "psql") Nothing user ((ipv4Addr .~ "11.111.111.2") . (ipv6Addr .~ "::"))
        void $ createServer (RepoOwner $ ForgeLogin "owner") (RepoName "repo") (Branch "feat") Nothing (PackageName "psql") Nothing user ((ipv4Addr .~ "11.111.111.3") . (ipv6Addr .~ "::"))
        hosts <- getHostsForTraefik
        case hosts of
          HostList [_, _, _] _ -> pure ()
          h -> liftIO $ expectationFailure $ "Expected exactly three hosts. Got: " <> cs (show h)
        pure $ defaultGolden "HostSpec/supports-exposing-more-than-one-configuration-per-repo" $ cs $ encodePretty hosts

      it "serves configurations for pull request servers" $ do
        user <- testUser
        void $ createServer (RepoOwner $ ForgeLogin "owner") (RepoName "repo") (Branch "main") (Just 42) (PackageName "nginx") Nothing user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        hosts <- getHostsForTraefik
        case hosts of
          HostList [_] _ -> pure ()
          h -> liftIO $ expectationFailure $ "Expected exactly one host. Got: " <> cs (show h)
        pure $ defaultGolden "HostSpec/serves-configurations-for-pull-request-servers" $ cs $ encodePretty hosts

      it "serves pull request configurations when the branch name is not a valid subdomain" $ do
        user <- testUser
        void $ createServer (RepoOwner $ ForgeLogin "owner") (RepoName "repo") (Branch "sh/my-cool-feature") (Just 42) (PackageName "nginx") Nothing user ((ipv4Addr .~ "11.111.111.1") . (ipv6Addr .~ "::"))
        hosts <- getHostsForTraefik
        case hosts of
          HostList [_] _ -> pure ()
          h -> liftIO $ expectationFailure $ "Expected exactly one host. Got: " <> cs (show h)
        pure $ defaultGolden "HostSpec/serves-pull-request-configurations-when-the-branch-name-is-not-a-valid-subdomain" $ cs $ encodePretty hosts

      describe "invalid DNS" $ do
        forM_
          [ (RepoOwner $ ForgeLogin "dots.are.invalid", RepoName "some-repo", Branch "some-branch", PackageName "some-host"),
            (RepoOwner $ ForgeLogin "some-owner", RepoName "dots.are.invalid", Branch "some-branch", PackageName "some-host"),
            (RepoOwner $ ForgeLogin "some-owner", RepoName "some-repo", Branch "dots.are.invalid", PackageName "some-host"),
            (RepoOwner $ ForgeLogin "some-owner", RepoName "some-repo", Branch "some-branch", PackageName "dots.are.invalid")
          ]
          $ \(owner, repo, branch, packageName) -> do
            let invalidSubdomain = showPretty packageName <> "." <> showPretty branch <> "." <> showPretty repo <> "." <> showPretty owner
            it ("rejects names with special DNS characters (" <> cs invalidSubdomain <> ")") $ do
              let ipAddr = "11.111.111.1"
              user <- testUser
              void
                $ createServer
                  owner
                  repo
                  branch
                  Nothing
                  packageName
                  Nothing
                  user
                  ((ipv4Addr .~ ipAddr) . (ipv6Addr .~ "::"))
              hosts <- getHostsForTraefik

              skipMiddleware (toJSON hosts)
                `shouldBeM` [aesonQQ| {
                "http": { "routers": {}, "services": {} }
                } |]

    describe "/api/hosts/dns" $ do
      let setupTestServers = do
            user <- testUser
            void
              $ createServer
                (RepoOwner $ ForgeLogin "owner")
                (RepoName "repo")
                (Branch "main")
                Nothing
                (PackageName "frontend")
                (Just "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-package-name.drv")
                user
                ((ipv4Addr .~ "1.2.3.4") . (ipv6Addr .~ "01:23:45:67:89:ab:cd:ef"))
            void
              $ createServer
                (RepoOwner $ ForgeLogin "owner")
                (RepoName "repo")
                (Branch "main")
                Nothing
                (PackageName "backend")
                (Just "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-package-name.drv")
                user
                ((ipv4Addr .~ "5.6.7.8") . (ipv6Addr .~ "ab:cd:ef:12:34:56:78:90"))

      it "returns a list of all active servers by derivation hash" $ do
        setupTestServers
        withServer $ \testServer -> do
          hosts <- (^?! responseBody . key "byHash" . _Value) <$> testServer.get "/api/hosts/dns"
          hosts
            `shouldBeM` [aesonQQ|
              {
                "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa": { ipv4: "1.2.3.4", ipv6: "01:23:45:67:89:ab:cd:ef" },
                "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb": { ipv4: "5.6.7.8", ipv6: "ab:cd:ef:12:34:56:78:90" }
              }
            |]

      it "returns a list of all active servers by name" $ do
        setupTestServers
        withServer $ \testServer -> do
          hosts <- (^?! responseBody . key "byName" . _Value) <$> testServer.get "/api/hosts/dns"
          hosts
            `shouldBeM` [aesonQQ|
              {
                "frontend.main.repo.owner": { ipv4: "1.2.3.4", ipv6: "01:23:45:67:89:ab:cd:ef" },
                "backend.main.repo.owner": { ipv4: "5.6.7.8", ipv6: "ab:cd:ef:12:34:56:78:90" }
              }
            |]

    describe "/api/hosts/on-demand-resolver" $ do
      it "returns a list of routable domain names" $ do
        user <- testUser
        void
          $ createServer
            (RepoOwner $ ForgeLogin "owner")
            (RepoName "repo")
            (Branch "branch")
            (Just 42)
            (PackageName "foo")
            Nothing
            user
            identity
        void
          $ createServer
            (RepoOwner $ ForgeLogin "owner")
            (RepoName "repo")
            (Branch "branch")
            Nothing
            (PackageName "bar")
            Nothing
            user
            identity
        withServer $ \testServer -> do
          hosts <- assert200 $ testServer.get "/api/hosts/on-demand-resolver"
          sort (hosts ^?! responseBody . key "domains" . _Array . to (map (^. _String) . toList))
            `shouldBeM` sort
              [ "foo.pull-42.repo.owner.garnix.me",
                "bar.branch.repo.owner.garnix.me"
              ]

      it "includes primary domain aliases" $ do
        user <- testUser
        void
          $ createServer
            (RepoOwner $ ForgeLogin "owner")
            (RepoName "repo")
            (Branch "branch")
            Nothing
            (PackageName "foo")
            Nothing
            user
            (isPrimary .~ True)
        withServer $ \testServer -> do
          hosts <- assert200 $ testServer.get "/api/hosts/on-demand-resolver"
          sort (hosts ^?! responseBody . key "domains" . _Array . to (map (^. _String) . toList))
            `shouldBeM` sort
              [ "foo.branch.repo.owner.garnix.me",
                "repo.owner.garnix.me"
              ]

    beforeM_ truncateDBM $ describe "delete host" $ do
      it "responds with 401 when logged out" $ withServer $ \testServer -> do
        result <- testServer.delete "/api/hosts/vBV73Z9e"
        result `shouldHaveStatusCode` 401

      it "responds with a 404 if the user has no servers" $ withServer $ \testServer -> do
        _user <- testServer.login
        result <- testServer.delete "/api/hosts/vBV73Z9e"
        result `shouldHaveStatusCode` 404

      it "responds with a 404 if the server is not running" $ withServer $ \testServer -> do
        user <- testServer.login
        server <- createSimpleServer user identity
        markServerDead server
        result <- testServer.delete (cs ("/api/hosts/" <> getHashId (getServerId (server ^. id))))
        result `shouldHaveStatusCode` 404

      it "responds with a 200 if the user can delete one server" $ withServer $ \testServer -> do
        user <- testServer.login
        server <- createSimpleServer user identity
        result <- testServer.delete (cs ("/api/hosts/" <> getHashId (getServerId (server ^. id))))
        result `shouldHaveStatusCode` 200
        result <- testServer.delete (cs ("/api/hosts/" <> getHashId (getServerId (server ^. id))))
        result `shouldHaveStatusCode` 404
        let owner = RepoOwner $ user ^. githubLogin
        server <- DB.getHetznerServerById [owner] (server ^. id)
        case server of
          Nothing -> pure ()
          Just _ -> liftIO $ expectationFailure "Server was not deleted"

createSimpleServer :: User -> (ServerInfo -> ServerInfo) -> M ServerInfo
createSimpleServer user =
  createServer
    (RepoOwner $ user ^. githubLogin)
    (RepoName "repo")
    (Branch "branch")
    Nothing
    (PackageName "package")
    Nothing
    user

createServer :: RepoOwner -> RepoName -> Branch -> Maybe PullRequestId -> PackageName -> Maybe FilePath -> User -> (ServerInfo -> ServerInfo) -> M ServerInfo
createServer repoOwner repoName branch pr packageName drvPath' user updateServerInfo = do
  let commitInfo =
        CommitInfo
          (user ^. githubLogin)
          (RepoIsPublic True)
          (fakeRepoInfo repoOwner repoName)
          (Just branch)
          Nothing
          (CommitHash "baz")
  build <-
    DB.newBuildDB
      commitInfo
      (PackageInfo TypePackage (IsSystem X8664Linux) packageName)
      "garnix-server-test"
      False
  DB.reportBuildResultDB $ build & drvPath .~ drvPath'
  now <- liftIO getCurrentTime
  addTestServer $ updateServerInfo . \server ->
    server
      & configurationBuildId .~ (build ^. id)
      & readyAt ?~ now
      & pullRequest .~ pr

markServerDead :: ServerInfo -> M ()
markServerDead serverInfo = do
  now <- liftIO getCurrentTime
  DB.updateServerPostDeploy $ serverInfo & endedAt ?~ now
