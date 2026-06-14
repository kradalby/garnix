{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.AccountSpec where

import Control.Lens ((^?!))
import Control.Lens.Unsound (lensProduct)
import Data.Aeson (Value)
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Lens
import Data.Functor ((<&>))
import Data.Map.Strict (fromList, (!))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Row ((.+), (.==))
import Data.Yaml (decodeThrow)
import Data.Yaml.TH (yamlQQ)
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.API.Account
  ( EnabledRepos (..),
    OrgUsage (..),
    UpgradeOption (..),
    UsageOverview (..),
    createSubscription,
    enabledReposOf,
    getUpgradeOptionByToken,
    handleInvoiceCreated,
    handleSubscriptionAdded,
    plan,
    upgradeOptions,
    usageOverview,
  )
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (ProductToken (..), getPlans, setExtraUsageLimits)
import Garnix.Entitlements qualified as Entitlements
import Garnix.GithubInterface.Types
import Garnix.Hosting.ServerPool.Types
import Garnix.Monad
import Garnix.MonetaryCost
import Garnix.Prelude
import Garnix.StripeLib qualified as StripeLib
import Garnix.Forge.Types
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (Admin, context, head)
import Network.HTTP.Types (badRequest400)
import Network.Wreq.Lens
import Servant.Auth.Server (AuthResult (..))
import Test.Hspec

spec :: Spec
spec = inM $ beforeM_ truncateDBM $ aroundM_ suppressLogsWhenPassing $ do
  describe "AccountAPI" $ do
    let mockGithubInterface :: ForgeToken -> [RepoOwner] -> M a -> M a
        mockGithubInterface expectedToken orgs =
          withGithubMock
            installedOrgsLens
            ( \tok -> do
                liftIO $ tok `shouldBe` expectedToken
                pure $ map (`UserOrgMembership` Admin) orgs
            )

    let getDefaultPlan = fromJust <$> Entitlements.getPlanByName Entitlements.defaultPlanName

    describe "ci minutes" $ do
      it "reports empty usage when the user has no installations" $ do
        defaultPlan <- getDefaultPlan
        mockGithubInterface (ForgeToken "user-with-no-builds") [] $ do
          testUser <- mkTestUser
          usage <- usageOverview $ pure $ WebSession testUser (ForgeToken "user-with-no-builds")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage defaultPlan emptyDuration emptyDuration 0 Nothing NoActiveInstallation)])

      it "reports empty usage when the user has no builds this month" $ do
        defaultPlan <- getDefaultPlan
        monthsAgo <- liftIO getCurrentTime <&> subTime (fromDays @Int 90)
        mockGithubInterface (ForgeToken "user-with-one-org") [] $ do
          testUser <- mkTestUser
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          _ <- addTestBuild "owner" monthsAgo (fromSeconds @Int 100)
          usage <- usageOverview $ pure $ WebSession testUser (ForgeToken "user-with-one-org")
          liftIO $ usage `shouldBe` UsageOverview (fromList [("mock-user", OrgUsage defaultPlan emptyDuration emptyDuration 0 Nothing NoActiveInstallation)])

      it "reports usage of all build minutes for the user's installation" $ do
        defaultPlan <- getDefaultPlan
        now <- liftIO getCurrentTime
        mockGithubInterface (ForgeToken "user-with-many-orgs") ["work-org", "org-with-no-builds"] $ do
          testUser <- mkTestUser
          _ <- addTestBuild "mock-user" now (fromSeconds @Int 100)
          _ <- addTestBuild "mock-user" now (fromSeconds @Int 200)
          _ <- addTestBuild "work-org" now (fromSeconds @Int 400)
          _ <- addTestBuild "unrelated-org" now (fromSeconds @Int 100)
          usage <- usageOverview $ pure $ WebSession testUser (ForgeToken "user-with-many-orgs")
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ (RepoOwner $ ForgeLogin "org-with-no-builds", OrgUsage defaultPlan emptyDuration emptyDuration 0 Nothing NoActiveInstallation),
                    (RepoOwner $ ForgeLogin "mock-user", OrgUsage defaultPlan (fromSeconds @Int 300) emptyDuration 0 Nothing NoActiveInstallation),
                    (RepoOwner $ ForgeLogin "work-org", OrgUsage defaultPlan (fromSeconds @Int 400) emptyDuration 0 Nothing NoActiveInstallation)
                  ]
              )

      it "reports over-time in allotted minutes" $ do
        let planBaseCiTime = fromHours @Int 1000
            extraCiTime = fromHours @Int 20
        setExtraUsageLimits "mock-user" (Entitlements.emptyUsageLimits & #ciTime .~ extraCiTime)
        withTestEntitlement "test" (baseCiTime .~ planBaseCiTime) "mock-user" $ do
          mockGithubInterface (ForgeToken "mock-user") [] $ do
            testUser <- mkTestUser
            usage <- usageOverview $ pure $ WebSession testUser (ForgeToken "mock-user")
            usage
              `shouldBeM` UsageOverview
                ( fromList
                    [ ( RepoOwner $ ForgeLogin "mock-user",
                        OrgUsage
                          ( ProductPlan
                              { _productPlanDisplayName = "Plan Title for test",
                                _productPlanDescription = Just "test plan description",
                                _productPlanBaseCiTime = planBaseCiTime,
                                _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                _productPlanIncludedBranchDeploymentHosts = 2,
                                _productPlanMaximumPackagesPerFlake = 100,
                                _productPlanPackageEvaluationTimeout = 30,
                                _productPlanPackageBuildTimeout = 120,
                                _productPlanExtraUsage = emptyUsageLimits & #ciTime .~ extraCiTime,
                                _productPlanIsPaid = True
                              }
                          )
                          emptyDuration
                          emptyDuration
                          0
                          Nothing
                          NoActiveInstallation
                      )
                    ]
                )

    describe "pr deployment minutes" $ do
      it "sums up pr deployment minutes" $ do
        defaultPlan <- getDefaultPlan
        now <- liftIO getCurrentTime
        mockGithubInterface (ForgeToken "token") ["org"] $ do
          testUser <- mkTestUser
          build <- addTestBuild "mock-user" now emptyDuration
          addServer build (Just 42) now (Just $ fromSeconds @Int 1)
          build <- addTestBuild "org" now emptyDuration
          addServer build (Just 42) now (Just $ fromSeconds @Int 2)
          usage <- usageOverview (pure $ WebSession testUser (ForgeToken "token"))
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ ( "org",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = fromSeconds @Int 2,
                          _orgUsageBranchDeploymentHosts = 0,
                          _orgUsageUpgradeOption = Nothing,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    ),
                    ( "mock-user",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = fromSeconds @Int 1,
                          _orgUsageBranchDeploymentHosts = 0,
                          _orgUsageUpgradeOption = Nothing,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    )
                  ]
              )

    describe "branch deployments" $ do
      it "returns the number of running hosts" $ do
        defaultPlan <- getDefaultPlan
        now <- liftIO getCurrentTime
        mockGithubInterface (ForgeToken "token") ["org"] $ do
          testUser <- mkTestUser
          build <- addTestBuild "mock-user" now emptyDuration
          addServer build Nothing now Nothing
          build <- addTestBuild "org" now emptyDuration
          addServer build Nothing now Nothing
          addServer build Nothing now Nothing
          usage <- usageOverview (pure $ WebSession testUser (ForgeToken "token"))
          liftIO
            $ usage
            `shouldBe` UsageOverview
              ( fromList
                  [ ( "org",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = emptyDuration,
                          _orgUsageBranchDeploymentHosts = 2,
                          _orgUsageUpgradeOption = Nothing,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    ),
                    ( "mock-user",
                      OrgUsage
                        { _orgUsagePlan = defaultPlan,
                          _orgUsageCiTime = emptyDuration,
                          _orgUsagePrDeploymentTime = emptyDuration,
                          _orgUsageBranchDeploymentHosts = 1,
                          _orgUsageUpgradeOption = Nothing,
                          _orgUsageInstallationStatus = NoActiveInstallation
                        }
                    )
                  ]
              )

    describe "plans" $ do
      it "returns the current plan of the user"
        $ withTestEntitlement "test" identity "mock-user"
        $ do
          now <- liftIO getCurrentTime
          mockGithubInterface (ForgeToken "token") [] $ do
            testUser <- mkTestUser
            _ <- addTestBuild "mock-user" now (fromSeconds @Int 50)
            (owner, usage) <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> fromSingleton . Map.toList . _usageOverviewByOrg
            liftIO $ do
              owner `shouldBe` RepoOwner (ForgeLogin "mock-user")
              _orgUsagePlan usage
                `shouldBe` ( ProductPlan
                               { _productPlanDisplayName = "Plan Title for test",
                                 _productPlanDescription = Just "test plan description",
                                 _productPlanBaseCiTime = fromMinutes @Int 200000,
                                 _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                 _productPlanIncludedBranchDeploymentHosts = 2,
                                 _productPlanMaximumPackagesPerFlake = 100,
                                 _productPlanPackageEvaluationTimeout = 30,
                                 _productPlanPackageBuildTimeout = 120,
                                 _productPlanExtraUsage = emptyUsageLimits,
                                 _productPlanIsPaid = True
                               }
                           )

      it "returns the current plan of the user, when there's no builds"
        $ withTestEntitlement "test" identity "mock-user"
        $ do
          mockGithubInterface (ForgeToken "token") [] $ do
            testUser <- mkTestUser
            (owner, usage) <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> fromSingleton . Map.toList . _usageOverviewByOrg
            liftIO $ do
              owner `shouldBe` RepoOwner (ForgeLogin "mock-user")
              _orgUsagePlan usage
                `shouldBe` ( ProductPlan
                               { _productPlanDisplayName = "Plan Title for test",
                                 _productPlanDescription = Just "test plan description",
                                 _productPlanBaseCiTime = fromMinutes @Int 200000,
                                 _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                                 _productPlanIncludedBranchDeploymentHosts = 2,
                                 _productPlanMaximumPackagesPerFlake = 100,
                                 _productPlanPackageEvaluationTimeout = 30,
                                 _productPlanPackageBuildTimeout = 120,
                                 _productPlanExtraUsage = emptyUsageLimits,
                                 _productPlanIsPaid = True
                               }
                           )

      it "returns plans for orgs that the user is an admin for (with usage)" $ do
        withTestEntitlement "test" identity "mock-org" $ do
          now <- liftIO getCurrentTime
          mockGithubInterface (ForgeToken "token") ["mock-org"] $ do
            testUser <- mkTestUser
            _ <- addTestBuild "mock-org" now (fromSeconds @Int 50)
            usage <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> _usageOverviewByOrg
            liftIO $ do
              usage ! "mock-org"
                `shouldBe` OrgUsage
                  ( ProductPlan
                      { _productPlanDisplayName = "Plan Title for test",
                        _productPlanDescription = Just "test plan description",
                        _productPlanBaseCiTime = fromMinutes @Int 200000,
                        _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                        _productPlanIncludedBranchDeploymentHosts = 2,
                        _productPlanMaximumPackagesPerFlake = 100,
                        _productPlanPackageEvaluationTimeout = 30,
                        _productPlanPackageBuildTimeout = 120,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                  )
                  (fromSeconds @Int 50)
                  emptyDuration
                  0
                  Nothing
                  NoActiveInstallation

      it "returns plans for orgs that the user is an admin for (without usage)" $ do
        withTestEntitlement "test" identity "mock-org" $ do
          mockGithubInterface (ForgeToken "token") ["mock-org"] $ do
            testUser <- mkTestUser
            usage <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> _usageOverviewByOrg
            liftIO $ do
              usage ! "mock-org"
                `shouldBe` OrgUsage
                  ( ProductPlan
                      { _productPlanDisplayName = "Plan Title for test",
                        _productPlanDescription = Just "test plan description",
                        _productPlanBaseCiTime = fromMinutes @Int 200000,
                        _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                        _productPlanIncludedBranchDeploymentHosts = 2,
                        _productPlanMaximumPackagesPerFlake = 100,
                        _productPlanPackageEvaluationTimeout = 30,
                        _productPlanPackageBuildTimeout = 120,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                  )
                  emptyDuration
                  emptyDuration
                  0
                  Nothing
                  NoActiveInstallation

      it "merges multiple plans into one" $ do
        withTestEntitlement "a" ((includedBranchDeploymentHosts .~ 2) . (maximumPrDeploymentTime .~ fromMinutes @Int 20) . (baseCiTime .~ fromMinutes @Int 200000)) "mock-user" $ do
          withTestEntitlement "b" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 30) . (baseCiTime .~ fromMinutes @Int 300000)) "mock-user" $ do
            mockGithubInterface (ForgeToken "token") [] $ do
              testUser <- mkTestUser
              usage <-
                usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                  <&> _usageOverviewByOrg
              liftIO $ do
                usage
                  `shouldBe` fromList
                    [ ( RepoOwner (ForgeLogin "mock-user"),
                        OrgUsage
                          ( ProductPlan
                              { _productPlanDisplayName = "Plan Title for a, Plan Title for b",
                                _productPlanDescription = Just "a plan description, b plan description",
                                _productPlanBaseCiTime = fromMinutes @Int 300000,
                                _productPlanMaximumPrDeploymentTime = fromMinutes @Int 30,
                                _productPlanIncludedBranchDeploymentHosts = 3,
                                _productPlanMaximumPackagesPerFlake = 100,
                                _productPlanPackageEvaluationTimeout = 30,
                                _productPlanPackageBuildTimeout = 120,
                                _productPlanExtraUsage = emptyUsageLimits,
                                _productPlanIsPaid = True
                              }
                          )
                          emptyDuration
                          emptyDuration
                          0
                          Nothing
                          NoActiveInstallation
                      )
                    ]

      context "when there are plans with and without a priceId" $ do
        let wrap :: M () -> M ()
            wrap action = do
              withTestEntitlement "with-price-id" ((includedBranchDeploymentHosts .~ 3) . (maximumPrDeploymentTime .~ fromMinutes @Int 300) . (baseCiTime .~ fromMinutes @Int 700000)) "mock-user"
                $ withTestEntitlement
                  "without-price-id"
                  ((includedBranchDeploymentHosts .~ 4) . (maximumPrDeploymentTime .~ fromMinutes @Int 400) . (baseCiTime .~ fromMinutes @Int 800000) . (isPaid .~ False))
                  "mock-user"
                  action

        aroundM_ wrap $ do
          it "hides plans without price ids" $ do
            testUser <- mkTestUser
            plan <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> (^. lensProduct displayName description)
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ plan `shouldBe` ("Plan Title for with-price-id", Just "with-price-id plan description")

          it "returns the maximum for entitlements" $ do
            testUser <- mkTestUser
            entitlements <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> ( \p ->
                        ( p ^. includedBranchDeploymentHosts,
                          p ^. maximumPrDeploymentTime,
                          p ^. baseCiTime
                        )
                    )
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ entitlements `shouldBe` (4, fromMinutes @Int 400, fromMinutes @Int 800000)

      context "when there's products without any priceIds" $ do
        it "returns correct entitlements" $ do
          withTestEntitlement "without-price-id" ((maximumPrDeploymentTime .~ fromMinutes @Int 400) . (isPaid .~ False)) "mock-user" $ do
            testUser <- mkTestUser
            entitlements <-
              usageOverview (pure $ WebSession testUser (ForgeToken "token"))
                <&> ( \p ->
                        ( p ^. includedBranchDeploymentHosts,
                          p ^. maximumPrDeploymentTime,
                          p ^. baseCiTime
                        )
                    )
                  . _orgUsagePlan
                  . (! "mock-user")
                  . _usageOverviewByOrg
            liftIO $ entitlements `shouldBe` (2, fromMinutes @Int 400, fromMinutes @Int 200000)

    describe "upgrades" $ do
      let mkTestPlan = do
            stripeApiKey <- view $ #stripe . #publishableKey
            pure
              ( UpgradeOption
                  stripeApiKey
                  (ProductToken "test-product-token")
                  "usd"
                  4200
                  ( ProductPlan
                      { _productPlanDisplayName = "Plan Title for test",
                        _productPlanDescription = Just "test plan description",
                        _productPlanBaseCiTime = fromMinutes @Int 200000,
                        _productPlanMaximumPrDeploymentTime = fromMinutes @Int 100,
                        _productPlanIncludedBranchDeploymentHosts = 2,
                        _productPlanMaximumPackagesPerFlake = 100,
                        _productPlanPackageEvaluationTimeout = 30,
                        _productPlanPackageBuildTimeout = 120,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                  )
              )
      describe "upgradeOptions" $ do
        it "returns an upgrade option, when available" $ do
          withTestProduct "test" identity $ do
            mockGithubInterface (ForgeToken "token") [] $ do
              testUser <- mkTestUser
              options <- upgradeOptions (RepoOwner (testUser ^. githubLogin))
              testPlan <- mkTestPlan
              liftIO $ options `shouldBe` Just testPlan

        it "does not return an upgrade option, when no product is marked visible" $ do
          withTestProduct "test" identity $ do
            mockGithubInterface (ForgeToken "token") [] $ do
              1 <-
                DB.pgExec
                  [pgSQL|
                    UPDATE products
                      SET visible = false
                      WHERE products.name = 'test'
                  |]
              testUser <- mkTestUser
              options <- upgradeOptions (RepoOwner (testUser ^. githubLogin))
              liftIO $ options `shouldBe` Nothing

        it "does not offer non-visible products that have a price_id" $ do
          withTestProduct "non-visible" identity $ do
            withTestProduct "visible" identity $ do
              1 <-
                DB.pgExec
                  [pgSQL|
                    UPDATE products
                      SET visible = false
                      WHERE products.name = 'non-visible'
                  |]
              testUser <- mkTestUser
              options <- upgradeOptions (RepoOwner (testUser ^. githubLogin))
              liftIO $ (options ^? _Just . plan . displayName) `shouldBe` Just "Plan Title for visible"

        it "only offers products that the user is not subscribed to" $ do
          withTestEntitlement "test" identity "mock-user" $ do
            mockGithubInterface (ForgeToken "token") [] $ do
              testUser <- mkTestUser
              options <- upgradeOptions (RepoOwner (testUser ^. githubLogin))
              liftIO $ options `shouldBe` Nothing

        it "offers plans when other users have subscriptions" $ do
          withTestEntitlement "test" identity "other-user" $ do
            mockGithubInterface (ForgeToken "token") [] $ do
              testUser <- mkTestUser
              options <- upgradeOptions (RepoOwner (testUser ^. githubLogin))
              testPlan <- mkTestPlan
              liftIO $ options `shouldBe` Just testPlan

      describe "getUpgradeOptionByToken" $ do
        it "returns an UpgradeOption by product token" $ do
          withTestProduct "test" identity $ do
            option <- getUpgradeOptionByToken $ Just $ ProductToken "test-product-token"
            testPlan <- mkTestPlan
            option `shouldBeM` testPlan

        it "responds with a 404 if the token doesn't exist" $ do
          withTestProduct "test" identity $ do
            response <- try $ getUpgradeOptionByToken $ Just $ ProductToken "does-not-exist"
            (response & _Left %~ err) `shouldBeM` Left NotFound

        it "responds with a 404 if the token exist, but there's no price_id" $ do
          withTestProduct "test" (isPaid .~ False) $ do
            response <- try $ getUpgradeOptionByToken $ Just $ ProductToken "test-product-token"
            (response & _Left %~ err) `shouldBeM` Left NotFound

    describe "subscription status and cancelations"
      $ aroundM_
        ( suppressLogs
            . withTestProduct "test" identity
            . mockGithubInterface (ForgeToken "tok") ["test-org"]
        )
      $ do
        let testBody =
              #product_token .== ProductToken "test-product-token"
                .+ #github_org .== "test-org"

        it "responds with status NoActiveInstallation if the user has not subscribed" $ withServer $ \server -> do
          void server.login
          res <- assert200 $ server.get "/api/account/usage/test-org"
          res ^?! responseBody . key "installation_status" . _Value `shouldBeM` [aesonQQ| { tag: "NoActiveInstallation" } |]

        it "responds with status NoActiveInstallation if stripe has not ever sent us a subscription webhook" $ withServer $ \server -> do
          user <- server.login
          void $ createSubscription (pure $ WebSession user (ForgeToken "tok")) testBody
          res <- assert200 $ server.get "/api/account/usage/test-org"
          res ^?! responseBody . key "installation_status" . _Value `shouldBeM` [aesonQQ| { tag: "NoActiveInstallation" } |]

        it "responds with status InstallationRenewing if the installation has a period attached to it" $ withServer $ \server -> do
          user <- server.login
          void $ createSubscription (pure $ WebSession user (ForgeToken "tok")) testBody
          customerId <- fromJust <$> DB.getInstallationStripeCustomer "test-org"
          DB.updatePeriodForCustomer customerId (parseTimestamp "2025-04-05T00:00:00Z") (parseTimestamp "2025-05-05T00:00:00Z")
          res <- assert200 $ server.get "/api/account/usage/test-org"
          res ^?! responseBody . key "installation_status" . _Value `shouldBeM` [aesonQQ| { tag: "InstallationRenewing", contents: "2025-05-05T00:00:00Z" } |]

        it "responds with status InstallationCancelling if the installation has been cancelled" $ withServer $ \server -> do
          user <- server.login
          void $ createSubscription (pure $ WebSession user (ForgeToken "tok")) testBody
          customerId <- fromJust <$> DB.getInstallationStripeCustomer "test-org"
          DB.updatePeriodForCustomer customerId (parseTimestamp "2025-04-05T00:00:00Z") (parseTimestamp "2025-05-05T00:00:00Z")
          void $ assert200 $ server.delete "/api/account/subscription/test-org"
          res <- assert200 $ server.get "/api/account/usage/test-org"
          res ^?! responseBody . key "installation_status" . _Value `shouldBeM` [aesonQQ| { tag: "InstallationCancelling", contents: "2025-05-05T00:00:00Z" } |]

    describe "createSubscription" $ do
      let org = "test-org"
          testBody =
            #product_token .== ProductToken "test-product-token"
              .+ #github_org .== org
          token = ForgeToken "token"

      it "creates a new stripe customer"
        . withTestProduct "test" identity
        . mockGithubInterface token [org]
        $ do
          user <- mkDbTestUser
          void $ createSubscription (pure $ WebSession user token) testBody
          calls <- getMockCalls #createCustomerMock
          liftIO $ calls `shouldBe` [(org, StripeLib.Name "mock-user", user ^. email)]

      it "stores the stripe id in our DB"
        . withTestProduct "test" identity
        . mockGithubInterface token [org]
        $ do
          user <- mkDbTestUser
          void $ createSubscription (pure $ WebSession user token) testBody
          customerId <- DB.getInstallationStripeCustomer org
          liftIO $ customerId `shouldBe` Just (CustomerId "test-customer-id-0")

      it "re-uses a stripe customer, if exists"
        . withTestProduct "test" identity
        . mockGithubInterface token [org]
        $ do
          user <- mkDbTestUser
          void $ createSubscription (pure $ WebSession user token) testBody
          void $ createSubscription (pure $ WebSession user token) testBody
          customerId <- DB.getInstallationStripeCustomer org
          liftIO $ customerId `shouldBe` Just (CustomerId "test-customer-id-0")

      it "returns a client secret"
        . withTestProduct "test" identity
        . mockGithubInterface token [org]
        $ do
          user <- mkDbTestUser
          result <- createSubscription (pure $ WebSession user token) testBody
          liftIO
            $ result
            ^. #client_secret
            `shouldBe` StripeLib.ClientSecret "client_secret(test-customer-id-0)"
          calls <- fromSingleton <$> getMockCalls #createSubscriptionMock
          liftIO $ calls `shouldBe` (CustomerId "test-customer-id-0", StripeLib.PriceId "test-price", "test", "Plan Title for test")

      it "creates a subscription for the logged in user"
        . withTestProduct "test" identity
        . mockGithubInterface token [org]
        $ do
          user <- mkDbTestUser
          let testBodyWithUserAsOrg =
                testBody
                  & #github_org
                  .~ RepoOwner (user ^. githubLogin)
          void $ createSubscription (pure $ WebSession user token) testBodyWithUserAsOrg
          customerId <- DB.getInstallationStripeCustomer $ RepoOwner $ user ^. githubLogin
          liftIO $ customerId `shouldBe` Just (CustomerId "test-customer-id-0")

    describe "handleSubscriptionAdded" $ do
      it "adds products on SubscriptionCreatedOrUpdated events for user installations"
        $ withTestProduct "test" identity
        $ do
          user <- mkDbTestUser
          let customerId = CustomerId "test-stripe-id"
          DB.setStripeCustomerId (RepoOwner (user ^. githubLogin)) customerId
          now <- liftIO getCurrentTime
          handleSubscriptionAdded $ StripeLib.SubscriptionCreatedOrUpdatedEvent StripeLib.Created customerId (StripeLib.PriceId "test-price") StripeLib.SubscriptionStatusActive now (addTime (fromDays @Int 30) now)
          plan <-
            getPlans [RepoOwner $ user ^. githubLogin]
              <&> (^. displayName) . (! RepoOwner (user ^. githubLogin))
          liftIO $ plan `shouldBe` "Plan Title for test"

      it "adds products on SubscriptionCreatedOrUpdated events for org installations"
        $ withTestProduct "test" identity
        $ do
          let customerId = CustomerId "test-stripe-id"
              testOrg = "test-org"
          DB.setStripeCustomerId testOrg customerId
          now <- liftIO getCurrentTime
          handleSubscriptionAdded $ StripeLib.SubscriptionCreatedOrUpdatedEvent StripeLib.Created customerId (StripeLib.PriceId "test-price") StripeLib.SubscriptionStatusActive now (addTime (fromDays @Int 30) now)
          plan <-
            getPlans [testOrg] <&> (^. displayName) . (! testOrg)
          liftIO $ plan `shouldBe` "Plan Title for test"

      it "doesn't add products on SubscriptionCreatedOrUpdated events, when status isn't 'active'"
        $ withTestProduct "test" identity
        $ do
          defaultPlan <- getDefaultPlan
          let customerId = CustomerId "test-stripe-id"
              testOrg = "test-org"
          DB.setStripeCustomerId testOrg customerId
          now <- liftIO getCurrentTime
          handleSubscriptionAdded $ StripeLib.SubscriptionCreatedOrUpdatedEvent StripeLib.Created customerId (StripeLib.PriceId "test-price") StripeLib.SubscriptionStatusIncomplete now (addTime (fromDays @Int 30) now)
          plans <- getPlans [testOrg]
          liftIO $ plans `shouldBe` ("test-org" ~> defaultPlan)

    describe "handleInvoiceCreated" $ do
      let setupUserWithSubscription :: ExtraUsageLimits -> M (User, CustomerId)
          setupUserWithSubscription extraLimits = do
            now <- liftIO getCurrentTime
            user <- mkDbTestUser
            let customerId = CustomerId "test-stripe-id"
            let repoOwner = RepoOwner (user ^. githubLogin)
            setExtraUsageLimits repoOwner extraLimits
            DB.setStripeCustomerId repoOwner customerId
            handleSubscriptionAdded $ StripeLib.SubscriptionCreatedOrUpdatedEvent StripeLib.Created customerId (StripeLib.PriceId "test-price") StripeLib.SubscriptionStatusActive now (addTime (fromDays @Int 30) now)
            pure (user, customerId)

          useUpCiTime :: User -> Duration -> M ()
          useUpCiTime user amount = do
            now <- liftIO getCurrentTime
            let num2HourBuilds = floor $ amount `divideDuration` fromHours @Int 2
                finalBuildTime = amount `subtractDuration` fromHours (num2HourBuilds * 2)
            replicateM_ num2HourBuilds $ do
              void $ addTestBuild (RepoOwner $ user ^. githubLogin) now (fromHours @Int 2)
            void $ addTestBuild (RepoOwner $ user ^. githubLogin) now finalBuildTime

          useUpPrTime :: User -> Duration -> M ()
          useUpPrTime user amount =
            do
              now <- liftIO getCurrentTime
              testBuild <- addTestBuild (RepoOwner $ user ^. githubLogin) now emptyDuration
              void
                $ addTestServer
                $ (pullRequest ?~ 123)
                . (configurationBuildId .~ testBuild ^. id)
                . (createdAt .~ now)
                . (readyAt ?~ now)
                . (endedAt ?~ addTime amount now)

          mkTestServer :: User -> RepoName -> PackageName -> (ServerInfo -> ServerInfo) -> M ()
          mkTestServer user serverRepoName serverCfgName serverConfig = do
            build <-
              testBuild
                $ (repoUser .~ RepoOwner (user ^. githubLogin))
                . (repoName .~ serverRepoName)
                . (package .~ serverCfgName)
            void $ addTestServer $ (configurationBuildId .~ build ^. id) . serverConfig

          dummyInvoiceCreatedEvent =
            StripeLib.InvoiceCreatedEvent
              { customerId = CustomerId "cus_123",
                invoiceId = InvoiceId "inv_123",
                reason = StripeLib.SubscriptionCycle,
                periodStart = parseTimestamp "2025-01-01T00:00:00Z",
                periodEnd = parseTimestamp "2025-02-01T00:00:00Z"
              }

      describe "ci minutes overage" $ do
        it "adds a line item for minutes used over the provided plan time"
          $ withTestProduct "test" (baseCiTime .~ fromMinutes @Int 100000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #ciTime .~ fromMinutes @Int 200
            useUpCiTime user $ fromMinutes @Int 100123
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCycle)
            getMockCalls #createInvoiceItemMock `shouldReturnM` [(customerId, InvoiceId "inv_123", "Extra CI minutes above plan", StripeLib.UnitAmount "0.6", 123)]

        it "does not bill for comped builds"
          $ withTestProduct "test" (baseCiTime .~ fromMinutes @Int 100000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #ciTime .~ fromMinutes @Int 200
            useUpCiTime user $ fromMinutes @Int 100123
            compAllUserBuilds $ RepoOwner $ user ^. githubLogin
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCycle)
            getMockCalls #createInvoiceItemMock `shouldReturnM` []

        it "does nothing for the initial invoice (SubscriptionCreate billing reason)"
          $ withTestProduct "test" (baseCiTime .~ fromMinutes @Int 100000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #ciTime .~ fromMinutes @Int 200
            useUpCiTime user $ fromMinutes @Int 100201
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCreate)
            getMockCalls #createInvoiceItemMock `shouldReturnM` []

        it "adds no line items if the user is within the provided plan limits"
          $ withTestProduct "test" (baseCiTime .~ fromMinutes @Int 100000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #ciTime .~ fromMinutes @Int 200
            useUpCiTime user $ fromMinutes @Int 100000
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCycle)
            getMockCalls #createInvoiceItemMock `shouldReturnM` []

      describe "pr deployment minutes overage" $ do
        it "adds a line item for deployment minutes used over the provided plan time"
          $ withTestProduct "test" (maximumPrDeploymentTime .~ fromMinutes @Int 1000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #prDeployTime .~ fromMinutes @Int 200
            useUpPrTime user $ fromMinutes @Int 1123
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCycle)
            getMockCalls #createInvoiceItemMock `shouldReturnM` [(customerId, InvoiceId "inv_123", "Extra PR deploy minutes above plan", StripeLib.UnitAmount "0.6", 123)]

        it "does nothing for the initial invoice (SubscriptionCreate billing reason)"
          $ withTestProduct "test" (maximumPrDeploymentTime .~ fromMinutes @Int 1000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #prDeployTime .~ fromMinutes @Int 200
            useUpPrTime user $ fromMinutes @Int 1123
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCreate)
            getMockCalls #createInvoiceItemMock `shouldReturnM` []

        it "adds no line items if the user is within the provided plan limits"
          $ withTestProduct "test" (maximumPrDeploymentTime .~ fromMinutes @Int 1000)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #prDeployTime .~ fromMinutes @Int 200
            useUpPrTime user $ fromMinutes @Int 1000
            handleInvoiceCreated $ dummyInvoiceCreatedEvent & (#customerId .~ customerId) & (#reason .~ StripeLib.SubscriptionCycle)
            getMockCalls #createInvoiceItemMock `shouldReturnM` []

      describe "branch deployment overage" $ do
        it "groups multiple server deployments by owner+repo+server"
          $ withTestProduct "test" (includedBranchDeploymentHosts .~ 1)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #hostingSpend .~ usd 30
            mkTestServer user "my-repo" "my-server"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-05T00:00:00Z")
            mkTestServer user "my-repo" "my-server"
              $ (readyAt ?~ parseTimestamp "2025-01-05T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-10T00:00:00Z")
            mkTestServer user "my-repo" "my-server"
              $ (readyAt ?~ parseTimestamp "2025-01-10T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-15T00:00:00Z")
            mkTestServer user "my-repo" "other-server"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-15T00:00:00Z")
            handleInvoiceCreated
              $ dummyInvoiceCreatedEvent
              & (#customerId .~ customerId)
              & (#reason .~ StripeLib.SubscriptionCycle)
              & (#periodStart .~ parseTimestamp "2025-01-01T00:00:00Z")
              & (#periodEnd .~ parseTimestamp "2025-02-01T00:00:00Z")
            getMockCalls #createInvoiceItemMock
              `shouldReturnM` [ (customerId, InvoiceId "inv_123", "mock-user/my-repo#my-server i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                                (customerId, InvoiceId "inv_123", "mock-user/my-repo#other-server i2x4 (included in plan)", StripeLib.UnitAmount "0", 1)
                              ]

        it "bills for branch server usage above the free tier"
          $ withTestProduct "test" (includedBranchDeploymentHosts .~ 1)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #hostingSpend .~ usd 30
            mkTestServer user "my-repo" "serverA"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-02-01T00:00:00Z")
            mkTestServer user "my-repo" "serverB"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-02-01T00:00:00Z")
            handleInvoiceCreated
              $ dummyInvoiceCreatedEvent
              & (#customerId .~ customerId)
              & (#reason .~ StripeLib.SubscriptionCycle)
              & (#periodStart .~ parseTimestamp "2025-01-01T00:00:00Z")
              & (#periodEnd .~ parseTimestamp "2025-02-01T00:00:00Z")
            getMockCalls #createInvoiceItemMock
              `shouldReturnM` [ (customerId, InvoiceId "inv_123", "mock-user/my-repo#serverA i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                                (customerId, InvoiceId "inv_123", "mock-user/my-repo#serverB i2x4", StripeLib.UnitAmount "1500", 1)
                              ]

        it "bills partial month usage correctly"
          $ withTestProduct "test" (includedBranchDeploymentHosts .~ 1)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #hostingSpend .~ usd 30
            mkTestServer user "my-repo" "serverA"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-02-01T00:00:00Z")
            mkTestServer user "my-repo" "serverB"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-16T00:00:00Z")
            handleInvoiceCreated
              $ dummyInvoiceCreatedEvent
              & (#customerId .~ customerId)
              & (#reason .~ StripeLib.SubscriptionCycle)
              & (#periodStart .~ parseTimestamp "2025-01-01T00:00:00Z")
              & (#periodEnd .~ parseTimestamp "2025-02-01T00:00:00Z")
            getMockCalls #createInvoiceItemMock
              `shouldReturnM` [ (customerId, InvoiceId "inv_123", "mock-user/my-repo#serverA i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                                (customerId, InvoiceId "inv_123", "mock-user/my-repo#serverB i2x4", StripeLib.UnitAmount "726", 1) -- (15 / 31 days used) * $15 = ~$7.258
                              ]

        it "bills exactly the monthly server cost even if there are multiple deploys"
          $ withTestProduct "test" (includedBranchDeploymentHosts .~ 1)
          $ do
            (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #hostingSpend .~ usd 30
            mkTestServer user "my-repo" "free-server"
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-02-01T00:00:00Z")
            mkTestServer user "my-repo" "server-with-many-deploys"
              $ (readyAt ?~ parseTimestamp "2024-12-01T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-05T00:00:00Z")
            mkTestServer user "my-repo" "server-with-many-deploys"
              $ (readyAt ?~ parseTimestamp "2025-01-05T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-10T00:00:00Z")
            mkTestServer user "my-repo" "server-with-many-deploys"
              $ (readyAt ?~ parseTimestamp "2025-01-10T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-15T00:00:00Z")
            mkTestServer user "my-repo" "server-with-many-deploys"
              $ (readyAt ?~ parseTimestamp "2025-01-15T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-01-25T00:00:00Z")
            mkTestServer user "my-repo" "server-with-many-deploys"
              $ (readyAt ?~ parseTimestamp "2025-01-25T00:00:00Z")
              . (endedAt ?~ parseTimestamp "2025-02-05T00:00:00Z")
            handleInvoiceCreated
              $ dummyInvoiceCreatedEvent
              & (#customerId .~ customerId)
              & (#reason .~ StripeLib.SubscriptionCycle)
              & (#periodStart .~ parseTimestamp "2025-01-01T00:00:00Z")
              & (#periodEnd .~ parseTimestamp "2025-02-01T00:00:00Z")
            getMockCalls #createInvoiceItemMock
              `shouldReturnM` [ (customerId, InvoiceId "inv_123", "mock-user/my-repo#free-server i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                                (customerId, InvoiceId "inv_123", "mock-user/my-repo#server-with-many-deploys i2x4", StripeLib.UnitAmount "1500", 1)
                              ]

        it "allows plans to specify the amount of servers included" $ withTestProduct "test" (includedBranchDeploymentHosts .~ 3) $ do
          (user, customerId) <- setupUserWithSubscription $ Entitlements.emptyUsageLimits & #hostingSpend .~ usd 30
          forM_ [1 .. 5 :: Int] $ \i -> do
            mkTestServer user "my-repo" (PackageName $ "server" <> show i)
              $ (readyAt ?~ parseTimestamp "2025-01-01T00:00:00Z")
              . (endedAt .~ Nothing)
          handleInvoiceCreated
            $ dummyInvoiceCreatedEvent
            & (#customerId .~ customerId)
            & (#reason .~ StripeLib.SubscriptionCycle)
            & (#periodStart .~ parseTimestamp "2025-01-01T00:00:00Z")
            & (#periodEnd .~ parseTimestamp "2025-02-01T00:00:00Z")
          getMockCalls #createInvoiceItemMock
            `shouldReturnM` [ (customerId, InvoiceId "inv_123", "mock-user/my-repo#server1 i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                              (customerId, InvoiceId "inv_123", "mock-user/my-repo#server2 i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                              (customerId, InvoiceId "inv_123", "mock-user/my-repo#server3 i2x4 (included in plan)", StripeLib.UnitAmount "0", 1),
                              (customerId, InvoiceId "inv_123", "mock-user/my-repo#server4 i2x4", StripeLib.UnitAmount "1500", 1),
                              (customerId, InvoiceId "inv_123", "mock-user/my-repo#server5 i2x4", StripeLib.UnitAmount "1500", 1)
                            ]

    describe "setting usage limits" $ do
      let newLimits =
            [aesonQQ| {
              ciTime: #{1234 * 60 :: Int},
              prDeployTime: #{567 * 60 :: Int},
              hostingSpend: #{890 * 100 :: Int}
            } |]

      it "returns 401 when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.put "/api/account/usage/some-org" newLimits
        res `shouldHaveStatusCode` 401

      it "returns 401 when making a request to an org that doesn't exist" $ suppressLogs $ withServer $ \server -> do
        void server.login
        res <- server.put "/api/account/usage/some-org" newLimits
        res `shouldHaveStatusCode` 401

      it "returns 400 if the user does not have a plan"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ const
        $ withServer
        $ \server -> do
          user <- RepoOwner . (^. githubLogin) <$> server.login
          res <- server.put ("/api/account/usage/" <> cs (getForgeLogin $ getRepoOwner user)) newLimits
          res `shouldHaveStatusCode` 400

      it "returns 401 when making a request to an org that the user is not an admin of"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [UserOrgMembership "some-org" (Other "user")]
              res <- server.put "/api/account/usage/some-org" newLimits
              res `shouldHaveStatusCode` 401
              (Entitlements.getPlan "some-org" <&> (^. extraUsage . #ciTime)) `shouldReturnM` fromMinutes @Int 0

      it "returns 400 if any values are negative"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [UserOrgMembership "some-org" Admin]
              let assert400 :: Int -> Int -> Int -> String -> M ()
                  assert400 ciTime prDeployTime hostingSpend expectedErr = do
                    let json =
                          [aesonQQ| {
                            ciTime: #{ciTime},
                            prDeployTime: #{prDeployTime},
                            hostingSpend: #{hostingSpend}
                          } |]
                    res <- server.put "/api/account/usage/some-org" json
                    res `shouldHaveStatusCode` 400
                    cs (res ^. responseBody) `shouldContainM` expectedErr
              assert400 (-123) 456 789 "CI time cannot be negative"
              assert400 123 (-456) 789 "PR deploy time cannot be negative"
              assert400 123 456 (-789) "Hosting spend cannot be negative"

      it "updates the extra ci entitlements time"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ \st ->
          withServer $ \server ->
            withTestEntitlement "test" identity "some-org" $ do
              void server.login
              GH.addOrgMembers st [UserOrgMembership "some-org" Admin]
              void $ assert200 $ server.put "/api/account/usage/some-org" newLimits
              plan <- Entitlements.getPlan "some-org"
              plan ^. extraUsage . #ciTime `shouldBeM` fromMinutes @Int 1234
              plan ^. extraUsage . #prDeployTime `shouldBeM` fromMinutes @Int 567
              plan ^. extraUsage . #hostingSpend `shouldBeM` usd 890

      it "allows a GH repo owner to modify their own usage limits (github does not report these as admin)"
        $ suppressLogs
        $ GH.withFakeGithubInterface
        $ const
        $ withServer
        $ \server -> do
          user <- RepoOwner . (^. githubLogin) <$> server.login
          withTestEntitlement "test" identity user $ do
            void $ assert200 $ server.put ("/api/account/usage/" <> cs (getForgeLogin $ getRepoOwner user)) newLimits
            plan <- Entitlements.getPlan user
            plan ^. extraUsage . #ciTime `shouldBeM` fromMinutes @Int 1234
            plan ^. extraUsage . #prDeployTime `shouldBeM` fromMinutes @Int 567
            plan ^. extraUsage . #hostingSpend `shouldBeM` usd 890

    describe "/api/events/stripe" $ do
      it "rejects messages with invalid signatures"
        $ suppressLogs
        $ withServer
        $ \server -> do
          let event =
                [aesonQQ|
                  {
                    "object": "event",
                    "type": "customer.subscription.created"
                  }
                |]
          res <-
            server.postWithHeaders
              "/api/events/stripe"
              [("Stripe-Signature", "t=1715107747,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd")]
              (toJSON event :: Value)
          res `shouldHaveStatusCode` 401

    describe "/api/account/tokens" $ do
      it "return 401 status for GET when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.get "/api/account/tokens"
        res `shouldHaveStatusCode` 401

      it "return 401 status for POST when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.post "/api/account/tokens" [aesonQQ| { name: "my-token" } |]
        res `shouldHaveStatusCode` 401

      it "return 401 status for DELETE when logged out" $ suppressLogs $ withServer $ \server -> do
        res <- server.delete "/api/account/tokens/123"
        res `shouldHaveStatusCode` 401

      it "returns no access tokens when none have been generated yet" $ suppressLogs $ withServer $ \server -> do
        void server.login
        res <- assert200 $ server.get "/api/account/tokens"
        liftIO $ res ^?! responseBody . _Value `shouldBe` [aesonQQ| { tokens: [] } |]

      it "allows generating valid tokens" $ suppressLogs $ withServer $ \server -> do
        user <- server.login
        res <- assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "my-token" } |]
        let token = AccessToken $ res ^?! responseBody . key "token" . _String
        isValid <- isAccessTokenValid (user ^. id) token (^. #cache)
        liftIO $ isValid `shouldBe` True

      it "allows querying generated tokens" $ suppressLogs $ withServer $ \server -> do
        void server.login
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-a", scopes: { cache: true } } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-b", scopes: { api: true } } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-c", scopes: { cache: true, api: true } } |]
        -- for backwards compatibility
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-d" } |]
        res <-
          assert200 (server.get "/api/account/tokens")
            <&> (^. responseBody)
            <&> key "tokens"
              . _Array
              . mapped
              . _Object
              %~ ( Aeson.delete "created"
                     . Aeson.delete "id"
                 )
        decodeThrow (cs res)
          `shouldReturnM` [yamlQQ|
            tokens:
              - name: token-a
                scopes:
                  cache: true
                  api: false
              - name: token-b
                scopes:
                  cache: false
                  api: true
              - name: token-c
                scopes:
                  cache: true
                  api: true
              - name: token-d
                scopes:
                  cache: true
                  api: false
          |]

      it "errors on access tokens with no scopes" $ suppressLogs $ withServer $ \server -> do
        void server.login
        let cases =
              [ [aesonQQ| { name: "token", scopes: {  } } |],
                [aesonQQ| { name: "token", scopes: { cache: false } } |],
                [aesonQQ| { name: "token", scopes: { cache: false, api: false } } |]
              ]
        forM_ cases $ \body -> do
          res <- server.post "/api/account/tokens" body
          res ^. responseStatus `shouldBeM` badRequest400

      it "allows deleting generated tokens" $ suppressLogs $ withServer $ \server -> do
        void server.login
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-a" } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-b" } |]
        void $ assert200 $ server.post "/api/account/tokens" [aesonQQ| { name: "token-c" } |]
        res <- assert200 $ server.get "/api/account/tokens"
        let [a, _b, c] = res ^.. responseBody . key "tokens" . _Array . traverse . key "id" . _Integer
        void $ assert200 $ server.delete $ cs ("/api/account/tokens/" <> show a)
        void $ assert200 $ server.delete $ cs ("/api/account/tokens/" <> show c)
        res <- assert200 $ server.get "/api/account/tokens"
        liftIO
          $ sort (res ^.. responseBody . key "tokens" . _Array . traverse . key "name" . _String)
          `shouldBe` ["token-b"]

    describe "getEnabledRepos" $ do
      let mockGithubInterface =
            withForgeMock
              ( \x ->
                  x
                    { _forgeGetInstallations =
                        const
                          $ pure
                            [ ForgeInstallationId 1,
                              ForgeInstallationId 2
                            ],
                      _forgeGetReposAccessibleTo = \org _ ->
                        pure
                          $ case getForgeInstallationId org of
                            1 -> ["org1/repo1"]
                            2 -> ["org2/repo2"]
                            _ -> []
                    }
              )
      it "lists garnix-enabled repos the user has access to" $ suppressLogs $ do
        mockGithubInterface $ do
          testUser <- mkTestUser
          enabledReposOf (Authenticated $ WebSession testUser (ForgeToken "user-with-no-builds"))
            `shouldReturnM` EnabledRepos ["org1/repo1", "org2/repo2"]

mkTestUser :: M User
mkTestUser = do
  now <- liftIO getCurrentTime
  pure
    $ User
      { _userId = UserId 1,
        _userGithubLogin = ForgeLogin "mock-user",
        _userEmail = Email "mock-user@example.com",
        _userSubscriptionType = FreeSubscription,
        _userCreatedAt = now
      }

mkDbTestUser :: M User
mkDbTestUser = do
  user <- mkTestUser
  DB.newUser (user ^. githubLogin) (user ^. email) (user ^. subscriptionType) False

addServer :: Build -> Maybe PullRequestId -> UTCTime -> Maybe Duration -> M ()
addServer build pr now duration = do
  let (start, end) = case duration of
        Nothing -> (now, Nothing)
        Just duration -> (subTime duration now, Just now)
  res <-
    DB.pgExec
      [pgSQL|
        INSERT INTO servers
          (configuration_build_id, hetzner_id, created_at, ready_at, ended_at, pull_request, ipv4, ipv6, server_tier) VALUES
          (${build ^. id}, 1, ${start}, ${start}, ${end}, ${pr}, '<none>', '<none>', ${def :: ServerTier})
      |]
  case res of
    1 -> pure ()
    n -> throw $ OtherError $ "impossible: " <> show n
