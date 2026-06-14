{-# LANGUAGE TemplateHaskell #-}

module Garnix.API.Account where

import Control.Concurrent.Async.Lifted
import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Row (Rec, (.==), type (.+), type (.==))
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (ProductToken (..), getHosting, getPlanByProductToken, getPlans, setExtraUsageLimits)
import Garnix.Entitlements qualified as Entitlements
import Garnix.GithubInterface.Types
import Garnix.Hosting.Helpers (getBranchDeploymentBillingLineItems, groupIdentifierToLineItemDescription)
import Garnix.Monad
import Garnix.Prelude
import Garnix.StripeLib (AddressDto, ClientSecret, PriceId (PriceId), TaxCalculationDto, UnitAmount (UnitAmount), createInvoiceItem, taxCalculation, unitAmountFromCost)
import Garnix.StripeLib qualified as StripeLib
import Garnix.Types hiding (Admin, installationId)
import Servant (Put)
import Servant.Auth.Server

data AccountAPI route = AccountAPI
  { _accountAPIUsage :: route :- "usage" :> Get '[JSON] UsageOverview,
    _accountAPIOrgUsage :: route :- "usage" :> Capture "org" RepoOwner :> Get '[JSON] OrgUsage,
    _accountAPISetUsageLimits :: route :- "usage" :> Capture "org" RepoOwner :> ReqBody '[JSON] ExtraUsageLimits :> Put '[JSON] NoContent,
    _accountAPIUpgradeOption :: route :- "upgrade_option" :> QueryParam "product_token" ProductToken :> Get '[JSON] UpgradeOption,
    _accountAPITaxes :: route :- "taxes" :> ReqBody '[JSON] TaxesRequest :> Post '[JSON] TaxCalculationDto,
    _accountAPIEnabledRepos :: route :- "repos" :> Get '[JSON] EnabledRepos,
    _accountAPISubscribe :: route :- "subscribe" :> ReqBody '[JSON] SubscribeRequestBody :> Post '[JSON] SubscribeResponse,
    _accountAPIUnsubscribe :: route :- "subscription" :> Capture "org" RepoOwner :> Delete '[JSON] NoContent,
    _accountAPIGetAccessTokens :: route :- "tokens" :> Get '[JSON] GetTokensResponseBody,
    _accountAPICreateAccessToken :: route :- "tokens" :> ReqBody '[JSON] CreateTokenRequestBody :> Post '[JSON] CreateTokenResponseBody,
    _accountAPIRevokeAccessToken :: route :- "tokens" :> Capture "tokenId" Int64 :> Delete '[JSON] NoContent
  }
  deriving (Generic)

accountAPI :: AuthResult AuthJwtPayload -> AccountAPI (AsServerT M)
accountAPI user =
  AccountAPI
    { _accountAPIUsage = usageOverview user,
      _accountAPIOrgUsage = orgUsage user,
      _accountAPISetUsageLimits = setUsageLimits user,
      _accountAPIUpgradeOption = getUpgradeOptionByToken,
      _accountAPITaxes = taxes user,
      _accountAPIEnabledRepos = enabledReposOf user,
      _accountAPISubscribe = createSubscription user,
      _accountAPIUnsubscribe = cancelOrgSubscription user,
      _accountAPIGetAccessTokens = getAccessTokens user,
      _accountAPICreateAccessToken = createAccessToken user,
      _accountAPIRevokeAccessToken = revokeAccessToken user
    }

data UsageOverview = UsageOverview
  { _usageOverviewByOrg :: Map.Map RepoOwner OrgUsage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UsageOverview where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data OrgUsage = OrgUsage
  { _orgUsagePlan :: ProductPlan,
    _orgUsageCiTime :: Duration,
    _orgUsagePrDeploymentTime :: Duration,
    _orgUsageBranchDeploymentHosts :: Int64,
    _orgUsageUpgradeOption :: Maybe UpgradeOption,
    _orgUsageInstallationStatus :: InstallationStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OrgUsage where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getOrgsUserIsAdminIn :: User -> ForgeToken -> M [RepoOwner]
getOrgsUserIsAdminIn user token =
  (RepoOwner (user ^. githubLogin) :)
    . map organizationName
    . filter (\membership -> role membership == Admin)
    <$> getInstalledOrgs token

getUsageForOrg :: Map.Map RepoOwner Duration -> Map.Map RepoOwner ProductPlan -> RepoOwner -> M OrgUsage
getUsageForOrg usage plans org = do
  prDeploymentTime <- DB.getPrDeployDurationForOwner org
  branchDeployments <- sum <$> DB.getRunningBranchServersForOwner org
  upgradeOptions <- upgradeOptions org
  plan <- case Map.lookup org plans of
    Just plan -> pure plan
    Nothing -> throw $ OtherError "Impossible: plan missing for org passed into getPlans"
  installationStatus <- DB.getInstallationStatus org
  pure
    OrgUsage
      { _orgUsagePlan = plan,
        _orgUsageCiTime = fromMaybe emptyDuration $ Map.lookup org usage,
        _orgUsagePrDeploymentTime = prDeploymentTime,
        _orgUsageBranchDeploymentHosts = branchDeployments,
        _orgUsageUpgradeOption = upgradeOptions,
        _orgUsageInstallationStatus = installationStatus
      }

usageOverview :: AuthResult AuthJwtPayload -> M UsageOverview
usageOverview (Authenticated (WebSession user ghToken)) = do
  orgs <- getOrgsUserIsAdminIn user ghToken
  usage <- DB.getCurrentMonthUsages (RepoOwner (user ^. githubLogin) : orgs)
  plans <- getPlans orgs
  map <- mkMapM orgs $ getUsageForOrg usage plans
  pure $ UsageOverview map
usageOverview _ = throw Unauthorized

mkMapM :: (Monad m, Ord key) => [key] -> (key -> m value) -> m (Map.Map key value)
mkMapM keys f =
  Map.fromList
    <$> forM
      keys
      ( \key -> do
          value <- f key
          pure (key, value)
      )

orgUsage :: AuthResult AuthJwtPayload -> RepoOwner -> M OrgUsage
orgUsage (Authenticated (WebSession user ghToken)) org = do
  orgsUserIsAdminIn <- getOrgsUserIsAdminIn user ghToken
  when (org `notElem` orgsUserIsAdminIn) $ throw NotFound
  usage <- DB.getCurrentMonthUsages [org]
  plans <- getPlans [org]
  getUsageForOrg usage plans org
orgUsage _ _ = throw Unauthorized

setUsageLimits :: AuthResult AuthJwtPayload -> RepoOwner -> ExtraUsageLimits -> M NoContent
setUsageLimits (Authenticated (WebSession user ghToken)) org newLimits = do
  orgsUserIsAdminIn <- getOrgsUserIsAdminIn user ghToken
  when (org `notElem` orgsUserIsAdminIn) $ throw Unauthorized
  plan <- Entitlements.getPlan org
  unless (plan ^. isPaid) $ do
    throw $ BadRequest "User does not have an active plan"
  setExtraUsageLimits org newLimits
  pure NoContent
setUsageLimits _ _ _ = throw Unauthorized

data UpgradeOption = UpgradeOption
  { _upgradeOptionApiKey :: Text,
    _upgradeOptionProductToken :: ProductToken,
    _upgradeOptionCurrency :: Text,
    _upgradeOptionUnitAmount :: Int64,
    _upgradeOptionPlan :: ProductPlan
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UpgradeOption where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getUpgradeOptionByToken :: Maybe ProductToken -> M UpgradeOption
getUpgradeOptionByToken token = case token of
  Nothing -> throw $ BadRequest "query parameter missing: token"
  Just token -> do
    apiKey <- view $ #stripe . #publishableKey
    result :: [UpgradeOption] <- do
      result <-
        DB.pgQuery
          [pgSQL|
            SELECT
              name,
              title,
              token,
              price_id,
              description,
              ci_minutes,
              pr_hosting,
              hosting,
              packages_per_flake,
              package_eval_timeout_in_minutes,
              package_build_timeout_in_minutes
            FROM products
            WHERE
              token = ${getProductToken token} AND
              price_id IS NOT NULL
          |]
      forM result $ \case
        ( name,
          title,
          Just token,
          Just priceId,
          description,
          ciMinutes :: Maybe Int64,
          prHosting :: Maybe Int64,
          hosting,
          packagesPerFlake,
          packageEvalTimeout :: Maybe Int16,
          packageBuildTimeout :: Maybe Int16
          ) -> do
            stripePrice <- StripeLib.getPrice (PriceId priceId)
            pure
              $ UpgradeOption
                { _upgradeOptionApiKey = apiKey,
                  _upgradeOptionProductToken = ProductToken token,
                  _upgradeOptionCurrency = stripePrice ^. #currency,
                  _upgradeOptionUnitAmount = stripePrice ^. #unit_amount,
                  _upgradeOptionPlan =
                    ProductPlan
                      { _productPlanDisplayName = fromMaybe name title,
                        _productPlanDescription = description,
                        _productPlanBaseCiTime = maybe emptyDuration fromMinutes ciMinutes,
                        _productPlanMaximumPrDeploymentTime = maybe emptyDuration fromMinutes prHosting,
                        _productPlanIncludedBranchDeploymentHosts = fromMaybe 0 hosting,
                        _productPlanMaximumPackagesPerFlake = fromMaybe 100 packagesPerFlake,
                        _productPlanPackageEvaluationTimeout = fromMaybe 30 packageEvalTimeout,
                        _productPlanPackageBuildTimeout = fromMaybe 120 packageBuildTimeout,
                        _productPlanExtraUsage = emptyUsageLimits,
                        _productPlanIsPaid = True
                      }
                }
        _ -> throw $ OtherError "impossible"
    case result of
      [option] -> pure option
      [] -> throw NotFound
      _ -> throw $ OtherError "impossible"

upgradeOptions :: RepoOwner -> M (Maybe UpgradeOption)
upgradeOptions repoOwner = do
  res ::
    [ ( Text,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe Int64,
        Maybe Int64,
        Maybe Int64,
        Maybe Int32,
        Maybe Text,
        Maybe Int16,
        Maybe Int16
      )
    ] <-
    DB.pgQuery
      [pgSQL|
        SELECT
          name,
          title,
          description,
          price_id,
          hosting,
          pr_hosting,
          ci_minutes,
          packages_per_flake,
          token,
          package_eval_timeout_in_minutes,
          package_build_timeout_in_minutes
        FROM products
        WHERE
          visible
          AND token IS NOT NULL
          AND price_id IS NOT NULL
          AND NOT EXISTS
            (SELECT 1 FROM repo_owner_has_product WHERE
              repo_owner_has_product.product = products.name
              AND repo_owner_has_product.repo_owner = ${repoOwner}
            )
      |]
  case res of
    ( name,
      title,
      description,
      Just priceId,
      branchHosting,
      prDeployMinutes,
      ciMinutes,
      packagesPerFlake,
      Just token,
      packageEvalTimeout,
      packageBuildTimeout
      )
      : _ -> do
        stripePrice <- StripeLib.getPrice (PriceId priceId)
        apiKey <- view $ #stripe . #publishableKey
        pure
          $ Just
          $ UpgradeOption
            apiKey
            (ProductToken token)
            (stripePrice ^. #currency)
            (stripePrice ^. #unit_amount)
            ( ProductPlan
                { _productPlanDisplayName = fromMaybe name title,
                  _productPlanDescription = description,
                  _productPlanBaseCiTime = maybe emptyDuration fromMinutes ciMinutes,
                  _productPlanMaximumPrDeploymentTime = maybe emptyDuration fromMinutes prDeployMinutes,
                  _productPlanIncludedBranchDeploymentHosts = fromMaybe 0 branchHosting,
                  _productPlanMaximumPackagesPerFlake = fromMaybe 100 packagesPerFlake,
                  _productPlanPackageEvaluationTimeout = fromMaybe 30 packageEvalTimeout,
                  _productPlanPackageBuildTimeout = fromMaybe 120 packageBuildTimeout,
                  _productPlanExtraUsage = emptyUsageLimits,
                  _productPlanIsPaid = True
                }
            )
    _ : _ -> throw $ OtherError "impossible"
    [] -> pure Nothing

type TaxesRequest =
  Rec
    ( "unit_amount" .== Int64
        .+ "currency" .== String
        .+ "address" .== AddressDto
    )

taxes :: AuthResult AuthJwtPayload -> TaxesRequest -> M TaxCalculationDto
taxes user request = case user of
  Authenticated _ -> taxCalculation (request ^. #unit_amount) (request ^. #currency) (request ^. #address)
  _ -> throw Unauthorized

type SubscribeRequestBody = Rec ("product_token" .== ProductToken .+ "github_org" .== RepoOwner)

type SubscribeResponse = Rec ("client_secret" .== ClientSecret)

createSubscription :: AuthResult AuthJwtPayload -> SubscribeRequestBody -> M SubscribeResponse
createSubscription (Authenticated (WebSession user ghToken)) body = do
  orgs <- getOrgsUserIsAdminIn user ghToken
  when (body ^. #github_org `notElem` orgs) $ throw Forbidden
  mCustomerId <- DB.getInstallationStripeCustomer (body ^. #github_org)
  customer <-
    case mCustomerId of
      Nothing -> do
        customer <-
          StripeLib.createCustomer
            (body ^. #github_org)
            (StripeLib.Name $ user ^. githubLogin . to getForgeLogin)
            (user ^. email)
        DB.setStripeCustomerId (body ^. #github_org) (customer ^. #id)
        pure $ customer ^. #id
      Just id -> pure id
  plan <- getPlanByProductToken (body ^. #product_token)
  let throwNotFound = throw $ OtherError $ "No price_id found for product with product_token: " <> show (body ^. #product_token)
  case plan of
    Nothing -> throwNotFound
    (Just (_, Nothing, _)) -> throwNotFound
    Just (name, Just priceId, plan') -> do
      subscription <- StripeLib.createSubscription customer priceId name (plan' ^. displayName)
      pure
        ( #client_secret
            .== subscription
              ^. #latest_invoice
                . #payment_intent
                . #client_secret
        )
createSubscription _ _ = throw Unauthorized

cancelOrgSubscription :: AuthResult AuthJwtPayload -> RepoOwner -> M NoContent
cancelOrgSubscription (Authenticated (WebSession user ghToken)) org = do
  orgsUserIsAdminIn <- getOrgsUserIsAdminIn user ghToken
  when (org `notElem` orgsUserIsAdminIn) $ throw NotFound
  mCustomer <- DB.getInstallationStripeCustomer org
  case mCustomer of
    Nothing -> throw NotFound
    Just customerId -> do
      withTextSpans
        [ ("stripe_customer_id", show customerId),
          ("repo_owner", show org)
        ]
        $ do
          subscriptions <- (^.. #data . traverse . #id) <$> StripeLib.listSubscriptions customerId
          case subscriptions of
            [] -> do
              log Critical "Failed to unsubscribe: No subscription found"
              throw NotFound
            _ : _ : _ -> do
              log Critical "Failed to unsubscribe: More than one subscription found"
              throw NotFound
            [subscription] -> do
              StripeLib.cancelSubscription subscription
              DB.setRequestedCancellation org True
              pure NoContent
cancelOrgSubscription _ _ = throw Unauthorized

handleSubscriptionAdded :: StripeLib.SubscriptionCreatedOrUpdatedEvent -> M ()
handleSubscriptionAdded (StripeLib.SubscriptionCreatedOrUpdatedEvent _eventType customerId priceId status periodStart periodEnd) = do
  when (status == StripeLib.SubscriptionStatusActive) $ do
    repoOwner <- getRepoOwner customerId
    Entitlements.addProductByPriceId repoOwner priceId
    DB.updatePeriodForCustomer customerId periodStart periodEnd
  where
    getRepoOwner :: CustomerId -> M RepoOwner
    getRepoOwner customerId = do
      res <-
        DB.pgQuery
          [pgSQL|
            SELECT repo_owner FROM installations
            WHERE stripe_customer = ${getCustomerId customerId}
          |]
      case res of
        [] -> throw $ OtherError $ "Cannot find stripe customer " <> getCustomerId customerId
        [id'] -> pure id'
        _ -> throw $ OtherError "impossible: stripe_customer should be unique"

handleInvoiceCreated :: StripeLib.InvoiceCreatedEvent -> M ()
handleInvoiceCreated (StripeLib.InvoiceCreatedEvent customerId invoiceId reason periodStart periodEnd) = do
  withTextSpans
    [ ("stripe_customer_id", show customerId),
      ("stripe_invoice_id", show invoiceId),
      ("billing_reason", show reason)
    ]
    $ do
      log Notice "Invoice created"
      when (reason == StripeLib.SubscriptionCycle) $ do
        mRepoOwner <- DB.getRepoOwnerForStripeCustomer customerId
        case mRepoOwner of
          Nothing -> do
            let err = "Invoice was created for an existing subscription, but we are missing the installation"
            log Critical err
            throw $ OtherError err
          Just repoOwner -> do
            plan <- Entitlements.getPlan repoOwner
            billOverage "CI minutes" (UnitAmount "0.6") (plan ^. baseCiTime)
              =<< DB.getCurrentMonthUsage repoOwner
            billOverage "PR deploy minutes" (UnitAmount "0.6") (plan ^. maximumPrDeploymentTime)
              =<< DB.getPrDeployDurationForOwner repoOwner
            numFreeServers <- (^. #planIncludedBranchDeploymentHosts) <$> getHosting repoOwner
            branchDeploymentLineItems <- getBranchDeploymentBillingLineItems numFreeServers periodStart periodEnd repoOwner
            forM_ branchDeploymentLineItems $ \lineItem -> do
              let groupName =
                    groupIdentifierToLineItemDescription (lineItem ^. #group)
                      <> if lineItem ^. #includedInPlan then " (included in plan)" else ""
              createInvoiceItem customerId invoiceId groupName (unitAmountFromCost $ lineItem ^. #cost) 1
  where
    billOverage :: Text -> UnitAmount -> Duration -> Duration -> M ()
    billOverage name unitAmount basePlanTime currentUsage = do
      log Notice $ "Plan " <> name <> " time has " <> show (toMinutes basePlanTime) <> " min. Current usage is " <> show (toMinutes currentUsage) <> " min"
      let usedExtraMinutes :: Int64 = floor $ toMinutes $ currentUsage `subtractDuration` basePlanTime
      when (usedExtraMinutes > 0) $ do
        log Notice $ "Adding " <> name <> " invoice item of " <> show usedExtraMinutes <> " extra minutes"
        createInvoiceItem customerId invoiceId ("Extra " <> name <> " above plan") unitAmount usedExtraMinutes

data GetTokensResponseBody = GetTokensResponseBody
  { _getTokensResponseBodyTokens :: [AccessTokenMetadata]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GetTokensResponseBody where
  toJSON = ourToJSON

data CreateTokenRequestBody = CreateTokenRequestBody
  { _createTokenRequestBodyName :: Text,
    _createTokenRequestBodyScopes :: Maybe AccessTokenScopes
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateTokenRequestBody where
  parseJSON = ourParseJSON

data CreateTokenResponseBody = CreateTokenResponseBody
  { _createTokenResponseBodyToken :: AccessToken
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CreateTokenResponseBody where
  toJSON = ourToJSON

getAccessTokens :: AuthResult AuthJwtPayload -> M GetTokensResponseBody
getAccessTokens (Authenticated ((^. #user) -> user)) = GetTokensResponseBody <$> DB.getAccessTokensForUser (user ^. id)
getAccessTokens _ = throw Unauthorized

-- For backwards compatability during the first deploy, if `scopes` is not provided, we default to just a cache scope.
fallbackAccessTokenScopes :: AccessTokenScopes
fallbackAccessTokenScopes = AccessTokenScopes {api = False, cache = True}

createAccessToken :: AuthResult AuthJwtPayload -> CreateTokenRequestBody -> M CreateTokenResponseBody
createAccessToken (Authenticated (WebSession user _)) (CreateTokenRequestBody name (fromMaybe fallbackAccessTokenScopes -> scopes)) = do
  when (scopes == AccessTokenScopes {api = False, cache = False}) $ do
    throw $ BadRequest "no scopes enabled"
  accessToken <- generateToken (user ^. id) name scopes
  pure $ CreateTokenResponseBody accessToken
createAccessToken (Authenticated (ApiSession _)) _ = throw $ ForbiddenWithMessage "This endpoint is not available through the programmatic api."
createAccessToken _ _ = throw Unauthorized

revokeAccessToken :: AuthResult AuthJwtPayload -> Int64 -> M NoContent
revokeAccessToken (Authenticated ((^. #user) -> user)) tokenId = do
  DB.deleteAccessTokenForUser (user ^. id) tokenId
  pure NoContent
revokeAccessToken _ _ = throw Unauthorized

enabledReposOf :: AuthResult AuthJwtPayload -> M EnabledRepos
enabledReposOf (Authenticated (WebSession _ ghToken)) = do
  installationIds <- getInstallations ghToken
  repos <- forConcurrently installationIds $ \id ->
    getReposInInstallationAccessibleTo id ghToken
  return
    $ EnabledRepos
    $ mconcat repos
enabledReposOf _ = throw Unauthorized

data EnabledRepos = EnabledRepos {_enabledReposRepos :: [Text]}
  deriving stock (Eq, Show, Generic)

instance ToJSON EnabledRepos where
  toJSON = ourToJSON

makeFields ''UpgradeOption
