{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Garnix.StripeLib (module Garnix.StripeLib, module Garnix.StripeLib.Types) where

import Control.Concurrent
import Control.Exception.Safe qualified as SafeException
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Map.Strict (Map, insert, member)
import Data.Row (Rec, (.+), (.==), type (.+), type (.==))
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude hiding (get, insert, putStr)
import Garnix.StripeLib.Types
import Garnix.Types
import Network.Wreq (FormParam (..))
import Network.Wreq qualified as Wreq

testImplementation ::
  IO
    ( Mock (RepoOwner, Name, Email) CustomerDto,
      Mock (CustomerId, PriceId, Text, Text) SubscriptionDto,
      Mock (CustomerId, InvoiceId, Text, UnitAmount, Int64) (),
      Mock CustomerId SubscriptionListDto,
      Mock SubscriptionId (),
      Mock PriceId PriceDto
    )
testImplementation = do
  customers :: MVar (Int, Map Email Int) <- newMVar (0, mempty)
  createCustomer <- newMock $ \(repoOwner, name, email) ->
    liftIO $ modifyMVar customers $ \(currentId, customers) -> do
      when (email `member` customers) $ do
        error $ "already a customer: " <> getEmail email
      let id = CustomerId $ "test-customer-id-" <> show currentId
      pure
        ( (currentId + 1, insert email currentId customers),
          #object
            .== ConstString @"customer"
            .+ #id
              .== id
            .+ #name
              .== name
            .+ #email
              .== email
            .+ #metadata
              .== insert "github_account" (getForgeLogin $ getRepoOwner repoOwner) mempty
        )
  now <- getCurrentTime
  subscriptions :: MVar (Int, [SubscriptionId]) <- newMVar (0, mempty)
  createSubscription <- newMock $ \(customerId, _priceId, description, product) -> do
    liftIO $ modifyMVar subscriptions $ \(currentId, subscriptions) -> do
      let subscriptionId = SubscriptionId $ "test-subscription-id-" <> show currentId
      let subscription =
            #object
              .== (ConstString @"subscription")
              .+ #id
                .== subscriptionId
              .+ #customer
                .== customerId
              .+ #latest_invoice
                .== ( #object
                        .== (ConstString @"invoice")
                        .+ #id
                          .== InvoiceId "test-invoice-id"
                        .+ #payment_intent
                          .== ( #object
                                  .== (ConstString @"payment_intent")
                                  .+ #id
                                    .== "test-payment-intent-id"
                                  .+ #status
                                    .== "requires_payment_method"
                                  .+ #client_secret
                                    .== ClientSecret ("client_secret(" <> getCustomerId customerId <> ")")
                              )
                    )
              .+ #status .== SubscriptionStatusIncomplete
              .+ #pending_setup_intent
                .== Null
              .+ #description
                .== Just description
              .+ #metadata .== insert "product" product mempty
              .+ #current_period_start .== UnixTimeStamp now
              .+ #current_period_end .== UnixTimeStamp (addTime (fromDays @Int 30) now)
      pure ((currentId + 1, subscriptionId : subscriptions), subscription)
  createInvoice <- newMock $ \(_customerId, _invoiceId, _description, _unitAmount, _quantity) -> pure ()
  listSubscriptions <- newMock $ \_customer -> do
    (_, subscriptionIds) <- liftIO $ readMVar subscriptions
    pure
      $ #object
      .== (ConstString @"list")
      .+ #data
        .== map
          ( \subscriptionId ->
              #object .== (ConstString @"subscription")
                .+ #id .== subscriptionId
          )
          subscriptionIds
  cancelSubscription <- newMock $ \subscriptionIdToCancel -> do
    liftIO $ modifyMVar_ subscriptions $ pure . second (filter (== subscriptionIdToCancel))
  getPrice <- newMock $ \priceId -> do
    pure
      ( #id
          .== priceId
          .+ #currency
            .== "usd"
          .+ #unit_amount
            .== 4200
      )
  pure (createCustomer, createSubscription, createInvoice, listSubscriptions, cancelSubscription, getPrice)

createCustomer :: RepoOwner -> Name -> Email -> M CustomerDto
createCustomer = curry3 $ mockable #createCustomerMock $ \(repoOwner, name, email) -> do
  let body :: [FormParam] =
        [ "name" := getName name,
          "email" := getEmail email,
          "metadata[github_account]" := getForgeLogin (getRepoOwner repoOwner)
        ]
  post "/customers" body

getCustomers :: M (StripeListPage CustomerDto)
getCustomers = get "/customers"

createSubscription :: CustomerId -> PriceId -> Text -> Text -> M SubscriptionDto
createSubscription = curry4 $ mockable #createSubscriptionMock $ \(customerId, priceId, product, description) -> do
  let body :: [FormParam] =
        [ "customer" := customerId,
          "items[0][price]" := getPriceId priceId,
          "payment_behavior" := "default_incomplete",
          "payment_settings[save_default_payment_method]" := "on_subscription",
          "expand[]" := "latest_invoice.payment_intent",
          "expand[]" := "pending_setup_intent",
          "description" := description,
          "metadata[product]" := product
        ]
  post "/subscriptions" body

listSubscriptions :: CustomerId -> M SubscriptionListDto
listSubscriptions = mockable #listSubscriptionsMock $ \customer -> do
  getWith (Wreq.param "customer" .~ [getCustomerId customer]) "/subscriptions"

cancelSubscription :: SubscriptionId -> M ()
cancelSubscription = mockable #cancelSubscriptionMock $ \subscription -> do
  delete $ "/subscriptions/" <> getSubscriptionId subscription

createInvoiceItem :: CustomerId -> InvoiceId -> Text -> UnitAmount -> Int64 -> M ()
createInvoiceItem = curry5 $ mockable #createInvoiceItemMock $ \(customerId, invoiceId, description, unitAmount, quantity) -> do
  let body :: [FormParam] =
        [ "customer" := customerId,
          "currency" := "usd",
          "description" := description,
          "invoice" := invoiceId,
          "unit_amount_decimal" := toDecimalInCents unitAmount,
          "quantity" := quantity
        ]
  post "/invoiceitems" body

getEvents :: M (StripeListPage (EventDto Text ()))
getEvents = get "/events"

getEvent :: Text -> M (EventDto Text ())
getEvent id = get $ "/events/" <> id

getPrice :: PriceId -> M PriceDto
getPrice = mockable #getPriceMock $ \id -> get $ "/prices/" <> getPriceId id

taxCalculation :: Int64 -> String -> AddressDto -> M TaxCalculationDto
taxCalculation unitAmount currency address = do
  let body :: [FormParam] =
        [ "currency" := currency,
          "line_items[0][amount]" := unitAmount,
          "line_items[0][reference]" := "L1",
          "line_items[0][tax_behavior]" := "inclusive",
          "customer_details[address_source]" := "billing",
          "customer_details[address][line1]" := address ^. #line1,
          "customer_details[address][line2]" := address ^. #line2,
          "customer_details[address][city]" := address ^. #city,
          "customer_details[address][state]" := address ^. #state,
          "customer_details[address][postal_code]" := address ^. #postal_code,
          "customer_details[address][country]" := address ^. #country
        ]
  post "/tax/calculations" body

-- * webhooks

data SubscriptionCreatedOrUpdatedEvent = SubscriptionCreatedOrUpdatedEvent
  { eventType :: CustomerSubscriptionEventType,
    customerId :: CustomerId,
    priceId :: PriceId,
    status :: SubscriptionStatus,
    periodStart :: UTCTime,
    periodEnd :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

data InvoiceCreatedEvent = InvoiceCreatedEvent
  { customerId :: CustomerId,
    invoiceId :: InvoiceId,
    reason :: InvoiceBillingReason,
    periodStart :: UTCTime,
    periodEnd :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

data WebhookEvent
  = SubscriptionCreatedOrUpdated SubscriptionCreatedOrUpdatedEvent
  | InvoiceCreated InvoiceCreatedEvent
  deriving stock (Show, Eq)

fromWebhookRequest :: Value -> M (Maybe WebhookEvent)
fromWebhookRequest v = case fromJSON v :: Result (Rec ("object" .== ConstString "event" .+ "type" .== Text)) of
  Data.Aeson.Error e -> throw $ DecodeError (cs $ encodePretty v) (cs e)
  Data.Aeson.Success event -> do
    case event ^. #type of
      "customer.subscription.created" -> parseSubscriptionCreatedOrUpdated
      "customer.subscription.updated" -> parseSubscriptionCreatedOrUpdated
      "invoice.created" -> parseInvoiceCreated
      _ -> pure Nothing
  where
    parseSubscriptionCreatedOrUpdated =
      case fromJSON v :: Result CustomerSubscriptionCreatedOrUpdatedEventDto of
        Data.Aeson.Success event ->
          case event ^. #data . #object . #items . #data . to (map (^. #price . #id)) of
            [priceId] ->
              pure
                $ Just
                $ SubscriptionCreatedOrUpdated
                $ SubscriptionCreatedOrUpdatedEvent
                  { eventType = event ^. #type,
                    customerId = event ^. #data . #object . #customer,
                    priceId,
                    status = event ^. #data . #object . #status,
                    periodStart = getUnixTimeStamp $ event ^. #data . #object . #current_period_start,
                    periodEnd = getUnixTimeStamp $ event ^. #data . #object . #current_period_end
                  }
            priceIds -> throw $ DecodeError {original = cs $ encode priceIds, message = "expected: exactly one price_id"}
        Data.Aeson.Error e -> throw $ DecodeError (cs $ encodePretty v) (cs e)

    parseInvoiceCreated =
      case fromJSON v :: Result InvoiceCreatedEventDto of
        Data.Aeson.Success event ->
          pure
            $ Just
            $ InvoiceCreated
            $ InvoiceCreatedEvent
              { customerId = event ^. #data . #object . #customer,
                invoiceId = event ^. #data . #object . #id,
                reason = event ^. #data . #object . #billing_reason,
                periodStart = getUnixTimeStamp $ event ^. #data . #object . #period_start,
                periodEnd = getUnixTimeStamp $ event ^. #data . #object . #period_end
              }
        Data.Aeson.Error e -> throw $ DecodeError (cs $ encodePretty v) (cs e)

-- * stripe http helpers

type StripeListPage a =
  Rec
    ( "object"
        .== ConstString "list"
        .+ "has_more"
          .== Bool
        .+ "data"
          .== [a]
    )

withStripeAuth :: (Wreq.Options -> IO a) -> M a
withStripeAuth action = do
  secretKey <- view $ #stripe . #secretKey . to cs
  withWreqOptions $ \options -> do
    action
      $ options
      & Wreq.auth
      ?~ Wreq.basicAuth secretKey ""

get :: (FromJSON a) => Text -> M a
get = getWith identity

getWith :: (FromJSON a) => (Wreq.Options -> Wreq.Options) -> Text -> M a
getWith setOpts path = do
  res <-
    withStripeAuth (\opts -> Wreq.getWith (setOpts opts) ("https://api.stripe.com/v1" <> cs path))
      `SafeException.catchAny` (throw . OtherError . show)
  aesonDecode ("stripe GET /v1" <> path) parseJSON $ cs $ res ^. Wreq.responseBody

post :: (FromJSON b) => Text -> [FormParam] -> M b
post path body = do
  res <-
    withStripeAuth (\opts -> Wreq.postWith opts ("https://api.stripe.com/v1" <> cs path) body)
      `SafeException.catchAny` (throw . OtherError . show)
  aesonDecode ("stripe POST /v1" <> path) parseJSON $ cs $ res ^. Wreq.responseBody

delete :: (FromJSON b) => Text -> M b
delete path = do
  res <-
    withStripeAuth (\opts -> Wreq.deleteWith opts ("https://api.stripe.com/v1" <> cs path))
      `SafeException.catchAny` (throw . OtherError . show)
  aesonDecode ("stripe POST /v1" <> path) parseJSON $ cs $ res ^. Wreq.responseBody
