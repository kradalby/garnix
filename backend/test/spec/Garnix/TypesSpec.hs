module Garnix.TypesSpec where

import Control.Lens
import Data.Aeson hiding (Error)
import Data.Aeson.Lens
import Data.String.Interpolate (i)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestInstances ()
import Garnix.Types
import Servant (ServerError (..))
import Test.Hspec

spec :: Spec
spec = describe "Types" $ do
  describe "AuthJwtPayload" $ do
    it "correctly serializes to the old User type for backwards compatability" $ do
      now <- getCurrentTime
      let jwtPayload =
            WebSession
              ( User
                  { _userId = UserId 123,
                    _userGithubLogin = "some-user",
                    _userEmail = "foo@example.org",
                    _userSubscriptionType = FreeSubscription,
                    _userCreatedAt = now
                  }
              )
              (ForgeToken "tok")

      toJSON jwtPayload
        `shouldBe` [aesonQQ|
                     {
                       "id": 123,
                       "github_login": "some-user",
                       "email": "foo@example.org",
                       "subscription_type": "free",
                       "created_at": #{now},
                       "github_token": "tok"
                     }
                   |]

    it "correctly deserializes the old User type for backwards compatability" $ do
      now <- getCurrentTime
      let json =
            [i|
              {
                "id": 123,
                "github_login": "some-user",
                "email": "foo@example.org",
                "subscription_type": "free",
                "created_at": #{encode now},
                "github_token": "tok"
              }
            |]
      eitherDecode' (cs json)
        `shouldBe` Right
          ( WebSession
              ( User
                  { _userId = UserId 123,
                    _userGithubLogin = "some-user",
                    _userEmail = "foo@example.org",
                    _userSubscriptionType = FreeSubscription,
                    _userCreatedAt = now
                  }
              )
              (ForgeToken "tok")
          )

  describe "asPackageType" $ do
    it "roundtrips correctly"
      $ forM_ [minBound .. maxBound]
      $ \pkgType ->
        review asPackageType pkgType ^? asPackageType `shouldBe` Just pkgType

  describe "servantizeError" $ do
    describe "NoSuchError" $ do
      it "contains the user as a field" $ do
        let error = wrapError Error $ NoSuchUser "test-user"
            Right (json :: Value) = eitherDecode' $ errBody $ servantizeError error
            user = json ^?! key "garnixUser"
        user `shouldBe` "test-user"

  describe "errorDetails" $ do
    it "obfuscates github tokens in `RunProcessError`" $ do
      let token = "ghs_puMag5LeethueBee2oof"
          error = wrapError Error $ RunProcessError "git" ["clone", token] ("stderr: " <> token) ("stdout: " <> token) 1
      userMessage (toErrorDetails error) `shouldBe` "git clone XXXXXXXXXXXXXXXX failed with exit code 1\nStderr:\nstderr: XXXXXXXXXXXXXXXX"

    it "obfuscates github tokens in `OtherError`" $ do
      let token = "ghs_puMag5LeethueBee2oof"
          error = wrapError Error $ OtherError ("token: " <> token)
      userMessage (toErrorDetails error) `shouldBe` "token: XXXXXXXXXXXXXXXX"

  describe "buildComment" $ do
    it "converts builds to comments" $ runTestM $ do
      build <- testBuild identity
      liftIO
        $ buildComment build
        `shouldBe` cs
          [i|#{pretty (build ^. id)}_test-owner/test-repo/test-branch|]
