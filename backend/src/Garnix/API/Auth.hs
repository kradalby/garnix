module Garnix.API.Auth where

import Control.Lens
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Forge.Gitea qualified as Gitea
import Garnix.Forge.Types
import Garnix.Monad
import Garnix.ParseHttpBasicAuth
import Garnix.Prelude
import Garnix.Types hiding (login)
import GitHub qualified as GH
import Network.OAuth2 qualified as OA
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    Cookie,
    JWT,
    acceptLogin,
    clearSession,
  )
import Servant.Auth.Server.Internal.JWT (makeJWT)
import Web.Cookie

data UserDto = UserDto
  { _userDtoUsername :: ForgeLogin,
    _userDtoEmail :: Email,
    _userDtoIsAdmin :: Bool
  }
  deriving stock (Generic)

instance ToJSON UserDto where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

whoAmIAPI :: AuthResult AuthJwtPayload -> M (Maybe UserDto)
whoAmIAPI (Authenticated ((^. #user) -> user)) = do
  pure
    $ Just
    $ UserDto
      (user ^. githubLogin)
      (user ^. email)
      (user ^. subscriptionType == Admin)
whoAmIAPI _ = pure Nothing

data AuthJwtAPI route = AuthJwtAPI
  { jwt :: route :- Header "Authorization" Text :> Auth '[JWT, Cookie] AuthJwtPayload :> Post '[JSON] AuthJwtDto
  }
  deriving stock (Generic)

data AuthJwtDto = AuthJwtDto
  { token :: Text,
    expiresAt :: UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

authJwtAPI :: AuthJwtAPI (AsServerT M)
authJwtAPI =
  AuthJwtAPI
    { jwt = getJwt
    }

getJwt :: Maybe Text -> AuthResult AuthJwtPayload -> M AuthJwtDto
getJwt mAuthHeader authResult = do
  authHeader <- case (mAuthHeader, authResult) of
    (_, Authenticated _) -> throw $ ForbiddenWithMessage "Creating JWTs is only allowed with the api access tokens."
    (Nothing, _) -> throw $ UnauthorizedWithMessage "Missing Authorization header"
    (Just authHeader, _) -> pure authHeader
  (username, password) <- case parseBasicAuth authHeader of
    Left err -> throw $ BadRequest $ cs err
    Right creds -> pure creds
  user <-
    withError
      ( errLens %~ \case
          NoSuchUser _ -> Unauthorized
          err -> err
      )
      $ DB.getUser
      $ ForgeLogin username
  isValid <- isAccessTokenValid (user ^. id) (AccessToken password) (^. #api)
  when (not isValid) $ do
    throw Unauthorized
  jwtSettings' <- view #jwtSettings
  expiresAt <-
    liftIO getCurrentTime
      <&> addUTCTime 3600
  jwt <- liftIO $ makeJWT (ApiSession user) jwtSettings' (Just expiresAt)
  jwt <- case jwt of
    Left err -> throw $ OtherError $ "Failed to create JWT: " <> show err
    Right jwt -> pure jwt
  pure
    $ AuthJwtDto
      { token = cs jwt,
        expiresAt
      }

data LoginAPI route = LoginAPI
  { _loginAPILogin :: route :- Get '[JSON] LoginLinks,
    _loginAPILogout ::
      route
        :- Delete
             '[JSON]
             ( Headers
                 '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
                 ()
             ),
    _loginAPILoginCallback ::
      route
        :- "cb"
        :> QueryParam "code" OAuthCode
        :> Get
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 ForgeLogin
             )
  }
  deriving (Generic)

data SignupAPI route = SignupAPI
  { _signupAPISignup :: route :- Get '[JSON] SignupLinks,
    _signupAPISignupCallback ::
      route
        :- "fill"
        :> QueryParam "code" OAuthCode
        :> Get
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 (CreatingUser ())
             ),
    _signupAPIFinishSignup ::
      route
        :- Auth '[Cookie] (CreatingUser ForgeToken)
        :> ReqBody '[JSON] CreateUser
        :> Post
             '[JSON]
             ( Headers
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ]
                 ForgeLogin
             )
  }
  deriving (Generic)

loginAPI :: LoginAPI (AsServerT M)
loginAPI =
  LoginAPI
    { _loginAPILogin = login,
      _loginAPILogout = logout,
      _loginAPILoginCallback = loginCallback
    }

signupAPI :: SignupAPI (AsServerT M)
signupAPI =
  SignupAPI
    { _signupAPISignup = signup,
      _signupAPISignupCallback = signupCallback,
      _signupAPIFinishSignup = finishSignup
    }

login :: M LoginLinks
login = do
  oaState <- OA.newOAuthState
  ghOauth <- forgeOAuthConfig GitHub OAuthLogin
  githubLink <- OA.getAuthorize oaState ghOauth "foo"
  return $ LoginLinks {_loginLinksGithub = githubLink}

logout ::
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        ()
    )
logout = do
  cookieSettings' <- view #cookieSettings
  return $ clearSession cookieSettings' ()

signup :: M SignupLinks
signup = do
  oaState <- OA.newOAuthState
  ghOauth <- forgeOAuthConfig GitHub OAuthSignup
  githubLink <- OA.getAuthorize oaState ghOauth "foo"
  return
    $ SignupLinks
      { _signupLinksGithub = githubLink
      }

loginCallback ::
  Maybe OAuthCode ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        ForgeLogin
    )
loginCallback code = do
  (login', _, token) <- callbackHelper GitHub OAuthLogin code
  cookieSettings' <- view #cookieSettings
  jwtSettings' <- view #jwtSettings
  user <- DB.getUser login' <?> "calling getUser"
  mApplyCookies <-
    liftIO (acceptLogin cookieSettings' jwtSettings' (WebSession user token))
      <?> "calling acceptLogin"
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies ->
      return
        $ applyCookies
        $ user
        ^. githubLogin

signupCallback ::
  Maybe OAuthCode ->
  M
    ( Headers
        '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
        (CreatingUser ())
    )
signupCallback code = do
  (login', email', token) <- callbackHelper GitHub OAuthSignup code
  eUser <- try $ DB.getUser login' <?> "calling getUser"
  creatingUser <- case eUser of
    Right _ ->
      pure
        $ CreatingUser
          { _creatingUserExists = True,
            _creatingUserGithubLogin = login',
            _creatingUserEmail = email',
            _creatingUserGithubToken = token
          }
    Left ErrorWithContext {err = NoSuchUser {}} ->
      pure
        $ CreatingUser
          { _creatingUserExists = False,
            _creatingUserGithubLogin = login',
            _creatingUserEmail = email',
            _creatingUserGithubToken = token
          }
    Left e -> throwError e
  cookieSettings' <- view #cookieSettings
  jwtSettings' <- view #jwtSettings
  mApplyCookies <- liftIO $ case eUser of
    Right user -> acceptLogin cookieSettings' jwtSettings' (WebSession user token)
    _ -> acceptLogin cookieSettings' jwtSettings' creatingUser
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> return $ applyCookies (void creatingUser)

callbackHelper :: ForgeKind -> OAuthFlow -> Maybe OAuthCode -> M (ForgeLogin, Email, ForgeToken)
callbackHelper _ _ Nothing = throw $ OtherError "'code' param missing"
callbackHelper kind flow (Just (OAuthCode code)) = do
  oaState <- OA.newOAuthState <?> "Creating new oauth state"
  oauth <- forgeOAuthConfig kind flow
  mToken <-
    liftIO (OA.getAuthorized oauth oaState (Just code) Nothing)
      <?> "calling OA.getAuthorized"
  case mToken of
    Nothing -> throw GithubDidntGiveUsAToken
    Just (token, _) -> do
      let forgeToken = ForgeToken $ cs token
      (login', email') <- forgeUserInfo kind forgeToken <?> "fetching forge user info"
      pure (login', email', forgeToken)

-- | Fetch the authenticated user's login and primary email from a forge, given an
-- OAuth user token. Forge-specific; GitHub implemented, others pending (#10).
forgeUserInfo :: ForgeKind -> ForgeToken -> M (ForgeLogin, Email)
forgeUserInfo GitHub (ForgeToken token) = do
  let auth = GH.OAuth $ cs token
  eGhUser <- liftIO (GH.github auth GH.userInfoCurrentR) <?> "calling userInfoCurrentR"
  case eGhUser of
    Left e -> throw $ OtherError $ show e
    Right ghUser -> do
      e <- getEmail auth ghUser <?> "calling getEmail"
      pure (ForgeLogin . GH.untagName $ GH.userLogin ghUser, e)
  where
    getEmail auth ghUser = case GH.userEmail ghUser of
      Just e -> pure $ Email e
      Nothing -> do
        emails <-
          liftIO (GH.github auth $ GH.currentUserEmailsR GH.FetchAll)
            <?> "calling currentUserEmailsR"
        case find GH.emailPrimary <$> emails of
          Right (Just e') -> pure $ Email $ GH.emailAddress e'
          _ -> throw $ OtherError "No email address"
forgeUserInfo Gitea tok = Gitea.giteaUserInfo tok
forgeUserInfo GitLab _ = throw $ OtherError "GitLab user info not implemented yet"

finishSignup ::
  AuthResult (CreatingUser ForgeToken) ->
  CreateUser ->
  M (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ForgeLogin)
finishSignup (Authenticated cUser) addenda = do
  -- The things in AuthResult we can trust, because we put them there
  user <-
    DB.newUser
      (cUser ^. githubLogin)
      (addenda ^. email)
      (addenda ^. subscriptionType)
      (addenda ^. agreeToEmails)
  cookieSettings' <- view #cookieSettings
  jwtSettings' <- view #jwtSettings
  mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings' (WebSession user (cUser ^. githubToken))
  case mApplyCookies of
    Nothing -> throw Unauthorized
    Just applyCookies -> return $ applyCookies $ user ^. githubLogin
finishSignup _ _ = throw $ OtherError "Did not receive expected user info"

-- | Which OAuth flow we're in; determines the callback URL.
data OAuthFlow = OAuthLogin | OAuthSignup

oauthCallbackPath :: OAuthFlow -> Text
oauthCallbackPath = \case
  OAuthLogin -> "login/cb"
  OAuthSignup -> "signup/fill"

-- | The OAuth2 client config for a given forge and flow. Forge-specific endpoints;
-- GitHub implemented, others pending per-forge config (#7) and impls (#10).
forgeOAuthConfig :: ForgeKind -> OAuthFlow -> M OA.OAuth2
forgeOAuthConfig kind flow = do
  fromRelativeUrl <- relativeUrlConverter
  config <- forgeConfig kind
  let clientId = forgeConfigOAuthClientId config
      clientSecret = forgeConfigOAuthClientSecret config
  case kind of
    GitHub ->
      pure
        $ OA.OAuth2
          { oauthClientId = clientId,
            oauthClientSecret = clientSecret,
            oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize",
            oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token",
            oauthCallback = fromRelativeUrl (oauthCallbackPath flow),
            oauthScopes = []
          }
    Gitea ->
      pure
        $ OA.OAuth2
          { oauthClientId = clientId,
            oauthClientSecret = clientSecret,
            oauthOAuthorizeEndpoint = forgeConfigBaseUrl config <> "/login/oauth/authorize",
            oauthAccessTokenEndpoint = forgeConfigBaseUrl config <> "/login/oauth/access_token",
            oauthCallback = fromRelativeUrl (oauthCallbackPath flow),
            oauthScopes = []
          }
    GitLab -> throw $ OtherError "GitLab OAuth not implemented yet"
