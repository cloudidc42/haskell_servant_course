# Part 24: Security เชิงลึก
## ขั้นตอนที่ 461-480: Web Security ใน Haskell

---

## ขั้นตอนที่ 461: OWASP Top 10 - SQL Injection

```haskell
-- SQL Injection Prevention

-- ❌ อันตราย: string concatenation
dangerousQuery :: Text -> IO [User]
dangerousQuery userInput = do
  conn <- getConn
  query conn ("SELECT * FROM users WHERE name = '" <> userInput <> "'")
  -- Input: "admin'; DROP TABLE users; --"

-- ✓ ปลอดภัย: parameterized queries ด้วย Persistent
safeQuery :: Text -> Handler [User]
safeQuery name = do
  users <- runDB $ selectList [UserName ==. name] []
  return (map entityVal users)

-- ✓ ปลอดภัย: postgresql-simple with ? placeholders
safeRawQuery :: Text -> Handler [User]
safeRawQuery name = do
  pool <- getPool
  users <- liftIO $ withResource pool $ \conn ->
    query conn "SELECT * FROM users WHERE name = ?" (Only name)
  return users

-- ✓ ปลอดภัย: Esqueleto type-safe SQL
safeEsqQuery :: Text -> Int -> DB [Entity User]
safeEsqQuery name minAge = do
  E.select $ E.from $ \u -> do
    E.where_ (u ^. UserName E.==. E.val name
         E.&&. u ^. UserAge  E.>=. E.val minAge)
    return u
```

---

## ขั้นตอนที่ 462: XSS Prevention

```haskell
-- Cross-Site Scripting (XSS) Prevention

-- ✓ Hamlet templates escape ให้อัตโนมัติ
safeTemplate :: Text -> Widget
safeTemplate userInput = [whamlet|
  <p>#{userInput}
|]
-- #{userInput} -> HTML-escaped automatically
-- "<script>alert('xss')</script>" -> "&lt;script&gt;..."

-- ❌ อันตราย: preEscapedText
dangerousTemplate :: Text -> Widget
dangerousTemplate userInput = [whamlet|
  <p>#{preEscapedText userInput}
|]
-- ห้ามใช้กับ user input!

-- ✓ Content Security Policy
addCspHeader :: Handler ()
addCspHeader = addHeader "Content-Security-Policy" $
  T.intercalate "; "
    [ "default-src 'self'"
    , "script-src 'self' 'nonce-" <> nonce <> "'"
    , "style-src 'self' 'unsafe-inline'"
    , "img-src 'self' data: https:"
    , "connect-src 'self' https://api.example.com"
    , "form-action 'self'"
    , "frame-ancestors 'none'"
    ]

-- ✓ Sanitize HTML (allow safe tags only)
import Text.HTML.SanitizeXSS

sanitizeUserHtml :: Text -> Text
sanitizeUserHtml = sanitize

-- Allow only specific tags
safeTags :: TagSet
safeTags = S.fromList ["p", "br", "strong", "em", "ul", "ol", "li", "a"]

safeAttr :: Text -> Text -> Bool
safeAttr "a" "href" = True
safeAttr _ _         = False
```

---

## ขั้นตอนที่ 463: CSRF Protection

```haskell
-- CSRF (Cross-Site Request Forgery) Protection

-- Yesod มี CSRF protection built-in
-- defaultCsrfMiddleware ใน defaultYesodMiddleware

-- CSRF token ใน forms
getFormR :: Handler Html
getFormR = do
  token <- getCsrfToken
  defaultLayout [whamlet|
    <form method=post>
      <input type=hidden name=_token value=#{token}>
      <input type=text name=data>
      <button type=submit>Submit
  |]

-- หรือใช้ csrfHiddenInput
getFormSafeR :: Handler Html
getFormSafeR = do
  defaultLayout [whamlet|
    <form method=post>
      ^{csrfHiddenInput}
      <input type=text name=data>
      <button type=submit>Submit
  |]

-- Servant: CSRF header check
type CSRFProtected api = Header "X-CSRF-Token" Text :> api

checkCsrfToken :: Maybe Text -> Handler ()
checkCsrfToken Nothing    = throwError err403 { errBody = "Missing CSRF token" }
checkCsrfToken (Just tok) = do
  valid <- validateCsrfToken tok
  unless valid $ throwError err403 { errBody = "Invalid CSRF token" }

-- Double Submit Cookie pattern (for SPAs)
addCsrfCookie :: Response -> Response
addCsrfCookie res = do
  token <- generateToken
  mapResponseHeaders
    (("Set-Cookie", "csrf_token=" <> encodeUtf8 token <> "; SameSite=Strict"):)
    res
```

---

## ขั้นตอนที่ 464: JWT Advanced

```haskell
-- JWT (JSON Web Tokens) ขั้นสูง

import Jose.Jwt
import Jose.Jwa
import Jose.Jws

-- RS256 (asymmetric) JWT
data JwtConfig = JwtConfig
  { jwtPrivateKey :: RSAPrivateKey
  , jwtPublicKey  :: RSAPublicKey
  , jwtExpiry     :: Int  -- seconds
  , jwtIssuer     :: Text
  , jwtAudience   :: [Text]
  }

-- Create JWT with RS256
createRs256Token :: JwtConfig -> UserClaims -> IO Text
createRs256Token cfg claims = do
  now <- getCurrentTime
  let payload = JwtClaims
        { jwtSub  = Just (T.pack (show (unUserId (claimsUserId claims))))
        , jwtIss  = Just (jwtIssuer cfg)
        , jwtAud  = Just (jwtAudience cfg)
        , jwtExp  = Just (addUTCTime (fromIntegral (jwtExpiry cfg)) now)
        , jwtIat  = Just now
        , jwtNbf  = Just now
        , jwtJti  = Nothing
        , jwtExtra = Map.fromList
            [ "email" .= claimsEmail claims
            , "roles" .= claimsRoles claims
            ]
        }
  signWith (jwtPrivateKey cfg) RS256 (encode payload)

-- Verify JWT
verifyToken :: JwtConfig -> Text -> IO (Either JwtError UserClaims)
verifyToken cfg token = do
  result <- verifyJws (jwtPublicKey cfg) (encodeUtf8 token)
  case result of
    Left err -> return (Left (InvalidSignature (T.pack (show err))))
    Right payload -> do
      case decode payload of
        Nothing     -> return (Left MalformedPayload)
        Just claims -> do
          now <- getCurrentTime
          case jwtExp claims of
            Just exp | exp < now -> return (Left TokenExpired)
            _ -> return (Right (extractClaims claims))

-- Token refresh
data RefreshToken = RefreshToken
  { rtUserId    :: UserId
  , rtTokenId   :: Text
  , rtExpiresAt :: UTCTime
  , rtRevoked   :: Bool
  }

issueRefreshToken :: UserId -> IO (RefreshToken, Text)
issueRefreshToken uid = do
  tokenId <- generateSecureToken 32
  expiry  <- addUTCTime (30 * 24 * 3600) <$> getCurrentTime
  let rt = RefreshToken uid tokenId expiry False
  saveRefreshToken rt
  return (rt, tokenId)

refreshAccessToken :: Text -> JwtConfig -> IO (Either RefreshError (Text, Text))
refreshAccessToken refreshToken cfg = do
  mToken <- loadRefreshToken refreshToken
  case mToken of
    Nothing -> return (Left TokenNotFound)
    Just rt
      | rtRevoked   rt -> return (Left TokenRevoked)
      | rtExpiresAt rt < getCurrentTime -> return (Left TokenExpired)
      | otherwise -> do
          -- Revoke old refresh token (rotation)
          revokeRefreshToken refreshToken
          -- Issue new tokens
          user     <- getUser (rtUserId rt)
          newAccess  <- createRs256Token cfg (userToClaims user)
          (_, newRefresh) <- issueRefreshToken (rtUserId rt)
          return (Right (newAccess, newRefresh))
```

---

## ขั้นตอนที่ 465: OAuth 2.0

```haskell
-- OAuth 2.0 implementation

data OAuthConfig = OAuthConfig
  { oauthClientId     :: Text
  , oauthClientSecret :: Text
  , oauthRedirectUri  :: Text
  , oauthAuthEndpoint :: Text
  , oauthTokenEndpoint :: Text
  , oauthScopes       :: [Text]
  }

-- Authorization Code Flow
-- Step 1: Redirect user to authorization server
getAuthorizationUrlR :: Handler ()
getAuthorizationUrlR = do
  state <- generateState
  setSession "oauth_state" state
  
  let params =
        [ ("response_type", "code")
        , ("client_id", oauthClientId config)
        , ("redirect_uri", oauthRedirectUri config)
        , ("scope", T.intercalate " " (oauthScopes config))
        , ("state", state)
        ]
  redirect (oauthAuthEndpoint config <> "?" <> urlEncodeParams params)

-- Step 2: Handle callback
getCallbackR :: Handler ()
getCallbackR = do
  code  <- lookupGetParam "code"  >>= maybe (invalidArgs ["Missing code"]) return
  state <- lookupGetParam "state" >>= maybe (invalidArgs ["Missing state"]) return
  
  -- Verify state
  mSavedState <- lookupSession "oauth_state"
  deleteSession "oauth_state"
  when (mSavedState /= Just state) $ permissionDenied "State mismatch"
  
  -- Exchange code for tokens
  tokens <- exchangeCode code
  
  -- Get user info
  userInfo <- fetchUserInfo (accessToken tokens)
  
  -- Create/update user
  uid <- upsertOAuthUser userInfo
  
  -- Set session
  setUserSession uid
  redirect HomeR

-- Exchange authorization code for tokens
exchangeCode :: Text -> Handler TokenResponse
exchangeCode code = do
  manager <- liftIO (newManager defaultManagerSettings)
  let body =
        [ ("grant_type",    "authorization_code")
        , ("code",          encodeUtf8 code)
        , ("redirect_uri",  encodeUtf8 (oauthRedirectUri config))
        , ("client_id",     encodeUtf8 (oauthClientId config))
        , ("client_secret", encodeUtf8 (oauthClientSecret config))
        ]
  response <- liftIO $ post manager (T.unpack (oauthTokenEndpoint config))
    (urlEncodeBody body)
  case decode (responseBody response) of
    Nothing -> throwError err500 { errBody = "Invalid token response" }
    Just t  -> return t

-- Client Credentials Flow (server-to-server)
getClientCredentialsToken :: OAuthConfig -> IO Text
getClientCredentialsToken cfg = do
  let body =
        [ ("grant_type",    "client_credentials")
        , ("client_id",     encodeUtf8 (oauthClientId cfg))
        , ("client_secret", encodeUtf8 (oauthClientSecret cfg))
        , ("scope",         encodeUtf8 (T.intercalate " " (oauthScopes cfg)))
        ]
  response <- post manager (T.unpack (oauthTokenEndpoint cfg)) (urlEncodeBody body)
  case decode (responseBody response) of
    Nothing -> throwIO TokenRequestFailed
    Just t  -> return (accessToken t)
```

---

## ขั้นตอนที่ 466: OpenID Connect

```haskell
-- OpenID Connect (OIDC)

data OidcConfig = OidcConfig
  { oidcIssuer       :: Text
  , oidcClientId     :: Text
  , oidcClientSecret :: Text
  , oidcRedirectUri  :: Text
  }

data OidcEndpoints = OidcEndpoints
  { oidcAuthEndpoint  :: Text
  , oidcTokenEndpoint :: Text
  , oidcUserinfoEndpoint :: Text
  , oidcJwksUri       :: Text
  }

-- Discover OIDC configuration
discoverOidc :: Text -> IO OidcEndpoints
discoverOidc issuer = do
  let discoveryUrl = issuer <> "/.well-known/openid-configuration"
  response <- httpJSON discoveryUrl
  return OidcEndpoints
    { oidcAuthEndpoint     = response .: "authorization_endpoint"
    , oidcTokenEndpoint    = response .: "token_endpoint"
    , oidcUserinfoEndpoint = response .: "userinfo_endpoint"
    , oidcJwksUri          = response .: "jwks_uri"
    }

-- Validate ID Token
data IdToken = IdToken
  { idTokenSub   :: Text
  , idTokenEmail :: Text
  , idTokenName  :: Maybe Text
  , idTokenIss   :: Text
  , idTokenAud   :: [Text]
  , idTokenExp   :: UTCTime
  , idTokenIat   :: UTCTime
  , idTokenNonce :: Maybe Text
  }

validateIdToken :: OidcConfig -> Text -> IO (Either OidcError IdToken)
validateIdToken cfg rawToken = do
  -- Get JWKS
  jwks <- fetchJwks
  
  -- Decode and verify
  result <- verifyJwtWithJwks jwks (encodeUtf8 rawToken)
  case result of
    Left err -> return (Left (InvalidSignature (show err)))
    Right payload -> do
      case decode payload of
        Nothing    -> return (Left MalformedToken)
        Just token -> do
          now <- getCurrentTime
          
          -- Validate claims
          let checks =
                [ (idTokenIss token == oidcIssuer cfg, WrongIssuer)
                , (oidcClientId cfg `elem` idTokenAud token, WrongAudience)
                , (idTokenExp token > now, TokenExpired)
                ]
          
          case find (not . fst) checks of
            Just (_, err) -> return (Left err)
            Nothing       -> return (Right token)

-- Fetch user info
fetchOidcUserInfo :: OidcEndpoints -> Text -> IO OidcUserInfo
fetchOidcUserInfo endpoints accessToken = do
  req <- parseRequest (T.unpack (oidcUserinfoEndpoint endpoints))
  let authReq = req { requestHeaders = ("Authorization", "Bearer " <> encodeUtf8 accessToken) : requestHeaders req }
  response <- httpJSON authReq
  return response
```

---

## ขั้นตอนที่ 467: Role-Based Access Control (RBAC)

```haskell
-- RBAC: Role-Based Access Control

-- Permission types
data Permission
  = ReadUsers | WriteUsers | DeleteUsers
  | ReadPosts | WritePosts | DeletePosts
  | ManageSystem
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Role definitions
data Role = GuestRole | UserRole | ModeratorRole | AdminRole
  deriving (Show, Eq, Ord)

rolePermissions :: Role -> Set Permission
rolePermissions GuestRole = S.fromList
  [ ReadPosts ]
rolePermissions UserRole = S.fromList
  [ ReadPosts, WritePosts ]
rolePermissions ModeratorRole = S.fromList
  [ ReadPosts, WritePosts, DeletePosts, ReadUsers ]
rolePermissions AdminRole = S.fromList [minBound..maxBound]

-- ABAC: Attribute-Based Access Control
data Resource = Resource
  { resourceType  :: Text
  , resourceId    :: Int64
  , resourceOwner :: Maybe UserId
}

data AccessRequest = AccessRequest
  { arUser       :: User
  , arResource   :: Resource
  , arPermission :: Permission
}

-- Policy
data Policy = Policy
  { policyName    :: Text
  , policyEffect  :: Effect
  , policyApplies :: AccessRequest -> Bool
  }

data Effect = Allow | Deny

-- Policy engine
checkAccess :: [Policy] -> AccessRequest -> Effect
checkAccess policies req =
  let applicablePolicies = filter (\p -> policyApplies p req) policies
      denyPolicies       = filter (\p -> policyEffect p == Deny) applicablePolicies
      allowPolicies      = filter (\p -> policyEffect p == Allow) applicablePolicies
  in if not (null denyPolicies)
     then Deny
     else if not (null allowPolicies)
          then Allow
          else Deny  -- Default deny

-- Built-in policies
ownerPolicy :: Policy
ownerPolicy = Policy
  { policyName   = "owner"
  , policyEffect = Allow
  , policyApplies = \req ->
      resourceOwner (arResource req) == Just (userId (arUser req))
  }

adminPolicy :: Policy
adminPolicy = Policy
  { policyName   = "admin"
  , policyEffect = Allow
  , policyApplies = \req -> userRole (arUser req) == AdminRole
  }

-- Usage in handlers
requirePermission :: Permission -> UserId -> Resource -> Handler ()
requirePermission perm uid resource = do
  user <- runDB $ get404 uid
  let req = AccessRequest user resource perm
  case checkAccess builtinPolicies req of
    Allow -> return ()
    Deny  -> permissionDenied "Access denied"
```

---

## ขั้นตอนที่ 468: Input Validation

```haskell
-- Input validation และ sanitization

-- Validation library
import Data.Validation

type Validated a = V.Validation [ValidationError] a

data ValidationError
  = Required Text
  | TooShort Text Int
  | TooLong  Text Int
  | InvalidFormat Text Text
  | OutOfRange Text Int Int
  deriving (Show)

-- Field validators
required :: Text -> Text -> Validated Text
required field value
  | T.null value = V.Failure [Required field]
  | otherwise    = V.Success value

minLength :: Text -> Int -> Text -> Validated Text
minLength field n value
  | T.length value < n = V.Failure [TooShort field n]
  | otherwise          = V.Success value

maxLength :: Text -> Int -> Text -> Validated Text
maxLength field n value
  | T.length value > n = V.Failure [TooLong field n]
  | otherwise          = V.Success value

emailFormat :: Text -> Text -> Validated Text
emailFormat field value
  | isValidEmail value = V.Success value
  | otherwise          = V.Failure [InvalidFormat field "email"]

-- Validate complex types
validateRegisterInput :: RegisterInput -> Validated ValidatedRegisterInput
validateRegisterInput input = ValidatedRegisterInput
  <$> (required "email" (riEmail input) *> emailFormat "email" (riEmail input))
  <*> (required "username" (riUsername input)
       *> minLength "username" 3 (riUsername input)
       *> maxLength "username" 30 (riUsername input))
  <*> (required "password" (riPassword input)
       *> minLength "password" 8 (riPassword input)
       *> validatePasswordStrength "password" (riPassword input))

-- Number validation
validateAge :: Int -> Validated Int
validateAge age
  | age < 0   = V.Failure [OutOfRange "age" 0 150]
  | age > 150 = V.Failure [OutOfRange "age" 0 150]
  | otherwise = V.Success age

-- File validation
validateUpload :: FileInfo -> Validated FileInfo
validateUpload file = file
  <$ validateMimeType (fileContentType file)
  <* validateFileSize (BS.length (fileContent file))
  where
    validateMimeType mime
      | mime `elem` allowedMimes = V.Success ()
      | otherwise = V.Failure [InvalidFormat "file" "allowed types: jpg, png, pdf"]
    
    validateFileSize size
      | size > maxFileSize = V.Failure [TooLong "file" maxFileSize]
      | otherwise = V.Success ()
    
    allowedMimes = ["image/jpeg", "image/png", "application/pdf"]
    maxFileSize  = 10 * 1024 * 1024  -- 10MB
```

---

## ขั้นตอนที่ 469: Rate Limiting ขั้นสูง

```haskell
-- Advanced Rate Limiting

data RateLimitStrategy
  = FixedWindow    { fwLimit :: Int, fwWindowSeconds :: Int }
  | SlidingWindow  { swLimit :: Int, swWindowSeconds :: Int }
  | TokenBucket    { tbCapacity :: Int, tbRefillRate :: Double }
  | LeakyBucket    { lbCapacity :: Int, lbLeakRate :: Double }

-- Token bucket
data TokenBucketState = TokenBucketState
  { tbsTokens   :: Double
  , tbsLastTime :: UTCTime
  }

checkTokenBucket :: TVar TokenBucketState -> Int -> Double -> IO Bool
checkTokenBucket stateVar capacity refillRate = do
  now <- getCurrentTime
  atomically $ do
    state <- readTVar stateVar
    let elapsed = realToFrac (diffUTCTime now (tbsLastTime state))
        newTokens = min (fromIntegral capacity)
                       (tbsTokens state + elapsed * refillRate)
    if newTokens >= 1
      then do
        writeTVar stateVar (TokenBucketState (newTokens - 1) now)
        return True
      else return False

-- Sliding window
data SlidingWindowState = SlidingWindowState
  { swsRequests :: Seq (UTCTime, Int)  -- (time, count)
  }

checkSlidingWindow :: TVar SlidingWindowState -> Int -> Int -> IO (Bool, Int, Int)
checkSlidingWindow stateVar limit windowSeconds = do
  now <- getCurrentTime
  let windowStart = addUTCTime (fromIntegral (-windowSeconds)) now
  atomically $ do
    state <- readTVar stateVar
    let recent = Seq.filter (\(t, _) -> t > windowStart) (swsRequests state)
        count  = sum (map snd (toList recent))
    if count < limit
      then do
        writeTVar stateVar $ SlidingWindowState (recent |> (now, 1))
        let remaining = limit - count - 1
            resetTime = windowSeconds
        return (True, remaining, resetTime)
      else do
        let resetAt = addUTCTime (fromIntegral windowSeconds)
              (fst (Seq.index recent 0))
            resetIn = ceiling (diffUTCTime resetAt now)
        return (False, 0, resetIn)

-- Rate limit by different keys
data RateLimitKey
  = ByIp      Text
  | ByUser    UserId
  | ByApiKey  Text
  | ByRoute   Text Text  -- method, path
  | Combined  RateLimitKey RateLimitKey

computeKey :: RateLimitKey -> Text
computeKey (ByIp ip)          = "ip:" <> ip
computeKey (ByUser uid)       = "user:" <> T.pack (show (unUserId uid))
computeKey (ByApiKey key)     = "key:" <> key
computeKey (ByRoute m p)      = "route:" <> m <> ":" <> p
computeKey (Combined k1 k2)   = computeKey k1 <> "|" <> computeKey k2
```

---

## ขั้นตอนที่ 470: Security Headers

```haskell
-- Security headers middleware

addSecurityHeaders :: Middleware
addSecurityHeaders app req respond = app req $ \res ->
  respond (mapResponseHeaders addHeaders res)
  where
    addHeaders hdrs = hdrs ++
      -- Prevent clickjacking
      [ ("X-Frame-Options", "DENY")
      -- Prevent MIME sniffing
      , ("X-Content-Type-Options", "nosniff")
      -- XSS filter
      , ("X-XSS-Protection", "1; mode=block")
      -- HSTS
      , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains; preload")
      -- Referrer policy
      , ("Referrer-Policy", "strict-origin-when-cross-origin")
      -- Permissions policy
      , ("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
      -- CSP
      , ("Content-Security-Policy", cspPolicy)
      -- Cache control (prevent caching sensitive data)
      , ("Cache-Control", "no-store, max-age=0")
      , ("Pragma", "no-cache")
      ]
    
    cspPolicy = T.intercalate "; "
      [ "default-src 'self'"
      , "script-src 'self'"
      , "style-src 'self' 'unsafe-inline'"
      , "img-src 'self' data:"
      , "connect-src 'self'"
      , "frame-ancestors 'none'"
      , "base-uri 'self'"
      , "form-action 'self'"
      ]

-- Cookie security
secureCookieSettings :: CookieSettings
secureCookieSettings = defaultCookieSettings
  { cookieIsSecure    = Secure
  , cookieHttpOnly    = HttpOnly
  , cookieSameSite    = SameSiteStrict
  , cookieMaxAge      = Just (3600)  -- 1 hour
  , cookieDomain      = Just "example.com"
  , cookiePath        = Just "/"
  }
```

---

## ขั้นตอนที่ 471: Secrets Management

```haskell
-- Secrets management

import System.Environment
import Data.Vault (Vault)

-- Never hardcode secrets
-- ❌ Bad:
-- dbPassword = "my_secret_password"
-- apiKey = "sk-1234567890"

-- ✓ Good: environment variables
loadSecrets :: IO AppSecrets
loadSecrets = AppSecrets
  <$> requireEnv "DATABASE_PASSWORD"
  <*> requireEnv "JWT_SECRET_KEY"
  <*> requireEnv "STRIPE_SECRET_KEY"
  <*> requireEnv "SENDGRID_API_KEY"

requireEnv :: Text -> IO Text
requireEnv name = do
  mVal <- lookupEnv (T.unpack name)
  case mVal of
    Nothing -> throwIO (MissingSecret name)
    Just v  -> return (T.pack v)

-- Secrets from AWS Secrets Manager
import Amazonka.SecretsManager

loadAwsSecret :: Text -> IO (Map Text Text)
loadAwsSecret secretId = do
  env <- Amazonka.newEnv Amazonka.discover
  let req = GetSecretValue.newGetSecretValue secretId
  resp <- Amazonka.runResourceT (Amazonka.send env req)
  case resp ^. GetSecretValue.getSecretValueResponse_secretString of
    Nothing -> throwIO (SecretNotFound secretId)
    Just s  -> case decode (encodeUtf8 s) of
      Nothing -> throwIO (InvalidSecretFormat secretId)
      Just m  -> return m

-- Secret rotation
rotateApiKey :: UserId -> DB (Text, Text)  -- (old, new)
rotateApiKey uid = do
  oldKey <- apiKeyForUser uid
  newKey <- liftIO generateApiKey
  update uid [UserApiKey =. newKey, UserApiKeyCreated =. now]
  return (oldKey, newKey)

-- Audit trail for secret access
logSecretAccess :: Text -> UserId -> Text -> IO ()
logSecretAccess secretName uid reason = do
  insertAuditLog AuditLog
    { alUserId    = uid
    , alAction    = "secret_access"
    , alResource  = secretName
    , alReason    = reason
    , alTimestamp = now
    }
```

---

## ขั้นตอนที่ 472: Encryption

```haskell
-- Encryption ใน Haskell

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Random
import Data.ByteArray (convert)

-- AES-256-GCM encryption
encrypt :: ByteString -> ByteString -> ByteString -> Either CryptoError (ByteString, ByteString)
encrypt key plaintext aad = do
  cipher <- cipherInit key :: CryptoFailable AES256
  let iv = generate 12 getRandomBytes  -- 96-bit IV for GCM
  let (ciphertext, authTag) = aeadEncrypt (aeadInit AEAD_GCM cipher iv) aad plaintext
  return (iv <> convert authTag <> ciphertext, iv)

decrypt :: ByteString -> ByteString -> ByteString -> Either DecryptionError ByteString
decrypt key encryptedData aad = do
  let (iv, rest) = BS.splitAt 12 encryptedData
      (tag, ciphertext) = BS.splitAt 16 rest
  cipher <- cipherInit key :: Either CryptoError AES256
  aead   <- aeadInit AEAD_GCM cipher iv
  case aeadDecrypt aead aad ciphertext (AuthTag (convert tag)) of
    Nothing        -> Left AuthenticationFailed
    Just plaintext -> Right plaintext

-- Database field encryption
data EncryptedField a = EncryptedField
  { efCiphertext :: ByteString
  , efIv         :: ByteString
  }

encryptField :: EncryptionKey -> Text -> IO (EncryptedField Text)
encryptField key plaintext = do
  iv <- getRandomBytes 12
  let (ciphertext, _) = encrypt key (encodeUtf8 plaintext) ""
  return (EncryptedField ciphertext iv)

decryptField :: EncryptionKey -> EncryptedField Text -> IO (Either DecryptionError Text)
decryptField key ef = do
  case decrypt key (efIv ef <> efCiphertext ef) "" of
    Left err  -> return (Left err)
    Right bs  -> return (Right (decodeUtf8 bs))

-- Hashing (one-way, for passwords)
import Crypto.BCrypt

hashPassword :: Text -> IO ByteString
hashPassword password = do
  result <- hashPasswordUsingPolicy
    (HashingPolicy 12 "$2b$")
    (encodeUtf8 password)
  case result of
    Nothing -> throwIO PasswordHashingFailed
    Just h  -> return h

verifyPassword :: Text -> ByteString -> Bool
verifyPassword password hash =
  validatePassword hash (encodeUtf8 password)
```

---

## ขั้นตอนที่ 473: API Key Management

```haskell
-- API Key management

data ApiKey = ApiKey
  { apiKeyId       :: ApiKeyId
  , apiKeyHash     :: ByteString   -- hashed key
  , apiKeyUserId   :: UserId
  , apiKeyName     :: Text
  , apiKeyScopes   :: [Permission]
  , apiKeyCreated  :: UTCTime
  , apiKeyLastUsed :: Maybe UTCTime
  , apiKeyExpires  :: Maybe UTCTime
  , apiKeyRevoked  :: Bool
  }

-- Generate API key
generateApiKey :: IO (Text, ByteString)  -- (raw_key, hash)
generateApiKey = do
  randomBytes <- getRandomBytes 32
  let rawKey = "sk_" <> B64.encode randomBytes  -- prefix for identification
  let hash   = hashSHA256 randomBytes
  return (decodeUtf8 rawKey, hash)

-- API key authentication middleware
apiKeyAuth :: ConnectionPool -> Middleware
apiKeyAuth pool app req respond = do
  let mKey = lookup "X-API-Key" (requestHeaders req)
             <|> fmap (BS.drop 7) (lookup "Authorization" (requestHeaders req))
  
  case mKey of
    Nothing -> app req respond  -- pass through, let handler decide
    Just keyBytes -> do
      let keyHash = hashSHA256 keyBytes
      mApiKey <- runSqlPool (getBy (UniqueApiKeyHash keyHash)) pool
      case mApiKey of
        Nothing -> respond (responseLBS status401 [] "Invalid API key")
        Just (Entity _ apiKey) -> do
          -- Check expiry
          now <- getCurrentTime
          case apiKeyExpires apiKey of
            Just exp | exp < now ->
              respond (responseLBS status401 [] "API key expired")
            _ -> do
              if apiKeyRevoked apiKey
                then respond (responseLBS status401 [] "API key revoked")
                else do
                  -- Update last used
                  runSqlPool (update (apiKeyId apiKey) [ApiKeyLastUsed =. Just now]) pool
                  -- Set user context
                  let reqWithUser = req { vault = Vault.insert userKey (apiKeyUserId apiKey) (vault req) }
                  app reqWithUser respond
```

---

## ขั้นตอนที่ 474: Security Audit Logging

```haskell
-- Security audit logging

data AuditEvent
  = LoginSuccess    { aeUserId :: UserId, aeIp :: Text, aeAgent :: Text }
  | LoginFailed     { aeEmail :: Text, aeIp :: Text, aeReason :: Text }
  | PasswordChanged { aeUserId :: UserId, aeIp :: Text }
  | PermissionDenied { aeUserId :: UserId, aeResource :: Text, aeAction :: Text }
  | DataAccessed    { aeUserId :: UserId, aeDataType :: Text, aeRecordId :: Int64 }
  | DataModified    { aeUserId :: UserId, aeDataType :: Text, aeRecordId :: Int64, aeDiff :: Value }
  | DataDeleted     { aeUserId :: UserId, aeDataType :: Text, aeRecordId :: Int64 }
  | ApiKeyCreated   { aeUserId :: UserId, aeKeyName :: Text }
  | ApiKeyRevoked   { aeUserId :: UserId, aeKeyId :: Int64 }
  | AdminAction     { aeUserId :: UserId, aeAction :: Text, aeDetails :: Value }

logAuditEvent :: AuditEvent -> Handler ()
logAuditEvent event = do
  now <- liftIO getCurrentTime
  req <- waiRequest
  let ip      = decodeUtf8 (getRemoteHost req)
      agent   = decodeUtf8 (fromMaybe "" (lookup "User-Agent" (requestHeaders req)))
      eventJ  = toJSON event
  
  runDB $ insert_ AuditLog
    { auditLogEvent     = eventJ
    , auditLogTimestamp = now
    , auditLogIp        = ip
    , auditLogAgent     = agent
    }
  
  -- Alert on suspicious activity
  case event of
    LoginFailed _ _ _ -> checkBruteForce ip
    PermissionDenied uid _ _ -> checkSuspiciousAccess uid
    _ -> return ()

-- Brute force detection
checkBruteForce :: Text -> Handler ()
checkBruteForce ip = do
  now    <- liftIO getCurrentTime
  let window = addUTCTime (-300) now  -- 5 minutes
  count  <- runDB $ count
    [ AuditLogEvent `like` "%LoginFailed%"
    , AuditLogTimestamp >=. window
    , AuditLogIp ==. ip
    ]
  when (count > 10) $ do
    -- Auto-block IP
    blockIp ip 3600  -- 1 hour
    alertSecurityTeam $ "Possible brute force from " <> ip
```

---

## ขั้นตอนที่ 475: Vulnerability Scanning

```haskell
-- Dependency vulnerability scanning

-- cabal-audit: ตรวจสอบ known vulnerabilities
-- stack-audit: สำหรับ Stack projects

-- Security checklist สำหรับ Haskell projects

-- 1. Update dependencies regularly
-- cabal update && cabal upgrade

-- 2. Use ghc-pkg check
-- ghc-pkg check

-- 3. Run HLint for common issues
-- hlint src/

-- 4. Test for common vulnerabilities
testForVulnerabilities :: Spec
testForVulnerabilities = do
  describe "Security" $ do
    it "prevents SQL injection" $ do
      let maliciousInput = "'; DROP TABLE users; --"
      result <- runDB $ selectList [UserName ==. maliciousInput] []
      -- Should return empty, not execute the injection
      result `shouldBe` []
    
    it "enforces HTTPS" $ do
      get "http://example.com/"
      statusIs 301  -- redirect to HTTPS
    
    it "has CSRF protection" $ do
      post "/users" []
      statusIs 403  -- CSRF token missing
    
    it "rate limits requests" $ do
      replicateM_ 101 (get "/api/public")
      get "/api/public"
      statusIs 429  -- rate limited
    
    it "hides internal errors" $ do
      triggerInternalError
      bodyContains "Internal Server Error"
      bodyNotContains "stack trace"
      bodyNotContains "SQL"
```

---

## ขั้นตอนที่ 476: HTTPS Configuration

```haskell
-- HTTPS setup ด้วย warp-tls

import Network.Wai.Handler.WarpTLS
import Network.TLS

-- TLS configuration
tlsSettings :: TLSSettings
tlsSettings = tlsSettingsChain
  "certs/server.crt"    -- certificate
  ["certs/chain.crt"]   -- intermediate certs
  "certs/server.key"    -- private key

-- TLS settings with modern cipher suites
secureTlsSettings :: TLSSettings
secureTlsSettings = (tlsSettingsChain "cert.pem" [] "key.pem")
  { tlsSessionManagerConfig = Just defaultSessionManagerConfig
  , tlsWantClientCert = False
  , tlsServerHooks = def
      { onCipherChoosing = chooseCipher
      }
  }
  where
    chooseCipher _ ciphers = head $
      filter (\c -> cipherID c `elem` preferredCiphers) ciphers
      ++ ciphers
    
    preferredCiphers =
      [ 0x1301  -- TLS_AES_128_GCM_SHA256
      , 0x1302  -- TLS_AES_256_GCM_SHA384
      , 0x1303  -- TLS_CHACHA20_POLY1305_SHA256
      ]

-- Run HTTPS server
mainHttps :: IO ()
mainHttps = do
  app <- makeApplication
  let warpSettings = setPort 443 defaultSettings
  runTLS secureTlsSettings warpSettings app

-- HTTP -> HTTPS redirect
httpRedirectApp :: Application
httpRedirectApp req respond = do
  let httpsUrl = "https://" <> decodeUtf8 (serverName req)
                          <> decodeUtf8 (rawPathInfo req)
  respond (responseLBS status301 [("Location", encodeUtf8 httpsUrl)] "")

mainWithRedirect :: IO ()
mainWithRedirect = do
  -- HTTP redirect server on port 80
  void $ forkIO $ run 80 httpRedirectApp
  -- HTTPS server on port 443
  mainHttps
```

---

## ขั้นตอนที่ 477: Dependency Injection Security

```haskell
-- Security-focused DI

-- Configuration that hides secrets
data SecureConfig = SecureConfig
  { scDbUrl    :: Masked Text   -- masked in logs
  , scJwtKey   :: Masked Text
  , scApiKeys  :: Map Text (Masked Text)
  }

-- Masked type: shows "***" in Show
newtype Masked a = Masked { unmask :: a }

instance Show (Masked a) where
  show _ = "***"

instance ToJSON (Masked a) where
  toJSON _ = String "***"

-- Security context
data SecurityContext = SecurityContext
  { scCurrentUser :: Maybe User
  , scRequiredRoles :: Set Role
  , scAuditLogger  :: AuditEvent -> IO ()
  }

-- Capability-based security
data Capability perm where
  HasRead  :: UserId -> Capability Read
  HasWrite :: UserId -> Capability Write
  HasAdmin :: UserId -> Capability Admin

-- Only users with Capability can access protected operations
protectedOperation
  :: Capability Write
  -> Resource
  -> Handler ()
protectedOperation (HasWrite userId) resource = do
  -- safe to proceed, capability proves authorization
  modifyResource userId resource

-- Get capability (checks permissions)
getWriteCapability :: UserId -> Handler (Capability Write)
getWriteCapability uid = do
  user <- runDB $ get404 uid
  case userRole user of
    AdminRole -> return (HasWrite uid)
    UserRole  -> return (HasWrite uid)
    _         -> throwError err403
```

---

## ขั้นตอนที่ 478: Data Privacy (GDPR)

```haskell
-- GDPR compliance

-- Data anonymization
anonymizeUser :: User -> User
anonymizeUser user = user
  { userName  = "Anonymous"
  , userEmail = hashEmail (userEmail user)
  , userPhone = Nothing
  , userBio   = Nothing
  , userAvatar = Nothing
  }

hashEmail :: Email -> Email
hashEmail (Email e) = Email $ T.take 8 (sha256Hex (encodeUtf8 e)) <> "@anonymized.local"

-- Right to erasure
eraseUserData :: UserId -> DB ()
eraseUserData uid = do
  -- Anonymize rather than delete (preserve referential integrity)
  update uid
    [ UserName       =. "Deleted User"
    , UserEmail      =. Email ("deleted-" <> T.pack (show (unUserId uid)) <> "@deleted.invalid")
    , UserPassword   =. ""
    , UserBio        =. Nothing
    , UserAvatar     =. Nothing
    , UserDeletedAt  =. Just now
    ]
  -- Delete sensitive data in related tables
  deleteWhere [UserSessionUserId ==. uid]
  deleteWhere [UserOAuthUserId   ==. uid]

-- Data export (Right to portability)
exportUserData :: UserId -> DB UserDataExport
exportUserData uid = do
  user    <- get404 uid
  posts   <- selectList [PostAuthorId ==. uid] []
  comments <- selectList [CommentAuthorId ==. uid] []
  orders  <- selectList [OrderUserId ==. uid] []
  
  return UserDataExport
    { udeUser     = userToExport user
    , udePosts    = map (postToExport . entityVal) posts
    , udeComments = map (commentToExport . entityVal) comments
    , udeOrders   = map (orderToExport . entityVal) orders
    }

-- Consent management
recordConsent :: UserId -> ConsentType -> Bool -> IO ()
recordConsent uid consentType granted = do
  now <- getCurrentTime
  insertConsent Consent
    { consentUserId   = uid
    , consentType     = consentType
    , consentGranted  = granted
    , consentAt       = now
    , consentVersion  = currentPrivacyPolicyVersion
    }

data ConsentType
  = MarketingEmails
  | Analytics
  | ThirdPartySharing
  | PersonalizedAds
```

---

## ขั้นตอนที่ 479: Penetration Testing Helpers

```haskell
-- Security testing utilities

-- Fuzzing input
fuzzInputs :: [Text]
fuzzInputs =
  [ "<script>alert('xss')</script>"
  , "'; DROP TABLE users; --"
  , "../../../etc/passwd"
  , "null"
  , ""
  , T.replicate 10000 "a"  -- long string
  , "\0"                   -- null byte
  , "{{7*7}}"              -- template injection
  , "${7*7}"               -- EL injection
  , "$(whoami)"            -- command injection
  , "%00"                  -- URL encoding
  , "\r\n"                 -- CRLF injection
  ]

-- Test all endpoints with fuzz inputs
fuzzTest :: Application -> IO ()
fuzzTest app = do
  endpoints <- discoverEndpoints app
  forM_ endpoints $ \(method, path) -> do
    forM_ fuzzInputs $ \input -> do
      let req = setBody (encode (object ["input" .= input])) $
                defaultRequest { requestMethod = method, pathInfo = pathParts path }
      response <- runApp app req
      
      -- Check for unexpected behavior
      when (responseStatus response >= status500) $
        putStrLn $ "Potential issue: " ++ T.unpack method ++ " " ++ T.unpack path
             ++ " with input: " ++ T.unpack (T.take 50 input)

-- Security headers check
checkSecurityHeaders :: Response -> [SecurityIssue]
checkSecurityHeaders resp =
  let hdrs = responseHeaders resp
      check name expected =
        case lookup name hdrs of
          Nothing -> [MissingHeader name]
          Just v  -> if v == expected then [] else [IncorrectHeader name v expected]
  in concatMap (uncurry check)
      [ ("X-Frame-Options", "DENY")
      , ("X-Content-Type-Options", "nosniff")
      , ("Strict-Transport-Security", "max-age=31536000")
      ]
```

---

## ขั้นตอนที่ 480: โปรเจกต์: Secure Authentication System

```haskell
-- Complete secure authentication system

module Auth where

import Crypto.BCrypt
import Data.Time.Clock (addUTCTime)

-- User model with security features
data SecureUser = SecureUser
  { suId              :: UserId
  , suEmail           :: Email
  , suPasswordHash    :: ByteString
  , suVerified        :: Bool
  , suTwoFactorSecret :: Maybe ByteString  -- encrypted
  , suLoginAttempts   :: Int
  , suLockedUntil     :: Maybe UTCTime
  , suLastLogin       :: Maybe UTCTime
  , suLastPasswordChange :: UTCTime
  , suActiveSessions  :: Int
  }

-- Registration with all security checks
registerSecurely
  :: (UserRepository m, EmailService m, AuditLogger m, MonadIO m)
  => RegisterRequest
  -> m (Either RegistrationError AuthTokens)
registerSecurely req = do
  -- Validate inputs
  validated <- validateRegistrationInput req
  case validated of
    Invalid errs -> return (Left (ValidationError errs))
    Valid input  -> do
      -- Check for existing email (timing-safe)
      mExisting <- findUserByEmail (riEmail input)
      when (isJust mExisting) $ do
        -- Don't reveal if email exists (timing attack prevention)
        liftIO (threadDelay 200000)  -- constant time
        return (Left EmailAlreadyExists)
      
      -- Hash password with bcrypt (cost 12)
      hash <- liftIO $ hashPasswordUsingPolicy
        (HashingPolicy 12 "$2b$")
        (encodeUtf8 (riPassword input))
      
      case hash of
        Nothing -> return (Left HashingFailed)
        Just h  -> do
          -- Create user
          uid <- createUser (input { riPasswordHash = h })
          
          -- Send verification email
          token <- liftIO generateVerificationToken
          saveVerificationToken uid token
          sendVerificationEmail (riEmail input) token
          
          -- Audit log
          logAudit (UserRegistered uid (riEmail input))
          
          -- Issue tokens
          tokens <- issueTokens uid []  -- no roles until verified
          return (Right tokens)

-- Login with security checks
loginSecurely
  :: (UserRepository m, AuditLogger m, MonadIO m)
  => LoginRequest
  -> m (Either LoginError AuthTokens)
loginSecurely req = do
  -- Find user (constant time to prevent enumeration)
  mUser <- findUserByEmail (lrEmail req)
  
  case mUser of
    Nothing -> do
      -- Fake password check to prevent timing attacks
      liftIO (threadDelay 200000)
      logAudit (LoginFailed (lrEmail req) "user not found")
      return (Left InvalidCredentials)
    
    Just user -> do
      -- Check if locked
      now <- liftIO getCurrentTime
      case suLockedUntil user of
        Just until' | until' > now -> do
          logAudit (LoginFailed (lrEmail user) "account locked")
          return (Left (AccountLocked until'))
        _ -> do
          -- Verify password
          let valid = validatePassword
                (suPasswordHash user)
                (encodeUtf8 (lrPassword req))
          
          if not valid
            then do
              -- Increment failure count
              let failures = suLoginAttempts user + 1
              if failures >= 5
                then do
                  lockUntil <- addUTCTime 900 <$> liftIO getCurrentTime  -- 15 min
                  updateUser (suId user)
                    [ UserLoginAttempts =. failures
                    , UserLockedUntil   =. Just lockUntil
                    ]
                  logAudit (AccountLocked (suId user) lockUntil)
                  return (Left (AccountLocked lockUntil))
                else do
                  updateUser (suId user) [UserLoginAttempts =. failures]
                  logAudit (LoginFailed (lrEmail req) "wrong password")
                  return (Left InvalidCredentials)
            else do
              -- Success: reset failures, issue tokens
              now <- liftIO getCurrentTime
              updateUser (suId user)
                [ UserLoginAttempts =. 0
                , UserLockedUntil   =. Nothing
                , UserLastLogin     =. Just now
                ]
              
              -- Check if 2FA required
              case suTwoFactorSecret user of
                Nothing  -> do
                  tokens <- issueTokens (suId user) (userRoles user)
                  logAudit (LoginSuccess (suId user))
                  return (Right tokens)
                Just _   ->
                  return (Left (TwoFactorRequired (suId user)))

-- 2FA TOTP verification
verifyTwoFactor :: UserId -> Text -> Handler AuthTokens
verifyTwoFactor uid totpCode = do
  user <- runDB $ get404 uid
  case suTwoFactorSecret user of
    Nothing -> throwError err400 { errBody = "2FA not enabled" }
    Just encryptedSecret -> do
      secret <- decryptSecret encryptedSecret
      valid  <- liftIO $ verifyTOTP secret (T.unpack totpCode)
      unless valid $ throwError err401 { errBody = "Invalid 2FA code" }
      
      tokens <- issueTokens uid (userRoles user)
      logAuditEvent (LoginSuccess uid)
      return tokens
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 25 เราจะเรียน **Performance Engineering ขั้นสูง**:
- Profiling ด้วย Criterion และ ThreadScope
- Memory optimization
- CPU optimization
- Database query optimization

---

*[← Part 23](part-23.md) | [Part 25 →](part-25.md)*
