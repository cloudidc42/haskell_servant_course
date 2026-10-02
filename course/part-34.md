# Part 34: Security Hardening
## ขั้นตอนที่ 661-680

---

## ขั้นตอนที่ 661: OWASP Security Headers

```haskell
-- Security headers middleware

import Network.Wai

securityHeadersMiddleware :: Application -> Application
securityHeadersMiddleware app request respond =
  app request $ \response ->
    respond (addSecurityHeaders response)

addSecurityHeaders :: Response -> Response
addSecurityHeaders = foldr addHeader'
  [ ("Strict-Transport-Security", "max-age=31536000; includeSubDomains; preload")
  , ("X-Content-Type-Options",    "nosniff")
  , ("X-Frame-Options",           "DENY")
  , ("X-XSS-Protection",          "1; mode=block")
  , ("Referrer-Policy",           "strict-origin-when-cross-origin")
  , ("Content-Security-Policy",   cspPolicy)
  , ("Permissions-Policy",        "geolocation=(), microphone=(), camera=()")
  , ("Cache-Control",             "no-store, no-cache, must-revalidate, private")
  ]

cspPolicy :: BS.ByteString
cspPolicy = BS.intercalate "; "
  [ "default-src 'self'"
  , "script-src 'self' 'nonce-{NONCE}'"
  , "style-src 'self' 'unsafe-inline'"
  , "img-src 'self' data: https:"
  , "font-src 'self'"
  , "connect-src 'self'"
  , "frame-ancestors 'none'"
  , "base-uri 'self'"
  , "form-action 'self'"
  , "upgrade-insecure-requests"
  ]

addHeader' :: Header -> Response -> Response
addHeader' h response =
  let (status, headers, body) = responseToStream response
  in responseStream status (h : headers) body
```

---

## ขั้นตอนที่ 662: Input Sanitization

```haskell
-- Input sanitization and validation

import Text.HTML.Sanitize
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- XSS prevention
sanitizeHtml :: Text -> Text
sanitizeHtml = sanitize allowedTags allowedAttributes
  where
    allowedTags = S.fromList ["p", "b", "i", "em", "strong", "a", "ul", "ol", "li", "br"]
    allowedAttributes = Map.fromList
      [ ("a", S.fromList ["href", "title"])
      ]

-- SQL injection prevention (parameterized queries)
safeQuery :: MonadIO m => ConnectionPool -> Text -> [PersistValue] -> m [Single Int]
safeQuery pool sql params = runSqlPool (rawSql sql params) pool

-- Path traversal prevention
safePath :: FilePath -> FilePath -> Either Text FilePath
safePath base path =
  let canonical = normalise (base </> path)
  in if base `isPrefixOf` canonical
     then Right canonical
     else Left "Path traversal detected"

-- Command injection prevention (never use shell)
safeProcess :: FilePath -> [String] -> IO (ExitCode, String, String)
safeProcess prog args = readProcessWithExitCode prog args ""

-- Validate file upload
validateUpload :: FileInfo -> Either [Text] FileInfo
validateUpload fi = do
  validateMimeType (fileContentType fi)
  validateSize (fileContent fi)
  validateFilename (fileName fi)
  return fi

validateMimeType :: ByteString -> Either [Text] ()
validateMimeType mime
  | mime `elem` allowedMimes = Right ()
  | otherwise = Left ["Unsupported file type: " <> decodeUtf8 mime]
  where
    allowedMimes = ["image/jpeg", "image/png", "image/gif", "application/pdf"]

validateSize :: LByteString -> Either [Text] ()
validateSize bs
  | BSL.length bs <= maxSize = Right ()
  | otherwise = Left ["File too large (max 10MB)"]
  where maxSize = 10 * 1024 * 1024
```

---

## ขั้นตอนที่ 663: Cryptographic Operations

```haskell
-- Cryptography ด้วย crypton/cryptonite

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Crypto.Hash
import Crypto.MAC.HMAC
import Crypto.Random
import Data.ByteArray (convert)

-- AES-256-GCM encryption
encrypt :: SecretKey AES256 -> ByteString -> IO (ByteString, ByteString, ByteString)
encrypt key plaintext = do
  iv <- getRandomBytes 12  -- 96-bit nonce
  let cipher     = throwCryptoError (cipherInit key) :: AES256
  let aead       = throwCryptoError (aeadInit AEAD_GCM cipher iv)
  let (auth, ciphertext) = aeadEncrypt aead plaintext
  return (iv, ciphertext, convert auth)

decrypt :: SecretKey AES256 -> ByteString -> ByteString -> ByteString -> Either Text ByteString
decrypt key iv ciphertext authTag =
  case cipherInit key of
    CryptoFailed err -> Left (T.pack (show err))
    CryptoPassed cipher ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err -> Left (T.pack (show err))
        CryptoPassed aead ->
          case aeadDecrypt aead ciphertext (convert authTag) of
            Nothing        -> Left "Authentication failed"
            Just plaintext -> Right plaintext

-- Key derivation (Argon2)
import Crypto.KDF.Argon2

hashPassword :: Text -> IO ByteString
hashPassword password = do
  salt <- getRandomBytes 32
  let config = Options
        { iterations  = 3
        , memory      = 65536  -- 64MB
        , parallelism = 4
        , variant     = Argon2id
        , version     = Version13
        }
  hash <- argon2 config (encodeUtf8 password) salt 32
  return (salt <> hash)

verifyPassword :: Text -> ByteString -> Bool
verifyPassword password stored =
  let (salt, hash) = BS.splitAt 32 stored
      config = Options 3 65536 4 Argon2id Version13
  in case argon2 config (encodeUtf8 password) salt 32 of
       Left _  -> False
       Right h -> h == hash

-- HMAC-SHA256 for API keys
signPayload :: ByteString -> ByteString -> ByteString
signPayload secret payload =
  convert (hmac secret payload :: HMAC SHA256)

verifySignature :: ByteString -> ByteString -> ByteString -> Bool
verifySignature secret payload sig =
  signPayload secret payload == sig
```

---

## ขั้นตอนที่ 664: JWT Security

```haskell
-- JWT ที่มีความปลอดภัยสูง

import Jose.Jwt
import Jose.Jwa

-- JWT with short expiry
createSecureJwt :: JwtKeys -> UserId -> [Permission] -> IO Text
createSecureJwt keys uid perms = do
  now <- getPOSIXTime
  let claims = JwtClaims
        { jcSub  = Just (T.pack (show uid))
        , jcIss  = Just "https://auth.example.com"
        , jcAud  = Just ["https://api.example.com"]
        , jcIat  = Just (round now)
        , jcExp  = Just (round now + 900)  -- 15 minutes
        , jcNbf  = Just (round now - 60)   -- Allow 1 min clock skew
        , jcJti  = Just (generateJti uid now)
        , jcOther = Map.fromList [("perms", toJSON perms)]
        }
  
  signJwt keys RS256 claims

-- Token rotation
data TokenPair = TokenPair
  { tpAccess  :: Text  -- 15 min
  , tpRefresh :: Text  -- 7 days
  }

createTokenPair :: JwtKeys -> UserId -> IO TokenPair
createTokenPair keys uid = do
  access  <- createSecureJwt keys uid []
  refresh <- createRefreshToken keys uid
  return (TokenPair access refresh)

-- Refresh token with rotation
rotateTokens :: JwtKeys -> Text -> IO (Either Text TokenPair)
rotateTokens keys refreshToken = do
  result <- verifyRefreshToken keys refreshToken
  case result of
    Left err  -> return (Left err)
    Right uid -> do
      -- Invalidate old refresh token
      revokeRefreshToken refreshToken
      -- Issue new pair
      Right <$> createTokenPair keys uid

-- Token blacklist for revocation
data TokenBlacklist = TokenBlacklist
  { tbStore :: Redis.Connection
  }

revokeToken :: TokenBlacklist -> Text -> UTCTime -> IO ()
revokeToken bl token expiry = do
  let key = "revoked:" <> token
  let ttl = ceiling (diffUTCTime expiry =<< getCurrentTime)
  Redis.runRedis (tbStore bl) $ do
    Redis.setex (encodeUtf8 key) ttl "1"
  return ()

isTokenRevoked :: TokenBlacklist -> Text -> IO Bool
isTokenRevoked bl token = do
  let key = "revoked:" <> token
  result <- Redis.runRedis (tbStore bl) $
    Redis.exists (encodeUtf8 key)
  return (either (const False) id result)
```

---

## ขั้นตอนที่ 665: Rate Limiting

```haskell
-- Rate limiting with sliding window algorithm

import Data.IORef
import qualified Data.HashMap.Strict as HM

data RateLimiter = RateLimiter
  { rlStore     :: TVar (HM.HashMap Text [UTCTime])
  , rlMax       :: Int
  , rlWindowSec :: Int
  }

-- Check and record request
checkRateLimit :: RateLimiter -> Text -> IO RateLimitResult
checkRateLimit rl key = do
  now <- getCurrentTime
  atomically $ do
    store <- readTVar (rlStore rl)
    let windowStart = addUTCTime (negate (fromIntegral (rlWindowSec rl))) now
    let times       = filter (> windowStart) (fromMaybe [] (HM.lookup key store))
    let count       = length times
    
    if count >= rlMax rl
      then do
        writeTVar (rlStore rl) (HM.insert key times store)
        let oldest = head (sort times)
        let retry  = ceiling (diffUTCTime (addUTCTime (fromIntegral (rlWindowSec rl)) oldest) now)
        return (RateLimited count retry)
      else do
        writeTVar (rlStore rl) (HM.insert key (now : times) store)
        return (Allowed count (rlMax rl - count - 1))

-- Middleware
rateLimitMiddleware :: RateLimiter -> (Request -> Text) -> Application -> Application
rateLimitMiddleware rl keyFn app request respond = do
  let key = keyFn request
  result <- checkRateLimit rl key
  
  case result of
    Allowed _ remaining ->
      app request $ \response ->
        respond (addRateLimitHeaders response remaining)
    
    RateLimited count retryAfter ->
      respond $ responseLBS
        status429
        [ ("Retry-After", BS.pack (show retryAfter))
        , ("X-RateLimit-Limit", BS.pack (show (rlMax rl)))
        , ("X-RateLimit-Remaining", "0")
        ]
        (encode (object ["error" .= ("rate_limit_exceeded" :: Text)]))
```

---

## ขั้นตอนที่ 666: CSRF Protection

```haskell
-- CSRF protection

import Web.ClientSession

-- CSRF token management
data CsrfConfig = CsrfConfig
  { ccSecret  :: Key  -- ClientSession key
  , ccTtl     :: Int  -- seconds
  }

-- Generate CSRF token
generateCsrfToken :: CsrfConfig -> SessionId -> IO Text
generateCsrfToken cfg sessionId = do
  now     <- getPOSIXTime
  let payload = encode (sessionId, round now :: Int)
  token   <- encryptIO (ccSecret cfg) payload
  return (decodeUtf8 (B64.encode token))

-- Validate CSRF token
validateCsrfToken :: CsrfConfig -> SessionId -> Text -> IO Bool
validateCsrfToken cfg sessionId token = do
  now <- getPOSIXTime
  case B64.decode (encodeUtf8 token) >>= decrypt (ccSecret cfg) of
    Left _  -> return False
    Right bs ->
      case decode bs of
        Nothing -> return False
        Just (sid, issuedAt :: Int) ->
          return $ sid == sessionId
               && fromIntegral issuedAt + ccTtl cfg > round now

-- CSRF middleware (check on mutation requests)
csrfMiddleware :: CsrfConfig -> Application -> Application
csrfMiddleware cfg app request respond = do
  if requestMethod request `elem` ["POST", "PUT", "PATCH", "DELETE"]
    then do
      let token     = getHeaderToken request <|> getBodyToken request
      let sessionId = getSessionId request
      
      valid <- maybe (return False) (\t -> validateCsrfToken cfg sessionId t) token
      
      if valid
        then app request respond
        else respond (responseLBS status403 [] (encode (object ["error" .= ("csrf_token_invalid" :: Text)])))
    else app request respond
```

---

## ขั้นตอนที่ 667: SQL Injection Prevention

```haskell
-- Safe database operations

-- Always use parameterized queries
safeUserLookup :: ConnectionPool -> Text -> IO (Maybe User)
safeUserLookup pool email = runSqlPool query pool
  where
    -- Persistent generates safe queries
    query = selectFirst [UserEmail ==. email] []

-- Raw SQL must use parameters
safeSearch :: ConnectionPool -> Text -> IO [User]
safeSearch pool searchTerm = runSqlPool query pool
  where
    query = rawSql
      "SELECT ?? FROM users WHERE name LIKE ?"
      [PersistText ("%" <> searchTerm <> "%")]

-- Never interpolate user input into SQL
-- BAD (never do this):
-- rawSql ("SELECT * FROM users WHERE name = '" <> userInput <> "'") []

-- Query builder that prevents injection
data SafeQuery = SafeQuery
  { sqSql    :: Text
  , sqParams :: [PersistValue]
  }

buildUserQuery :: UserFilter -> SafeQuery
buildUserQuery f =
  let conditions = catMaybes
        [ fmap (\n -> ("name LIKE ?", PersistText ("%" <> n <> "%"))) (ufName f)
        , fmap (\e -> ("email = ?",   PersistText e))                (ufEmail f)
        , fmap (\a -> ("age > ?",     PersistInt64 (fromIntegral a)))(ufMinAge f)
        ]
      sql    = "SELECT ?? FROM users" <> whereClause conditions
      params = map snd conditions
  in SafeQuery sql params
  where
    whereClause [] = ""
    whereClause cs = " WHERE " <> T.intercalate " AND " (map fst cs)
```

---

## ขั้นตอนที่ 668: Secret Management

```haskell
-- Secret management ด้วย HashiCorp Vault

data VaultConfig = VaultConfig
  { vcAddr      :: Text
  , vcToken     :: Text
  , vcMountPath :: Text
  }

-- Read secret from Vault
readSecret :: VaultConfig -> Text -> IO (Map Text Text)
readSecret cfg path = do
  let url = vcAddr cfg <> "/v1/" <> vcMountPath cfg <> "/data/" <> path
  response <- httpJSONEither (setRequestHeader "X-Vault-Token" [encodeUtf8 (vcToken cfg)]
                               (parseRequest_ (T.unpack url)))
  case response of
    Left err    -> throwIO (VaultError (T.pack (show err)))
    Right body  -> case body ^? key "data" . key "data" of
      Nothing -> throwIO (VaultError "No data in response")
      Just v  -> case fromJSON v of
        Error err -> throwIO (VaultError (T.pack err))
        Success m -> return m

-- Secret rotation
data SecretRotationConfig = SecretRotationConfig
  { srcDbCredentials  :: Text  -- Vault path
  , srcApiKeys        :: Text  -- Vault path
  , srcRotationPeriod :: NominalDiffTime
  }

rotateSecrets :: VaultConfig -> SecretRotationConfig -> IO ()
rotateSecrets vaultCfg rotCfg = do
  -- Rotate DB password
  newDbPass <- generateSecurePassword
  updateDbPassword (rcPrimaryDb rotCfg) newDbPass
  writeSecret vaultCfg (srcDbCredentials rotCfg) (Map.singleton "password" newDbPass)
  
  -- Rotate API keys
  newApiKey <- generateApiKey
  writeSecret vaultCfg (srcApiKeys rotCfg) (Map.singleton "key" newApiKey)

-- Never hardcode secrets
data AppConfig = AppConfig
  { acDbUrl    :: Text  -- from environment variable
  , acJwtSecret :: ByteString  -- from Vault
  , acApiKey   :: Text  -- from Vault
  }

loadConfig :: IO AppConfig
loadConfig = do
  dbUrl    <- getEnv "DATABASE_URL"
  vaultCfg <- loadVaultConfig
  secrets  <- readSecret vaultCfg "app/secrets"
  
  return AppConfig
    { acDbUrl    = T.pack dbUrl
    , acJwtSecret = maybe (error "Missing JWT_SECRET") encodeUtf8 (Map.lookup "jwt_secret" secrets)
    , acApiKey   = fromMaybe (error "Missing API_KEY") (Map.lookup "api_key" secrets)
    }
```

---

## ขั้นตอนที่ 669: mTLS (Mutual TLS)

```haskell
-- Mutual TLS for service-to-service auth

import Network.TLS
import Network.TLS.Extra.Cipher

-- Server with mTLS
data MtlsConfig = MtlsConfig
  { mtlsCaCert   :: FilePath
  , mtlsCert     :: FilePath
  , mtlsKey      :: FilePath
  , mtlsRequireClientCert :: Bool
  }

mkTlsServerParams :: MtlsConfig -> IO ServerParams
mkTlsServerParams cfg = do
  creds <- credentialLoadX509 (mtlsCert cfg) (mtlsKey cfg)
  caCert <- readCertificate (mtlsCaCert cfg)
  
  return def
    { serverShared = def
        { sharedCredentials = Credentials [either (error . show) id creds] }
    , serverSupported = def
        { supportedCiphers  = ciphersuite_strong
        , supportedVersions = [TLS13, TLS12]
        }
    , serverWantClientCert = mtlsRequireClientCert cfg
    , serverCACertificates = [caCert]
    , serverHooks = def
        { onClientCertificate = \chain -> do
            valid <- validateCertChain chain caCert
            if valid then return CertificateUsageAccept
                     else return (CertificateUsageReject CertificateRejectRevoked)
        }
    }

-- Client with mTLS
mkTlsClientParams :: MtlsConfig -> Text -> IO ClientParams
mkTlsClientParams cfg hostname = do
  creds  <- credentialLoadX509 (mtlsCert cfg) (mtlsKey cfg)
  caCert <- readCertificate (mtlsCaCert cfg)
  
  return (defaultParamsClient (T.unpack hostname) "")
    { clientSupported = def { supportedCiphers = ciphersuite_strong }
    , clientShared    = def
        { sharedCredentials    = Credentials [either (error . show) id creds]
        , sharedCAStore        = makeCertificateStore [caCert]
        }
    }
```

---

## ขั้นตอนที่ 670: Security Scanning

```haskell
-- Security scanning integration

-- Dependency vulnerability check
checkDependencies :: IO VulnerabilityReport
checkDependencies = do
  -- Run safety check via subprocess
  (code, out, err) <- readProcessWithExitCode "cabal-audit" ["--json"] ""
  case code of
    ExitSuccess ->
      case eitherDecode (BS.pack out) of
        Right report -> return report
        Left e       -> throwIO (ScanError e)
    ExitFailure code ->
      throwIO (ScanError ("cabal-audit failed: " ++ err))

-- Static analysis with HLint
runHlint :: [FilePath] -> IO [HlintSuggestion]
runHlint files = do
  ideas <- apply files
  return (mapMaybe toSuggestion ideas)

toSuggestion :: Idea -> Maybe HlintSuggestion
toSuggestion idea
  | ideaSeverity idea `elem` [Error, Warning] = Just HlintSuggestion
      { hsFile     = T.pack (ideaFilepath idea)
      , hsLine     = fst (ideaSpan idea)
      , hsMessage  = T.pack (show idea)
      , hsSeverity = if ideaSeverity idea == Error then SecurityError else SecurityWarning
      }
  | otherwise = Nothing

-- OWASP dependency check
checkOwaspDependencies :: IO [CVE]
checkOwaspDependencies = do
  reportPath <- runOwaspDependencyCheck
  parseOwaspReport reportPath
```

---

## ขั้นตอนที่ 671: Penetration Testing Helpers

```haskell
-- Security testing utilities

-- Fuzz test HTTP API
fuzzHttpApi :: Text -> IO [FuzzResult]
fuzzHttpApi baseUrl = do
  let payloads = xssPayloads ++ sqlInjectionPayloads ++ pathTraversalPayloads
  
  results <- forM payloads $ \payload -> do
    response <- httpGet (baseUrl <> "?q=" <> urlEncode payload)
    return FuzzResult
      { frPayload  = payload
      , frStatus   = responseStatus response
      , frBody     = responseBody response
      , frIssue    = detectIssue payload response
      }
  
  return (filter (isJust . frIssue) results)

xssPayloads :: [Text]
xssPayloads =
  [ "<script>alert(1)</script>"
  , "'\"><script>alert(1)</script>"
  , "javascript:alert(1)"
  , "<img src=x onerror=alert(1)>"
  , "<svg onload=alert(1)>"
  ]

sqlInjectionPayloads :: [Text]
sqlInjectionPayloads =
  [ "' OR '1'='1"
  , "'; DROP TABLE users; --"
  , "' UNION SELECT * FROM users --"
  , "1; SELECT * FROM information_schema.tables"
  ]

detectIssue :: Text -> Response -> Maybe SecurityIssue
detectIssue payload response
  | T.isInfixOf payload (responseBodyText response) = Just (XssReflection payload)
  | responseStatus response == status500             = Just (PossibleSqlInjection payload)
  | otherwise                                        = Nothing
```

---

## ขั้นตอนที่ 672: API Security Testing

```haskell
-- Automated API security tests

data SecurityTest = SecurityTest
  { stName    :: Text
  , stCheck   :: Request -> IO SecurityResult
  }

-- Test suite
apiSecurityTests :: [SecurityTest]
apiSecurityTests =
  [ SecurityTest "auth_required" testAuthRequired
  , SecurityTest "no_sensitive_headers" testSensitiveHeaders
  , SecurityTest "rate_limit_enforced" testRateLimit
  , SecurityTest "cors_configured"     testCors
  , SecurityTest "error_messages_safe" testErrorMessages
  ]

testAuthRequired :: Request -> IO SecurityResult
testAuthRequired req = do
  -- Try without auth token
  response <- httpNoAuth req
  if statusCode (responseStatus response) `elem` [401, 403]
    then return SecurityPass
    else return (SecurityFail "Endpoint accessible without auth")

testRateLimit :: Request -> IO SecurityResult
testRateLimit req = do
  responses <- replicateM 200 (httpSend req)
  if any (\r -> responseStatus r == status429) responses
    then return SecurityPass
    else return (SecurityFail "No rate limiting enforced")

testErrorMessages :: Request -> IO SecurityResult
testErrorMessages req = do
  let badReq = addHeader "Content-Type" "application/json" $
               setRequestBody "invalid json" req
  response <- httpSend badReq
  let body  = responseBodyText response
  if any (`T.isInfixOf` body) sensitivePatterns
    then return (SecurityFail "Error leaks sensitive info")
    else return SecurityPass
  where
    sensitivePatterns =
      [ "stacktrace", "exception", "database", "postgresql"
      , "password", "secret", "token", "internal error"
      ]
```

---

## ขั้นตอนที่ 673: Data Encryption at Rest

```haskell
-- Encrypting sensitive database columns

import Database.Persist.Sql

-- Encrypted field type
newtype Encrypted a = Encrypted { getEncrypted :: ByteString }

class EncryptedField a where
  encryptField  :: EncryptionKey -> a -> IO (Encrypted a)
  decryptField  :: EncryptionKey -> Encrypted a -> IO (Either Text a)

instance EncryptedField Text where
  encryptField key value = do
    (iv, ciphertext, tag) <- encrypt key (encodeUtf8 value)
    return (Encrypted (iv <> tag <> ciphertext))
  
  decryptField key (Encrypted bs) = do
    let (iv, rest) = BS.splitAt 12 bs
    let (tag, ciphertext) = BS.splitAt 16 rest
    return (fmap decodeUtf8 (decrypt key iv ciphertext tag))

-- Custom Persistent type
instance PersistField (Encrypted Text) where
  toPersistValue (Encrypted bs) = PersistByteString bs
  fromPersistValue (PersistByteString bs) = Right (Encrypted bs)
  fromPersistValue _ = Left "Expected ByteString"

-- Model with encrypted field
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name       Text
  email      Text
  ssn        (Encrypted Text)
  creditCard (Encrypted Text)
  createdAt  UTCTime default=now()
|]

-- Save with encryption
createUserSecure :: EncryptionKey -> CreateUserReq -> DB (Key User)
createUserSecure key req = do
  encSsn    <- liftIO $ encryptField key (curSsn req)
  encCard   <- liftIO $ encryptField key (curCreditCard req)
  insert User
    { userName       = curName req
    , userEmail      = curEmail req
    , userSsn        = encSsn
    , userCreditCard = encCard
    , userCreatedAt  = currentTime
    }
```

---

## ขั้นตอนที่ 674: Privacy by Design

```haskell
-- Privacy-preserving data handling

-- PII data types
newtype PII a = PII { getPII :: a }
  deriving (Eq)

instance Show (PII a) where
  show _ = "[REDACTED]"

instance ToJSON (PII a) where
  toJSON _ = String "[REDACTED]"

-- Data minimization
data UserPublic = UserPublic
  { upId   :: UserId
  , upName :: Text
  }

data UserPrivate = UserPrivate
  { uprId    :: UserId
  , uprName  :: Text
  , uprEmail :: PII Text
  , uprPhone :: PII Text
  , uprSsn   :: PII Text
  }

-- Redact for logging
redactUser :: UserPrivate -> Value
redactUser u = object
  [ "id"    .= uprId u
  , "name"  .= uprName u
  , "email" .= show (uprEmail u)  -- "[REDACTED]"
  ]

-- Data retention
purgeExpiredData :: ConnectionPool -> IO Int
purgeExpiredData pool = do
  cutoff <- addUTCTime (negate retentionPeriod) <$> getCurrentTime
  count  <- runSqlPool
    (deleteWhere [UserCreatedAt <. cutoff, UserActive ==. False])
    pool
  return count
  where retentionPeriod = 365 * 24 * 3600  -- 1 year

-- Anonymization for analytics
anonymizeUser :: User -> AnonymizedUser
anonymizeUser u = AnonymizedUser
  { auId       = hashUserId (userId u)  -- one-way hash
  , auRegion   = extractRegion (userCountry u)
  , auAgeGroup = ageGroup (userAge u)
  , auPlan     = userPlan u
  }
  where
    ageGroup age
      | age < 18  = "under_18"
      | age < 30  = "18-29"
      | age < 50  = "30-49"
      | otherwise = "50+"
```

---

## ขั้นตอนที่ 675: Security Event Monitoring

```haskell
-- Security event monitoring and SIEM integration

data SecurityEvent
  = LoginSuccess    { seUserId :: UserId, seIp :: Text }
  | LoginFailed     { seUsername :: Text, seIp :: Text, seReason :: Text }
  | PasswordChanged { seUserId :: UserId }
  | MfaEnabled      { seUserId :: UserId }
  | SuspiciousLogin { seUserId :: UserId, seIp :: Text, seReason :: Text }
  | ApiKeyRotated   { seApiKeyId :: Text }
  | UnauthorizedAccess { seUserId :: Maybe UserId, seIp :: Text, sePath :: Text }
  | DataExport      { seUserId :: UserId, seRecordCount :: Int }

-- Detect anomalies
data AnomalyDetector = AnomalyDetector
  { adLoginHistory :: TVar (Map UserId [LoginRecord])
  , adThresholds   :: AnomalyThresholds
  }

checkLoginAnomaly :: AnomalyDetector -> UserId -> Text -> IO [SecurityAlert]
checkLoginAnomaly detector uid ip = do
  history <- Map.findWithDefault [] uid <$> readTVarIO (adLoginHistory detector)
  
  let recentLogins   = filter recentTime history
  let uniqueIps      = nub (map lrIp recentLogins)
  let failedCount    = length (filter (not . lrSuccess) recentLogins)
  
  catMaybes <$> sequence
    [ if length uniqueIps > maxLocations then return (Just (MultipleLocations uid))
      else return Nothing
    , if failedCount > maxFailures then return (Just (BruteForce uid failedCount))
      else return Nothing
    , if ip `elem` knownBadIps then return (Just (SuspiciousIp uid ip))
      else return Nothing
    ]
  where
    maxLocations = adMaxLocations (adThresholds detector)
    maxFailures  = adMaxFailures  (adThresholds detector)
    recentTime lr = loginTime lr > recentCutoff
    recentCutoff  = addUTCTime (-3600) getCurrentTime  -- 1 hour

-- Send to SIEM
sendToSiem :: SecurityEvent -> IO ()
sendToSiem event = do
  let payload = encode (toJsonLog event)
  httpPost siemEndpoint payload
```

---

## ขั้นตอนที่ 676: Zero-Trust Architecture

```haskell
-- Zero-trust network security

-- Every request must be verified
data TrustContext = TrustContext
  { tcIdentity   :: Identity
  , tcDevice     :: DeviceInfo
  , tcLocation   :: GeoLocation
  , tcRiskScore  :: Double  -- 0-1
  }

data Identity
  = ServiceIdentity  { siCertificate :: X509 }
  | UserIdentity     { uiId :: UserId, uiMfa :: Bool, uiSession :: SessionId }
  | ApiIdentity      { aiKeyId :: Text, aiScopes :: [Text] }

-- Compute risk score
computeRiskScore :: TrustContext -> IO Double
computeRiskScore ctx = do
  scores <- mapM ($ ctx)
    [ checkDeviceTrust
    , checkLocationRisk
    , checkBehavioralRisk
    , checkTimeRisk
    ]
  return (sum scores / fromIntegral (length scores))

checkDeviceTrust :: TrustContext -> IO Double
checkDeviceTrust ctx =
  case tcDevice ctx of
    ManagedDevice  -> return 0.1  -- low risk
    UnknownDevice  -> return 0.7  -- high risk
    _              -> return 0.4

-- Policy decision
data AccessPolicy = AccessPolicy
  { apRequirements :: [TrustRequirement]
  , apActions      :: Map AccessLevel [Action]
  }

data TrustRequirement
  = RequireMfa
  | RequireKnownDevice
  | RequireLowRisk Double
  | RequireNetwork [CIDR]

evaluatePolicy :: AccessPolicy -> TrustContext -> IO AccessDecision
evaluatePolicy policy ctx = do
  riskScore <- computeRiskScore ctx
  
  let failed = filter (not . checkRequirement ctx riskScore) (apRequirements policy)
  
  if null failed
    then return (AccessGranted (determineLevel ctx riskScore))
    else return (AccessDenied (map describeRequirement failed))
```

---

## ขั้นตอนที่ 677: Secure Session Management

```haskell
-- Secure session management

import Web.ClientSession

data SessionConfig = SessionConfig
  { scKey      :: Key
  , scTtl      :: Int  -- seconds
  , scDomain   :: Text
  , scSecure   :: Bool
  , scHttpOnly :: Bool
  , scSameSite :: SameSiteOption
  }

data Session = Session
  { sesId        :: SessionId
  , sesUserId    :: Maybe UserId
  , sesCreatedAt :: UTCTime
  , sesLastSeen  :: UTCTime
  , sesIp        :: Text
  , sesData      :: Map Text Value
  }

-- Create secure session
createSession :: SessionConfig -> UserId -> Text -> IO (Session, SetCookie)
createSession cfg uid ip = do
  now <- getCurrentTime
  sid <- generateSessionId
  let session = Session sid (Just uid) now now ip Map.empty
  
  -- Encrypt session data
  encrypted <- encryptIO (scKey cfg) (encode session)
  
  let cookie = defaultSetCookie
        { setCookieName     = "session"
        , setCookieValue    = B64.encode encrypted
        , setCookiePath     = Just "/"
        , setCookieDomain   = Just (T.unpack (scDomain cfg))
        , setCookieMaxAge   = Just (fromIntegral (scTtl cfg))
        , setCookieSecure   = scSecure cfg
        , setCookieHttpOnly = scHttpOnly cfg
        , setCookieSameSite = Just (scSameSite cfg)
        }
  
  return (session, cookie)

-- Validate and refresh session
validateSession :: SessionConfig -> ByteString -> IO (Either Text Session)
validateSession cfg cookieVal = do
  now <- getCurrentTime
  case B64.decode cookieVal >>= decrypt (scKey cfg) of
    Left _  -> return (Left "Invalid cookie")
    Right bs ->
      case decode bs of
        Nothing -> return (Left "Corrupt session")
        Just session ->
          if diffUTCTime now (sesCreatedAt session) > fromIntegral (scTtl cfg)
          then return (Left "Session expired")
          else return (Right session { sesLastSeen = now })
```

---

## ขั้นตอนที่ 678: OAuth 2.0 Provider

```haskell
-- OAuth 2.0 authorization server

data OAuthConfig = OAuthConfig
  { ocIssuer        :: Text
  , ocAuthzEndpoint :: Text
  , ocTokenEndpoint :: Text
  , ocSigningKey    :: JWK
  }

-- Authorization code flow
data AuthzRequest = AuthzRequest
  { arClientId     :: ClientId
  , arRedirectUri  :: Text
  , arScope        :: [Scope]
  , arState        :: Text
  , arCodeChallenge :: Maybe Text  -- PKCE
  , arCodeMethod   :: Maybe Text   -- S256
  }

-- Generate authorization code
generateAuthzCode :: AuthzRequest -> UserId -> IO AuthzCode
generateAuthzCode req uid = do
  code <- secureRandom 32
  now  <- getCurrentTime
  return AuthzCode
    { acCode        = B64Url.encode code
    , acClientId    = arClientId req
    , acUserId      = uid
    , acRedirectUri = arRedirectUri req
    , acScope       = arScope req
    , acExpiresAt   = addUTCTime 600 now  -- 10 minutes
    , acChallenge   = arCodeChallenge req
    }

-- Exchange code for tokens
exchangeCode :: OAuthConfig -> TokenRequest -> IO TokenResponse
exchangeCode cfg req = do
  code <- lookupAuthzCode (trCode req)
  
  -- Validate
  when (acClientId code /= trClientId req) $
    throwError "client_id_mismatch"
  when (acRedirectUri code /= trRedirectUri req) $
    throwError "redirect_uri_mismatch"
  
  -- Verify PKCE
  forM_ (acChallenge code) $ \challenge ->
    unless (verifyPkce challenge (trCodeVerifier req)) $
      throwError "invalid_code_verifier"
  
  -- Issue tokens
  accessToken  <- createAccessToken cfg (acUserId code) (acScope code)
  refreshToken <- createRefreshToken cfg (acUserId code)
  
  deleteAuthzCode (acCode code)
  
  return (TokenResponse accessToken refreshToken "Bearer" 3600 (acScope code))
```

---

## ขั้นตอนที่ 679: Security Testing Pipeline

```haskell
-- CI/CD security pipeline

data SecurityPipeline = SecurityPipeline
  { spStages :: [SecurityStage]
  }

data SecurityStage
  = DependencyCheck    { dcCommand :: Text }
  | StaticAnalysis     { saCommand :: Text, saThreshold :: Severity }
  | SecretsScanning    { ssPatterns :: [Text] }
  | ContainerScan      { csImage :: Text }
  | DastScan           { dsScanTarget :: Text }

-- Run security pipeline
runSecurityPipeline :: SecurityPipeline -> IO PipelineResult
runSecurityPipeline pipeline = do
  results <- forM (spStages pipeline) runStage
  let passed = all stgPassed results
  return PipelineResult { prPassed = passed, prStages = results }

runStage :: SecurityStage -> IO StageResult
runStage (SecretsScanning patterns) = do
  files   <- findAllFiles "."
  secrets <- concat <$> mapM (scanForSecrets patterns) files
  
  return StageResult
    { stgName   = "secrets_scanning"
    , stgPassed = null secrets
    , stgIssues = map showSecret secrets
    }

scanForSecrets :: [Text] -> FilePath -> IO [SecretFinding]
scanForSecrets patterns file = do
  content <- readFile file
  return [ SecretFinding file line pattern
         | pattern <- patterns
         , (line, text) <- zip [1..] (lines content)
         , T.pack pattern `T.isInfixOf` text
         ]

-- Block merge on critical findings
gateCheck :: PipelineResult -> IO ()
gateCheck result = do
  let critical = filter ((== SecurityError) . issueSeverity) (allIssues result)
  unless (null critical) $ do
    putStrLn "SECURITY GATE FAILED:"
    mapM_ print critical
    exitWith (ExitFailure 1)
```

---

## ขั้นตอนที่ 680: โปรเจกต์: Secure API Platform

```haskell
-- Complete secure API platform

-- Security config
data SecurityConfig = SecurityConfig
  { scJwtKeys      :: JwtKeys
  , scEncKey       :: EncryptionKey
  , scRateLimiter  :: RateLimiter
  , scCsrfConfig   :: CsrfConfig
  , scVaultConfig  :: VaultConfig
  , scAuditLog     :: AuditLogger
  , scAlerts       :: SecurityAlertManager
  }

-- Apply all security middleware
secureApplication :: SecurityConfig -> Application -> Application
secureApplication cfg =
    securityHeadersMiddleware
  . httpsRedirectMiddleware
  . csrfMiddleware     (scCsrfConfig cfg)
  . rateLimitMiddleware (scRateLimiter cfg) clientIpKey
  . auditMiddleware    (scAuditLog cfg)
  . corsMiddleware     corsConfig
  . jwtAuthMiddleware  (scJwtKeys cfg)
  where
    corsConfig = defaultCorsConfig
      { corsOrigins = ["https://app.example.com"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE"]
      }
    clientIpKey req = fromMaybe "unknown" (remoteIp req)

-- Secure route handlers
type SecureAPI = AuthProtect "jwt" :> SecureRoutes

secureServer :: SecurityConfig -> Server SecureAPI
secureServer cfg authResult = secureHandlers
  where
    secureHandlers = getUserHandler :<|> updateUserHandler :<|> deleteHandler
    
    getUserHandler uid = withAudit cfg "get_user" uid $
      getUser uid
    
    updateUserHandler uid body = do
      validated <- liftEither (validateUpdateReq body)
      withAudit cfg "update_user" uid $
        updateUser uid validated
    
    deleteHandler uid = do
      requirePermission authResult "users:delete"
      withAudit cfg "delete_user" uid $
        deleteUser uid
```

---

*[← Part 33](part-33.md) | [Part 35 →](part-35.md)*
