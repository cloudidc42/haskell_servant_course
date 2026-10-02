# Part 27: Real-World Servant Applications
## ขั้นตอนที่ 521-540

---

## ขั้นตอนที่ 521: Servant API Design Patterns

```haskell
-- Best practices ในการออกแบบ Servant API

-- 1. Versioned API
type V1API = "v1" :> V1Routes
type V2API = "v2" :> V2Routes

type API = V1API :<|> V2API

-- 2. Resource-based routing
type UsersAPI
  =    "users" :> Get '[JSON] [User]               -- GET /users
  :<|> "users" :> ReqBody '[JSON] CreateUserReq
                :> Post '[JSON] User                -- POST /users
  :<|> "users" :> Capture "id" UserId
                :> Get '[JSON] User                 -- GET /users/:id
  :<|> "users" :> Capture "id" UserId
                :> ReqBody '[JSON] UpdateUserReq
                :> Put '[JSON] User                 -- PUT /users/:id
  :<|> "users" :> Capture "id" UserId
                :> Delete '[JSON] NoContent         -- DELETE /users/:id

-- 3. Nested resources
type PostsAPI
  =    "posts" :> Get '[JSON] [Post]
  :<|> "posts" :> Capture "id" PostId
                :> "comments" :> Get '[JSON] [Comment]  -- GET /posts/:id/comments
  :<|> "posts" :> Capture "id" PostId
                :> "comments" :> ReqBody '[JSON] CreateCommentReq
                :> Post '[JSON] Comment

-- 4. Pagination
type PaginatedAPI
  = "items" :> QueryParam "page" Int
            :> QueryParam "per_page" Int
            :> QueryParam "sort" SortField
            :> QueryParam "order" SortOrder
            :> Get '[JSON] (PaginatedResponse Item)

data PaginatedResponse a = PaginatedResponse
  { prData     :: [a]
  , prTotal    :: Int
  , prPage     :: Int
  , prPerPage  :: Int
  , prLastPage :: Int
  } deriving (Generic, ToJSON)

-- 5. Filtering
type FilteredAPI
  = "products" :> QueryParams "category" Text
               :> QueryParam "min_price" Double
               :> QueryParam "max_price" Double
               :> QueryFlag "in_stock"
               :> Get '[JSON] [Product]
```

---

## ขั้นตอนที่ 522: Request Validation

```haskell
-- ตรวจสอบ request ก่อน handler

import Data.Validation
import qualified Data.List.NonEmpty as NE

-- Validation types
data ValidationError
  = FieldRequired Text
  | FieldTooShort Text Int
  | FieldTooLong Text Int
  | FieldInvalidFormat Text Text
  | FieldOutOfRange Text Double Double
  deriving (Show, Generic, ToJSON)

type Validation' = Validation (NE.NonEmpty ValidationError)

-- Validate CreateUserReq
validateCreateUser :: CreateUserReq -> Validation' ValidCreateUserReq
validateCreateUser req =
  ValidCreateUserReq
    <$> validateName (curName req)
    <*> validateEmail (curEmail req)
    <*> validatePassword (curPassword req)
    <*> validateAge (curAge req)

validateName :: Text -> Validation' Text
validateName name
  | T.null name       = Failure (NE.singleton (FieldRequired "name"))
  | T.length name < 2 = Failure (NE.singleton (FieldTooShort "name" 2))
  | T.length name > 100 = Failure (NE.singleton (FieldTooLong "name" 100))
  | otherwise         = Success name

validateEmail :: Text -> Validation' Text
validateEmail email
  | T.null email = Failure (NE.singleton (FieldRequired "email"))
  | not (emailRegex `T.isInfixOf` email) =
      Failure (NE.singleton (FieldInvalidFormat "email" "must be valid email"))
  | otherwise = Success email

-- Use validation in Servant handler
postUsersHandler :: CreateUserReq -> Handler User
postUsersHandler req = do
  case validateCreateUser req of
    Failure errs -> throwError err422
      { errBody = encode (object ["errors" .= NE.toList errs]) }
    Success validReq -> do
      user <- liftIO (createUser validReq)
      return user
```

---

## ขั้นตอนที่ 523: Response Formatting

```haskell
-- Consistent API responses

-- Standard response envelope
data ApiResponse a = ApiResponse
  { arData    :: Maybe a
  , arError   :: Maybe ApiError
  , arMeta    :: Maybe Meta
  , arLinks   :: Maybe Links
  } deriving (Generic)

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON ApiResponse{..} = object $ concat
    [ [ "data"  .= arData  | isJust arData  ]
    , [ "error" .= arError | isJust arError ]
    , [ "meta"  .= arMeta  | isJust arMeta  ]
    , [ "links" .= arLinks | isJust arLinks ]
    ]

data ApiError = ApiError
  { aeCode    :: Text
  , aeMessage :: Text
  , aeDetails :: Maybe Value
  } deriving (Generic, ToJSON)

data Meta = Meta
  { metaTotal   :: Maybe Int
  , metaPage    :: Maybe Int
  , metaPerPage :: Maybe Int
  } deriving (Generic, ToJSON)

data Links = Links
  { linkSelf  :: Maybe Text
  , linkFirst :: Maybe Text
  , linkPrev  :: Maybe Text
  , linkNext  :: Maybe Text
  , linkLast  :: Maybe Text
  } deriving (Generic, ToJSON)

-- Helpers
success :: a -> ApiResponse a
success x = ApiResponse (Just x) Nothing Nothing Nothing

successWith :: a -> Meta -> Links -> ApiResponse a
successWith x meta links = ApiResponse (Just x) Nothing (Just meta) (Just links)

failure :: Text -> Text -> ApiResponse a
failure code msg = ApiResponse Nothing (Just (ApiError code msg Nothing)) Nothing Nothing

-- Apply to Servant
type API = "users" :> Get '[JSON] (ApiResponse [User])

getUsersHandler :: Handler (ApiResponse [User])
getUsersHandler = do
  users <- liftIO getAllUsers
  return $ success users
```

---

## ขั้นตอนที่ 524: Streaming Responses

```haskell
-- Streaming ใน Servant

import Servant.Types.SourceT
import Data.Conduit

-- Streaming JSON array
type StreamAPI
  = "users" :> StreamGet NewlineFraming JSON (SourceIO User)
  :<|> "events" :> StreamGet ServerSentEvents JSON (SourceIO ServerEvent)

-- Handler สำหรับ streaming users
streamUsersHandler :: Handler (SourceIO User)
streamUsersHandler = do
  pool <- getPool
  return $ source $ \step -> do
    runConduit $
      selectSource [] [Asc UserId]
      .| mapC entityVal
      .| sinkToStep step

-- Server-Sent Events
streamEventsHandler :: Handler (SourceIO ServerEvent)
streamEventsHandler = do
  chan <- liftIO newBroadcastChan
  return $ source $ \step -> do
    subChan <- dupChan chan
    forever $ do
      event <- readChan subChan
      step (ServerEvent
        { eventName = Just "message"
        , eventId   = Just (T.pack (show (eventId event)))
        , eventData = [T.encodeUtf8 (toText event)]
        })

-- Streaming download
type DownloadAPI
  = "download" :> Capture "file" FilePath
               :> StreamGet NoFraming OctetStream (SourceIO ByteString)

streamFileHandler :: FilePath -> Handler (SourceIO ByteString)
streamFileHandler path = do
  exists <- liftIO $ doesFileExist path
  unless exists $ throwError err404
  return $ source $ \step -> do
    runConduit $ sourceFile path .| mapM_C step
```

---

## ขั้นตอนที่ 525: Content Negotiation

```haskell
-- Content negotiation ใน Servant

-- Support multiple formats
type MultiFormatAPI
  = "data" :> Get '[JSON, XML, CSV, PlainText] DataResponse

data DataResponse = DataResponse
  { drItems :: [Item]
  , drCount :: Int
  } deriving (Generic, ToJSON)

-- XML instance
instance MimeRender XML DataResponse where
  mimeRender _ resp = renderXml resp

renderXml :: DataResponse -> BSL.ByteString
renderXml DataResponse{..} = BSL.fromStrict . T.encodeUtf8 $
  "<?xml version=\"1.0\"?>\n<response>\n" <>
  "  <count>" <> T.pack (show drCount) <> "</count>\n" <>
  "  <items>" <>
  T.concat (map itemToXml drItems) <>
  "</items>\n</response>"

-- CSV instance
instance MimeRender CSV DataResponse where
  mimeRender _ resp =
    BSL.fromStrict . T.encodeUtf8 $
    "id,name,price\n" <>
    T.unlines (map itemToCsv (drItems resp))

-- Handler (works for all formats!)
getDataHandler :: Handler DataResponse
getDataHandler = do
  items <- liftIO getAllItems
  return DataResponse
    { drItems = items
    , drCount = length items
    }

-- Custom Accept header parsing
data XML

instance Accept XML where
  contentType _ = "application" M.// "xml"

data CSV

instance Accept CSV where
  contentType _ = "text" M.// "csv"
```

---

## ขั้นตอนที่ 526: API Versioning Strategies

```haskell
-- API versioning strategies

-- Strategy 1: URL versioning
type V1API = "v1" :> V1Routes
type V2API = "v2" :> V2Routes

-- Strategy 2: Header versioning
type HeaderVersionedAPI
  = Header "API-Version" Text :> VersionedRoutes

-- Strategy 3: Content-type versioning
type ContentVersionedAPI
  = ReqBody '[JSON] CreateUserV1
  :> Post '[JSON] UserV1

-- Version negotiation middleware
versionMiddleware :: Middleware
versionMiddleware app req respond = do
  let version = fromMaybe "v1" $ lookup "API-Version" (requestHeaders req)
  let req' = req { requestHeaders = 
        ("X-API-Version", version) : requestHeaders req }
  app req' respond

-- Backward compatibility
data UserV1 = UserV1
  { v1Name  :: Text
  , v1Email :: Text
  } deriving (Generic, ToJSON, FromJSON)

data UserV2 = UserV2
  { v2Name        :: Text
  , v2Email       :: Text
  , v2PhoneNumber :: Maybe Text  -- new in v2
  , v2Avatar      :: Maybe Text  -- new in v2
  } deriving (Generic, ToJSON, FromJSON)

-- Migration function
migrateV1ToV2 :: UserV1 -> UserV2
migrateV1ToV2 UserV1{..} = UserV2
  { v2Name        = v1Name
  , v2Email       = v1Email
  , v2PhoneNumber = Nothing  -- not in v1
  , v2Avatar      = Nothing  -- not in v1
  }

-- Handler that serves both versions
getUserHandler :: Maybe Text -> UserId -> Handler Value
getUserHandler mVersion uid = do
  user <- getUser uid
  case mVersion of
    Just "v2" -> return $ toJSON (userToV2 user)
    _         -> return $ toJSON (userToV1 user)  -- default v1
```

---

## ขั้นตอนที่ 527: API Key Management

```haskell
-- API Key authentication system

data ApiKey = ApiKey
  { akKey       :: Text    -- hashed
  , akPrefix    :: Text    -- first 8 chars (plain)
  , akUserId    :: UserId
  , akScopes    :: [Scope]
  , akCreatedAt :: UTCTime
  , akExpiresAt :: Maybe UTCTime
  , akLastUsed  :: Maybe UTCTime
  , akEnabled   :: Bool
  , akName      :: Text    -- "Production key"
  } deriving (Generic)

-- Create new API key
generateApiKey :: UserId -> [Scope] -> Text -> DB (Text, ApiKey)
generateApiKey uid scopes name = do
  rawKey <- liftIO $ T.pack . take 32 . show <$> randomIO @UUID
  let prefix = T.take 8 rawKey
  let hashed = hashKey rawKey
  now <- liftIO getCurrentTime
  let apiKey = ApiKey
        { akKey       = hashed
        , akPrefix    = prefix
        , akUserId    = uid
        , akScopes    = scopes
        , akCreatedAt = now
        , akExpiresAt = Nothing
        , akLastUsed  = Nothing
        , akEnabled   = True
        , akName      = name
        }
  insert apiKey
  return ("sk_" <> rawKey, apiKey)

-- Validate API key
validateApiKey :: Text -> DB (Maybe ApiKey)
validateApiKey rawKey = do
  let hashed = hashKey rawKey
  mKey <- selectFirst [ApiKeyKey ==. hashed, ApiKeyEnabled ==. True] []
  case mKey of
    Nothing  -> return Nothing
    Just (Entity kid k) -> do
      now <- liftIO getCurrentTime
      case akExpiresAt k of
        Just expiry | expiry < now -> return Nothing
        _ -> do
          update kid [ApiKeyLastUsed =. Just now]
          return (Just k)

-- Servant auth with API key
data ApiKeyAuth

instance HasContextEntry context ApiKeyAuthConfig
  => IsAuth ApiKeyAuth UserId context where
  
  authCheck _ context = \req -> do
    let mKey = lookup "X-API-Key" (requestHeaders req)
    case mKey of
      Nothing  -> return Indefinite
      Just key -> do
        mApiKey <- runDB (validateApiKey (T.decodeUtf8 key))
        case mApiKey of
          Nothing    -> return (Authenticated (Left Unauthorized))
          Just apiKey -> return (Authenticated (Right (akUserId apiKey)))
```

---

## ขั้นตอนที่ 528: CORS สำหรับ Production

```haskell
-- CORS configuration

import Network.Wai.Middleware.Cors

-- Production CORS policy
productionCors :: Middleware
productionCors = cors (const $ Just corsPolicy)
  where
    corsPolicy = simpleCorsResourcePolicy
      { corsOrigins        = Just (["https://myapp.com", "https://www.myapp.com"], True)
      , corsMethods        = ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization", "X-Request-ID", "X-API-Key"]
      , corsExposedHeaders = Just ["X-Request-ID", "X-RateLimit-Remaining"]
      , corsMaxAge         = Just 86400  -- 24 hours
      , corsVaryOrigin     = True
      , corsIgnoreFailures = False
      }

-- Development CORS (allow all)
developmentCors :: Middleware
developmentCors = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods        = ["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  })

-- Conditional CORS based on environment
appCors :: Environment -> Middleware
appCors Production = productionCors
appCors Development = developmentCors
appCors Testing = developmentCors

-- Apply to application
main :: IO ()
main = do
  env <- getEnvironment
  app <- mkApplication
  run 8080 $ appCors env app
```

---

## ขั้นตอนที่ 529: File Uploads

```haskell
-- File upload ใน Servant

import Servant.Multipart

-- Multipart form data
type UploadAPI
  = "upload" :> MultipartForm Mem MultipartData
             :> Post '[JSON] UploadResponse

-- Handle file upload
postUploadHandler :: MultipartData Mem -> Handler UploadResponse
postUploadHandler mpData = do
  case lookupFile "file" mpData of
    Nothing   -> throwError err400 { errBody = "No file provided" }
    Just file -> do
      -- Validate file
      let contentType = fdFileCType file
      unless (isAllowedType contentType) $
        throwError err400 { errBody = "Invalid file type" }
      
      let size = BS.length (fdPayload file)
      when (size > maxFileSize) $
        throwError err413 { errBody = "File too large" }
      
      -- Process and store
      key <- liftIO $ storeFile (fdFileCType file) (fdPayload file)
      return UploadResponse
        { urKey      = key
        , urFilename = fdFileName file
        , urSize     = size
        , urUrl      = "https://cdn.example.com/" <> key
        }
  where
    maxFileSize   = 10 * 1024 * 1024  -- 10MB
    allowedTypes  = ["image/jpeg", "image/png", "image/gif", "application/pdf"]
    isAllowedType = flip elem allowedTypes

-- Multi-file upload
type MultiUploadAPI
  = "upload" :> "multiple"
             :> MultipartForm Mem MultipartData
             :> Post '[JSON] [UploadResponse]

multiUploadHandler :: MultipartData Mem -> Handler [UploadResponse]
multiUploadHandler mpData = do
  let files = lookupFiles "files" mpData
  forM files $ \file -> do
    key <- liftIO $ storeFile (fdFileCType file) (fdPayload file)
    return UploadResponse
      { urKey      = key
      , urFilename = fdFileName file
      , urSize     = BS.length (fdPayload file)
      , urUrl      = "https://cdn.example.com/" <> key
      }
```

---

## ขั้นตอนที่ 530: WebSocket Handler

```haskell
-- WebSocket ใน Servant

import Network.WebSockets
import Servant.API.WebSocket

-- WebSocket API
type WsAPI = "ws" :> WebSocket

wsHandler :: Connection -> Handler ()
wsHandler conn = do
  liftIO $ sendTextData conn ("Connected!" :: Text)
  forever $ do
    msg <- liftIO $ receiveData conn
    let response = processMessage msg
    liftIO $ sendTextData conn response
  where
    processMessage :: Text -> Text
    processMessage msg
      | "ping" `T.isPrefixOf` msg = "pong"
      | otherwise = "Echo: " <> msg

-- Chat room WebSocket
data ChatRoom = ChatRoom
  { crClients :: TVar (Map ClientId Connection)
  , crHistory :: TVar [ChatMessage]
  }

chatHandler :: ChatRoom -> Connection -> Handler ()
chatHandler room conn = do
  clientId <- liftIO newClientId
  liftIO $ atomically $ modifyTVar (crClients room)
    (Map.insert clientId conn)
  
  -- Send history to new client
  history <- liftIO $ readTVarIO (crHistory room)
  liftIO $ forM_ (reverse history) $ \msg ->
    sendTextData conn (encode msg)
  
  -- Handle messages
  finally
    (forever $ do
      raw <- liftIO $ receiveData conn
      case decode raw of
        Nothing  -> liftIO $ sendTextData conn ("Invalid message" :: Text)
        Just msg -> do
          liftIO $ atomically $ modifyTVar (crHistory room)
            (take 100 . (msg:))
          clients <- liftIO $ readTVarIO (crClients room)
          forM_ (Map.elems clients) $ \c ->
            E.try @SomeException (sendTextData c (encode msg)) >>= \_ -> return ())
    (liftIO $ atomically $ modifyTVar (crClients room)
      (Map.delete clientId))
```

---

## ขั้นตอนที่ 531: Request Throttling

```haskell
-- Rate limiting ขั้นสูง

import Control.Monad.Trans.Control
import Data.Cache.LRU

-- Token bucket per user
data UserBucket = UserBucket
  { ubTokens    :: TVar Double
  , ubLastRefill :: TVar UTCTime
  }

newUserBucket :: Int -> IO UserBucket
newUserBucket initialTokens = do
  tokens    <- newTVarIO (fromIntegral initialTokens)
  lastRefill <- newTVarIO =<< getCurrentTime
  return UserBucket{..}

-- Token bucket algorithm
consumeToken :: UserBucket -> Double -> Int -> IO Bool
consumeToken bucket refillRate capacity = do
  now <- getCurrentTime
  atomically $ do
    tokens    <- readTVar (ubTokens bucket)
    lastRefill <- readTVar (ubLastRefill bucket)
    
    let elapsed     = realToFrac (diffUTCTime now lastRefill) :: Double
        newTokens   = min (fromIntegral capacity) (tokens + elapsed * refillRate)
    
    if newTokens >= 1
      then do
        writeTVar (ubTokens bucket)    (newTokens - 1)
        writeTVar (ubLastRefill bucket) now
        return True
      else return False

-- LRU cache of buckets
data RateLimiter = RateLimiter
  { rlBuckets    :: TVar (LRU Text UserBucket)
  , rlRefillRate :: Double  -- tokens per second
  , rlCapacity   :: Int     -- max tokens
  }

checkRateLimit :: RateLimiter -> Text -> IO RateLimitResult
checkRateLimit rl key = do
  bucket <- getOrCreateBucket rl key
  allowed <- consumeToken bucket (rlRefillRate rl) (rlCapacity rl)
  tokens  <- readTVarIO (ubTokens bucket)
  return RateLimitResult
    { rrlAllowed   = allowed
    , rrlRemaining = floor tokens
    , rrlResetAt   = Nothing
    }

-- Apply as middleware
rateLimitMiddleware :: RateLimiter -> (Request -> Text) -> Middleware
rateLimitMiddleware rl keyExtractor app req respond = do
  let key = keyExtractor req
  result <- checkRateLimit rl key
  if rrlAllowed result
    then app req $ \resp ->
      respond (mapResponseHeaders (addRateLimitHeaders result) resp)
    else respond $ responseLBS status429
      [ ("Retry-After", "60")
      , ("X-RateLimit-Remaining", "0")
      ] "Too Many Requests"
```

---

## ขั้นตอนที่ 532: API Monitoring

```haskell
-- API monitoring ด้วย Prometheus

import Prometheus

-- Define metrics
data ApiMetrics = ApiMetrics
  { amRequestCount    :: Counter
  , amRequestDuration :: Histogram
  , amActiveRequests  :: Gauge
  , amErrorCount      :: Counter
  , amResponseSize    :: Histogram
  }

initApiMetrics :: IO ApiMetrics
initApiMetrics = ApiMetrics
  <$> registerCounter (Info "http_requests_total" "Total HTTP requests")
         [("method", ""), ("path", ""), ("status", "")]
  <*> registerHistogram (Info "http_request_duration_seconds" "Request duration")
         [("method", ""), ("path", "")] defaultBuckets
  <*> registerGauge (Info "http_active_requests" "Active requests") []
  <*> registerCounter (Info "http_errors_total" "HTTP errors")
         [("method", ""), ("path", ""), ("error", "")]
  <*> registerHistogram (Info "http_response_size_bytes" "Response size")
         [("path", "")] [100, 1000, 10000, 100000]

-- Metrics middleware
metricsMiddleware :: ApiMetrics -> Middleware
metricsMiddleware metrics app req respond = do
  let method = T.decodeUtf8 (requestMethod req)
      path   = T.decodeUtf8 (rawPathInfo req)
  
  incGauge (amActiveRequests metrics)
  start <- getCurrentTime
  
  app req $ \resp -> do
    decGauge (amActiveRequests metrics)
    end <- getCurrentTime
    
    let duration = realToFrac (diffUTCTime end start) :: Double
        status   = T.pack (show (statusCode (responseStatus resp)))
    
    observeHistogram (amRequestDuration metrics)
      [("method", method), ("path", path)] duration
    
    incCounterWith (amRequestCount metrics)
      [("method", method), ("path", path), ("status", status)] 1
    
    respond resp

-- Metrics endpoint
metricsApp :: Application
metricsApp _ respond = do
  metrics <- exportMetricsAsText
  respond $ responseLBS status200
    [("Content-Type", "text/plain; version=0.0.4")]
    (BSL.fromStrict metrics)
```

---

## ขั้นตอนที่ 533: Error Recovery

```haskell
-- Error recovery และ fault tolerance

-- Retry ด้วย exponential backoff
import Control.Retry

retryPolicy :: RetryPolicy
retryPolicy = exponentialBackoff 100000  -- 100ms initial
           <> limitRetries 3

withRetry :: MonadIO m => m a -> m a
withRetry action = recovering retryPolicy handlers (const action)
  where
    handlers = [const $ Handler handleIOException]
    handleIOException :: IOError -> IO Bool
    handleIOException _ = return True  -- retry all IO errors

-- Circuit breaker state
data CircuitState = Closed | Open UTCTime | HalfOpen

data CircuitBreaker = CircuitBreaker
  { cbState      :: TVar CircuitState
  , cbFailures   :: TVar Int
  , cbThreshold  :: Int
  , cbTimeout    :: NominalDiffTime
  }

callWithBreaker :: CircuitBreaker -> IO a -> IO (Either CircuitError a)
callWithBreaker cb action = do
  state <- readTVarIO (cbState cb)
  now   <- getCurrentTime
  case state of
    Open openedAt
      | diffUTCTime now openedAt < cbTimeout cb ->
          return (Left CircuitOpen)
      | otherwise -> do
          atomically $ writeTVar (cbState cb) HalfOpen
          tryAction cb action
    _ -> tryAction cb action

tryAction :: CircuitBreaker -> IO a -> IO (Either CircuitError a)
tryAction cb action = do
  result <- E.try action
  case result of
    Left (e :: SomeException) -> do
      recordFailure cb
      return (Left (ActionFailed e))
    Right v -> do
      atomically $ do
        writeTVar (cbFailures cb) 0
        writeTVar (cbState cb) Closed
      return (Right v)

recordFailure :: CircuitBreaker -> IO ()
recordFailure cb = do
  atomically $ modifyTVar (cbFailures cb) (+1)
  failures <- readTVarIO (cbFailures cb)
  when (failures >= cbThreshold cb) $ do
    now <- getCurrentTime
    atomically $ writeTVar (cbState cb) (Open now)
```

---

## ขั้นตอนที่ 534: API Documentation

```haskell
-- OpenAPI/Swagger documentation

import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

-- Generate Swagger spec from API type
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy MyAPI)
  & info.title       .~ "My API"
  & info.version     .~ "1.0.0"
  & info.description ?~ "My API documentation"
  & info.license     ?~ License "MIT" Nothing
  & host             ?~ "api.example.com"
  & schemes          ?~ [Https]
  & securityDefinitions .~ SecurityDefinitions
      [ ("BearerAuth", SecurityScheme
          { _securitySchemeType = SecuritySchemeApiKey
              (ApiKeyParams "Authorization" ApiKeyHeader)
          , _securitySchemeDescription = Just "JWT Bearer token"
          })
      ]

-- Add documentation to models
instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A user in the system"
    & mapped.schema.example     ?~ toJSON exampleUser

exampleUser :: User
exampleUser = User
  { userId   = 1
  , userName = "Alice"
  , userEmail = "alice@example.com"
  }

-- Add to Servant API
type DocumentedAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"
                :<|> "swagger.json" :> Get '[JSON] Swagger
                :<|> MyAPI

documentedServer :: Server DocumentedAPI
documentedServer = swaggerSchemaUIServer swaggerDoc
               :<|> return swaggerDoc
               :<|> myServer

-- Add endpoint descriptions
type CreateUserEndpoint
  = Summary "Create a new user"
  :> Description "Creates a new user account with the provided information"
  :> "users"
  :> ReqBody '[JSON] CreateUserReq
  :> Post '[JSON] User
```

---

## ขั้นตอนที่ 535: Database Transactions

```haskell
-- Transaction management ใน Servant+Persistent

-- Atomic operations
transferFunds :: AccountId -> AccountId -> Amount -> DB (Either TransferError ())
transferFunds fromId toId amount = do
  mFrom <- get fromId
  mTo   <- get toId
  
  case (mFrom, mTo) of
    (Nothing, _) -> return (Left (AccountNotFound fromId))
    (_, Nothing) -> return (Left (AccountNotFound toId))
    (Just from, Just to) -> do
      if accountBalance from < amount
        then return (Left InsufficientFunds)
        else do
          update fromId [AccountBalance -=. amount]
          update toId   [AccountBalance +=. amount]
          insert_ (Transaction fromId toId amount)
          return (Right ())

-- Serializable isolation
withSerializable :: DB a -> Handler a
withSerializable action = do
  pool <- getPool
  liftIO $ runSqlPool (transactionSerializable action) pool

-- Optimistic locking
updateWithVersion :: EntityId -> Int -> Updates -> DB (Either ConcurrencyError ())
updateWithVersion entityId version updates = do
  existing <- get entityId
  case existing of
    Nothing -> return (Left EntityNotFound)
    Just e ->
      if entityVersion e /= version
        then return (Left VersionMismatch)
        else do
          update entityId (updates ++ [EntityVersion +=. 1])
          return (Right ())

-- Deadlock handling
withDeadlockRetry :: DB a -> DB a
withDeadlockRetry action = go 3
  where
    go 0   = action
    go n = do
      result <- E.try action
      case result of
        Left (DeadlockDetected _) -> do
          liftIO $ threadDelay 10000  -- 10ms backoff
          go (n-1)
        Left err -> E.throwIO err
        Right val -> return val
```

---

## ขั้นตอนที่ 536: Webhook Handling

```haskell
-- Webhook handling system

data Webhook = Webhook
  { wId         :: WebhookId
  , wUrl        :: Text
  , wEvents     :: [EventType]
  , wSecret     :: Text  -- for signature verification
  , wEnabled    :: Bool
  , wCreatedAt  :: UTCTime
  }

-- Verify webhook signature (HMAC-SHA256)
import Crypto.MAC.HMAC

verifyWebhookSignature :: ByteString -> ByteString -> ByteString -> Bool
verifyWebhookSignature secret payload signature =
  let expected = hmacGetDigest (hmac secret payload :: HMAC SHA256)
      actual   = decodeBase64Lenient signature
  in expected `constEq` actual

-- Deliver webhook
data DeliveryResult = Success | Failed Text | Retrying Int

deliverWebhook :: Webhook -> Event -> IO DeliveryResult
deliverWebhook webhook event = do
  let payload = encode event
  let signature = computeSignature (wSecret webhook) payload
  
  manager <- newManager tlsManagerSettings
  req     <- parseRequest (T.unpack (wUrl webhook))
  
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS payload
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("X-Webhook-Signature", signature)
            , ("X-Webhook-Event", T.encodeUtf8 (eventType event))
            , ("X-Webhook-ID", T.encodeUtf8 (eventId event))
            ]
        }
  
  result <- E.try $ httpLbs req' manager
  case result of
    Left (e :: HttpException) -> return (Failed (T.pack (show e)))
    Right resp ->
      if statusCode (responseStatus resp) `elem` [200..299]
        then return Success
        else return (Failed $ "HTTP " <> T.pack (show (statusCode (responseStatus resp))))

-- Webhook endpoint (receive webhooks from external services)
type WebhookReceiveAPI
  = "webhooks" :> "github"
               :> Header "X-Hub-Signature-256" Text
               :> ReqBody '[JSON] Value
               :> Post '[JSON] NoContent

receiveGithubWebhook :: Maybe Text -> Value -> Handler NoContent
receiveGithubWebhook mSig payload = do
  secret <- getGithubWebhookSecret
  case mSig of
    Nothing  -> throwError err401
    Just sig -> do
      let payloadBs = BSL.toStrict (encode payload)
      unless (verifyWebhookSignature (T.encodeUtf8 secret) payloadBs (T.encodeUtf8 sig)) $
        throwError err401
      liftIO $ processGithubEvent payload
      return NoContent
```

---

## ขั้นตอนที่ 537: Background Jobs

```haskell
-- Background job processing

import Control.Concurrent.Async
import Data.Queue

data Job = Job
  { jobId       :: JobId
  , jobType     :: Text
  , jobPayload  :: Value
  , jobAttempts :: Int
  , jobMaxRetry :: Int
  , jobStatus   :: JobStatus
  , jobRunAt    :: UTCTime
  }

data JobStatus = Pending | Running | Completed | Failed Text

-- Job queue with Redis
enqueueJob :: Text -> Value -> DB JobId
enqueueJob jobType payload = do
  now <- liftIO getCurrentTime
  jid <- insert Job
    { jobId       = undefined  -- auto-generated
    , jobType     = jobType
    , jobPayload  = payload
    , jobAttempts = 0
    , jobMaxRetry = 3
    , jobStatus   = Pending
    , jobRunAt    = now
    }
  return jid

-- Worker
processJobs :: Map Text (Value -> IO ()) -> DB () -> IO ()
processJobs handlers runDB' = forever $ do
  mJob <- runDB' $ do
    mEntity <- selectFirst
      [JobStatus ==. Pending, JobRunAt <=. getCurrentTime]
      [Asc JobRunAt]
    case mEntity of
      Nothing -> return Nothing
      Just (Entity jid job) -> do
        update jid [JobStatus =. Running]
        return (Just (jid, job))
  
  case mJob of
    Nothing -> threadDelay 1000000  -- poll every 1s
    Just (jid, job) -> do
      result <- E.try $ case Map.lookup (jobType job) handlers of
        Nothing  -> E.throwIO (UnknownJobType (jobType job))
        Just h   -> h (jobPayload job)
      
      case result of
        Right () -> runDB' $ update jid [JobStatus =. Completed]
        Left (e :: SomeException) ->
          if jobAttempts job < jobMaxRetry job
            then runDB' $ update jid
              [ JobStatus   =. Pending
              , JobAttempts +=. 1
              , JobRunAt    =. addUTCTime 60 (jobRunAt job)  -- retry after 1min
              ]
            else runDB' $ update jid
              [JobStatus =. Failed (T.pack (show e))]
```

---

## ขั้นตอนที่ 538: Search API

```haskell
-- Full-text search API

-- Elasticsearch integration
import Database.Bloodhound

-- Index documents
indexUser :: ES.BHEnv -> User -> IO ()
indexUser env user = do
  let doc = ES.IndexDocument
        { ES.indexDocumentMappingName = ES.MappingName "user"
        , ES.indexDocumentId          = Just (ES.DocId (T.pack (show (userId user))))
        , ES.indexDocumentData        = toJSON user
        }
  ES.runBH env $ ES.indexDocument (ES.IndexName "users") doc

-- Search
searchUsers :: ES.BHEnv -> Text -> IO [User]
searchUsers env query = do
  let search = ES.Search
        { ES.queryBody    = Just (ES.QueryMatchQuery (ES.MatchQuery
            (ES.FieldName "name") (ES.QueryString query)
            ES.Or False Nothing Nothing Nothing Nothing Nothing))
        , ES.filterBody   = Nothing
        , ES.sortBody     = Nothing
        , ES.aggBody      = Nothing
        , ES.highlight    = Nothing
        , ES.trackSortScores = Nothing
        , ES.from         = ES.From 0
        , ES.size         = ES.Size 10
        , ES.searchType   = Nothing
        , ES.fields       = Nothing
        , ES.source       = Nothing
        , ES.suggestBody  = Nothing
        }
  
  reply <- ES.runBH env $
    ES.searchByIndex (ES.IndexName "users") search
  
  case ES.decodeEsResponse reply of
    Left err  -> throwIO (ESError err)
    Right hits ->
      return [fromMaybe mempty (ES.hitSource hit) | hit <- ES.hits (ES.searchHits hits)]

-- PostgreSQL full-text search (simpler)
searchUsersDB :: Text -> DB [User]
searchUsersDB query = do
  rawSql
    "SELECT ?? FROM users WHERE to_tsvector('english', name || ' ' || email) @@ plainto_tsquery('english', ?)"
    [PersistText query]
```

---

## ขั้นตอนที่ 539: Caching Layer

```haskell
-- Multi-level caching ใน Servant

-- Cache key generation
class CacheKey a where
  cacheKey :: a -> Text

instance CacheKey UserId where
  cacheKey uid = "user:" <> T.pack (show uid)

instance CacheKey (UserId, PostId) where
  cacheKey (uid, pid) = "post:" <> T.pack (show uid) <> ":" <> T.pack (show pid)

-- Cached query
cachedQuery :: (ToJSON a, FromJSON a, MonadIO m)
            => RedisConn -> Text -> Int -> m a -> m a
cachedQuery redis key ttl fetch = do
  mCached <- liftIO $ Redis.runRedis redis $ Redis.get (T.encodeUtf8 key)
  case mCached of
    Right (Just bs) ->
      case eitherDecodeStrict bs of
        Right val -> return val
        Left _    -> fetch >>= cacheResult
    _ -> fetch >>= cacheResult
  where
    cacheResult val = do
      liftIO $ Redis.runRedis redis $
        Redis.setex (T.encodeUtf8 key) (fromIntegral ttl) (BSL.toStrict (encode val))
      return val

-- Tag-based cache invalidation
invalidateUserCache :: MonadIO m => RedisConn -> UserId -> m ()
invalidateUserCache redis uid = liftIO $ do
  let pattern = T.encodeUtf8 $ "user:" <> T.pack (show uid) <> "*"
  Redis.runRedis redis $ do
    keys <- Redis.keys pattern
    case keys of
      Right ks -> mapM_ Redis.del [[k] | k <- ks]
      Left _   -> return ()

-- Cache warming
warmCache :: MonadIO m => RedisConn -> DB () -> m ()
warmCache redis runDB = liftIO $ do
  -- Warm top 1000 users
  users <- runDB $ selectList [] [Desc UserCreatedAt, LimitTo 1000]
  forM_ users $ \(Entity uid user) ->
    Redis.runRedis redis $
      Redis.setex (T.encodeUtf8 (cacheKey uid)) 3600 (BSL.toStrict (encode user))
```

---

## ขั้นตอนที่ 540: โปรเจกต์: Complete REST API

```haskell
-- Complete production-ready REST API

-- Types
newtype UserId = UserId Int64
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

data User = User
  { userId        :: UserId
  , userName      :: Text
  , userEmail     :: Text
  , userCreatedAt :: UTCTime
  , userActive    :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Full CRUD API
type UserAPI
  = "api" :> "v1" :> "users" :>
    (    Get '[JSON] (ApiResponse [User])
    :<|> ReqBody '[JSON] CreateUserReq :> Post '[JSON] (ApiResponse User)
    :<|> Capture "id" UserId :>
         (    Get '[JSON] (ApiResponse User)
         :<|> ReqBody '[JSON] UpdateUserReq :> Put '[JSON] (ApiResponse User)
         :<|> Delete '[JSON] (ApiResponse NoContent)
         )
    )

-- Server implementation
userServer :: AppEnv -> Server UserAPI
userServer env
  =    listUsersH env
  :<|> createUserH env
  :<|> \uid -> getUserH env uid
           :<|> updateUserH env uid
           :<|> deleteUserH env uid

listUsersH :: AppEnv -> Handler (ApiResponse [User])
listUsersH env = do
  users <- runDB env $ selectList [UserActive ==. True] [Asc UserId]
  return $ success (map entityVal users)

createUserH :: AppEnv -> CreateUserReq -> Handler (ApiResponse User)
createUserH env req = do
  case validateCreateUser req of
    Failure errs -> throwError err422 { errBody = encode errs }
    Success validReq -> do
      uid  <- runDB env $ insert (newUser validReq)
      user <- runDB env $ get404 uid
      return (success user)

getUserH :: AppEnv -> UserId -> Handler (ApiResponse User)
getUserH env uid = do
  user <- runDB env $ get404 (fromUserId uid)
  return (success user)

updateUserH :: AppEnv -> UserId -> UpdateUserReq -> Handler (ApiResponse User)
updateUserH env uid req = do
  runDB env $ update (fromUserId uid)
    [ UserName  =. fromMaybe (userId uid) (uurName req)
    , UserEmail =. fromMaybe (userEmail) (uurEmail req)
    ]
  user <- runDB env $ get404 (fromUserId uid)
  return (success user)

deleteUserH :: AppEnv -> UserId -> Handler (ApiResponse NoContent)
deleteUserH env uid = do
  runDB env $ update (fromUserId uid) [UserActive =. False]
  return (success NoContent)

-- Main application
main :: IO ()
main = do
  env <- initAppEnv
  let app = serve (Proxy :: Proxy UserAPI) (userServer env)
  let middleware = logger . cors . rateLimit
  run 8080 (middleware app)
```

---

*[← Part 26](part-26.md) | [Part 28 →](part-28.md)*
