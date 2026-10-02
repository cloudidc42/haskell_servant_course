# Part 16: Servant Advanced Features
## ขั้นตอนที่ 301-320: Authentication, Streaming, Versioning

---

## บทนำ

Servant มี features ขั้นสูงสำหรับ production-grade APIs ได้แก่ JWT authentication, streaming responses, API versioning, และ custom content types

---

## ขั้นตอนที่ 301: JWT Authentication

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auth where

import Servant
import Servant.Auth.Server
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Crypto.JWT

-- JWT Claims
data UserClaims = UserClaims
  { userId   :: Int
  , userRole :: Text
  , username :: Text
  } deriving (Show, Generic)

instance ToJSON UserClaims
instance FromJSON UserClaims
instance ToJWT UserClaims
instance FromJWT UserClaims

-- Protected API
type AuthAPI = 
  Auth '[JWT] UserClaims :> (
       "me" :> Get '[JSON] UserProfile
  :<|> "admin" :> AdminOnly :> Get '[JSON] AdminData
  )

-- Admin-only check
type AdminOnly = ()  -- simplified

-- Handlers
authServer :: JWTSettings -> Server AuthAPI
authServer jwtSettings (Authenticated claims) =
       getMe claims
  :<|> getAdminData claims
authServer _ _ = throwAll err401

getMe :: UserClaims -> Handler UserProfile
getMe claims = return (UserProfile (userId claims) (username claims))

getAdminData :: UserClaims -> Handler AdminData
getAdminData claims = do
  when (userRole claims /= "admin") $ throwError err403
  return (AdminData "Secret admin data")

-- Login endpoint
data LoginRequest = LoginRequest
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving (Generic, FromJSON)

data LoginResponse = LoginResponse
  { token :: Text
  } deriving (Generic, ToJSON)

type LoginAPI = "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

login :: JWTSettings -> LoginRequest -> Handler LoginResponse
login jwtSettings req = do
  -- Validate credentials
  mUser <- liftIO $ validateCredentials (loginEmail req) (loginPassword req)
  case mUser of
    Nothing -> throwError err401
    Just user -> do
      let claims = UserClaims (userId user) "user" (userName user)
      eToken <- liftIO $ makeJWT claims jwtSettings Nothing
      case eToken of
        Left  err -> throwError err500
        Right tok -> return $ LoginResponse (decodeUtf8 tok)

data UserProfile = UserProfile { upId :: Int, upName :: Text } deriving (Generic, ToJSON)
data AdminData = AdminData { adData :: Text } deriving (Generic, ToJSON)
data User' = User' { userId :: Int, userName :: Text }

validateCredentials :: Text -> Text -> IO (Maybe User')
validateCredentials email password = return (Just (User' 1 "Alice"))
```

---

## ขั้นตอนที่ 302: API Key Authentication

```haskell
-- Custom authentication ด้วย API Key

data APIKey = APIKey Text deriving (Show)

-- AuthHandler: validates authentication
apiKeyAuthHandler :: IORef (Set Text) -> AuthHandler Request APIKey
apiKeyAuthHandler validKeys = mkAuthHandler handler
  where
    handler :: Request -> Handler APIKey
    handler req = do
      case lookup "X-API-Key" (requestHeaders req) of
        Nothing  -> throwError err401 { errBody = "API key missing" }
        Just key -> do
          keys <- liftIO (readIORef validKeys)
          if Set.member (decodeUtf8 key) keys
            then return (APIKey (decodeUtf8 key))
            else throwError err401 { errBody = "Invalid API key" }

-- Context ที่ใช้กับ server
type APIKeyContext = '[AuthHandler Request APIKey]

apiKeyContext :: IORef (Set Text) -> Context APIKeyContext
apiKeyContext validKeys = apiKeyAuthHandler validKeys :. EmptyContext

-- API ที่ต้องการ API Key
type KeyProtectedAPI = AuthProtect "api-key" :> Get '[JSON] [Resource]

instance HasContextEntry APIKeyContext (AuthHandler Request APIKey) where
  getContextEntry (x :. _) = x

-- Server ที่ใช้ context
keyProtectedServer :: APIKey -> Handler [Resource]
keyProtectedServer key = return [Resource 1 "Item", Resource 2 "Item2"]

data Resource = Resource { rid :: Int, rname :: Text } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 303: Role-Based Access Control

```haskell
-- RBAC ใน Servant

data Role = User | Admin | SuperAdmin deriving (Show, Eq, Ord)

data AuthUser = AuthUser
  { authId   :: Int
  , authRole :: Role
  , authName :: Text
  }

-- Type-level role checking
data RequireRole (r :: Symbol)

-- หรือใช้ runtime check
requireRole :: Role -> AuthUser -> Handler ()
requireRole requiredRole user = do
  when (authRole user < requiredRole) $
    throwError err403 { errBody = encode (object ["error" .= ("Insufficient permissions" :: Text)]) }

-- ตัวอย่าง handlers ด้วย RBAC
type AdminAPI = Auth '[JWT] AuthUser :> 
  ( "users"  :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" Int :> Delete '[JSON] NoContent
  :<|> "config" :> Get '[JSON] SystemConfig
  )

adminServer :: Server AdminAPI
adminServer (Authenticated user) =
       listAllUsers user
  :<|> deleteUser user
  :<|> getSystemConfig user
adminServer _ = throwAll err401

listAllUsers :: AuthUser -> Handler [User]
listAllUsers user = do
  requireRole Admin user
  -- fetch all users
  return []

deleteUser :: AuthUser -> Int -> Handler NoContent
deleteUser user uid = do
  requireRole Admin user
  -- delete user
  return NoContent

getSystemConfig :: AuthUser -> Handler SystemConfig
getSystemConfig user = do
  requireRole SuperAdmin user
  return (SystemConfig 10 30)

data User = User { uid :: Int, uname :: Text } deriving (Generic, ToJSON)
data SystemConfig = SystemConfig { maxConn :: Int, timeout :: Int } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 304: Request Context

```haskell
-- หาข้อมูลจาก request headers

import Network.Wai

-- Request ID middleware
requestIdMiddleware :: Application -> Application
requestIdMiddleware app req sendResponse = do
  let reqId = lookup "X-Request-Id" (requestHeaders req)
  app (addRequestId reqId req) sendResponse
  where
    addRequestId Nothing  req = req
    addRequestId (Just i) req = req { requestHeaders = 
        ("X-Request-Id", i) : requestHeaders req }

-- Custom request data
data RequestContext = RequestContext
  { reqId      :: Maybe Text
  , reqUser    :: Maybe AuthUser
  , reqIpAddr  :: Text
  }

extractContext :: Request -> RequestContext
extractContext req = RequestContext
  { reqId     = fmap decodeUtf8 (lookup "X-Request-Id" (requestHeaders req))
  , reqUser   = Nothing  -- filled by auth middleware
  , reqIpAddr = T.pack $ show $ remoteHost req
  }

-- Thread-local storage ด้วย IORef
type ContextStore = IORef (Map Text RequestContext)

withContext :: ContextStore -> Application -> Application
withContext store app req sendResponse = do
  let ctx = extractContext req
  reqIdKey <- maybe (pure "unknown") pure (reqId ctx)
  modifyIORef store (Map.insert reqIdKey ctx)
  app req sendResponse
```

---

## ขั้นตอนที่ 305: Streaming Responses

```haskell
import Servant
import Servant.Types.SourceT

-- Server-Sent Events (SSE)
type SSEEndpoint = "events" :> StreamGet NewlineFraming JSON (SourceIO Event)

data Event = Event
  { eventType :: Text
  , eventData :: Value
  } deriving (Generic, ToJSON)

-- Stream events
streamEvents :: Handler (SourceIO Event)
streamEvents = return $ source generateEvents
  where
    generateEvents :: [Event]
    generateEvents = map makeEvent [1..10]
    makeEvent n = Event "update" (object ["count" .= n])

-- OutputStream: write events as they happen
streamFromIO :: IO a -> Handler (SourceIO a)
streamFromIO action = return $ SourceT $ \k -> do
  -- ทำงาน continuously
  loop k
  where
    loop k = do
      val <- action
      k (Yield val (Effect (loop k)))

-- WebSocket ด้วย servant-websockets
import Network.WebSockets
import Servant.API.WebSocket

type WSEndpoint = "ws" :> WebSocket

wsHandler :: Connection -> Handler ()
wsHandler conn = liftIO $ do
  -- Read messages
  sendTextData conn ("Hello from server!" :: Text)
  msg <- receiveData conn :: IO Text
  putStrLn $ "Received: " ++ T.unpack msg
```

---

## ขั้นตอนที่ 306: API Versioning

```haskell
-- Versioning strategies

-- 1. URL versioning: /api/v1, /api/v2
type V1API = "api" :> "v1" :> UserAPIV1
type V2API = "api" :> "v2" :> UserAPIV2

type UserAPIV1 = "users" :> Get '[JSON] [UserV1]
type UserAPIV2 = "users" :> Get '[JSON] [UserV2]  -- เพิ่ม fields

data UserV1 = UserV1 { id :: Int, name :: Text } deriving (Generic, ToJSON)
data UserV2 = UserV2 { id :: Int, name :: Text, email :: Text } deriving (Generic, ToJSON)

-- Combine versions
type VersionedAPI = V1API :<|> V2API

versionedServer :: Server VersionedAPI
versionedServer = v1Server :<|> v2Server

v1Server :: Server V1API
v1Server = return [UserV1 1 "Alice"]

v2Server :: Server V2API
v2Server = return [UserV2 1 "Alice" "alice@example.com"]

-- 2. Header versioning
type HeaderVersionedAPI = 
  Header "API-Version" Text :> "users" :> Get '[JSON] Value

headerVersionedServer :: Maybe Text -> Handler Value
headerVersionedServer mVersion = case mVersion of
  Just "v2" -> return (toJSON [UserV2 1 "Alice" "alice@example.com"])
  _         -> return (toJSON [UserV1 1 "Alice"])

-- 3. Content negotiation
-- ใช้ Content-Type header
type NegotiatedAPI = "users" :> Get '[JSON, XML] User
```

---

## ขั้นตอนที่ 307: Content Negotiation

```haskell
-- Custom content types ใน Servant

import Servant

-- ประกาศ content type
data CSV

instance Accept CSV where
  contentType _ = "text/csv"

instance ToJSON a => MimeRender CSV a where
  mimeRender _ val = 
    let js = decode (encode val) :: Maybe [[Value]]
    in LBS.pack (maybe "" toCsv js)
    where
      toCsv rows = unlines (map (intercalate "," . map show) rows)

-- API ที่รองรับ JSON และ CSV
type DataAPI = "export" :> Get '[JSON, CSV] [User]

-- Client จะได้รับ format ตาม Accept header:
-- Accept: application/json -> JSON
-- Accept: text/csv         -> CSV

-- XML content type
import Text.XML
import Data.XML.Types

data XML'

instance Accept XML' where
  contentType _ = "application/xml"

instance ToJSON a => MimeRender XML' a where
  mimeRender _ val = renderLBS def (Document prologue (fromJSON' val) [])
  where
    fromJSON' = undefined  -- convert JSON to XML Element
```

---

## ขั้นตอนที่ 308: Error Handling Patterns

```haskell
-- Structured error handling ใน Servant

-- Error types
data AppError
  = UserNotFound Int
  | DuplicateEmail Text
  | InvalidInput [ValidationError]
  | DatabaseError Text
  | AuthError AuthError
  deriving (Show)

data AuthError
  = InvalidToken
  | TokenExpired
  | InsufficientPermissions
  deriving (Show)

data ValidationError = ValidationError
  { field   :: Text
  , message :: Text
  } deriving (Show, Generic, ToJSON)

-- Convert to ServerError
toServerError :: AppError -> ServerError
toServerError (UserNotFound uid) = err404
  { errBody = encode (errorBody "NOT_FOUND" ("User " <> T.pack (show uid) <> " not found")) }
toServerError (DuplicateEmail email) = err409
  { errBody = encode (errorBody "DUPLICATE" ("Email already exists: " <> email)) }
toServerError (InvalidInput errs) = err422
  { errBody = encode (object 
      [ "code"   .= ("VALIDATION_ERROR" :: Text)
      , "errors" .= errs
      ]) }
toServerError (DatabaseError _) = err500
  { errBody = encode (errorBody "DB_ERROR" "Database error occurred") }
toServerError (AuthError InvalidToken) = err401
  { errBody = encode (errorBody "INVALID_TOKEN" "Invalid or missing token") }

errorBody :: Text -> Text -> Value
errorBody code msg = object ["code" .= code, "message" .= msg]

-- AppM monad ที่รวม error handling
type AppM = ExceptT AppError IO

runAppM :: AppM a -> IO (Either AppError a)
runAppM = runExceptT

-- Handler ที่ใช้ AppM
appMToHandler :: AppM a -> Handler a
appMToHandler action = do
  result <- liftIO (runAppM action)
  case result of
    Left err  -> throwError (toServerError err)
    Right val -> return val

-- Servant Hoisting
type UserAPI = "users" :> Get '[JSON] [User]

userServerM :: AppM [User]
userServerM = return []

hoistedServer :: Server UserAPI
hoistedServer = hoistServer (Proxy :: Proxy UserAPI) appMToHandler userServerM
```

---

## ขั้นตอนที่ 309: Request Logging

```haskell
-- Structured request logging

import Data.Aeson
import Data.Time

data RequestLog = RequestLog
  { rlMethod     :: Text
  , rlPath       :: Text
  , rlStatusCode :: Int
  , rlDuration   :: Double
  , rlUserId     :: Maybe Int
  , rlIpAddress  :: Text
  } deriving (Generic, ToJSON)

-- Logging middleware
loggingMiddleware :: Application -> Application
loggingMiddleware app req sendResponse = do
  start <- getCurrentTime
  app req $ \response -> do
    end <- getCurrentTime
    let duration = realToFrac (end `diffUTCTime` start) :: Double
    let log = RequestLog
          { rlMethod     = decodeUtf8 (requestMethod req)
          , rlPath       = decodeUtf8 (rawPathInfo req)
          , rlStatusCode = statusCode (responseStatus response)
          , rlDuration   = duration
          , rlUserId     = Nothing
          , rlIpAddress  = T.pack $ show $ remoteHost req
          }
    LBS.putStrLn (encode log)
    sendResponse response

-- ตัวอย่าง output:
-- {"method":"GET","path":"/api/users","statusCode":200,"duration":0.005,"userId":null}
```

---

## ขั้นตอนที่ 310: OpenAPI/Swagger Documentation

```haskell
-- Generate OpenAPI docs ด้วย servant-swagger2

import Data.Swagger
import Servant.Swagger
import Servant.Swagger.UI

-- Add descriptions
instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "A user in the system"
    & mapped.schema.example ?~ toJSON (User 1 "Alice" "alice@example.com" 30)

instance ToSchema CreateUser where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy

-- Generate swagger
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy UserAPI)
  & info.title   .~ "User API"
  & info.version .~ "1.0"
  & info.description ?~ "API for managing users"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.edu")

-- Swagger UI endpoint
type SwaggerUI = SwaggerSchemaUI "swagger-ui" "swagger.json"

type FullAPI = UserAPI :<|> SwaggerUI

fullServer :: Server FullAPI
fullServer = userServer :<|> swaggerSchemaUIServer swaggerDoc
  where userServer = undefined

-- หรือ ReDoc UI
-- type DocUI = "redoc" :> Raw
```

---

## ขั้นตอนที่ 311: Health Check Endpoint

```haskell
-- Health check สำหรับ Kubernetes และ monitoring

data HealthStatus = HealthStatus
  { status  :: Text
  , version :: Text
  , checks  :: Map Text CheckResult
  } deriving (Generic, ToJSON)

data CheckResult = CheckResult
  { checkStatus  :: Text
  , checkMessage :: Maybe Text
  } deriving (Generic, ToJSON)

type HealthAPI = 
       "health" :> Get '[JSON] HealthStatus
  :<|> "health" :> "live" :> Get '[JSON] HealthStatus
  :<|> "health" :> "ready" :> Get '[JSON] HealthStatus

-- Liveness: is the app running?
liveness :: Handler HealthStatus
liveness = return $ HealthStatus "ok" "1.0.0" Map.empty

-- Readiness: can the app serve traffic?
readiness :: DB -> Handler HealthStatus
readiness db = do
  dbStatus <- checkDatabase db
  let checks = Map.fromList [("database", dbStatus)]
  let overallStatus = if all ((== "ok") . checkStatus) (Map.elems checks)
        then "ok" else "degraded"
  return $ HealthStatus overallStatus "1.0.0" checks

checkDatabase :: DB -> Handler CheckResult
checkDatabase db = do
  result <- liftIO $ try $ testDbConnection db
  return $ case result of
    Left  err -> CheckResult "error" (Just (show err))
    Right _   -> CheckResult "ok" Nothing

testDbConnection :: DB -> IO ()
testDbConnection _ = return ()  -- mock

-- Startup probe
startup :: Handler HealthStatus
startup = return $ HealthStatus "ok" "1.0.0" Map.empty
```

---

## ขั้นตอนที่ 312: Graceful Shutdown

```haskell
-- Graceful shutdown ของ server

import Network.Wai.Handler.Warp (runSettings, Settings, setPort, setGracefulShutdownTimeout, setTimeout)
import System.Posix.Signals
import Control.Concurrent.MVar

-- Graceful shutdown ด้วย signal handling
runWithGracefulShutdown :: Application -> IO ()
runWithGracefulShutdown app = do
  shutdownMVar <- newEmptyMVar
  
  -- Install signal handler
  installHandler sigTERM (Catch $ putMVar shutdownMVar ()) Nothing
  installHandler sigINT  (Catch $ putMVar shutdownMVar ()) Nothing
  
  -- Run server in background
  serverThread <- async $ do
    let settings = setPort 8080
                 . setGracefulShutdownTimeout (Just 30)  -- 30 sec timeout
                 $ defaultSettings
    runSettings settings app
  
  -- Wait for shutdown signal
  takeMVar shutdownMVar
  putStrLn "Shutting down gracefully..."
  
  -- Cancel server (warp will drain existing connections)
  cancel serverThread
  putStrLn "Server stopped"

-- ด้วย SIGUSR1: reload config
reloadableServer :: IORef Config -> Application -> IO ()
reloadableServer configRef app = do
  installHandler sigUSR1 (Catch reloadConfig) Nothing
  run 8080 app
  where
    reloadConfig = do
      newConfig <- loadConfig
      writeIORef configRef newConfig
      putStrLn "Config reloaded"
```

---

## ขั้นตอนที่ 313: Caching

```haskell
-- HTTP caching headers

import Network.HTTP.Types
import Network.Wai

-- Add cache headers ด้วย middleware
cacheMiddleware :: Int -> Application -> Application
cacheMiddleware maxAge app req sendResponse =
  app req $ \response -> do
    let headers = responseHeaders response
        newHeaders = 
          [ ("Cache-Control", "public, max-age=" <> BSC.pack (show maxAge))
          , ("ETag", computeETag response)
          ] ++ headers
    sendResponse (mapResponseHeaders (const newHeaders) response)

computeETag :: Response -> BS.ByteString
computeETag _ = "\"abc123\""  -- simplified

-- ETag support
type ETagAPI = "resource" :> Header "If-None-Match" Text :> Get '[JSON] (Headers '[Header "ETag" Text] Resource)

resourceHandler :: Maybe Text -> Handler (Headers '[Header "ETag" Text] Resource)
resourceHandler mETag = do
  resource <- fetchResource
  let currentETag = computeResourceETag resource
  if mETag == Just currentETag
    then throwError err304  -- Not Modified
    else return (addHeader currentETag resource)

fetchResource :: Handler Resource
fetchResource = return (Resource 1 "item")

computeResourceETag :: Resource -> Text
computeResourceETag r = "\"" <> T.pack (show (rid r)) <> "\""

data Resource = Resource { rid :: Int, rname :: Text } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 314: File Upload

```haskell
-- File upload ใน Servant

import Servant.Multipart
import Network.Wai.Parse

-- Multipart form data
type UploadAPI = "upload" :> MultipartForm Mem FileUpload :> Post '[JSON] UploadResult

data FileUpload = FileUpload
  { uploadFile :: FileData Mem
  , uploadName :: Text
  }

instance FromMultipart Mem FileUpload where
  fromMultipart multipartData = do
    file <- lookupFile "file" multipartData
    name <- lookupInput "name" multipartData
    return (FileUpload file name)

data UploadResult = UploadResult
  { uploadId   :: Text
  , uploadSize :: Int
  } deriving (Generic, ToJSON)

handleUpload :: FileUpload -> Handler UploadResult
handleUpload upload = do
  let fileContent = fdPayload (uploadFile upload)
  let fileSize = BS.length fileContent
  let fileName = fdFileName (uploadFile upload)
  
  -- Save file
  liftIO $ BS.writeFile ("uploads/" ++ T.unpack fileName) fileContent
  
  return (UploadResult fileName fileSize)

-- Multiple files
type MultiUploadAPI = "upload-multiple" :> MultipartForm Mem [FileData Mem] :> Post '[JSON] [UploadResult]
```

---

## ขั้นตอนที่ 315: GraphQL ด้วย morpheus-graphql

```haskell
-- Morpheus GraphQL: GraphQL server ใน Haskell

{-# LANGUAGE TemplateHaskell #-}

import Data.Morpheus
import Data.Morpheus.Types

-- Define GraphQL schema
data Query m = Query
  { user  :: Arg "id"   Int  -> m (Maybe User')
  , users :: m [User']
  } deriving (Generic)

instance GQLType (Query m)

data User' = User'
  { userId'   :: Int
  , userName' :: Text
  , posts'    :: [Post']
  } deriving (Generic, GQLType)

data Post' = Post'
  { postId'    :: Int
  , postTitle' :: Text
  } deriving (Generic, GQLType)

-- Resolvers
resolveQuery :: Query IO
resolveQuery = Query
  { user  = \(Arg uid) -> fetchUser uid
  , users = fetchAllUsers
  }

fetchUser :: Int -> IO (Maybe User')
fetchUser uid = return (Just (User' uid "Alice" []))

fetchAllUsers :: IO [User']
fetchAllUsers = return [User' 1 "Alice" [], User' 2 "Bob" []]

-- Servant endpoint
type GraphQLAPI = "graphql" :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

graphqlHandler :: GQLRequest -> Handler GQLResponse
graphqlHandler req = liftIO $ interpreter resolveQuery req
```

---

## ขั้นตอนที่ 316: Metrics และ Monitoring

```haskell
-- Prometheus metrics ด้วย prometheus-client

import System.Metrics.Prometheus.Concurrent.Registry
import System.Metrics.Prometheus.Metric.Counter
import System.Metrics.Prometheus.Metric.Histogram
import System.Metrics.Prometheus.Http.Scrape

-- Define metrics
data Metrics = Metrics
  { requestsTotal    :: Counter
  , requestDuration  :: Histogram
  , activeRequests   :: Gauge
  , errorsTotal      :: Counter
  }

newMetrics :: Registry -> IO Metrics
newMetrics registry = Metrics
  <$> registerCounter "http_requests_total" mempty registry
  <*> registerHistogram "http_request_duration_seconds" [0.001, 0.01, 0.1, 1.0] mempty registry
  <*> registerGauge "http_active_requests" mempty registry
  <*> registerCounter "http_errors_total" mempty registry

-- Metrics middleware
metricsMiddleware :: Metrics -> Application -> Application
metricsMiddleware metrics app req sendResponse = do
  incGauge (activeRequests metrics)
  start <- getCurrentTime
  
  app req $ \response -> do
    end <- getCurrentTime
    let duration = realToFrac (end `diffUTCTime` start)
    
    incCounter (requestsTotal metrics)
    observe duration (requestDuration metrics)
    decGauge (activeRequests metrics)
    
    when (statusIsServerError (responseStatus response)) $
      incCounter (errorsTotal metrics)
    
    sendResponse response

-- Metrics endpoint
type MetricsAPI = "metrics" :> Raw

metricsHandler :: Registry -> Server MetricsAPI
metricsHandler registry = Tagged (scrapeMiddleware registry)
```

---

## ขั้นตอนที่ 317: Request Tracing

```haskell
-- Distributed tracing ด้วย OpenTelemetry

import OpenTelemetry.Trace
import OpenTelemetry.Context

-- Trace middleware
tracingMiddleware :: Tracer -> Application -> Application
tracingMiddleware tracer app req sendResponse = do
  let spanName = decodeUtf8 (requestMethod req) <> " " <> decodeUtf8 (rawPathInfo req)
  
  -- Extract trace context from headers
  let traceContext = extractTraceContext (requestHeaders req)
  
  inSpan tracer spanName defaultSpanArguments $ \span -> do
    -- Add attributes to span
    addAttribute span "http.method" (decodeUtf8 (requestMethod req))
    addAttribute span "http.path" (decodeUtf8 (rawPathInfo req))
    
    app req $ \response -> do
      addAttribute span "http.status_code" (statusCode (responseStatus response))
      sendResponse response

extractTraceContext :: RequestHeaders -> Maybe TraceContext
extractTraceContext headers = do
  traceparent <- lookup "traceparent" headers
  parseTraceparent (decodeUtf8 traceparent)

-- Span ใน handlers
tracedHandler :: Tracer -> Handler User
tracedHandler tracer = do
  liftIO $ inSpan tracer "fetch-user" defaultSpanArguments $ \span -> do
    addAttribute span "db.type" "postgresql"
    user <- fetchFromDB
    addAttribute span "user.id" (userId user)
    return user
  where
    fetchFromDB = return (User 1 "Alice" "alice@example.com" 30)
```

---

## ขั้นตอนที่ 318: Query String Handling

```haskell
-- Advanced query string handling

-- QueryParam: optional parameter
type SearchAPI = "search" 
  :> QueryParam  "q"       Text    -- optional search term
  :> QueryParam  "page"    Int     -- optional page number
  :> QueryParam  "limit"   Int     -- optional page size
  :> QueryParams "tags"    Text    -- multiple values
  :> QueryFlag   "verbose"         -- boolean flag
  :> Get '[JSON] SearchResult

data SearchResult = SearchResult
  { srItems :: [Item]
  , srTotal :: Int
  , srPage  :: Int
  } deriving (Generic, ToJSON)

searchHandler :: Maybe Text -> Maybe Int -> Maybe Int -> [Text] -> Bool -> Handler SearchResult
searchHandler mQuery mPage mLimit tags verbose = do
  let query  = fromMaybe "" mQuery
      page   = fromMaybe 1 mPage
      limit  = fromMaybe 10 mLimit
  
  when verbose $ liftIO $ do
    putStrLn $ "Searching: " ++ T.unpack query
    putStrLn $ "Tags: " ++ show tags
    putStrLn $ "Page: " ++ show page ++ ", Limit: " ++ show limit
  
  items <- liftIO (searchItems query tags)
  let pageItems = take limit (drop ((page-1) * limit) items)
  
  return SearchResult
    { srItems = pageItems
    , srTotal = length items
    , srPage  = page
    }

searchItems :: Text -> [Text] -> IO [Item]
searchItems _ _ = return []  -- mock

data Item = Item { iid :: Int, iname :: Text } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 319: Testing Strategies

```haskell
-- Comprehensive testing strategies สำหรับ Servant APIs

-- 1. Unit Tests (pure functions)
spec_validation :: Spec
spec_validation = describe "validateUser" $ do
  it "accepts valid user" $ do
    validateCreateUser (CreateUser "Alice" "alice@example.com" 30)
      `shouldBe` Right (CreateUser "Alice" "alice@example.com" 30)
  
  it "rejects empty name" $ do
    validateCreateUser (CreateUser "" "alice@example.com" 30)
      `shouldSatisfy` isLeft
  
  it "rejects invalid age" $ do
    validateCreateUser (CreateUser "Alice" "alice@example.com" (-1))
      `shouldSatisfy` isLeft

-- 2. Integration Tests (wai-extra)
spec_api :: Spec
spec_api = with (app <$> newDB) $ do
  describe "POST /users" $ do
    it "creates user and returns 201" $ do
      let body = [json| {"name": "Alice", "email": "alice@example.com", "age": 30} |]
      post "/users" body `shouldRespondWith` 201
    
    it "returns created user in body" $ do
      let body = [json| {"name": "Bob", "email": "bob@example.com", "age": 25} |]
      res <- post "/users" body
      liftIO $ do
        let mUser = decode (simpleBody res) :: Maybe User
        mUser `shouldSatisfy` isJust

-- 3. Property Tests
prop_userRoundTrip :: User -> Bool
prop_userRoundTrip user =
  decode (encode user) == Just user

-- 4. Contract Tests
-- ตรวจสอบว่า client และ server contract ตรงกัน
contractTest :: IO ()
contractTest = do
  let proxy = Proxy :: Proxy UserAPI
  putStrLn "API contract:"
  putStrLn $ show (typeOf proxy)
```

---

## ขั้นตอนที่ 320: โปรเจกต์: E-Commerce API

```haskell
-- E-Commerce API ที่สมบูรณ์

module EcommerceAPI where

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Servant
import Servant.Auth.Server
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.IORef
import qualified Data.Map.Strict as Map

-- Models
data Category = Category
  { catId   :: Int
  , catName :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Category
instance FromJSON Category

data Product = Product
  { pId       :: Int
  , pName     :: Text
  , pPrice    :: Double
  , pStock    :: Int
  , pCategory :: Int
  , pImages   :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON Product
instance FromJSON Product

data CartItem = CartItem
  { ciProductId :: Int
  , ciQuantity  :: Int
  , ciPrice     :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON CartItem
instance FromJSON CartItem

data Cart = Cart
  { cartId    :: Int
  , cartItems :: [CartItem]
  , cartTotal :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON Cart

data Order = Order
  { orderId      :: Int
  , orderUserId  :: Int
  , orderItems   :: [CartItem]
  , orderTotal   :: Double
  , orderStatus  :: Text
  , orderCreated :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Order

data AuthClaims = AuthClaims
  { acUserId :: Int
  , acEmail  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AuthClaims
instance FromJSON AuthClaims
instance ToJWT AuthClaims
instance FromJWT AuthClaims

-- API
type EcomAPI =
  "api" :> (
       "categories" :> Get '[JSON] [Category]
  :<|> "products" :> Get '[JSON] [Product]
  :<|> "products" :> Capture "id" Int :> Get '[JSON] Product
  :<|> "products" :> QueryParam "category" Int :> Get '[JSON] [Product]
  :<|> Auth '[JWT] AuthClaims :> AuthProtected
  )

type AuthProtected =
       "cart" :> Get '[JSON] Cart
  :<|> "cart" :> ReqBody '[JSON] CartItem :> Post '[JSON] Cart
  :<|> "cart" :> Capture "itemId" Int :> Delete '[JSON] Cart
  :<|> "orders" :> Get '[JSON] [Order]
  :<|> "orders" :> Post '[JSON] Order

-- Database
data EcomDB = EcomDB
  { dbCategories :: IORef (Map.Map Int Category)
  , dbProducts   :: IORef (Map.Map Int Product)
  , dbCarts      :: IORef (Map.Map Int Cart)
  , dbOrders     :: IORef (Map.Map Int Order)
  }

newEcomDB :: IO EcomDB
newEcomDB = do
  cats <- newIORef (Map.fromList [(1, Category 1 "Electronics"), (2, Category 2 "Books")])
  prods <- newIORef (Map.fromList 
    [ (1, Product 1 "Laptop" 999.99 10 1 ["laptop.jpg"])
    , (2, Product 2 "Phone"  499.99 20 1 ["phone.jpg"])
    , (3, Product 3 "Haskell Book" 49.99 100 2 ["book.jpg"])
    ])
  carts  <- newIORef Map.empty
  orders <- newIORef Map.empty
  return (EcomDB cats prods carts orders)

-- Server
ecomServer :: EcomDB -> Server EcomAPI
ecomServer db =
       getCategories
  :<|> getProducts
  :<|> getProductById
  :<|> getProductsByCategory
  :<|> protectedServer
  where
    getCategories = do
      m <- liftIO (readIORef (dbCategories db))
      return (Map.elems m)
    
    getProducts = do
      m <- liftIO (readIORef (dbProducts db))
      return (Map.elems m)
    
    getProductById pid = do
      m <- liftIO (readIORef (dbProducts db))
      maybe (throwError err404) return (Map.lookup pid m)
    
    getProductsByCategory mCat = do
      m <- liftIO (readIORef (dbProducts db))
      let prods = Map.elems m
      case mCat of
        Nothing  -> return prods
        Just cat -> return (filter (\p -> pCategory p == cat) prods)
    
    protectedServer (Authenticated claims) =
           getCart claims
      :<|> addToCart claims
      :<|> removeFromCart claims
      :<|> getUserOrders claims
      :<|> placeOrder claims
    protectedServer _ = throwAll err401
    
    getCart claims = do
      m <- liftIO (readIORef (dbCarts db))
      return $ Map.findWithDefault (Cart (acUserId claims) [] 0) (acUserId claims) m
    
    addToCart claims item = do
      m <- liftIO (readIORef (dbCarts db))
      let uid = acUserId claims
          oldCart = Map.findWithDefault (Cart uid [] 0) uid m
          newItems = item : cartItems oldCart
          newTotal = sum (map (\i -> ciPrice i * fromIntegral (ciQuantity i)) newItems)
          newCart = oldCart { cartItems = newItems, cartTotal = newTotal }
      liftIO $ modifyIORef (dbCarts db) (Map.insert uid newCart)
      return newCart
    
    removeFromCart claims itemIdx = do
      m <- liftIO (readIORef (dbCarts db))
      let uid = acUserId claims
          oldCart = Map.findWithDefault (Cart uid [] 0) uid m
          newItems = filter (\_ -> True) (cartItems oldCart)  -- simplified
          newTotal = sum (map (\i -> ciPrice i * fromIntegral (ciQuantity i)) newItems)
          newCart = oldCart { cartItems = newItems, cartTotal = newTotal }
      liftIO $ modifyIORef (dbCarts db) (Map.insert uid newCart)
      return newCart
    
    getUserOrders claims = do
      m <- liftIO (readIORef (dbOrders db))
      return (filter (\o -> orderUserId o == acUserId claims) (Map.elems m))
    
    placeOrder claims = do
      cart <- getCart claims
      now <- liftIO getCurrentTime
      m <- liftIO (readIORef (dbOrders db))
      let oid = if Map.null m then 1 else maximum (Map.keys m) + 1
          order = Order oid (acUserId claims) (cartItems cart) (cartTotal cart) "pending" now
      liftIO $ modifyIORef (dbOrders db) (Map.insert oid order)
      liftIO $ modifyIORef (dbCarts db) (Map.delete (acUserId claims))
      return order

-- Application
mkEcomApp :: IO Application
mkEcomApp = do
  db <- newEcomDB
  jwtKey <- generateKey
  let jwtSettings = defaultJWTSettings jwtKey
      cookieSettings = defaultCookieSettings
      ctx = cookieSettings :. jwtSettings :. EmptyContext
  return $ serveWithContext (Proxy :: Proxy EcomAPI) ctx (ecomServer db)

main :: IO ()
main = do
  app <- mkEcomApp
  putStrLn "E-Commerce API running on http://localhost:8080"
  run 8080 app
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 17 เราจะเรียนเรื่อง **Database Integration ขั้นสูง**:
- Persistent ORM
- Esqueleto DSL
- Database migrations
- Connection pooling

---

*[← Part 15](part-15.md) | [Part 17 →](part-17.md)*
