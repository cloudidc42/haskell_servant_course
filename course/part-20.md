# Part 20: Testing, Deployment และ Production Patterns
## ขั้นตอนที่ 381-400

---

## ขั้นตอนที่ 381: HSpec Testing

```haskell
-- HSpec: BDD-style testing framework

import Test.Hspec
import Test.Hspec.QuickCheck

-- ตัวอย่าง spec
spec :: Spec
spec = do
  describe "Math operations" $ do
    it "adds numbers" $ do
      (2 + 2 :: Int) `shouldBe` 4
    
    it "subtracts numbers" $ do
      (10 - 3 :: Int) `shouldBe` 7
    
    it "handles negative numbers" $ do
      (-1 + (-2) :: Int) `shouldBe` (-3)
  
  describe "String operations" $ do
    it "concatenates strings" $ do
      "hello" <> " " <> "world" `shouldBe` "hello world"
    
    it "reverses a string" $ do
      reverse "abcdef" `shouldBe` "fedcba"
  
  describe "List operations" $ do
    it "filters correctly" $ do
      filter even [1..10] `shouldBe` [2,4,6,8,10]
    
    it "maps correctly" $ do
      map (*2) [1..5] `shouldBe` [2,4,6,8,10]

-- Run tests
main :: IO ()
main = hspec spec
```

---

## ขั้นตอนที่ 382: QuickCheck Property Testing

```haskell
-- QuickCheck: property-based testing

import Test.QuickCheck
import Test.Hspec.QuickCheck

-- Properties
propReverseIdempotent :: [Int] -> Bool
propReverseIdempotent xs = reverse (reverse xs) == xs

propSortIdempotent :: [Int] -> Bool
propSortIdempotent xs = sort (sort xs) == sort xs

propMapLength :: [Int] -> Bool
propMapLength xs = length (map (*2) xs) == length xs

-- ใน spec
spec :: Spec
spec = do
  describe "QuickCheck properties" $ do
    prop "reverse . reverse = id" propReverseIdempotent
    prop "sort . sort = sort"     propSortIdempotent
    prop "map preserves length"   propMapLength
  
  describe "Custom generators" $ do
    it "generates valid emails" $ property $ do
      email <- arbitrary `suchThat` isValidEmail
      return $ isValidEmail email

-- Custom Arbitrary instances
newtype Email = Email { unEmail :: Text } deriving Show

instance Arbitrary Email where
  arbitrary = do
    user   <- listOf1 (elements (['a'..'z'] ++ ['0'..'9']))
    domain <- listOf1 (elements ['a'..'z'])
    tld    <- elements ["com", "org", "net", "io"]
    return $ Email $ T.pack $ user ++ "@" ++ domain ++ "." ++ tld

propEmailValid :: Email -> Bool
propEmailValid (Email e) = "@" `T.isInfixOf` e && "." `T.isInfixOf` e

-- Shrinking: ลด counterexample ให้เล็กที่สุด
newtype SmallList a = SmallList [a] deriving Show

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = SmallList <$> listOf (resize 10 arbitrary)
  shrink (SmallList xs) = SmallList <$> shrink xs
```

---

## ขั้นตอนที่ 383: Database Testing

```haskell
-- Database tests ด้วย in-memory SQLite

import Test.Hspec
import Database.Persist.Sqlite

runDbSpec :: SqlPersistM a -> IO a
runDbSpec action = runSqlite ":memory:" $ do
  runMigration migrateAll
  action

spec :: Spec
spec = do
  describe "User CRUD" $ do
    it "creates a user" $ do
      uid <- runDbSpec $ do
        insert $ User "Alice" "alice@example.com" 25
      uid `shouldBe` (toSqlKey 1)
    
    it "reads a user" $ do
      mUser <- runDbSpec $ do
        uid <- insert (User "Bob" "bob@example.com" 30)
        get uid
      fmap userName mUser `shouldBe` Just "Bob"
    
    it "updates a user" $ do
      updated <- runDbSpec $ do
        uid <- insert (User "Carol" "carol@example.com" 20)
        update uid [UserAge =. 21]
        get uid
      fmap userAge updated `shouldBe` Just 21
    
    it "deletes a user" $ do
      result <- runDbSpec $ do
        uid <- insert (User "Dave" "dave@example.com" 35)
        delete uid
        get uid :: SqlPersistM (Maybe User)
      result `shouldBe` Nothing
  
  describe "Queries" $ do
    it "finds users by age" $ do
      users <- runDbSpec $ do
        insert_ (User "User1" "u1@e.com" 20)
        insert_ (User "User2" "u2@e.com" 25)
        insert_ (User "User3" "u3@e.com" 30)
        selectList [UserAge >=. 25] [Asc UserName]
      length users `shouldBe` 2
```

---

## ขั้นตอนที่ 384: Integration Testing ด้วย Servant

```haskell
-- Integration testing สำหรับ Servant API

import Servant
import Servant.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Test.Hspec
import Test.Hspec.Wai

-- API spec
type TestAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User

testServer :: Server TestAPI
testServer = getUsers :<|> createUser
  where
    getUsers = return [User 1 "Alice" "alice@example.com"]
    createUser cu = return $ User 99 (cuName cu) (cuEmail cu)

testApp :: Application
testApp = serve (Proxy :: Proxy TestAPI) testServer

-- wai-extra integration tests
spec :: Spec
spec = with (return testApp) $ do
  describe "GET /users" $ do
    it "returns 200" $ do
      get "/users" `shouldRespondWith` 200
    
    it "returns JSON" $ do
      get "/users" `shouldRespondWith`
        200 { matchHeaders = ["Content-Type" <:> "application/json"] }
    
    it "returns users list" $ do
      get "/users" `shouldRespondWith`
        [json|[{"id": 1, "name": "Alice", "email": "alice@example.com"}]|]
  
  describe "POST /users" $ do
    it "creates a user" $ do
      post "/users"
        [json|{"name": "Bob", "email": "bob@example.com"}|]
        `shouldRespondWith` 200
    
    it "validates required fields" $ do
      post "/users"
        [json|{"name": "Bob"}|]
        `shouldRespondWith` 400

-- Servant client testing
clientSpec :: Spec
clientSpec = do
  describe "Client" $ do
    it "calls API" $ do
      mgr <- newManager defaultManagerSettings
      let url = BaseUrl Http "localhost" 8080 ""
      result <- runClientM getUsers (mkClientEnv mgr url)
      case result of
        Left err -> expectationFailure (show err)
        Right users -> length users `shouldBe` 1
```

---

## ขั้นตอนที่ 385: Docker Containerization

```dockerfile
# Dockerfile สำหรับ Haskell/Servant app

# Build stage
FROM haskell:9.4 AS builder

WORKDIR /app

# Copy dependency files first (for caching)
COPY *.cabal ./
COPY cabal.project ./

# Install dependencies
RUN cabal update && cabal build --only-dependencies -j4

# Copy source
COPY . .

# Build application
RUN cabal build -j4
RUN cp $(cabal list-bin myapp) /app/myapp

# Runtime stage
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libpq5 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from builder
COPY --from=builder /app/myapp .

# Copy static files
COPY static/ ./static/
COPY config.yaml ./

# Create non-root user
RUN useradd -r -s /bin/false appuser
USER appuser

EXPOSE 3000

CMD ["./myapp"]
```

```yaml
# docker-compose.yml
version: '3.8'

services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      - PORT=3000
      - DATABASE_URL=postgresql://postgres:password@db:5432/myapp
      - SECRET_KEY=${SECRET_KEY}
    depends_on:
      db:
        condition: service_healthy
    restart: unless-stopped
  
  db:
    image: postgres:15
    environment:
      POSTGRES_DB: myapp
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 5s
      timeout: 5s
      retries: 5
    restart: unless-stopped
  
  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./certs:/etc/nginx/certs
    depends_on:
      - app
    restart: unless-stopped

volumes:
  postgres_data:
```

---

## ขั้นตอนที่ 386: CI/CD Pipeline

```yaml
# .github/workflows/ci.yml

name: CI/CD

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: postgres
          POSTGRES_DB: testdb
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Haskell
        uses: haskell/actions/setup@v2
        with:
          ghc-version: '9.4.7'
          cabal-version: '3.10'
      
      - name: Cache Cabal
        uses: actions/cache@v3
        with:
          path: |
            ~/.cabal/store
            dist-newstyle
          key: ${{ runner.os }}-cabal-${{ hashFiles('**/*.cabal') }}
      
      - name: Install dependencies
        run: cabal build --only-dependencies -j4
      
      - name: Build
        run: cabal build -j4
      
      - name: Run tests
        env:
          DATABASE_URL: postgresql://postgres:postgres@localhost/testdb
        run: cabal test --test-show-details=always
      
      - name: Run HLint
        uses: haskell/actions/hlint-setup@v2
        with:
          version: '3.5'
      - run: hlint src/
  
  build-docker:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Build Docker image
        run: docker build -t myapp:${{ github.sha }} .
      
      - name: Push to registry
        run: |
          echo ${{ secrets.DOCKER_PASSWORD }} | docker login -u ${{ secrets.DOCKER_USERNAME }} --password-stdin
          docker tag myapp:${{ github.sha }} myregistry/myapp:latest
          docker push myregistry/myapp:latest
  
  deploy:
    needs: build-docker
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
      - name: Deploy to production
        uses: appleboy/ssh-action@master
        with:
          host: ${{ secrets.DEPLOY_HOST }}
          username: ${{ secrets.DEPLOY_USER }}
          key: ${{ secrets.DEPLOY_KEY }}
          script: |
            cd /app
            docker-compose pull
            docker-compose up -d
            docker-compose exec -T app ./run-migrations.sh
```

---

## ขั้นตอนที่ 387: Database Migrations

```haskell
-- Database migration management

import Database.Persist.Postgresql

-- Auto migration (Persistent)
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = do
  runSqlPool (runMigration migrateAll) pool

-- Safe migration (no data loss)
runSafeMigrations :: ConnectionPool -> IO ()
runSafeMigrations pool = do
  migrationPlan <- runSqlPool (getMigration migrateAll) pool
  
  -- Check for destructive operations
  let destructive = filter isDestructive migrationPlan
  unless (null destructive) $ do
    putStrLn "WARNING: Destructive migrations:"
    mapM_ (putStrLn . T.unpack) destructive
  
  -- Apply migration
  runSqlPool (runMigration migrateAll) pool

isDestructive :: Text -> Bool
isDestructive sql = any (`T.isPrefixOf` T.toUpper sql)
  ["DROP", "ALTER TABLE.*DROP", "ALTER COLUMN"]

-- Manual migrations with versioning
data Migration = Migration
  { migVersion :: Int
  , migName    :: Text
  , migUp      :: Text
  , migDown    :: Text
  }

migrations :: [Migration]
migrations =
  [ Migration 1 "create_users"
      "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT UNIQUE NOT NULL)"
      "DROP TABLE users"
  
  , Migration 2 "add_user_bio"
      "ALTER TABLE users ADD COLUMN bio TEXT"
      "ALTER TABLE users DROP COLUMN bio"
  
  , Migration 3 "create_posts"
      "CREATE TABLE posts (id SERIAL PRIMARY KEY, title TEXT NOT NULL, content TEXT, user_id INTEGER REFERENCES users(id))"
      "DROP TABLE posts"
  ]

applyMigrations :: Connection -> IO ()
applyMigrations conn = do
  -- Create migrations table
  execute_ conn "CREATE TABLE IF NOT EXISTS schema_migrations (version INTEGER PRIMARY KEY, applied_at TIMESTAMP DEFAULT NOW())"
  
  -- Get applied versions
  applied <- query_ conn "SELECT version FROM schema_migrations" :: IO [[Int]]
  let appliedVersions = S.fromList (map head applied)
  
  -- Apply pending migrations
  forM_ migrations $ \m -> do
    unless (migVersion m `S.member` appliedVersions) $ do
      putStrLn $ "Applying migration " ++ show (migVersion m) ++ ": " ++ T.unpack (migName m)
      execute_ conn (fromString (T.unpack (migUp m)))
      execute conn "INSERT INTO schema_migrations (version) VALUES (?)" (Only (migVersion m))
```

---

## ขั้นตอนที่ 388: Monitoring ด้วย Prometheus

```haskell
-- Prometheus metrics

import Prometheus

-- Define metrics
requestCounter :: Counter
requestCounter = unsafeRegister $ counter (Info "http_requests_total" "Total HTTP requests")

requestDuration :: Histogram
requestDuration = unsafeRegister $ histogram
  (Info "http_request_duration_seconds" "HTTP request duration")
  defaultBuckets

activeConnections :: Gauge
activeConnections = unsafeRegister $ gauge
  (Info "active_connections" "Active connections")

-- Metrics middleware
metricsMiddleware :: Middleware
metricsMiddleware app req respond = do
  let method = decodeUtf8 (requestMethod req)
      path   = "/" <> T.intercalate "/" (pathInfo req)
  
  incCounter requestCounter
  
  start <- getMonotonicTime
  incGauge activeConnections
  
  app req $ \res -> do
    end <- getMonotonicTime
    let duration = end - start
    observe requestDuration duration
    decGauge activeConnections
    respond res

-- Export metrics
getMetricsR :: Handler ()
getMetricsR = do
  metrics <- liftIO exportMetricsAsText
  sendResponse ("text/plain; version=0.0.4", toContent metrics)
```

---

## ขั้นตอนที่ 389: Structured Logging ด้วย JSON

```haskell
-- Structured logging ด้วย katip หรือ fast-logger

import Katip

-- Setup
initLogging :: IO LogEnv
initLogging = do
  handleScribe <- mkHandleScribeWithFormatter
    jsonFormat stdout (permitItem DebugS) V2
  
  le <- initLogEnv "MyApp" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings le

-- Log entry
data LogContext = LogContext
  { lcRequestId :: Maybe Text
  , lcUserId    :: Maybe Int
  } deriving (Generic, ToJSON)

instance ToObject LogContext
instance LogItem LogContext where
  payloadKeys _ _ = AllKeys

-- Logging
logRequest :: Request -> KatipContextT IO ()
logRequest req = do
  let path   = decodeUtf8 (rawPathInfo req)
      method = decodeUtf8 (requestMethod req)
  $(logTM) InfoS $ ls $ method <> " " <> path

-- สำหรับ Yesod
instance Yesod App where
  messageLoggerSource app _ loc src level msg = do
    let logLevel = case level of
          LevelDebug -> DebugS
          LevelInfo  -> InfoS
          LevelWarn  -> WarningS
          LevelError -> ErrorS
          LevelOther _ -> NoticeS
    
    runKatipT (appLogEnv app) $
      katipAddContext (sl "source" (show src) <> sl "location" (show loc)) $
        logMsg "yesod" logLevel (logStr msg)
```

---

## ขั้นตอนที่ 390: Production Configuration

```haskell
-- Production-ready configuration

-- Environment types
data Environment = Development | Testing | Production
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  env <- lookupEnv "APP_ENV"
  return $ case env of
    Just "production" -> Production
    Just "testing"    -> Testing
    _                 -> Development

-- Settings per environment
data AppSettings = AppSettings
  { asPort         :: Int
  , asDbPoolSize   :: Int
  , asLogLevel     :: LogLevel
  , asDebug        :: Bool
  , asSecureOnly   :: Bool
  , asSessionExpiry :: Int  -- seconds
  , asMaxFileSize   :: Int  -- bytes
  }

defaultSettings :: AppSettings
defaultSettings = AppSettings
  { asPort          = 3000
  , asDbPoolSize    = 10
  , asLogLevel      = LevelInfo
  , asDebug         = False
  , asSecureOnly    = True
  , asSessionExpiry = 3600
  , asMaxFileSize   = 10 * 1024 * 1024  -- 10MB
  }

developmentSettings :: AppSettings
developmentSettings = defaultSettings
  { asPort          = 3000
  , asDbPoolSize    = 2
  , asLogLevel      = LevelDebug
  , asDebug         = True
  , asSecureOnly    = False
  }

productionSettings :: AppSettings
productionSettings = defaultSettings
  { asPort          = 8080
  , asDbPoolSize    = 20
  , asLogLevel      = LevelWarn
  , asDebug         = False
  , asSecureOnly    = True
  }

getSettings :: IO AppSettings
getSettings = do
  env <- getEnvironment
  let base = case env of
        Production  -> productionSettings
        Testing     -> defaultSettings { asDbPoolSize = 2 }
        Development -> developmentSettings
  
  -- Override with env vars
  mPort <- lookupEnv "PORT"
  let port = maybe (asPort base) read mPort
  
  return base { asPort = port }
```

---

## ขั้นตอนที่ 391: Load Testing

```haskell
-- Load testing script ด้วย wreq

import Network.Wreq
import Control.Concurrent.Async

-- Concurrent requests
loadTest :: String -> Int -> Int -> IO ()
loadTest url concurrency totalRequests = do
  let batchSize = totalRequests `div` concurrency
  
  start <- getCurrentTime
  
  results <- mapConcurrently
    (\_ -> replicateM batchSize (doRequest url))
    [1..concurrency]
  
  end <- getCurrentTime
  
  let allResults = concat results
      successes  = length (filter (== 200) allResults)
      failures   = length allResults - successes
      duration   = diffUTCTime end start
      rps        = fromIntegral (length allResults) / realToFrac duration
  
  putStrLn $ "Total requests: " ++ show (length allResults)
  putStrLn $ "Successes: " ++ show successes
  putStrLn $ "Failures: " ++ show failures
  putStrLn $ "Duration: " ++ show duration
  putStrLn $ "Requests/sec: " ++ show (round rps :: Int)

doRequest :: String -> IO Int
doRequest url = do
  r <- get url
  return $ r ^. responseStatus . statusCode

main :: IO ()
main = loadTest "http://localhost:3000/api/posts" 10 1000
```

---

## ขั้นตอนที่ 392: Security Hardening

```haskell
-- Security best practices

import Yesod
import Network.Wai.Middleware.RequestSizeLimit
import Network.Wai.Middleware.Throttle

-- Security headers middleware
securityHeaders :: Middleware
securityHeaders app req respond = app req $ \res -> do
  let addHeaders = mapResponseHeaders
        ( ("X-Frame-Options", "DENY")
        : ("X-Content-Type-Options", "nosniff")
        : ("X-XSS-Protection", "1; mode=block")
        : ("Referrer-Policy", "strict-origin-when-cross-origin")
        : ("Permissions-Policy", "geolocation=(), microphone=()")
        : ("Content-Security-Policy", cspHeader)
        : )
  respond (addHeaders res)
  where
    cspHeader = "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'"

-- Request size limiting
maxRequestSize :: Middleware
maxRequestSize = requestSizeLimitMiddleware (1024 * 1024)  -- 1MB

-- Input sanitization
sanitizeHtml :: Text -> Text
sanitizeHtml = T.replace "<" "&lt;"
             . T.replace ">" "&gt;"
             . T.replace "\"" "&quot;"
             . T.replace "'" "&#x27;"
             . T.replace "/" "&#x2F;"

-- SQL injection prevention (use parameterized queries)
safeQuery :: Text -> Handler [Entity User]
safeQuery name = runDB $ selectList [UserName ==. name] []
-- ✓ Safe: Persistent uses parameterized queries

-- XSS prevention in templates
-- #{expr} in Hamlet escapes HTML automatically
-- [shamlet|#{userInput}|] -> safe
-- [shamlet|#{preEscapedText userInput}|] -> DANGEROUS, avoid

-- CSRF protection (built into Yesod)
-- defaultCsrfCheckMiddleware: checks POST/PUT/DELETE
-- csrfHiddenInput: adds hidden field to forms
-- setCsrfCookie: set CSRF cookie

-- Password hashing
import Crypto.BCrypt

hashPassword :: Text -> IO Text
hashPassword password = do
  mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy
    (encodeUtf8 password)
  case mHash of
    Nothing   -> error "Password hashing failed"
    Just hash -> return (decodeUtf8 hash)

verifyPassword :: Text -> Text -> Bool
verifyPassword password hash =
  validatePassword (encodeUtf8 hash) (encodeUtf8 password)
```

---

## ขั้นตอนที่ 393: Performance Optimization

```haskell
-- Performance optimization techniques

-- 1. Database query optimization
-- ใช้ selectList แทน การ selectAll ที่ไม่ได้ใช้ผล
-- ใช้ LimitTo เสมอสำหรับ pagination
-- ใช้ Index ในฐานข้อมูล

-- 2. Connection pooling
createPool :: IO ConnectionPool
createPool = runStderrLoggingT $ createPostgresqlPool
  "host=localhost dbname=myapp"
  20  -- pool size

-- 3. Caching ด้วย Redis
import Database.Redis

cacheGet :: Connection -> Text -> IO (Maybe Text)
cacheGet conn key = do
  result <- runRedis conn $ get (encodeUtf8 key)
  return $ case result of
    Right (Just bs) -> Just (decodeUtf8 bs)
    _               -> Nothing

cacheSet :: Connection -> Text -> Text -> Integer -> IO ()
cacheSet conn key val ttl = do
  runRedis conn $ setex (encodeUtf8 key) ttl (encodeUtf8 val)
  return ()

-- 4. Response compression
import Network.Wai.Middleware.Gzip

gzipMiddleware :: Middleware
gzipMiddleware = gzip def

-- 5. Static file caching
instance Yesod App where
  addStaticContent = addStaticContentExternal
    minifym
    genFileName
    "static/tmp"
    (StaticR . flip StaticRoute [])
  
  -- Browser caching headers
  jsLoader _ = BottomOfHeadBlocking
  makeSessionBackend _ = defaultClientSessionBackend
    120  -- minutes
    ".clientsession-key.aes"

-- 6. Lazy pagination with cursors
getCursorPaginatedR :: Handler Value
getCursorPaginatedR = do
  mCursor <- lookupGetParam "cursor"
  let limit = 20
  
  posts <- runDB $ case mCursor of
    Nothing -> selectList [PostPublished ==. True]
      [Desc PostId, LimitTo limit]
    Just c  -> do
      let cursorId = fromMaybe (toSqlKey 0) (fromPathPiece c)
      selectList [PostPublished ==. True, PostId <. cursorId]
        [Desc PostId, LimitTo limit]
  
  let nextCursor = case posts of
        [] -> Nothing
        _  -> Just $ toPathPiece (entityKey (last posts))
  
  return $ object
    [ "posts"  .= [entityVal p | p <- posts]
    , "cursor" .= nextCursor
    ]
```

---

## ขั้นตอนที่ 394: API Documentation

```haskell
-- Swagger/OpenAPI documentation

-- ด้วย servant-swagger
import Servant.Swagger
import Data.Swagger

-- Generate Swagger doc
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy API)
  & info . title   .~ "My API"
  & info . version .~ "1.0"
  & info . description ?~ "A sample Haskell API"
  & info . contact ?~ (mempty & name ?~ "Author" & email ?~ "author@example.com")
  & host ?~ "api.example.com"
  & schemes ?~ [Https]

-- Swagger UI endpoint
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
             :<|> "swagger-ui" :> Raw

swaggerServer :: Server SwaggerAPI
swaggerServer = return swaggerDoc
           :<|> serveDirectoryFileServer "swagger-ui/"

-- Add descriptions to API types
type UsersAPI =
  Summary "List all users"
    :> Description "Returns a paginated list of all users"
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> Get '[JSON] (PagedResponse User)

-- Operation ID for clients
type PostsAPI =
  OperationId "listPosts" :> "posts" :> Get '[JSON] [Post]
  :<|> OperationId "createPost" :> "posts" :> ReqBody '[JSON] CreatePost :> Post '[JSON] Post
```

---

## ขั้นตอนที่ 395: Real-time Features

```haskell
-- Server-Sent Events (SSE)

import Yesod
import Network.Wai.EventSource

-- SSE handler
getEventStreamR :: Handler ()
getEventStreamR = do
  chan <- appEventChannel <$> getYesod
  req <- waiRequest
  liftIO $ eventSourceAppChan chan req respond
  where
    respond res = return ()

-- Send events
broadcastEvent :: Text -> Text -> Handler ()
broadcastEvent eventType payload = do
  chan <- appEventChannel <$> getYesod
  let event = ServerEvent
        (Just (encodeUtf8 eventType))
        Nothing
        [encodeUtf8 payload]
  liftIO $ writeChan chan event

-- Client-side (JavaScript)
-- var es = new EventSource("/event-stream");
-- es.addEventListener("update", function(e) { ... });
-- es.onerror = function() { ... };

-- WebSocket with yesod-websockets
import Yesod.WebSockets

getLiveUpdatesR :: Handler ()
getLiveUpdatesR = webSockets liveUpdates

liveUpdates :: WebSocketsT Handler ()
liveUpdates = do
  app <- getYesod
  let chan = appLiveChannel app
  subChan <- liftIO $ atomically $ dupTChan chan
  
  forever $ do
    msg <- liftIO $ atomically $ readTChan subChan
    sendTextData msg
```

---

## ขั้นตอนที่ 396: GraphQL ด้วย Morpheus

```haskell
-- GraphQL API ด้วย morpheus-graphql

import Data.Morpheus
import Data.Morpheus.Types

-- Schema
data Query m = Query
  { user  :: GetUserArgs -> m (User m)
  , users :: m [User m]
  , posts :: m [Post m]
  }

data User m = User
  { userId    :: m Int
  , userName  :: m Text
  , userEmail :: m Text
  , userPosts :: m [Post m]
  }

data Post m = Post
  { postId      :: m Int
  , postTitle   :: m Text
  , postContent :: m Text
  , postAuthor  :: m (User m)
  }

data GetUserArgs = GetUserArgs
  { argId :: Int
  }

-- Resolvers
rootQuery :: Query (Resolver QUERY Event IO)
rootQuery = Query
  { user = resolveUser
  , users = resolveUsers
  , posts = resolvePosts
  }

resolveUser :: GetUserArgs -> Resolver QUERY Event IO (User (Resolver QUERY Event IO))
resolveUser args = do
  mUser <- liftIO $ getUser (argId args)
  case mUser of
    Nothing -> fail "User not found"
    Just u  -> return $ User
      { userId    = return (uId u)
      , userName  = return (uName u)
      , userEmail = return (uEmail u)
      , userPosts = liftIO $ getUserPosts (uId u)
      }

-- GraphQL endpoint ใน Servant
type GraphQLAPI = "graphql" :> ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse

graphqlHandler :: GQLRequest -> Handler GQLResponse
graphqlHandler = interpreter rootQuery
```

---

## ขั้นตอนที่ 397: Microservices Patterns

```haskell
-- Microservices ด้วย Servant

-- Service Discovery
data ServiceRegistry = ServiceRegistry
  { registryMap :: TVar (Map Text Text)  -- name -> url
  }

registerService :: ServiceRegistry -> Text -> Text -> IO ()
registerService reg name url = atomically $
  modifyTVar (registryMap reg) (Map.insert name url)

lookupService :: ServiceRegistry -> Text -> IO (Maybe Text)
lookupService reg name = do
  m <- readTVarIO (registryMap reg)
  return (Map.lookup name m)

-- Inter-service communication
callService :: Text -> Text -> IO (Either ClientError Value)
callService serviceUrl path = do
  mgr <- newManager defaultManagerSettings
  let baseUrl = parseBaseUrl (T.unpack serviceUrl)
  runClientM (getRoute path) (mkClientEnv mgr baseUrl)

-- Circuit breaker
data CircuitState = Closed | Open | HalfOpen
  deriving (Show, Eq)

data Circuit = Circuit
  { circuitState   :: TVar CircuitState
  , circuitFailures :: TVar Int
  , circuitLastFail :: TVar (Maybe UTCTime)
  }

callWithCircuit :: Circuit -> IO a -> IO (Either Text a)
callWithCircuit circuit action = do
  state <- readTVarIO (circuitState circuit)
  case state of
    Open -> do
      now <- getCurrentTime
      mLast <- readTVarIO (circuitLastFail circuit)
      case mLast of
        Just t | diffUTCTime now t < 60 ->
          return (Left "Circuit breaker open")
        _ -> do
          atomically $ writeTVar (circuitState circuit) HalfOpen
          tryAction
    _ -> tryAction
  where
    tryAction = do
      result <- E.try action
      case result of
        Left (e :: SomeException) -> do
          atomically $ do
            fails <- readTVar (circuitFailures circuit)
            let newFails = fails + 1
            writeTVar (circuitFailures circuit) newFails
            when (newFails >= 5) $
              writeTVar (circuitState circuit) Open
          return (Left (T.pack (show e)))
        Right v -> do
          atomically $ do
            writeTVar (circuitFailures circuit) 0
            writeTVar (circuitState circuit) Closed
          return (Right v)
```

---

## ขั้นตอนที่ 398: Message Queue Integration

```haskell
-- RabbitMQ ด้วย amqp

import Network.AMQP

-- Producer
publishMessage :: Connection -> Text -> Value -> IO ()
publishMessage conn queueName payload = do
  chan <- openChannel conn
  
  declareQueue chan newQueue
    { queueName    = T.unpack queueName
    , queueDurable = True
    }
  
  publishMsg chan ""
    (T.unpack queueName)
    newMsg
      { msgBody         = encode payload
      , msgDeliveryMode = Just Persistent
      , msgContentType  = Just "application/json"
      }
  
  closeChannel chan

-- Consumer
consumeMessages :: Connection -> Text -> (Value -> IO ()) -> IO ()
consumeMessages conn queueName handler = do
  chan <- openChannel conn
  
  declareQueue chan newQueue
    { queueName    = T.unpack queueName
    , queueDurable = True
    }
  
  consumeMsgs chan (T.unpack queueName) Ack $ \(msg, env) -> do
    case decode (msgBody msg) of
      Nothing      -> putStrLn "Failed to decode message"
      Just payload -> do
        handler payload
        ackEnv env

-- Event sourcing pattern
data DomainEvent = UserCreated Text Text
                 | UserUpdated Int Text
                 | PostPublished Int
                 deriving (Generic, ToJSON, FromJSON)

publishEvent :: Connection -> DomainEvent -> IO ()
publishEvent conn event = do
  let queueName = eventQueueName event
  publishMessage conn queueName (toJSON event)
  where
    eventQueueName (UserCreated _ _)   = "user.created"
    eventQueueName (UserUpdated _ _)   = "user.updated"
    eventQueueName (PostPublished _)   = "post.published"
```

---

## ขั้นตอนที่ 399: Advanced Error Handling

```haskell
-- Production error handling

import Control.Exception.Safe
import Data.Typeable

-- Custom exceptions
data AppError
  = DatabaseError Text
  | NotFoundError Text
  | ValidationError [Text]
  | AuthError Text
  | ExternalServiceError Text
  deriving (Show, Typeable, Exception)

-- Exception hierarchy
data DatabaseError' = ConnectionFailed Text
                   | QueryFailed Text
                   | TransactionFailed Text
  deriving (Show, Typeable, Exception)

-- Error handler
handleAppError :: AppError -> Handler TypedContent
handleAppError (NotFoundError msg) =
  sendResponseStatus status404 $ object ["error" .= msg]
handleAppError (ValidationError errs) =
  sendResponseStatus status400 $ object ["errors" .= errs]
handleAppError (AuthError msg) =
  sendResponseStatus status401 $ object ["error" .= msg]
handleAppError err =
  sendResponseStatus status500 $ object ["error" .= ("Internal error" :: Text)]

-- Use in handlers
getUserSafe :: UserId -> Handler (Entity User)
getUserSafe uid = do
  mUser <- runDB $ get uid
  case mUser of
    Nothing   -> throw (NotFoundError "User not found")
    Just user -> return (Entity uid user)

-- Global exception handler
instance Yesod App where
  errorHandler err = do
    logError $ "Error: " <> T.pack (show err)
    case err of
      NotFound -> fmap toTypedContent $ defaultLayout [whamlet|
        <h1>404 Not Found
      |]
      InternalError msg -> do
        -- Report to error tracking
        reportError msg
        fmap toTypedContent $ defaultLayout [whamlet|
          <h1>500 Internal Server Error
        |]
      _ -> defaultErrorHandler err
```

---

## ขั้นตอนที่ 400: โปรเจกต์: Production-Ready API Service

```haskell
-- Production-ready Servant API service

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Servant
import Servant.Auth.Server
import Database.Persist.Postgresql
import Control.Monad.Reader

-- Complete API type
type API = Auth '[JWT] UserClaims :> ProtectedAPI
      :<|> PublicAPI

type PublicAPI = "health" :> Get '[JSON] HealthStatus
           :<|> "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] TokenResponse
           :<|> "auth" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] User

type ProtectedAPI = "users" :> UsersAPI
               :<|> "posts" :> PostsAPI
               :<|> "profile" :> ProfileAPI

type UsersAPI = Get '[JSON] [User]
          :<|> Capture "id" UserId :> Get '[JSON] User
          :<|> Capture "id" UserId :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
          :<|> Capture "id" UserId :> Delete '[JSON] NoContent

type PostsAPI = QueryParam "page" Int
           :> QueryParam "per_page" Int
           :> Get '[JSON] (PagedResponse Post)
          :<|> Capture "id" PostId :> Get '[JSON] Post
          :<|> ReqBody '[JSON] CreatePost :> Post '[JSON] Post
          :<|> Capture "id" PostId :> ReqBody '[JSON] UpdatePost :> Put '[JSON] Post
          :<|> Capture "id" PostId :> Delete '[JSON] NoContent

type ProfileAPI = Get '[JSON] UserProfile
             :<|> ReqBody '[JSON] UpdateProfile :> Put '[JSON] UserProfile

-- Server environment
data AppEnv = AppEnv
  { envPool    :: ConnectionPool
  , envJwtKey  :: JWTSettings
  , envConfig  :: AppConfig
  , envLogger  :: Logger
  , envMetrics :: AppMetrics
  }

type AppM = ReaderT AppEnv Handler

-- Server implementation
server :: JWTSettings -> CookieSettings -> Server API
server jwts cs = protectedServer :<|> publicServer
  where
    publicServer = healthHandler
              :<|> loginHandler jwts
              :<|> registerHandler
    
    protectedServer (Authenticated claims) =
          usersServer claims
      :<|> postsServer claims
      :<|> profileServer claims
    protectedServer _ = throwAll err401

-- Health check
healthHandler :: Handler HealthStatus
healthHandler = return HealthStatus
  { hsStatus  = "ok"
  , hsVersion = "1.0.0"
  , hsUptime  = 0
  }

-- Login
loginHandler :: JWTSettings -> LoginRequest -> Handler TokenResponse
loginHandler jwts req = do
  mUser <- runDB $ getBy (UniqueEmail (lrEmail req))
  case mUser of
    Nothing -> throwError err401 { errBody = "Invalid credentials" }
    Just (Entity uid user) -> do
      valid <- liftIO $ verifyPassword (lrPassword req) (userPassword user)
      unless valid $ throwError err401 { errBody = "Invalid credentials" }
      
      let claims = UserClaims uid (userEmail user) (userRole user)
      eToken <- liftIO $ makeJWT claims jwts Nothing
      case eToken of
        Left _    -> throwError err500
        Right tok -> return TokenResponse
          { trToken     = decodeUtf8 (BSL.toStrict tok)
          , trUserId    = uid
          , trExpiresIn = 3600
          }

-- Users CRUD
usersServer :: UserClaims -> Server UsersAPI
usersServer claims = listUsers
                :<|> getUser'
                :<|> updateUser'
                :<|> deleteUser'
  where
    listUsers = do
      requireRole claims "admin"
      users <- runDB $ selectList [] [Asc UserId]
      return [entityVal u | u <- users]
    
    getUser' uid = do
      requireOwnerOrAdmin claims uid
      runDB (get404 uid)
    
    updateUser' uid body = do
      requireOwnerOrAdmin claims uid
      runDB $ do
        update uid [ UserName =. maybe undefined id (uuName body)
                   | isJust (uuName body) ]
        get404 uid
    
    deleteUser' uid = do
      requireAdmin claims
      runDB $ delete uid
      return NoContent

-- Main with all configuration
main :: IO ()
main = do
  config <- loadConfig
  
  -- Database
  pool <- runStderrLoggingT $ createPostgresqlPool
    (configDbUrl config) (configDbPool config)
  runSqlPool (runMigration migrateAll) pool
  
  -- JWT
  jwtKey <- generateKey
  let jwts = defaultJWTSettings jwtKey
      cs   = defaultCookieSettings
  
  -- Metrics
  metrics <- initMetrics
  
  -- Build WAI app
  let app = serveWithContext
        (Proxy :: Proxy API)
        (jwts :. cs :. EmptyContext)
        (server jwts cs)
  
  -- Apply middleware
  let finalApp = metricsMiddleware metrics
               . securityHeaders
               . gzip def
               . requestLogger
               $ app
  
  -- Start server
  let settings = setPort (configPort config) defaultSettings
  putStrLn $ "Server running on port " ++ show (configPort config)
  runSettings settings finalApp
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 21 เราจะเรียน **Advanced Haskell Patterns**:
- Free Monads
- Effect Systems (polysemy, fused-effects)
- Type-level Programming
- Template Haskell

---

*[← Part 19](part-19.md) | [Part 21 →](part-21.md)*
