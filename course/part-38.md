# Part 38: Real-World Haskell Applications
## ขั้นตอนที่ 741-760

---

## ขั้นตอนที่ 741: CLI Applications

```haskell
-- Command-line applications ด้วย optparse-applicative

import Options.Applicative

-- Command structure
data Command
  = ServerCmd ServerOptions
  | MigrateCmd MigrateOptions
  | SeedCmd SeedOptions
  | ExportCmd ExportOptions

data ServerOptions = ServerOptions
  { soPort    :: Int
  , soHost    :: Text
  , soWorkers :: Int
  , soLogLevel :: LogLevel
  }

data MigrateOptions = MigrateOptions
  { moDirection :: MigrateDirection
  , moSteps     :: Maybe Int
  }

-- Parser
commandParser :: Parser Command
commandParser = subparser
  ( command "server"  (info serverParser  (progDesc "Start the server"))
 <> command "migrate" (info migrateParser (progDesc "Run database migrations"))
 <> command "seed"    (info seedParser    (progDesc "Seed the database"))
 <> command "export"  (info exportParser  (progDesc "Export data"))
  )

serverParser :: Parser Command
serverParser = fmap ServerCmd $ ServerOptions
  <$> option auto
      ( long "port" <> short 'p'
     <> value 8080 <> metavar "PORT"
     <> help "Port to listen on" )
  <*> strOption
      ( long "host" <> short 'H'
     <> value "0.0.0.0" <> metavar "HOST"
     <> help "Host to bind to" )
  <*> option auto
      ( long "workers" <> short 'w'
     <> value 4 <> metavar "N"
     <> help "Number of worker threads" )
  <*> option auto
      ( long "log-level" <> value Info
     <> help "Logging level (debug, info, warn, error)" )

main :: IO ()
main = do
  cmd <- customExecParser (prefs showHelpOnError)
    (info (commandParser <**> helper)
      (fullDesc <> progDesc "My Haskell App" <> header "myapp - a Haskell application"))
  
  case cmd of
    ServerCmd opts  -> runServer opts
    MigrateCmd opts -> runMigrations opts
    SeedCmd opts    -> runSeed opts
    ExportCmd opts  -> runExport opts
```

---

## ขั้นตอนที่ 742: Streaming HTTP Client

```haskell
-- Streaming HTTP ด้วย http-conduit

import Network.HTTP.Conduit
import Conduit

-- Download large file with streaming
downloadFile :: Text -> FilePath -> IO Int64
downloadFile url destPath = do
  manager <- newTlsManager
  req     <- parseRequest (T.unpack url)
  
  withResponse req manager $ \resp -> do
    let body = responseBody resp
    
    runConduit $
      body
      .| sinkFileBS destPath

    let contentLen = responseContentLength resp
    return (maybe 0 fromIntegral contentLen)

-- Streaming JSON API
streamJsonLines :: Text -> (Value -> IO ()) -> IO ()
streamJsonLines url handler = do
  manager <- newTlsManager
  req     <- parseRequest (T.unpack url)
  
  withResponse req manager $ \resp ->
    runConduit $
      responseBody resp
      .| linesUnboundedAsciiC
      .| mapM_C (\line ->
           case eitherDecodeStrict line of
             Right v -> handler v
             Left _  -> return ())

-- Parallel downloads
downloadMany :: [(Text, FilePath)] -> Int -> IO [Either Text Int64]
downloadMany urls concurrency = do
  semaphore <- newQSem concurrency
  forConcurrently urls $ \(url, path) ->
    withQSem semaphore $
      E.try (downloadFile url path)
      >>= return . bimap (T.pack . show) id
```

---

## ขั้นตอนที่ 743: Email Service

```haskell
-- Email service integration

import Network.Mail.Mime
import Network.Mail.SMTP

-- Email configuration
data EmailConfig = EmailConfig
  { ecHost     :: String
  , ecPort     :: Int
  , ecUsername :: String
  , ecPassword :: String
  , ecFrom     :: Address
  , ecUseTls   :: Bool
  }

-- Build email
buildEmail :: Address -> [Address] -> Text -> Text -> Text -> Mail
buildEmail from recipients subject textBody htmlBody = Mail
  { mailFrom = from
  , mailTo   = recipients
  , mailCc   = []
  , mailBcc  = []
  , mailHeaders = [("Subject", subject)]
  , mailParts   = [ [ plainPart (TL.fromStrict textBody)
                    , htmlPart  (TL.fromStrict htmlBody)
                    ]
                  ]
  }

-- Send with retry
sendEmailWithRetry :: EmailConfig -> Mail -> IO ()
sendEmailWithRetry cfg mail = withRetry defaultRetry $ do
  if ecUseTls cfg
    then sendMailTLS (ecHost cfg) (ecPort cfg) 
                     (ecUsername cfg) (ecPassword cfg) mail
    else sendMail    (ecHost cfg) mail

-- Template emails using Shakespeare
welcomeEmailHtml :: Text -> Text -> Html
welcomeEmailHtml name link = [shamlet|
  <html>
    <body>
      <h1>Welcome, #{name}!
      <p>Thanks for signing up.
      <a href="#{link}">Verify your email
|]

-- Email queue (async)
data EmailQueue = EmailQueue
  { eqChan :: TChan QueuedEmail
  }

queueEmail :: EmailQueue -> QueuedEmail -> IO ()
queueEmail queue email = atomically $ writeTChan (eqChan queue) email

processEmailQueue :: EmailConfig -> EmailQueue -> IO ()
processEmailQueue cfg queue = forever $ do
  email <- atomically (readTChan (eqChan queue))
  result <- E.try (sendEmailWithRetry cfg (buildQueuedEmail email))
  case result of
    Left err -> logError ("Email failed: " <> T.pack (show err))
    Right () -> logInfo  ("Email sent: " <> qeSubject email)
```

---

## ขั้นตอนที่ 744: PDF Generation

```haskell
-- PDF generation ด้วย Cairo

import Graphics.Rendering.Cairo

-- Report structure
data ReportConfig = ReportConfig
  { rcTitle    :: Text
  , rcSubtitle :: Text
  , rcLogo     :: Maybe FilePath
  , rcPageSize :: (Double, Double)
  , rcMargins  :: (Double, Double, Double, Double)  -- top, right, bottom, left
  }

-- Generate PDF from data
generateReport :: ReportConfig -> [ReportSection] -> FilePath -> IO ()
generateReport cfg sections output = do
  withPDFSurface output width height $ \surface ->
    renderWith surface $ do
      forM_ (zip [1..] sections) $ \(pageNum, section) -> do
        save
        renderSection cfg section pageNum
        restore
        when (pageNum < length sections) showPage
  where
    (width, height) = rcPageSize cfg

renderSection :: ReportConfig -> ReportSection -> Int -> Render ()
renderSection cfg section pageNum = do
  -- Header
  setSourceRGBA 0.2 0.4 0.8 1.0
  rectangle 0 0 (fst (rcPageSize cfg)) 40
  fill
  
  -- Title
  selectFontFace "Sans" FontSlantNormal FontWeightBold
  setFontSize 16
  setSourceRGBA 1 1 1 1
  moveTo 20 28
  showText (T.unpack (rsTitle section))
  
  -- Content
  setSourceRGBA 0 0 0 1
  setFontSize 10
  moveTo 20 70
  renderContent (rsContent section)
  
  -- Footer
  renderFooter cfg pageNum

-- Table rendering
renderTable :: [(Text, Double)] -> [[(Text, Double)]] -> Render ()
renderTable headers rows = do
  let y0 = 0
  -- Draw header row
  drawRow headers y0 True
  -- Draw data rows
  forM_ (zip [1..] rows) $ \(i, row) ->
    drawRow row (y0 + fromIntegral i * rowHeight) False
  where rowHeight = 20
```

---

## ขั้นตอนที่ 745: Image Processing

```haskell
-- Image processing ด้วย JuicyPixels

import Codec.Picture
import Codec.Picture.Types

-- Resize image
resizeImage :: Int -> Int -> DynamicImage -> DynamicImage
resizeImage targetW targetH img =
  let w = dynamicMap imageWidth  img
      h = dynamicMap imageHeight img
  in dynamicPixelMap (scaleImage targetW targetH) img

scaleImage :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImage targetW targetH src = generateImage pixelAt targetW targetH
  where
    srcW = imageWidth  src
    srcH = imageHeight src
    pixelAt x y =
      let srcX = (x * srcW) `div` targetW
          srcY = (y * srcH) `div` targetH
      in pixelAt src srcX srcY

-- Thumbnail generation
makeThumbnail :: FilePath -> FilePath -> Int -> IO ()
makeThumbnail input output maxSize = do
  result <- readImage input
  case result of
    Left err  -> putStrLn ("Read error: " ++ err)
    Right img -> do
      let w = dynamicMap imageWidth  img
          h = dynamicMap imageHeight img
          (tw, th) = fitInBox maxSize maxSize w h
          thumb = resizeImage tw th img
      savePngImage output thumb

fitInBox :: Int -> Int -> Int -> Int -> (Int, Int)
fitInBox maxW maxH w h =
  let ratioW = fromIntegral maxW / fromIntegral w
      ratioH = fromIntegral maxH / fromIntegral h
      ratio  = min ratioW ratioH
  in (floor (fromIntegral w * ratio), floor (fromIntegral h * ratio))

-- Image optimization pipeline
processUploadedImage :: ByteString -> IO [(Text, ByteString)]
processUploadedImage raw = do
  case decodeImage raw of
    Left err  -> throwIO (InvalidImage err)
    Right img -> do
      let thumbnail = resizeImage 150 150 img
      let medium    = resizeImage 800 800 img
      let large     = resizeImage 1920 1920 img
      
      return
        [ ("thumb", encodePng thumbnail)
        , ("medium", encodePng medium)
        , ("large", encodePng large)
        ]
```

---

## ขั้นตอนที่ 746: OAuth 2.0 Client

```haskell
-- OAuth 2.0 client implementation

data OAuthClient = OAuthClient
  { oacClientId     :: Text
  , oacClientSecret :: Text
  , oacRedirectUri  :: Text
  , oacAuthUrl      :: Text
  , oacTokenUrl     :: Text
  , oacScopes       :: [Text]
  }

-- Generate authorization URL
authorizationUrl :: OAuthClient -> Text -> Text
authorizationUrl client state = mconcat
  [ oacAuthUrl client
  , "?client_id=", oacClientId client
  , "&redirect_uri=", urlEncode (oacRedirectUri client)
  , "&scope=", urlEncode (T.intercalate " " (oacScopes client))
  , "&state=", state
  , "&response_type=code"
  ]

-- Exchange code for tokens
exchangeCode :: OAuthClient -> Text -> IO OAuthTokens
exchangeCode client code = do
  let body = formUrlEncoded
        [ ("grant_type",    "authorization_code")
        , ("code",          code)
        , ("redirect_uri",  oacRedirectUri client)
        , ("client_id",     oacClientId client)
        , ("client_secret", oacClientSecret client)
        ]
  
  response <- httpJSON (postRequest (oacTokenUrl client) body)
  case fromJSON response of
    Success tokens -> return tokens
    Error err      -> throwIO (OAuthError err)

-- Refresh tokens
refreshToken :: OAuthClient -> Text -> IO OAuthTokens
refreshToken client refreshTok = do
  let body = formUrlEncoded
        [ ("grant_type",    "refresh_token")
        , ("refresh_token", refreshTok)
        , ("client_id",     oacClientId client)
        , ("client_secret", oacClientSecret client)
        ]
  
  response <- httpJSON (postRequest (oacTokenUrl client) body)
  case fromJSON response of
    Success tokens -> return tokens
    Error err      -> throwIO (OAuthError err)

-- Google OAuth
googleOAuth :: Text -> Text -> OAuthClient
googleOAuth clientId secret = OAuthClient
  { oacClientId     = clientId
  , oacClientSecret = secret
  , oacRedirectUri  = "http://localhost:8080/auth/google/callback"
  , oacAuthUrl      = "https://accounts.google.com/o/oauth2/auth"
  , oacTokenUrl     = "https://oauth2.googleapis.com/token"
  , oacScopes       = ["openid", "email", "profile"]
  }
```

---

## ขั้นตอนที่ 747: WebSocket Server

```haskell
-- WebSocket server ด้วย websockets library

import Network.WebSockets

-- Chat room
data ChatRoom = ChatRoom
  { crClients  :: TVar (Map ClientId Connection)
  , crMessages :: TVar [ChatMessage]
  }

data ChatMessage = ChatMessage
  { cmId       :: MessageId
  , cmUserId   :: UserId
  , cmContent  :: Text
  , cmSentAt   :: UTCTime
  }

-- Handle WebSocket client
handleWsClient :: ChatRoom -> UserId -> Connection -> IO ()
handleWsClient room uid conn = do
  clientId <- generateClientId
  
  -- Register client
  atomically $ modifyTVar (crClients room) (Map.insert clientId conn)
  
  -- Send history
  history <- readTVarIO (crMessages room)
  sendTextData conn (encode (HistoryMsg history))
  
  -- Message loop
  E.finally
    (messageLoop room uid clientId conn)
    (cleanup room clientId)

messageLoop :: ChatRoom -> UserId -> ClientId -> Connection -> IO ()
messageLoop room uid clientId conn = forever $ do
  msg <- receiveData conn
  
  case eitherDecode msg of
    Left err ->
      sendTextData conn (encode (ErrorMsg ("Invalid message: " <> T.pack err)))
    
    Right (SendMessage content) -> do
      now <- getCurrentTime
      msgId <- generateMessageId
      let chatMsg = ChatMessage msgId uid content now
      
      atomically $ modifyTVar (crMessages room) (chatMsg :)
      
      -- Broadcast to all clients
      clients <- readTVarIO (crClients room)
      forM_ (Map.elems clients) $ \c ->
        E.try @SomeException (sendTextData c (encode (NewMessage chatMsg)))
        >>= const (return ())

cleanup :: ChatRoom -> ClientId -> IO ()
cleanup room clientId =
  atomically $ modifyTVar (crClients room) (Map.delete clientId)
```

---

## ขั้นตอนที่ 748: GraphQL Schema

```haskell
-- GraphQL schema ด้วย morpheus-graphql

import Data.Morpheus
import Data.Morpheus.Types

-- Schema types
data User m = User
  { userId    :: m (ID "User")
  , userName  :: m Text
  , userEmail :: m Text
  , userPosts :: m [Post m]
  } deriving Generic

instance GQLType (User m)

data Post m = Post
  { postId      :: m (ID "Post")
  , postTitle   :: m Text
  , postContent :: m Text
  , postAuthor  :: m (User m)
  , postTags    :: m [Text]
  } deriving Generic

instance GQLType (Post m)

-- Query resolvers
data Query m = Query
  { user   :: GetUserArgs -> m (Maybe (User m))
  , users  :: ListUsersArgs -> m [User m]
  , post   :: GetPostArgs -> m (Maybe (Post m))
  , search :: SearchArgs -> m SearchResults
  } deriving Generic

instance GQLType (Query m)

-- Resolver implementation
resolveUser :: GetUserArgs -> ResolverQ e IO (Maybe (User (ResolverQ e IO)))
resolveUser args = do
  mUser <- liftIO (findUserById (guUserId args))
  return (fmap userToGql mUser)

userToGql :: UserRecord -> User (ResolverQ e IO)
userToGql record = User
  { userId    = pure (ID (T.pack (show (urId record))))
  , userName  = pure (urName record)
  , userEmail = pure (urEmail record)
  , userPosts = liftIO (findPostsByUser (urId record))
              >>= return . map postToGql
  }

-- Mutations
data Mutation m = Mutation
  { createUser  :: CreateUserInput -> m (User m)
  , updateUser  :: UpdateUserInput -> m (Maybe (User m))
  , createPost  :: CreatePostInput -> m (Post m)
  } deriving Generic
```

---

## ขั้นตอนที่ 749: Message Queue Integration

```haskell
-- RabbitMQ integration ด้วย amqp

import Network.AMQP

-- AMQP configuration
data AmqpConfig = AmqpConfig
  { acHost        :: Text
  , acPort        :: Int
  , acVhost       :: Text
  , acUsername    :: Text
  , acPassword    :: Text
  , acExchange    :: Text
  }

-- Publish message
publishMessage :: AmqpConfig -> Text -> Text -> ByteString -> IO ()
publishMessage cfg routingKey contentType body = do
  conn <- openConnection
    (T.unpack (acHost cfg))
    (acVhost cfg)
    (acUsername cfg)
    (acPassword cfg)
  
  ch <- openChannel conn
  
  let msg = newMsg
        { msgBody         = BSL.fromStrict body
        , msgContentType  = Just contentType
        , msgDeliveryMode = Just Persistent
        }
  
  publishMsg ch (acExchange cfg) routingKey msg
  
  closeChannel ch
  closeConnection conn

-- Consumer
consumeMessages :: AmqpConfig -> Text -> (Message -> IO AckMode) -> IO ()
consumeMessages cfg queueName handler = do
  conn <- openConnection (T.unpack (acHost cfg)) (acVhost cfg)
          (acUsername cfg) (acPassword cfg)
  ch <- openChannel conn
  
  -- Dead letter queue setup
  let dlxArgs = Map.fromList
        [ ("x-dead-letter-exchange", "deadletter")
        , ("x-message-ttl", 86400000)
        ]
  
  declareQueue ch newQueue { queueName = queueName, queueArguments = dlxArgs }
  
  consumeMsgs ch queueName Ack $ \(msg, env) -> do
    mode <- handler msg
    case mode of
      Ack -> ackEnv env
      Nack -> nackEnv env True  -- requeue

-- Message processing with retry
processWithRetry :: Message -> IO () -> IO AckMode
processWithRetry msg action = do
  let retries = fromMaybe 0 (getRetryCount msg)
  
  if retries > maxRetries
    then do
      logError ("Message exceeded max retries, sending to DLQ")
      return Nack
    else do
      result <- E.try @SomeException action
      case result of
        Right () -> return Ack
        Left err -> do
          logError ("Processing failed (retry " <> show (retries+1) <> "): " <> show err)
          return Nack
  where maxRetries = 3
```

---

## ขั้นตอนที่ 750: Scheduled Tasks

```haskell
-- Scheduled task system

import Control.Concurrent.Cron

data ScheduledTask = ScheduledTask
  { stName      :: Text
  , stSchedule  :: CronSchedule
  , stAction    :: IO ()
  , stTimeout   :: NominalDiffTime
  , stRetry     :: Bool
  }

-- Task scheduler
data Scheduler = Scheduler
  { schTasks    :: [ScheduledTask]
  , schRunning  :: TVar (Map Text Bool)
  , schHistory  :: TVar [TaskRun]
  }

data TaskRun = TaskRun
  { trTask      :: Text
  , trStartedAt :: UTCTime
  , trEndedAt   :: Maybe UTCTime
  , trStatus    :: TaskStatus
  , trError     :: Maybe Text
  }

-- Run scheduler
runScheduler :: Scheduler -> IO ()
runScheduler scheduler = do
  forkIO (cleanupLoop scheduler)
  
  forM_ (schTasks scheduler) $ \task ->
    scheduleTask scheduler task

scheduleTask :: Scheduler -> ScheduledTask -> IO ()
scheduleTask scheduler task = do
  schedule <- nextSchedule (stSchedule task)
  let delay = diffUTCTime schedule =<< getCurrentTime
  
  forkIO $ do
    threadDelay (floor delay * 1000000)
    runTask scheduler task
    scheduleTask scheduler task  -- reschedule

runTask :: Scheduler -> ScheduledTask -> IO ()
runTask scheduler task = do
  running <- Map.lookup (stName task) <$> readTVarIO (schRunning scheduler)
  
  when (fromMaybe False running) $ do
    logWarning ("Task already running, skipping: " <> stName task)
    return ()
  
  unless (fromMaybe False running) $ do
    now <- getCurrentTime
    atomically $ modifyTVar (schRunning scheduler) (Map.insert (stName task) True)
    
    result <- E.try @SomeException $
      withTimeout (floor (stTimeout task * 1e6)) (stAction task)
    
    end <- getCurrentTime
    let (status, err) = case result of
          Right ()     -> (Success, Nothing)
          Left e       -> (Failed, Just (T.pack (show e)))
    
    atomically $ do
      modifyTVar (schRunning scheduler) (Map.delete (stName task))
      modifyTVar (schHistory scheduler)
        (TaskRun (stName task) now (Just end) status err :)
```

---

## ขั้นตอนที่ 751: Full-Text Search

```haskell
-- Full-text search ด้วย PostgreSQL

-- Search query DSL
data SearchQuery
  = Terms    [Text]
  , Phrase   Text
  | And      SearchQuery SearchQuery
  | Or       SearchQuery SearchQuery
  | Exclude  SearchQuery
  | Boost    Double SearchQuery

-- Compile to PostgreSQL tsquery
toTsQuery :: SearchQuery -> Text
toTsQuery (Terms ts)    = T.intercalate " & " (map escapeToken ts)
toTsQuery (Phrase p)    = "'" <> p <> "'"
toTsQuery (And a b)     = "(" <> toTsQuery a <> ") & (" <> toTsQuery b <> ")"
toTsQuery (Or a b)      = "(" <> toTsQuery a <> ") | (" <> toTsQuery b <> ")"
toTsQuery (Exclude q)   = "!(" <> toTsQuery q <> ")"
toTsQuery (Boost w q)   = toTsQuery q  -- PostgreSQL handles boost differently

-- Search with highlighting
searchDocuments :: ConnectionPool -> SearchQuery -> SearchOptions -> IO [SearchResult]
searchDocuments pool query opts = runSqlPool (rawSql sql params) pool
  where
    tsq = toTsQuery query
    sql = T.unlines
      [ "SELECT id, title, content,"
      , "  ts_rank(search_vector, to_tsquery('english', ?)) as rank,"
      , "  ts_headline('english', content, to_tsquery('english', ?)) as excerpt"
      , "FROM documents"
      , "WHERE search_vector @@ to_tsquery('english', ?)"
      , "ORDER BY rank DESC"
      , "LIMIT ? OFFSET ?"
      ]
    params =
      [ PersistText tsq
      , PersistText tsq
      , PersistText tsq
      , PersistInt64 (fromIntegral (soLimit opts))
      , PersistInt64 (fromIntegral (soOffset opts))
      ]

-- Maintain search vector
updateSearchVector :: ConnectionPool -> DocumentId -> IO ()
updateSearchVector pool docId = runSqlPool update pool
  where
    update = rawExecute
      "UPDATE documents SET search_vector = to_tsvector('english', title || ' ' || content) WHERE id = ?"
      [PersistInt64 (fromIntegral docId)]
```

---

## ขั้นตอนที่ 752: File Storage

```haskell
-- File storage abstraction (S3, local)

class FileStorage m where
  upload    :: FilePath -> Text -> m FileKey
  download  :: FileKey -> FilePath -> m ()
  delete    :: FileKey -> m Bool
  exists    :: FileKey -> m Bool
  getUrl    :: FileKey -> m Text
  listFiles :: Text -> m [FileInfo]

-- S3 implementation
data S3Config = S3Config
  { s3Bucket :: Text
  , s3Region :: Text
  , s3Prefix :: Text
  }

instance FileStorage (ReaderT S3Config IO) where
  upload localPath key = do
    cfg <- ask
    let s3Key = s3Prefix cfg <> key
    liftIO $ uploadToS3 (s3Bucket cfg) s3Key localPath
    return s3Key
  
  download key localPath = do
    cfg <- ask
    liftIO $ downloadFromS3 (s3Bucket cfg) key localPath
  
  delete key = do
    cfg <- ask
    liftIO $ deleteFromS3 (s3Bucket cfg) key
  
  getUrl key = do
    cfg <- ask
    return $ "https://" <> s3Bucket cfg <> ".s3." <> s3Region cfg
           <> ".amazonaws.com/" <> key

-- Signed URL generation
generatePresignedUrl :: S3Config -> FileKey -> NominalDiffTime -> IO Text
generatePresignedUrl cfg key expiry = do
  now <- getCurrentTime
  let expireAt = addUTCTime expiry now
  -- Generate HMAC signature for presigned URL
  let signature = hmacSha256 (s3SecretKey cfg)
        (signaturePayload cfg key expireAt)
  return (buildPresignedUrl cfg key expireAt signature)

-- File processing pipeline
processUpload :: FileStorage m => ByteString -> Text -> m ProcessedFile
processUpload fileData filename = do
  -- Validate
  when (BS.length fileData > maxFileSize) $
    throwError "File too large"
  
  -- Generate unique key
  key <- generateFileKey filename
  
  -- Write to temp file
  tmpPath <- liftIO (writeTempFile fileData)
  
  -- Upload
  storageKey <- upload tmpPath key
  
  -- Cleanup
  liftIO (removeFile tmpPath)
  
  return ProcessedFile { pfKey = storageKey, pfOriginalName = filename }
```

---

## ขั้นตอนที่ 753: Multi-tenancy

```haskell
-- Multi-tenant application patterns

newtype TenantId = TenantId { getTenantId :: UUID }

data TenantContext = TenantContext
  { tcTenantId   :: TenantId
  , tcSchema     :: Text  -- PostgreSQL schema
  , tcConfig     :: TenantConfig
  }

-- Row-level security approach
withTenantFilter :: TenantId -> DB a -> DB a
withTenantFilter tid action = do
  setCurrentTenant tid
  result <- action
  clearCurrentTenant
  return result

setCurrentTenant :: TenantId -> DB ()
setCurrentTenant tid =
  rawExecute "SET app.current_tenant = ?" [PersistText (show tid)]

clearCurrentTenant :: DB ()
clearCurrentTenant = rawExecute "RESET app.current_tenant" []

-- Schema-per-tenant approach
runInSchema :: ConnectionPool -> Text -> DB a -> IO a
runInSchema pool schema action = runSqlPool (do
  rawExecute ("SET search_path TO " <> schema <> ", public") []
  result <- action
  rawExecute "SET search_path TO public" []
  return result) pool

-- Tenant routing middleware
tenantMiddleware :: TenantLookup -> Application -> Application
tenantMiddleware lookup app request respond = do
  hostname  <- getHostname request
  mTenant   <- lookup hostname
  
  case mTenant of
    Nothing     -> respond (responseLBS status404 [] "Tenant not found")
    Just tenant -> do
      let request' = addTenantToRequest tenant request
      app request' respond

-- Tenant-aware queries (via Context)
data AppCtx = AppCtx
  { ctxTenant :: TenantContext
  , ctxUser   :: Maybe AuthUser
  }

getUsers :: ReaderT AppCtx DB [User]
getUsers = do
  tid <- asks (tcTenantId . ctxTenant)
  lift $ selectList [UserTenantId ==. tid] [Asc UserId]
```

---

## ขั้นตอนที่ 754: API Versioning

```haskell
-- Comprehensive API versioning

-- URL versioning
type V1API = "v1" :> V1Routes
type V2API = "v2" :> V2Routes

type API = V1API :<|> V2API

-- Header versioning
type HeaderVersionAPI
  = Header "API-Version" Text :> VersionedRoutes

versionedHandler :: Maybe Text -> Server VersionedRoutes
versionedHandler mVersion = case fromMaybe "2" mVersion of
  "1" -> v1Handler
  "2" -> v2Handler
  v   -> throwError err400 { errBody = "Unknown API version: " <> encode v }

-- Content negotiation versioning
type ContentNegAPI
  = Header "Accept" Text :> UserRoutes

-- Deprecation headers
deprecatedMiddleware :: Application -> Application
deprecatedMiddleware app request respond =
  app request $ \response ->
    if isDeprecatedRoute request
    then respond $ addHeader
      ("Deprecation", "true")
      $ addHeader ("Sunset", "2025-01-01") response
    else respond response

-- Version compatibility matrix
data VersionCompat = VersionCompat
  { vcVersion    :: Text
  , vcEndpoints  :: Map Text EndpointInfo
  }

data EndpointInfo = EndpointInfo
  { eiAdded      :: Maybe Text
  , eiDeprecated :: Maybe Text
  , eiRemoved    :: Maybe Text
  }

checkEndpointVersion :: Text -> Text -> VersionCompat -> Bool
checkEndpointVersion version endpoint vc =
  case Map.lookup endpoint (vcEndpoints vc) of
    Nothing -> False
    Just info ->
      let added = maybe True (>= version) (eiAdded info)
          removed = maybe True (< version) (eiRemoved info)
      in added && removed
```

---

## ขั้นตอนที่ 755: API Documentation

```haskell
-- OpenAPI documentation generation

import Servant.OpenApi
import Data.OpenApi

-- Generate OpenAPI spec from Servant API
generateOpenApiSpec :: IO ()
generateOpenApiSpec = do
  let spec = toOpenApi (Proxy :: Proxy API)
  
  let spec' = spec
        & info . title     .~ "My API"
        & info . version   .~ "2.0.0"
        & info . description ?~ "API documentation"
        & servers .~ [Server "https://api.example.com" (Just "Production") mempty]
  
  BSL.writeFile "openapi.json" (encodePretty spec')
  BSL.writeFile "openapi.yaml" (encodeYaml spec')

-- Add documentation to routes
type DocumentedAPI
  = Summary "Get user by ID"
  :> Description "Returns the user with the given ID, or 404 if not found"
  :> Capture "id" UserId
  :> Get '[JSON] User

-- Response schemas
data ApiResponse a = ApiResponse
  { arData    :: a
  , arMeta    :: ResponseMeta
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResponseMeta = ResponseMeta
  { rmTotal       :: Maybe Int
  , rmPage        :: Maybe Int
  , rmPerPage     :: Maybe Int
  , rmHasMore     :: Maybe Bool
  } deriving (Generic, ToJSON, FromJSON, ToSchema)

-- Enum schemas
instance ToParamSchema SortDirection where
  toParamSchema _ = mempty
    & type_  ?~ OpenApiString
    & enum_  ?~ ["asc", "desc"]

-- Redoc/Swagger UI serving
swaggerUiServer :: Server SwaggerUI
swaggerUiServer = serveSwaggerUI swaggerUiIndexTemplate
```

---

## ขั้นตอนที่ 756: Search Engine Integration

```haskell
-- Elasticsearch integration

import Database.Bloodhound

-- Configure Elasticsearch
elasticConfig :: BHEnv
elasticConfig = mkBHEnv server manager
  where server  = Server "http://localhost:9200"
        manager = defaultManagerSettings

-- Index document
indexUser :: User -> IO ()
indexUser user = runBH elasticConfig $ do
  let index = IndexName "users"
  let doc   = DocId (T.pack (show (userId user)))
  indexDocument index MappingName defaultIndexDocumentSettings
    (userToDoc user) doc
  return ()

userToDoc :: User -> Value
userToDoc u = object
  [ "id"         .= userId u
  , "name"       .= userName u
  , "email"      .= userEmail u
  , "created_at" .= userCreatedAt u
  , "suggest"    .= object ["input" .= [userName u, userEmail u]]
  ]

-- Search
searchUsers :: Text -> IO [SearchHit User]
searchUsers query = runBH elasticConfig $ do
  let search = mkSearch
        (Just (QueryMultiMatchQuery (mkMultiMatchQuery [FieldName "name", FieldName "email"] query)))
        Nothing
  
  response <- searchByIndex (IndexName "users") search
  case response of
    Left err     -> throwIO (SearchError err)
    Right result -> return (hits (searchHits result))

-- Autocomplete with completion suggester
autocomplete :: Text -> IO [Text]
autocomplete prefix = runBH elasticConfig $ do
  let suggest = mkSuggestPayload "name_suggest"
        (mkCompletionSuggester (FieldName "suggest") prefix)
  
  response <- suggestQuery (IndexName "users") suggest
  case response of
    Left err -> throwIO (SearchError err)
    Right r  -> return (extractSuggestions r)
```

---

## ขั้นตอนที่ 757: Real-Time Notifications

```haskell
-- Server-Sent Events (SSE) for real-time updates

import Network.Wai.EventSource

-- Event types
data AppEvent
  = OrderStatusChanged   { osc :: OrderId, oscStatus :: OrderStatus }
  | NewMessage           { nm :: MessageId, nmContent :: Text }
  | SystemNotification   { sn :: NotificationPriority, snMsg :: Text }

-- Convert to SSE event
toSseEvent :: AppEvent -> ServerEvent
toSseEvent event = case event of
  OrderStatusChanged oid status ->
    ServerEvent (Just "order_status") Nothing
      [T.encodeUtf8 (encode (object ["order_id" .= oid, "status" .= status]))]
  
  NewMessage mid content ->
    ServerEvent (Just "new_message") Nothing
      [T.encodeUtf8 (encode (object ["id" .= mid, "content" .= content]))]
  
  SystemNotification priority msg ->
    ServerEvent (Just "notification") Nothing
      [T.encodeUtf8 (encode (object ["priority" .= priority, "message" .= msg]))]

-- SSE endpoint
sseHandler :: UserId -> App (EventSource)
sseHandler uid = do
  chan <- subscribe uid
  return (eventSourceAppChan chan)

-- Notification dispatcher
dispatchNotification :: NotificationBus -> UserId -> AppEvent -> IO ()
dispatchNotification bus uid event = do
  mChan <- getUserChannel bus uid
  case mChan of
    Nothing   -> return ()  -- user not connected
    Just chan  -> writeChan chan (toSseEvent event)
```

---

## ขั้นตอนที่ 758: Background Workers

```haskell
-- Background job processing

data Job = Job
  { jobId        :: JobId
  , jobType      :: Text
  , jobPayload   :: Value
  , jobStatus    :: JobStatus
  , jobRetries   :: Int
  , jobMaxRetries :: Int
  , jobScheduled :: UTCTime
  , jobCreatedAt :: UTCTime
  }

data JobStatus = Queued | Running | Completed | Failed | Scheduled

-- Job queue backed by Redis
enqueueJob :: Redis.Connection -> Job -> IO ()
enqueueJob conn job = Redis.runRedis conn $ do
  let key     = "jobs:queue:" <> jobType job
  let payload = BSL.toStrict (encode job)
  Redis.lpush key [payload]
  return ()

-- Worker loop
processJobs :: Redis.Connection -> Map Text (Job -> IO ()) -> IO ()
processJobs conn handlers = forever $ do
  job <- dequeueJob conn (Map.keys handlers)
  
  case job of
    Nothing  -> threadDelay 1000000  -- 1s poll
    Just j   -> forkIO $ processJob conn handlers j

processJob :: Redis.Connection -> Map Text (Job -> IO ()) -> Job -> IO ()
processJob conn handlers job = do
  updateJobStatus conn (jobId job) Running
  
  case Map.lookup (jobType job) handlers of
    Nothing -> updateJobStatus conn (jobId job) Failed
    Just handler -> do
      result <- E.try @SomeException (handler job)
      case result of
        Right () -> updateJobStatus conn (jobId job) Completed
        Left err ->
          if jobRetries job < jobMaxRetries job
          then requeue conn job
          else updateJobStatus conn (jobId job) Failed

-- Dead letter queue
moveToDlq :: Redis.Connection -> Job -> IO ()
moveToDlq conn job = Redis.runRedis conn $ do
  let key = "jobs:dlq"
  Redis.lpush key [BSL.toStrict (encode job)]
  return ()
```

---

## ขั้นตอนที่ 759: Data Migration

```haskell
-- Data migration tools

data Migration' = Migration'
  { migId     :: Int
  , migName   :: Text
  , migSql    :: [Text]
  , migRollback :: [Text]
  , migApplied :: Bool
  }

-- Migration runner with locking
runMigrations' :: ConnectionPool -> IO MigrationResult
runMigrations' pool = withDbLock pool "migrations" $ do
  applied <- getAppliedMigrations pool
  let pending = filter (\m -> migId m `notElem` applied) allMigrations
  
  if null pending
    then return NoMigrationsNeeded
    else do
      results <- forM pending $ \mig -> do
        putStrLn $ "Running migration " <> show (migId mig) <> ": " <> T.unpack (migName mig)
        E.try @SomeException (applyMigration pool mig) >>= \case
          Right () -> do
            recordMigration pool (migId mig)
            return (migId mig, True)
          Left err -> do
            putStrLn $ "Migration failed: " <> show err
            return (migId mig, False)
      
      return (MigrationsRan results)

applyMigration :: ConnectionPool -> Migration' -> IO ()
applyMigration pool mig = runSqlPool (mapM_ (\sql -> rawExecute sql []) (migSql mig)) pool

-- Rollback
rollbackMigration :: ConnectionPool -> Int -> IO ()
rollbackMigration pool targetId = do
  applied <- getAppliedMigrations pool
  let toRollback = filter (> targetId) applied
  forM_ (sort toRollback) $ \mid ->
    case find ((== mid) . migId) allMigrations of
      Nothing  -> putStrLn $ "Migration " <> show mid <> " not found"
      Just mig -> runSqlPool (mapM_ (\sql -> rawExecute sql []) (migRollback mig)) pool
```

---

## ขั้นตอนที่ 760: โปรเจกต์: SaaS Platform

```haskell
-- Complete SaaS platform architecture

-- Core components
data SaasPlatform = SaasPlatform
  { spTenantService  :: TenantService
  , spAuthService    :: AuthService
  , spBillingService :: BillingService
  , spEmailService   :: EmailService
  , spStorageService :: StorageService
  , spNotifications  :: NotificationService
  }

-- Tenant lifecycle
data TenantService = TenantService
  { tsCreate     :: CreateTenantReq -> IO Tenant
  , tsSuspend    :: TenantId -> IO ()
  , tsActivate   :: TenantId -> IO ()
  , tsDelete     :: TenantId -> IO ()
  , tsGetByHost  :: Text -> IO (Maybe Tenant)
  }

-- Billing
data BillingService = BillingService
  { bsCreateSubscription :: TenantId -> Plan -> IO Subscription
  , bsCancelSubscription :: TenantId -> IO ()
  , bsUpgrade            :: TenantId -> Plan -> IO ()
  , bsGetInvoices        :: TenantId -> IO [Invoice]
  }

-- Complete tenant onboarding flow
onboardTenant :: SaasPlatform -> OnboardingRequest -> IO OnboardingResult
onboardTenant platform req = do
  -- Create tenant
  tenant <- tsCreate (spTenantService platform) (toCreateTenantReq req)
  
  -- Create admin user
  admin  <- createAdminUser platform tenant (orAdminEmail req)
  
  -- Set up billing
  sub    <- bsCreateSubscription (spBillingService platform)
              (tenantId tenant) (orPlan req)
  
  -- Send welcome email
  sendWelcome (spEmailService platform) admin tenant
  
  -- Set up storage
  initStorage (spStorageService platform) (tenantId tenant)
  
  return OnboardingResult
    { orTenant   = tenant
    , orAdmin    = admin
    , orSubscription = sub
    }
```

---

*[← Part 37](part-37.md) | [Part 39 →](part-39.md)*
