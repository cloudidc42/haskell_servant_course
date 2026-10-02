# Part 28: Yesod Production Patterns
## ขั้นตอนที่ 541-560

---

## ขั้นตอนที่ 541: Yesod Application Architecture

```haskell
-- Production Yesod application structure

-- Foundation type
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appConnPool    :: ConnectionPool
  , appHttpManager :: Manager
  , appLogger      :: Logger
  , appRedis       :: RedisConn
  , appEmailConfig :: EmailConfig
  , appJobQueue    :: JobQueue
  , appCache       :: Cache
  }

-- Settings from environment
data AppSettings = AppSettings
  { appPort           :: Int
  , appDatabaseUrl    :: Text
  , appRedisUrl       :: Text
  , appSmtpHost       :: Text
  , appJwtSecret      :: ByteString
  , appEnv            :: AppEnvironment
  , appMaxUploadSize  :: Int64
  , appAllowedOrigins :: [Text]
  } deriving (Generic)

data AppEnvironment = Development | Testing | Staging | Production
  deriving (Show, Eq, Generic, FromJSON)

-- Load settings from environment
loadAppSettings :: IO AppSettings
loadAppSettings = do
  port  <- read <$> getEnv "PORT"
  dbUrl <- T.pack <$> getEnv "DATABASE_URL"
  env   <- read <$> getEnvDefault "APP_ENV" "Development"
  return AppSettings
    { appPort        = port
    , appDatabaseUrl = dbUrl
    , appEnv         = env
    , ...
    }

-- makeFoundation
makeFoundation :: AppSettings -> IO App
makeFoundation settings = do
  manager <- newManager tlsManagerSettings
  logger  <- newStderrLogger
  pool    <- runStderrLoggingT $ createPostgresqlPool
    (T.encodeUtf8 (appDatabaseUrl settings)) 20
  redis   <- Redis.connect (redisConnInfo settings)
  static  <- staticDevel "static"
  return App{..}
```

---

## ขั้นตอนที่ 542: Yesod Routes ขั้นสูง

```haskell
-- Advanced routing patterns

mkYesod "App" [parseRoutes|
/                      HomeR         GET
/auth                  AuthR         Auth appAuth

-- User management
/users                 UsersR        GET POST
/users/#UserId         UserR         GET PUT DELETE
/users/#UserId/profile UserProfileR  GET PUT
/users/#UserId/avatar  UserAvatarR   POST DELETE

-- Posts
/posts                 PostsR        GET POST
/posts/#PostId         PostR         GET PUT DELETE
/posts/#PostId/publish PostPublishR  POST
/posts/#PostId/like    PostLikeR     POST DELETE

-- Admin (requires admin role)
/admin                 AdminR        GET
/admin/users           AdminUsersR   GET
/admin/analytics       AdminAnalyticsR GET

-- API endpoints
/api/v1/+ApiRoute      ApiV1R        ApiV1

-- Static files
/static                StaticR       Static appStatic

-- WebSocket
/ws                    WsR           GET
|]

-- Subsystem routing
mkYesod "ApiV1" [parseRoutes|
/users                 ApiUsersR     GET POST
/users/#UserId         ApiUserR      GET PUT DELETE
|]
```

---

## ขั้นตอนที่ 543: Yesod Authentication ขั้นสูง

```haskell
-- Custom authentication plugin

data AppAuth = AppAuth

instance YesodAuth App where
  type AuthId App = UserId
  
  loginDest _     = HomeR
  logoutDest _    = HomeR
  authPlugins app = 
    [ authEmail
    , authGoogleEmail
    , authFacebook ["public_profile", "email"] oauthCreds
    ]
  
  authenticate creds = liftHandler $ runDB $ do
    let ident = credsIdent creds
    mUser <- getBy (UniqueEmail ident)
    case mUser of
      Just (Entity uid _) -> return (Authenticated uid)
      Nothing -> do
        uid <- insert User
          { userEmail     = ident
          , userName      = ident
          , userCreatedAt = getCurrentTime
          , userActive    = True
          }
        return (Authenticated uid)

-- Custom email authentication
instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  
  afterPasswordRoute _  = HomeR
  
  addUnverified email verkey = runDB $ insert User
    { userEmail   = email
    , userVerKey  = Just verkey
    , userVerified = False
    }
  
  sendVerifyEmail email verkey verurl = do
    sendEmail Email
      { emailTo      = [email]
      , emailSubject = "Verify your email"
      , emailBody    = TextPart ("Click: " <> getTextUrl verurl)
      }
  
  getVerifyKey uid = runDB $ do
    mUser <- get uid
    return (mUser >>= userVerKey)
  
  setVerifyKey uid key = runDB $
    update uid [UserVerKey =. Just key]
  
  verifyAccount uid = runDB $
    update uid [UserVerified =. True, UserVerKey =. Nothing]
  
  getPassword uid = runDB $ do
    mUser <- get uid
    return (mUser >>= userPassword)
  
  setPassword uid pass = runDB $
    update uid [UserPassword =. Just pass]
  
  getEmailCreds email = runDB $ do
    mUser <- getBy (UniqueEmail email)
    return $ fmap (\(Entity uid user) -> EmailCreds
      { emailCredsId     = uid
      , emailCredsAuthId = Just uid
      , emailCredsStatus = userVerified user
      , emailCredsVerkey = userVerKey user
      , emailCredsEmail  = email
      }) mUser
  
  getEmail uid = runDB $
    fmap (fmap userEmail) (get uid)
```

---

## ขั้นตอนที่ 544: Yesod Forms ขั้นสูง

```haskell
-- Complex forms ใน Yesod

-- Multi-step form
data ProfileStep1 = ProfileStep1
  { ps1Name   :: Text
  , ps1Email  :: Text
  , ps1Bio    :: Maybe Textarea
  }

data ProfileStep2 = ProfileStep2
  { ps2Avatar    :: Maybe FileInfo
  , ps2Website   :: Maybe Text
  , ps2Location  :: Maybe Text
  }

-- Step 1 form
profileStep1Form :: Maybe ProfileStep1 -> Form ProfileStep1
profileStep1Form mDefaults = renderDivs $ ProfileStep1
  <$> areq textField     (FieldSettings "Name"  Nothing (Just "name")  [] []) (ps1Name <$> mDefaults)
  <*> areq emailField    (FieldSettings "Email" Nothing (Just "email") [] []) (ps1Email <$> mDefaults)
  <*> aopt textareaField (FieldSettings "Bio"   Nothing (Just "bio")   [] []) (ps1Bio <$> mDefaults)

-- Dynamic form (add fields based on selection)
categoryForm :: Handler (Widget, Enctype)
categoryForm = do
  (formWidget, enctype) <- generateFormPost =<<
    renderDivs (buildForm <$> categorySelectField)
  return (formWidget, enctype)
  where
    buildForm category = case category of
      ProductCategory   -> productFields
      ServiceCategory   -> serviceFields
      _                 -> genericFields

-- Ajax-enhanced form submission
postContactR :: Handler Value
postContactR = do
  ((result, widget), enctype) <- runFormPost contactForm
  case result of
    FormSuccess contact -> do
      sendContactEmail contact
      returnJson (object ["success" .= True, "message" .= ("Message sent" :: Text)])
    FormFailure errors ->
      returnJson (object ["success" .= False, "errors" .= errors])
    FormMissing ->
      returnJson (object ["success" .= False, "errors" .= (["Form data missing"] :: [Text])])
```

---

## ขั้นตอนที่ 545: Yesod Persistent Queries

```haskell
-- Complex database queries ใน Yesod

-- Aggregation queries
getPostStats :: Handler Value
getPostStats = do
  stats <- runDB $ rawSql
    "SELECT category, COUNT(*), AVG(view_count) \
    \FROM posts \
    \WHERE published = ? AND created_at > ? \
    \GROUP BY category \
    \ORDER BY COUNT(*) DESC"
    [PersistBool True, PersistUTCTime (addUTCTime (-30*86400) now)]
  
  returnJson (map rowToJson stats)
  where
    rowToJson (Single cat, Single cnt, Single avg) =
      object ["category" .= cat, "count" .= (cnt :: Int), "avg_views" .= (avg :: Double)]

-- Full-text search
searchPosts :: Text -> Handler [Entity Post]
searchPosts query = runDB $ rawSql
  "SELECT ?? FROM posts \
  \WHERE to_tsvector('english', title || ' ' || content) @@ plainto_tsquery('english', ?) \
  \AND published = true \
  \ORDER BY ts_rank(to_tsvector('english', title || ' ' || content), plainto_tsquery('english', ?)) DESC \
  \LIMIT 20"
  [PersistText query, PersistText query]

-- Complex JOIN with Esqueleto
getUserFeed :: UserId -> Int -> DB [(Entity Post, Entity User)]
getUserFeed uid limit = E.select $ E.from $ \(post `E.InnerJoin` author) -> do
  E.on (post ^. PostAuthorId E.==. author ^. UserId)
  
  let following = E.subSelect $ E.from $ \follow -> do
        E.where_ (follow ^. FollowFollowerId E.==. E.val uid)
        return (follow ^. FollowFollowingId)
  
  E.where_ (post ^. PostAuthorId `E.in_` following)
  E.where_ (post ^. PostPublished E.==. E.val True)
  E.orderBy [E.desc (post ^. PostCreatedAt)]
  E.limit (fromIntegral limit)
  return (post, author)
```

---

## ขั้นตอนที่ 546: Yesod Widgets ขั้นสูง

```haskell
-- Reusable widgets

-- Navigation widget
navWidget :: Widget
navWidget = do
  mUser <- handlerToWidget maybeAuthId
  toWidget [hamlet|
    <nav .navbar>
      <div .container>
        <a .navbar-brand href=@{HomeR}>MyApp
        <div .navbar-menu>
          $maybe _ <- mUser
            <a href=@{ProfileR}>Profile
            <a href=@{LogoutR}>Logout
          $nothing
            <a href=@{LoginR}>Login
            <a href=@{SignupR}>Sign Up
  |]

-- Pagination widget
paginationWidget :: Int -> Int -> Route App -> Widget
paginationWidget currentPage totalPages baseRoute = do
  let pages = [1..totalPages]
  let prevPage = max 1 (currentPage - 1)
  let nextPage = min totalPages (currentPage + 1)
  toWidget [hamlet|
    <div .pagination>
      $if currentPage > 1
        <a href=@?{(baseRoute, [("page", T.pack (show prevPage))])}>Previous
      $forall page <- pages
        $if page == currentPage
          <span .current>#{page}
        $else
          <a href=@?{(baseRoute, [("page", T.pack (show page))])}>#{page}
      $if currentPage < totalPages
        <a href=@?{(baseRoute, [("page", T.pack (show nextPage))])}>Next
  |]
  toWidget [cassius|
    .pagination
      display: flex
      gap: 8px
      justify-content: center
      margin: 20px 0
    .pagination .current
      font-weight: bold
      color: #333
  |]

-- Flash message widget
flashWidget :: Widget
flashWidget = do
  mSuccess <- getMessage
  toWidget [hamlet|
    $maybe msg <- mSuccess
      <div .alert .alert-success>#{msg}
  |]
```

---

## ขั้นตอนที่ 547: Yesod API + Web Combined

```haskell
-- Serve both HTML and JSON from same routes

-- Accept-based content negotiation
getPostR :: PostId -> Handler TypedContent
getPostR pid = do
  post <- runDB $ get404 pid
  selectRep $ do
    provideRep $ do  -- HTML
      defaultLayout [whamlet|
        <h1>#{postTitle post}
        <p>#{postContent post}
      |]
    provideRep $     -- JSON
      return (toJSON post)

-- API subdomain routing
-- For /api/* routes, return JSON
-- For /* routes, return HTML
isApiRequest :: Request -> Bool
isApiRequest req =
  "application/json" `BS.isInfixOf` fromMaybe "" (lookup "Accept" (requestHeaders req))
  || "/api/" `BS.isPrefixOf` rawPathInfo req

-- Unified error handling
instance YesodDispatch App where
  yesodDispatch env req = do
    let isApi = isApiRequest req
    result <- E.try (yesodDispatchOld env req)
    case result of
      Right r -> return r
      Left (e :: SomeException) ->
        if isApi
          then return $ responseLBS status500 [("Content-Type", "application/json")]
            (encode (object ["error" .= show e]))
          else return $ responseLBS status500 [("Content-Type", "text/html")]
            "<h1>Internal Server Error</h1>"
```

---

## ขั้นตอนที่ 548: Yesod Middleware

```haskell
-- Custom middleware ใน Yesod

-- Request ID middleware
requestIdMiddleware :: Middleware
requestIdMiddleware app req respond = do
  reqId <- T.pack . show <$> randomIO @UUID
  let req' = req { requestHeaders =
        ("X-Request-ID", T.encodeUtf8 reqId) : requestHeaders req }
  app req' $ \resp -> respond
    (mapResponseHeaders (("X-Request-ID", T.encodeUtf8 reqId):) resp)

-- Security headers middleware
securityHeadersMiddleware :: Middleware
securityHeadersMiddleware app req respond =
  app req $ \resp -> respond $ mapResponseHeaders (++ securityHeaders) resp
  where
    securityHeaders =
      [ ("X-Frame-Options",           "DENY")
      , ("X-Content-Type-Options",    "nosniff")
      , ("X-XSS-Protection",          "1; mode=block")
      , ("Referrer-Policy",           "strict-origin-when-cross-origin")
      , ("Permissions-Policy",        "geolocation=(), microphone=()")
      , ("Content-Security-Policy",   defaultCSP)
      ]
    defaultCSP = "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'"

-- Maintenance mode middleware
maintenanceMiddleware :: TVar Bool -> Middleware
maintenanceMiddleware maintenanceMode app req respond = do
  inMaintenance <- readTVarIO maintenanceMode
  if inMaintenance && not (isAdminRequest req)
    then respond $ responseLBS status503
      [("Content-Type", "text/html"), ("Retry-After", "3600")]
      "<h1>Under Maintenance</h1><p>Back soon!</p>"
    else app req respond
```

---

## ขั้นตอนที่ 549: Email Templating

```haskell
-- Email templating ใน Yesod

-- Email template types
data EmailTemplate
  = WelcomeEmail
      { etUserName :: Text
      , etVerifyUrl :: Text
      }
  | PasswordResetEmail
      { etUserName :: Text
      , etResetUrl  :: Text
      , etExpiry    :: Text
      }
  | OrderConfirmationEmail
      { etOrderId   :: Text
      , etItems     :: [OrderItem]
      , etTotal     :: Text
      }

-- Render email ด้วย Shakespearean templates
renderEmailHtml :: EmailTemplate -> Html
renderEmailHtml template = case template of
  WelcomeEmail{..} -> $(hamletFile "templates/email/welcome.hamlet") ()
  PasswordResetEmail{..} -> $(hamletFile "templates/email/reset.hamlet") ()
  OrderConfirmationEmail{..} -> $(hamletFile "templates/email/order.hamlet") ()

-- templates/email/welcome.hamlet:
-- <html>
--   <body>
--     <h1>Welcome #{etUserName}!
--     <p>Please verify your email:
--     <a href=#{etVerifyUrl}>Verify Email

-- Send email
sendEmail :: EmailTemplate -> Text -> Handler ()
sendEmail template toEmail = do
  settings <- appEmailConfig <$> getYesod
  let html = renderEmailHtml template
      text = renderEmailText template
      subj = emailSubject template
  liftIO $ sendMail settings SMTPMail
    { smFrom    = "noreply@myapp.com"
    , smTo      = [toEmail]
    , smSubject = subj
    , smHtml    = Just (renderHtml html)
    , smText    = Just text
    }

-- Rate-limit emails
sendEmailRateLimited :: Text -> EmailTemplate -> Text -> Handler ()
sendEmailRateLimited key template toEmail = do
  redis <- appRedis <$> getYesod
  let rateKey = "email_rate:" <> key <> ":" <> toEmail
  sent <- liftIO $ Redis.runRedis redis $ do
    count <- Redis.incr (T.encodeUtf8 rateKey)
    case count of
      Right 1 -> do
        Redis.expire (T.encodeUtf8 rateKey) 3600  -- 1 hour window
        return True
      Right n | n <= 3 -> return True  -- max 3 emails per hour
      _ -> return False
  when sent $ sendEmail template toEmail
```

---

## ขั้นตอนที่ 550: Yesod Testing

```haskell
-- Comprehensive testing ใน Yesod

import Yesod.Test
import Test.HSpec

-- Test spec
spec :: Spec
spec = withApp $ do
  describe "Auth" $ do
    it "allows login with valid credentials" $ do
      createUser "test@example.com" "password123"
      get LoginR
      statusIs 200
      request $ do
        setMethod "POST"
        setUrl LoginR
        addPostParam "email"    "test@example.com"
        addPostParam "password" "password123"
      statusIs 303  -- redirect after login
      locationSatisfies ("http://localhost/" `T.isPrefixOf`)
    
    it "rejects invalid password" $ do
      createUser "test@example.com" "password123"
      request $ do
        setMethod "POST"
        setUrl LoginR
        addPostParam "email"    "test@example.com"
        addPostParam "password" "wrong"
      statusIs 403
  
  describe "Posts" $ do
    it "returns 401 for unauthenticated post creation" $ do
      request $ do
        setMethod "POST"
        setUrl PostsR
        setRequestBody (encode createPostReq)
        addRequestHeader "Content-Type" "application/json"
      statusIs 401
    
    it "creates post for authenticated user" $ do
      uid <- createAndLoginUser
      request $ do
        setMethod "POST"
        setUrl PostsR
        setRequestBody (encode createPostReq)
        addRequestHeader "Content-Type" "application/json"
      statusIs 201
      respBody <- getResponseBody
      case decode respBody of
        Nothing   -> fail "Invalid JSON response"
        Just post -> do
          liftIO (postTitle post `shouldBe` "Test Post")
          liftIO (postAuthorId post `shouldBe` uid)

-- Test helper
withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <- loadAppSettings
  foundation <- makeFoundation settings
  return (foundation, id)

createUser :: Text -> Text -> YesodExample App ()
createUser email password = do
  runDB $ insert_ User
    { userEmail    = email
    , userPassword = hashPassword password
    , userVerified = True
    }
```

---

## ขั้นตอนที่ 551: Deployment Configuration

```haskell
-- Production deployment configuration

-- Warp settings
warpSettings :: AppSettings -> Settings
warpSettings settings = setPort (appPort settings)
  $ setHost (fromString "0.0.0.0")
  $ setBeforeMainLoop (putStrLn "Server started")
  $ setGracefulShutdownTimeout (Just 30)
  $ setOnException (\_ e ->
      when (defaultShouldDisplayException e) $
        putStrLn $ "Exception: " ++ show e)
  defaultSettings

-- TLS settings for HTTPS
tlsSettings :: AppSettings -> TLSSettings
tlsSettings settings = tlsSettingsChain
  (appTlsCert settings)
  [appTlsChain settings]
  (appTlsKey settings)

-- Main entry point
main :: IO ()
main = do
  settings <- loadAppSettings
  
  -- Validate critical settings
  unless (appJwtSecret settings /= "") $
    die "JWT_SECRET environment variable required"
  
  app <- makeFoundation settings
  
  -- Run DB migrations
  runSqlPool doMigration (appConnPool app)
  
  -- Start background workers
  mapM_ (forkIO . runWorker app)
    [ emailWorker
    , jobWorker
    , cacheWarmer
    ]
  
  -- Start server
  let warpSets = warpSettings settings
  putStrLn $ "Starting server on port " ++ show (appPort settings)
  
  case appEnv settings of
    Production -> runTLS (tlsSettings settings) warpSets (toWaiApp app)
    _          -> runSettings warpSets (toWaiApp app)
```

---

## ขั้นตอนที่ 552: i18n ขั้นสูง

```haskell
-- Internationalization ขั้นสูง

-- Message definitions (messages/en.msg)
-- HomePage: Hello World
-- UserGreeting name@Text: Hello #{name}!
-- ItemCount count@Int: $2{count} item(s)

-- messages/th.msg  
-- HomePage: สวัสดีชาวโลก
-- UserGreeting name@Text: สวัสดี #{name}!
-- ItemCount 1: 1 รายการ
-- ItemCount count@Int: #{count} รายการ

-- Auto-generated types
data AppMessage
  = MsgHomePage
  | MsgUserGreeting Text
  | MsgItemCount Int

-- Use in templates
getHomeR :: Handler Html
getHomeR = do
  mUser <- maybeAuthId
  defaultLayout [whamlet|
    <h1>_{MsgHomePage}
    $maybe uid <- mUser
      <p>_{MsgUserGreeting "User"}
    $nothing
      <p>Please log in
  |]

-- Language detection
instance YesodJsMessages App where
  jsMessagesFile = "static/messages"
  
instance YesodI18nMessages App where
  messageMessageLogger = messageLoggerSource

-- Browser language detection
getPreferredLang :: Handler Lang
getPreferredLang = do
  mLang <- lookupCookie "lang"
  case mLang of
    Just lang -> return lang
    Nothing -> do
      langs <- languages
      return $ head (langs ++ ["en"])
```

---

## ขั้นตอนที่ 553: Image Processing

```haskell
-- Image processing ใน Haskell

import Vision.Image
import Vision.Image.Storage.DevIL
import qualified Vision.Image.Transform as Transform

-- Resize image
resizeImage :: FilePath -> Int -> Int -> IO (Either String FilePath)
resizeImage inputPath maxWidth maxHeight = do
  result <- load inputPath
  case result of
    Left err -> return (Left (show err))
    Right img -> do
      let (h, w) = manifestSize img
      let (newW, newH) = fitDimensions w h maxWidth maxHeight
      let resized = Transform.resize Transform.Bilinear (ix2 newH newW) img
      let outputPath = addSuffix inputPath "_resized"
      saveResult <- save outputPath resized
      case saveResult of
        Nothing  -> return (Right outputPath)
        Just err -> return (Left (show err))

fitDimensions :: Int -> Int -> Int -> Int -> (Int, Int)
fitDimensions w h maxW maxH
  | w <= maxW && h <= maxH = (w, h)
  | ratio > 1.0  = (maxW, round (fromIntegral h / ratio))
  | otherwise    = (round (fromIntegral w * ratio), maxH)
  where
    ratioW = fromIntegral maxW / fromIntegral w :: Double
    ratioH = fromIntegral maxH / fromIntegral h :: Double
    ratio  = min ratioW ratioH

-- Generate thumbnails
generateThumbnails :: FilePath -> IO [FilePath]
generateThumbnails originalPath = do
  let sizes = [(100,100), (300,300), (800,600)]
  forM sizes $ \(w, h) -> do
    let outPath = originalPath <> "_" <> show w <> "x" <> show h
    resizeImage originalPath w h >>= \case
      Left err -> do
        putStrLn $ "Error: " ++ err
        return originalPath
      Right path -> return path

-- Upload handler with image processing
postImageUploadR :: Handler Value
postImageUploadR = do
  req <- waiRequest
  files <- runConduit $ parseRequestBodyEx fileBackEnd (fileBackEndSettings "uploads") req
  case lookup "image" (snd files) of
    Nothing -> invalidArgs ["image file required"]
    Just fileInfo -> do
      let path = fileInfoFilePath fileInfo
      thumbnails <- liftIO $ generateThumbnails path
      returnJson $ object
        [ "original"   .= path
        , "thumbnails" .= thumbnails
        ]
```

---

## ขั้นตอนที่ 554: Realtime Features

```haskell
-- Real-time features ใน Yesod

-- Server-Sent Events handler
getNotificationsR :: Handler TypedContent
getNotificationsR = do
  uid <- requireAuthId
  chan <- appNotificationChan <$> getYesod
  
  -- Subscribe to user's notifications
  subChan <- liftIO $ dupChan chan
  
  respondSource "text/event-stream" $ do
    sendChunkText "retry: 3000\n\n"
    forever $ do
      notification <- liftIO $ readChan subChan
      when (notificationUserId notification == uid) $ do
        sendChunkText $ "data: " <> encodeText notification <> "\n\n"
        sendFlush

-- WebSocket chat
data ChatMessage = ChatMessage
  { cmUser    :: Text
  , cmContent :: Text
  , cmTime    :: UTCTime
  } deriving (Generic, ToJSON, FromJSON)

getWsChatR :: Handler ()
getWsChatR = do
  uid    <- requireAuthId
  user   <- runDB $ get404 uid
  room   <- appChatRoom <$> getYesod
  
  sendWaiResponse $ websocketsOr defaultConnectionOptions
    (handleChat room (userName user))
    notWebSocket
  where
    handleChat room userName conn = do
      WS.sendTextData conn (encode (SystemMessage $ userName <> " joined"))
      
      -- Broadcast join
      broadcast room (SystemMessage $ userName <> " joined")
      
      -- Handle messages
      forever $ do
        msg <- WS.receiveData conn
        case decode msg of
          Just (ChatMessage _ content _) -> do
            now <- getCurrentTime
            let fullMsg = ChatMessage userName content now
            broadcast room fullMsg
            storeMessage room fullMsg
          Nothing -> WS.sendTextData conn ("Invalid message" :: Text)

-- Push notifications
sendPushNotification :: UserId -> Text -> Text -> Handler ()
sendPushNotification uid title body = do
  mToken <- runDB $ getBy (UniquePushToken uid)
  case mToken of
    Nothing -> return ()
    Just (Entity _ token) ->
      liftIO $ sendFCMNotification (pushTokenValue token) title body
```

---

## ขั้นตอนที่ 555: Admin Panel

```haskell
-- Admin panel ใน Yesod

-- Admin authorization
isAdmin :: Handler AuthResult
isAdmin = do
  mUid <- maybeAuthId
  case mUid of
    Nothing  -> return AuthenticationRequired
    Just uid -> do
      mUser <- runDB $ get uid
      case mUser of
        Nothing   -> return (Unauthorized "User not found")
        Just user ->
          if userRole user == AdminRole
            then return Authorized
            else return (Unauthorized "Admin access required")

-- Admin dashboard
getAdminR :: Handler Html
getAdminR = do
  authResult <- isAdmin
  case authResult of
    Authorized -> do
      userCount    <- runDB $ count ([] :: [Filter User])
      postCount    <- runDB $ count ([] :: [Filter Post])
      recentUsers  <- runDB $ selectList [] [Desc UserCreatedAt, LimitTo 5]
      pendingJobs  <- runDB $ count [JobStatus ==. Pending]
      
      defaultLayout [whamlet|
        <h1 .text-3xl .font-bold>Admin Dashboard
        
        <div .grid .grid-cols-4 .gap-4 .mb-8>
          <div .stat-card>
            <h3>Total Users
            <p .stat-value>#{userCount}
          <div .stat-card>
            <h3>Total Posts
            <p .stat-value>#{postCount}
          <div .stat-card>
            <h3>Pending Jobs
            <p .stat-value>#{pendingJobs}
        
        <div .mb-8>
          <h2 .text-xl .mb-4>Recent Users
          <table .w-full>
            <thead>
              <tr>
                <th>ID
                <th>Name
                <th>Email
                <th>Created
            <tbody>
              $forall Entity uid user <- recentUsers
                <tr>
                  <td>#{show uid}
                  <td>#{userName user}
                  <td>#{userEmail user}
                  <td>#{show (userCreatedAt user)}
      |]
    AuthenticationRequired -> redirect LoginR
    Unauthorized msg -> permissionDenied msg

-- Admin CRUD for users
getAdminUsersR :: Handler Html
getAdminUsersR = do
  authResult <- isAdmin
  case authResult of
    Authorized -> do
      users <- runDB $ selectList [] [Asc UserId]
      defaultLayout [whamlet|
        <h1>Users Management
        <table>
          $forall Entity uid user <- users
            <tr>
              <td>#{userName user}
              <td>#{userEmail user}
              <td>
                <form method=post action=@{AdminToggleUserR uid}>
                  <button>
                    $if userActive user
                      Deactivate
                    $else
                      Activate
      |]
    _ -> permissionDenied "Admin only"
```

---

## ขั้นตอนที่ 556: Yesod Plugin System

```haskell
-- Plugin system ใน Yesod

-- Plugin type class
class YesodPlugin app where
  pluginName    :: Text
  pluginRoutes  :: [RouteDefinition]
  pluginHandler :: RouteDefinition -> Handler TypedContent
  pluginWidget  :: Maybe Widget  -- optional admin widget
  pluginMigration :: Maybe Migration  -- optional DB migration

-- Example: Comments plugin
data CommentsPlugin = CommentsPlugin

instance YesodPlugin CommentsPlugin where
  pluginName = "comments"
  
  pluginRoutes =
    [ RouteDefinition "GET"  "/comments"       ListComments
    , RouteDefinition "POST" "/comments"       CreateComment
    , RouteDefinition "PUT"  "/comments/:id"   UpdateComment
    , RouteDefinition "DELETE" "/comments/:id" DeleteComment
    ]
  
  pluginMigration = Just $ do
    runMigration migrateComment

-- Register plugin
registerPlugin :: YesodPlugin p => p -> App -> IO App
registerPlugin plugin app = do
  case pluginMigration plugin of
    Just migration -> runSqlPool migration (appConnPool app)
    Nothing        -> return ()
  return app { appPlugins = plugin : appPlugins app }

-- Auto-discovery of plugins
loadPlugins :: IO [SomePlugin]
loadPlugins = do
  pluginDirs <- getEnvDefault "PLUGIN_DIRS" "plugins"
  dirs       <- listDirectory pluginDirs
  fmap catMaybes $ forM dirs $ \dir -> do
    let configPath = pluginDirs </> dir </> "plugin.yaml"
    exists <- doesFileExist configPath
    if exists
      then Just <$> loadPlugin configPath
      else return Nothing
```

---

## ขั้นตอนที่ 557: GraphQL with Yesod

```haskell
-- GraphQL integration

import Morpheus.Types
import Morpheus

-- GraphQL schema
data Query = Query
  { user :: GetUserArgs -> IO (Maybe User)
  , users :: ListUsersArgs -> IO [User]
  , post :: GetPostArgs -> IO (Maybe Post)
  }

data Mutation = Mutation
  { createUser :: CreateUserArgs -> IO User
  , updateUser :: UpdateUserArgs -> IO User
  , deleteUser :: DeleteUserArgs -> IO Bool
  }

data User = User
  { userId    :: ID
  , userName  :: Text
  , userEmail :: Text
  , userPosts :: IO [Post]
  } deriving (GQLType)

-- Resolvers
userResolver :: GetUserArgs -> IO (Maybe User)
userResolver args = do
  mUser <- runDB $ get (UserId (read (T.unpack (guaId args))))
  return $ fmap dbUserToGQL mUser

usersResolver :: ListUsersArgs -> IO [User]
usersResolver args = do
  users <- runDB $ selectList [] [Asc UserId, LimitTo (luaLimit args)]
  return $ map dbUserToGQL users

-- Root resolver
rootResolver :: GQLRootResolver IO () Query Mutation Subscription
rootResolver = GQLRootResolver
  { queryResolver        = Query{..}
  , mutationResolver     = Mutation{..}
  , subscriptionResolver = Undefined
  }

-- Yesod handler
postGraphqlR :: Handler Value
postGraphqlR = do
  body <- requireCheckJsonBody @GQLRequest
  resp <- liftIO $ interpreter rootResolver body
  returnJson resp
```

---

## ขั้นตอนที่ 558: Yesod Hooks

```haskell
-- Application hooks ใน Yesod

-- Before/After request hooks
class YesodRequestHooks site where
  beforeRequest  :: SomeBase Route -> Handler ()
  afterRequest   :: SomeBase Route -> Handler ()
  onError        :: SomeBase Route -> SomeException -> Handler ()

instance YesodRequestHooks App where
  beforeRequest route = do
    logInfo $ "Request to: " <> routeToText route
    incrementRequestCounter
  
  afterRequest route = do
    logInfo $ "Completed: " <> routeToText route
  
  onError route err = do
    logError $ "Error on " <> routeToText route <> ": " <> T.pack (show err)
    sendErrorNotification route err

-- Event hooks
class YesodEvents site where
  onUserCreated  :: UserId -> User -> Handler ()
  onUserDeleted  :: UserId -> Handler ()
  onPostPublished :: PostId -> Post -> Handler ()

instance YesodEvents App where
  onUserCreated uid user = do
    sendWelcomeEmail (userEmail user)
    createDefaultProfile uid
    triggerWebhooks UserCreatedEvent uid
  
  onUserDeleted uid = do
    cleanupUserData uid
    triggerWebhooks UserDeletedEvent uid
  
  onPostPublished pid post = do
    notifySubscribers post
    updateSearchIndex pid post
    generateSitemap

-- Use hooks in handlers
postUsersR :: CreateUserReq -> Handler (ApiResponse User)
postUsersR req = do
  uid  <- runDB $ insert (newUser req)
  user <- runDB $ get404 uid
  onUserCreated uid user  -- trigger hook!
  return (success user)
```

---

## ขั้นตอนที่ 559: SEO Optimization

```haskell
-- SEO optimization ใน Yesod

-- Dynamic meta tags
class HasSEO a where
  seoTitle       :: a -> Text
  seoDescription :: a -> Text
  seoImage       :: a -> Maybe Text
  seoKeywords    :: a -> [Text]

instance HasSEO Post where
  seoTitle post    = postTitle post <> " | MyBlog"
  seoDescription   = T.take 160 . stripHtml . postContent
  seoImage post    = postFeaturedImage post
  seoKeywords post = postTags post

-- SEO widget
seoWidget :: HasSEO a => a -> Widget
seoWidget item = toWidget [hamlet|
  <title>#{seoTitle item}
  <meta name="description" content="#{seoDescription item}">
  $maybe img <- seoImage item
    <meta property="og:image" content="#{img}">
  <meta property="og:title"       content="#{seoTitle item}">
  <meta property="og:description" content="#{seoDescription item}">
  <meta name="twitter:card"        content="summary_large_image">
  <meta name="twitter:title"       content="#{seoTitle item}">
  <meta name="keywords"            content="#{T.intercalate ", " (seoKeywords item)}">
|]

-- Sitemap generation
getSitemapR :: Handler TypedContent
getSitemapR = do
  posts <- runDB $ selectList [PostPublished ==. True] [Desc PostUpdatedAt]
  users <- runDB $ selectList [] []
  
  let entries = map postEntry posts ++ map userEntry users
  
  respondSource "application/xml" $ do
    sendChunkText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    sendChunkText "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
    forM_ entries $ \entry -> do
      sendChunkText "<url>"
      sendChunkText $ "<loc>" <> sitemapUrl entry <> "</loc>"
      sendChunkText $ "<lastmod>" <> sitemapDate entry <> "</lastmod>"
      sendChunkText $ "<changefreq>" <> sitemapFreq entry <> "</changefreq>"
      sendChunkText "</url>\n"
    sendChunkText "</urlset>"

-- Robots.txt
getRobotsR :: Handler TypedContent
getRobotsR = respond "text/plain" $
  "User-agent: *\n\
  \Allow: /\n\
  \Disallow: /admin/\n\
  \Disallow: /api/\n\
  \Sitemap: https://myapp.com/sitemap.xml\n"
```

---

## ขั้นตอนที่ 560: โปรเจกต์: Full-Stack Yesod Application

```haskell
-- Full-stack application combining all patterns

module Application where

import Prelude hiding (writeFile, readFile)
import Import

-- Application startup
mkYesodDispatch "App" resourcesApp

-- Application entry point
appMain :: IO ()
appMain = do
  -- Load settings
  settings <- loadEnvAppSettings

  -- Build foundation
  foundation <- buildApp settings
  
  -- Run migrations
  flip runSqlPool (appConnPool foundation) $
    runMigration migrateAll
  
  -- Start background workers
  forM_ (appWorkers foundation) $ \worker ->
    forkIO (runWorker worker)
  
  -- Setup graceful shutdown
  shutdownVar <- newEmptyMVar
  installHandler sigTERM (Catch (putMVar shutdownVar ())) Nothing
  
  -- Start server
  let port = appPort (appSettings foundation)
  let warpSettings = setPort port
        $ setGracefulShutdownTimeout (Just 30)
        $ defaultSettings
  
  race_
    (runSettings warpSettings =<< toWaiApp foundation)
    (takeMVar shutdownVar >> putStrLn "Shutting down...")

-- Combined features
-- ✓ Authentication (email + OAuth)
-- ✓ Authorization (RBAC)
-- ✓ REST API (JSON)
-- ✓ Web UI (HTML templates)
-- ✓ Real-time (WebSocket + SSE)
-- ✓ File uploads
-- ✓ Email (SMTP)
-- ✓ Background jobs
-- ✓ Caching (Redis)
-- ✓ Search (PostgreSQL FTS)
-- ✓ Admin panel
-- ✓ i18n
-- ✓ SEO
-- ✓ Testing
-- ✓ Monitoring
-- ✓ Graceful shutdown
```

---

*[← Part 27](part-27.md) | [Part 29 →](part-29.md)*
