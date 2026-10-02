# Part 19: Yesod Advanced Features
## ขั้นตอนที่ 361-380: Yesod ขั้นสูง

---

## ขั้นตอนที่ 361: Persistent Relationships ใน Yesod

```haskell
-- One-to-Many, Many-to-Many Relationships

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name  Text
    email Text
    UniqueAuthorEmail email

Book
    title    Text
    authorId AuthorId
    year     Int

Tag
    name Text
    UniqueName name

BookTag
    bookId BookId
    tagId  TagId
    UniqueBookTag bookId tagId
|]

-- One-to-Many: Author -> Books
getAuthorBooksR :: AuthorId -> Handler Html
getAuthorBooksR aid = do
  author <- runDB $ get404 aid
  books  <- runDB $ selectList [BookAuthorId ==. aid] [Asc BookYear]
  defaultLayout [whamlet|
    <h1>Books by #{authorName author}
    <ul>
      $forall Entity _ book <- books
        <li>#{bookTitle book} (#{bookYear book})
  |]

-- Many-to-Many: Books <-> Tags
getBookTagsR :: BookId -> Handler Html
getBookTagsR bid = do
  book    <- runDB $ get404 bid
  tagRels <- runDB $ selectList [BookTagBookId ==. bid] []
  tags    <- runDB $ mapM (get404 . bookTagTagId . entityVal) tagRels
  defaultLayout [whamlet|
    <h1>#{bookTitle book}
    <p>Tags:
    $forall tag <- tags
      <span .tag>#{tagName tag}
  |]

-- JOIN query ด้วย Esqueleto
getBooksWithAuthorsR :: Handler Html
getBooksWithAuthorsR = do
  results <- runDB $ E.select $ E.from $ \(book `E.InnerJoin` author) -> do
    E.on (book ^. BookAuthorId E.==. author ^. AuthorId)
    E.orderBy [E.asc (book ^. BookYear)]
    return (book, author)
  
  defaultLayout [whamlet|
    <h1>All Books
    $forall (Entity _ book, Entity _ author) <- results
      <div>
        #{bookTitle book} by #{authorName author} (#{bookYear book})
  |]
```

---

## ขั้นตอนที่ 362: Complex Forms

```haskell
-- Forms ที่ซับซ้อน

import Yesod.Form.Bootstrap3

-- Nested form data
data OrderForm = OrderForm
  { ofCustomerName :: Text
  , ofEmail        :: Text
  , ofAddress      :: Text
  , ofItems        :: [OrderItemForm]
  }

data OrderItemForm = OrderItemForm
  { oifProductId :: Int64
  , oifQuantity  :: Int
  }

-- Form with dynamic fields
orderForm :: [Entity Product] -> Form OrderForm
orderForm products = renderBootstrap3 BootstrapBasicForm $ OrderForm
  <$> areq textField "Customer Name" Nothing
  <*> areq emailField "Email" Nothing
  <*> areq textField "Shipping Address" Nothing
  <*> pure []  -- items handled separately

-- Select field with options
productSelectForm :: [Entity Product] -> Form (Maybe ProductId)
productSelectForm products = renderDivs $ aopt
  (selectField opts)
  "Select Product"
  Nothing
  where
    opts = optionsPairs [(productName p, k) | Entity k p <- products]

-- Multi-select
tagsForm :: [Entity Tag] -> Form [TagId]
tagsForm tags = renderDivs $ areq
  (multiSelectField opts)
  "Tags"
  Nothing
  where
    opts = optionsPairs [(tagName t, k) | Entity k t <- tags]

-- Custom field validation
usernameField :: Field Handler Text
usernameField = check validateUsername textField
  where
    validateUsername :: Text -> Either Text Text
    validateUsername name
      | T.length name < 3  = Left "Username must be at least 3 characters"
      | T.length name > 20 = Left "Username must be at most 20 characters"
      | not (T.all isAlphaNum name) = Left "Username can only contain letters and numbers"
      | otherwise = Right name

-- Password confirmation
data RegForm = RegForm
  { rfUsername :: Text
  , rfPassword :: Text
  }

regForm :: Form RegForm
regForm extra = do
  (unameRes, unameView) <- mreq usernameField "Username" Nothing
  (pass1Res, pass1View) <- mreq passwordField "Password" Nothing
  (pass2Res, pass2View) <- mreq passwordField "Confirm Password" Nothing
  
  let passRes = case (pass1Res, pass2Res) of
        (FormSuccess p1, FormSuccess p2)
          | p1 == p2  -> FormSuccess p1
          | otherwise -> FormFailure ["Passwords do not match"]
        _             -> FormFailure ["Please fill in passwords"]
  
  let result = RegForm <$> unameRes <*> passRes
  let widget = do
        [whamlet|
          ^{extra}
          <div .field>
            ^{fvInput unameView}
          <div .field>
            ^{fvInput pass1View}
          <div .field>
            ^{fvInput pass2View}
        |]
  return (result, widget)
```

---

## ขั้นตอนที่ 363: Authorization

```haskell
-- Authorization ใน Yesod

import Yesod

-- isAuthorized: ตรวจสอบสิทธิ์สำหรับทุก route
instance Yesod App where
  isAuthorized AdminR _ = do
    mUser <- getCurrentUser
    case mUser of
      Nothing   -> return AuthenticationRequired
      Just user -> return $ if userAdmin user
        then Authorized
        else Unauthorized "Admin access required"
  
  isAuthorized (UserR uid) True = do  -- True = write
    mCurrentUser <- getCurrentUser
    case mCurrentUser of
      Nothing      -> return AuthenticationRequired
      Just curUser -> return $ if entityKey curUser == uid || userAdmin curUser
        then Authorized
        else Unauthorized "Cannot edit other users"
  
  isAuthorized _ _ = return Authorized  -- Default: allow all

-- getCurrentUser helper
getCurrentUser :: Handler (Maybe (Entity User))
getCurrentUser = do
  mUid <- maybeAuthId
  case mUid of
    Nothing  -> return Nothing
    Just uid -> runDB $ do
      mUser <- get uid
      return $ fmap (Entity uid) mUser

-- Permission check in handlers
deletePostR :: PostId -> Handler ()
deletePostR pid = do
  post  <- runDB $ get404 pid
  curUid <- requireAuthId
  unless (postAuthorId post == curUid) $
    permissionDenied "You can only delete your own posts"
  runDB $ delete pid
  redirect PostsR
```

---

## ขั้นตอนที่ 364: WebSockets ใน Yesod

```haskell
-- WebSockets ด้วย yesod-websockets

import Yesod.WebSockets
import qualified Network.WebSockets as WS
import Control.Concurrent.STM.TChan

-- Chat room state
data ChatState = ChatState
  { chatChannel :: TChan Text
  }

-- WebSocket handler
getChatR :: Handler ()
getChatR = webSockets chatApp <|> normalPage
  where
    normalPage = defaultLayout [whamlet|
      <h1>Chat Room
      <div #messages>
      <form #chat-form>
        <input #message type=text placeholder="Type a message...">
        <button type=submit>Send
      <script>
        var ws = new WebSocket("ws://" + location.host + "@{ChatR}");
        ws.onmessage = function(e) {
          var div = document.createElement('div');
          div.textContent = e.data;
          document.getElementById('messages').appendChild(div);
        };
        document.getElementById('chat-form').onsubmit = function(e) {
          e.preventDefault();
          var input = document.getElementById('message');
          ws.send(input.value);
          input.value = '';
        };
    |]

chatApp :: WebSocketsT Handler ()
chatApp = do
  app <- getYesod
  let chan = chatChannel app
  
  -- Subscribe to channel
  readChan <- liftIO $ atomically $ dupTChan chan
  
  -- Receive thread
  receiveLoop <- liftIO $ async $ do
    forever $ do
      msg <- receiveData
      atomically $ writeTChan chan msg
  
  -- Send loop
  forever $ do
    msg <- liftIO $ atomically $ readTChan readChan
    sendTextData msg
    
    liftIO $ cancel receiveLoop
```

---

## ขั้นตอนที่ 365: Admin Interface

```haskell
-- Admin panel ด้วย Yesod

-- Admin routes
mkYesod "App" [parseRoutes|
/admin              AdminR          GET
/admin/users        AdminUsersR     GET
/admin/users/#UserId AdminUserR     GET PUT DELETE
/admin/posts        AdminPostsR     GET
/admin/posts/#PostId AdminPostR     GET PUT DELETE
|]

-- Admin middleware
requireAdmin :: Handler ()
requireAdmin = do
  mUser <- getCurrentUser
  case mUser of
    Nothing   -> redirect (AuthR LoginR)
    Just user -> unless (userAdmin user) $ permissionDenied "Admin only"

-- Admin dashboard
getAdminR :: Handler Html
getAdminR = do
  requireAdmin
  userCount  <- runDB $ count ([] :: [Filter User])
  postCount  <- runDB $ count ([] :: [Filter Post])
  adminLayout $ do
    [whamlet|
      <h1>Admin Dashboard
      <div .stats-grid>
        <div .stat-card>
          <h3>Total Users
          <p .stat-number>#{userCount}
        <div .stat-card>
          <h3>Total Posts
          <p .stat-number>#{postCount}
    |]

-- Admin user list
getAdminUsersR :: Handler Html
getAdminUsersR = do
  requireAdmin
  users <- runDB $ selectList [] [Asc UserId]
  adminLayout $ do
    [whamlet|
      <h1>Manage Users
      <table .table>
        <thead>
          <tr>
            <th>ID</th><th>Name</th><th>Email</th><th>Admin</th><th>Actions</th>
        <tbody>
          $forall Entity uid user <- users
            <tr>
              <td>#{fromSqlKey uid}
              <td>#{userName user}
              <td>#{userEmail user}
              <td>#{if userAdmin user then "Yes" else "No"}
              <td>
                <a href=@{AdminUserR uid} .btn .btn-sm .btn-primary>Edit
                <button onclick="deleteUser(#{fromSqlKey uid})" .btn .btn-sm .btn-danger>Delete
    |]

-- Admin layout widget
adminLayout :: Widget -> Handler Html
adminLayout widget = do
  pc <- widgetToPageContent $ do
    widget
  withUrlRenderer [hamlet|
    $doctype 5
    <html>
      <head>
        <title>Admin - #{pageTitle pc}
      <body>
        <div .admin-sidebar>
          <a href=@{AdminR}>Dashboard
          <a href=@{AdminUsersR}>Users
          <a href=@{AdminPostsR}>Posts
        <div .admin-content>
          ^{pageBody pc}
  |]
```

---

## ขั้นตอนที่ 366: Email Verification Flow

```haskell
-- Email verification

-- Schema เพิ่มเติม
share [mkPersist sqlSettings] [persistLowerCase|
User
    email        Text
    passwordHash Text Maybe
    verified     Bool default=False
    verifyToken  Text Maybe
    UniqueEmail email
|]

-- Registration handler
postRegisterR :: Handler Html
postRegisterR = do
  email    <- runInputPost $ ireq emailField "email"
  password <- runInputPost $ ireq passwordField "password"
  
  -- Check if email exists
  existing <- runDB $ getBy (UniqueEmail email)
  when (isJust existing) $ do
    setMessage "Email already registered"
    redirect RegisterR
  
  -- Hash password
  hash <- liftIO $ hashPassword password
  
  -- Generate verify token
  token <- liftIO $ generateToken 32
  
  -- Create user
  uid <- runDB $ insert $ User email (Just hash) False (Just token)
  
  -- Send verification email
  render <- getUrlRender
  let verifyUrl = render (VerifyR token)
  sendVerificationEmail email verifyUrl
  
  setMessage "Please check your email to verify your account"
  redirect LoginR

-- Verification handler
getVerifyR :: Text -> Handler Html
getVerifyR token = do
  mUser <- runDB $ getBy (UniqueVerifyToken token)
  case mUser of
    Nothing -> do
      setMessage "Invalid verification token"
      redirect HomeR
    Just (Entity uid user) -> do
      runDB $ update uid
        [ UserVerified =. True
        , UserVerifyToken =. Nothing
        ]
      setMessage "Email verified! You can now log in."
      redirect LoginR

-- Password reset
postForgotPasswordR :: Handler Html
postForgotPasswordR = do
  email <- runInputPost $ ireq emailField "email"
  mUser <- runDB $ getBy (UniqueEmail email)
  
  -- Always show success message (don't leak if email exists)
  forM_ mUser $ \(Entity uid _) -> do
    token <- liftIO $ generateToken 32
    expiry <- liftIO $ addUTCTime (3600) <$> getCurrentTime
    runDB $ update uid [UserResetToken =. Just token, UserResetExpiry =. Just expiry]
    render <- getUrlRender
    sendPasswordResetEmail email (render (ResetPasswordR token))
  
  setMessage "If your email is registered, you'll receive a password reset link"
  redirect LoginR
```

---

## ขั้นตอนที่ 367: Pagination

```haskell
-- Pagination helper

data Pagination = Pagination
  { pPage    :: Int
  , pPerPage :: Int
  , pTotal   :: Int
  } deriving Show

paginate :: Int -> Handler Pagination
paginate perPage = do
  page  <- fromMaybe 1 . (>>= readMaybe . T.unpack) <$> lookupGetParam "page"
  return $ Pagination (max 1 page) perPage 0

totalPages :: Pagination -> Int
totalPages p = ceiling (fromIntegral (pTotal p) / fromIntegral (pPerPage p) :: Double)

paginationOffset :: Pagination -> Int
paginationOffset p = (pPage p - 1) * pPerPage p

-- Use in handlers
getPostsPagedR :: Handler Html
getPostsPagedR = do
  pg <- paginate 10
  total <- runDB $ count [PostPublished ==. True]
  let pg' = pg { pTotal = total }
  
  posts <- runDB $ selectList
    [PostPublished ==. True]
    [Desc PostCreated, LimitTo (pPerPage pg'), OffsetBy (paginationOffset pg')]
  
  defaultLayout $ do
    [whamlet|
      <h1>Posts
      $forall Entity pid post <- posts
        <article>
          <h2>#{postTitle post}
      
      ^{paginationWidget pg' PostsR}
    |]

-- Pagination widget
paginationWidget :: Pagination -> Route App -> Widget
paginationWidget pg route = do
  let current = pPage pg
      total   = totalPages pg
  [whamlet|
    <nav .pagination>
      $if current > 1
        <a href=@?{(route, [("page", T.pack (show (current-1)))])}>← Prev
      
      $forall p <- [max 1 (current-2)..min total (current+2)]
        $if p == current
          <span .active>#{p}
        $else
          <a href=@?{(route, [("page", T.pack (show p))])}>#{p}
      
      $if current < total
        <a href=@?{(route, [("page", T.pack (show (current+1)))])}>Next →
  |]
```

---

## ขั้นตอนที่ 368: Search Functionality

```haskell
-- Full-text search ใน Yesod

getSearchR :: Handler Html
getSearchR = do
  mQuery <- lookupGetParam "q"
  results <- case mQuery of
    Nothing -> return []
    Just q  -> if T.null q
      then return []
      else runDB $ do
        -- Simple LIKE search
        selectList
          [ PostTitle `like` ("%" <> q <> "%")
          , PostPublished ==. True
          ]
          [Desc PostCreated, LimitTo 20]
  
  defaultLayout $ do
    setTitle "Search"
    [whamlet|
      <h1>Search
      <form method=get action=@{SearchR}>
        <input type=search name=q value=#{fromMaybe "" mQuery} placeholder="Search...">
        <button type=submit>Search
      
      $case mQuery
        $of Nothing
          <p>Enter a search term above.
        $of Just q
          $if null results
            <p>No results for "#{q}".
          $else
            <p>Results for "#{q}":
            $forall Entity pid post <- results
              <article>
                <h2>
                  <a href=@{PostR pid}>#{postTitle post}
                <p>#{T.take 200 (postContent post)}...
    |]

-- PostgreSQL full-text search
import Database.Esqueleto.Experimental
import Database.PostgreSQL.Simple (Query)

searchPosts :: Text -> DB [Entity Post]
searchPosts q = E.select $ E.from $ \post -> do
  E.where_ $ E.unsafeSqlFunction "to_tsvector"
    [ E.val "english"
    , post ^. PostContent
    ] `E.matches` E.unsafeSqlFunction "plainto_tsquery"
    [ E.val "english"
    , E.val q
    ]
  return post
```

---

## ขั้นตอนที่ 369: Caching ใน Yesod

```haskell
-- Response caching

import Yesod.Core (cacheSeconds)

-- Cache static pages
getAboutR :: Handler Html
getAboutR = do
  cacheSeconds 3600  -- cache 1 hour
  defaultLayout [whamlet|<h1>About Us|]

-- ETag-based caching
getPostR :: PostId -> Handler Html
getPostR pid = do
  post <- runDB $ get404 pid
  let etag = hashContent (postContent post)
  setEtag etag
  defaultLayout [whamlet|<h1>#{postTitle post}|]

-- Cache computed results ด้วย IORef/STM
data AppCache = AppCache
  { cacheStats :: TVar (Maybe SiteStats)
  , cachePopular :: TVar [Entity Post]
  }

getStatsR :: Handler Html
getStatsR = do
  app <- getYesod
  cached <- liftIO $ readTVarIO (cacheStats (appCache app))
  stats <- case cached of
    Just s  -> return s
    Nothing -> do
      s <- computeStats
      liftIO $ atomically $ writeTVar (cacheStats (appCache app)) (Just s)
      return s
  defaultLayout [whamlet|
    <h1>Site Statistics
    <p>Total posts: #{statsPostCount stats}
    <p>Total users: #{statsUserCount stats}
  |]

-- Invalidate cache
invalidateStatsCache :: Handler ()
invalidateStatsCache = do
  app <- getYesod
  liftIO $ atomically $ writeTVar (cacheStats (appCache app)) Nothing
```

---

## ขั้นตอนที่ 370: API Rate Limiting

```haskell
-- Rate limiting ใน Yesod

import Data.IORef
import Data.Map.Strict (Map)

data RateLimit = RateLimit
  { rlCount  :: Int
  , rlReset  :: UTCTime
  }

type RateLimitStore = TVar (Map Text RateLimit)

checkRateLimit :: Text -> Int -> Int -> Handler ()
checkRateLimit key maxRequests windowSeconds = do
  store  <- appRateLimitStore <$> getYesod
  now    <- liftIO getCurrentTime
  result <- liftIO $ atomically $ do
    m <- readTVar store
    let reset   = addUTCTime (fromIntegral windowSeconds) now
        current = Map.lookup key m
    case current of
      Nothing -> do
        writeTVar store $ Map.insert key (RateLimit 1 reset) m
        return True
      Just rl
        | rlReset rl < now -> do
            writeTVar store $ Map.insert key (RateLimit 1 reset) m
            return True
        | rlCount rl >= maxRequests -> return False
        | otherwise -> do
            writeTVar store $ Map.insert key (rl { rlCount = rlCount rl + 1 }) m
            return True
  
  unless result $ do
    addHeader "Retry-After" "60"
    sendResponseStatus status429 ("Rate limit exceeded" :: Text)

-- Apply to handlers
getApiDataR :: Handler Value
getApiDataR = do
  ip <- remoteHost <$> waiRequest
  checkRateLimit (T.pack (show ip)) 100 60  -- 100 req/min
  -- handler logic...
  return (object ["data" .= ("ok" :: Text)])
```

---

## ขั้นตอนที่ 371: Logging

```haskell
-- Structured logging ใน Yesod

import System.Log.FastLogger
import Yesod.Core.Types

-- Log levels
logDebug, logInfo, logWarn, logError :: Text -> Handler ()
logDebug msg = logDebugNS "app" msg
logInfo  msg = logInfoNS  "app" msg
logWarn  msg = logWarnNS  "app" msg
logError msg = logErrorNS "app" msg

-- Request logging middleware
requestLogger :: Middleware
requestLogger app req respond = do
  let method = requestMethod req
      path   = pathInfo req
      query  = queryString req
  
  start <- getCurrentTime
  app req $ \res -> do
    end     <- getCurrentTime
    let dur = diffUTCTime end start
        status = statusCode (responseStatus res)
    
    putStrLn $ T.unpack $ T.intercalate " "
      [ decodeUtf8 method
      , "/" <> T.intercalate "/" path
      , T.pack (show status)
      , T.pack (show dur)
      ]
    
    respond res

-- Application logging
instance Yesod App where
  messageLoggerSource app logger loc src level str =
    when (level >= appLogLevel (appSettings app)) $
      defaultMessageLoggerSource logger loc src level str

  makeLogger = do
    loggerSet <- newStdoutLoggerSet defaultBufSize
    return $ Logger loggerSet (\_ -> return True)
```

---

## ขั้นตอนที่ 372: Configuration Management

```haskell
-- Environment-based configuration

import System.Environment (lookupEnv)
import Data.Yaml

data AppConfig = AppConfig
  { configPort     :: Int
  , configDbUrl    :: Text
  , configSecret   :: Text
  , configDebug    :: Bool
  , configLogLevel :: LogLevel
  } deriving (Show)

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v ->
    AppConfig
      <$> v .:? "port"     .!= 3000
      <*> v .:  "database"
      <*> v .:  "secret"
      <*> v .:? "debug"    .!= False
      <*> v .:? "logLevel" .!= LevelInfo

loadConfig :: IO AppConfig
loadConfig = do
  -- Read from YAML file
  mConfig <- decodeFileEither "config.yaml"
  baseConfig <- case mConfig of
    Left err -> error $ "Config error: " ++ show err
    Right c  -> return c
  
  -- Override with environment variables
  mPort    <- lookupEnv "PORT"
  mDbUrl   <- lookupEnv "DATABASE_URL"
  mSecret  <- lookupEnv "SECRET_KEY"
  mDebug   <- lookupEnv "DEBUG"
  
  return baseConfig
    { configPort   = maybe (configPort baseConfig) read mPort
    , configDbUrl  = maybe (configDbUrl baseConfig) T.pack mDbUrl
    , configSecret = maybe (configSecret baseConfig) T.pack mSecret
    , configDebug  = maybe (configDebug baseConfig) (== "true") mDebug
    }

-- config.yaml:
-- port: 3000
-- database: "host=localhost dbname=myapp user=postgres password=secret"
-- secret: "my-secret-key-change-in-production"
-- debug: false
-- logLevel: Info
```

---

## ขั้นตอนที่ 373: Internationalization Advanced

```haskell
-- Advanced i18n

-- messages/en.msg
-- WelcomeUser name@Text: Welcome, #{name}!
-- PostCount n@Int:
--   one: 1 post
--   other: #{n} posts
-- DateFormat date@Day: #{formatDate date}

-- messages/th.msg
-- WelcomeUser name@Text: ยินดีต้อนรับ #{name}!
-- PostCount n@Int:
--   one: 1 โพสต์
--   other: #{n} โพสต์
-- DateFormat date@Day: #{formatDateThai date}

-- Template หลายภาษา
getMultiLangR :: Handler Html
getMultiLangR = do
  langs <- languages
  let posts = 5 :: Int
  defaultLayout [whamlet|
    <h1>_{MsgWelcomeUser "Alice"}
    <p>_{MsgPostCount posts}
  |]

-- Language switcher
languageSwitcher :: Widget
languageSwitcher = do
  [whamlet|
    <div .lang-switcher>
      <a href=@?{(HomeR, [("lang", "en")])} .lang-btn>EN
      <a href=@?{(HomeR, [("lang", "th")])} .lang-btn>TH
      <a href=@?{(HomeR, [("lang", "ja")])} .lang-btn>JA
  |]

-- Set language from query param
getSetLangR :: Handler ()
getSetLangR = do
  mLang <- lookupGetParam "lang"
  case mLang of
    Just lang -> do
      setLanguage [lang]
      mRef <- lookupHeader "Referer"
      case mRef of
        Just ref -> redirect (decodeUtf8 ref)
        Nothing  -> redirect HomeR
    Nothing -> redirect HomeR
```

---

## ขั้นตอนที่ 374: Custom Widgets Library

```haskell
-- Reusable UI components

module Widgets where

import Yesod

-- Alert widget
alertWidget :: Text -> Text -> Widget
alertWidget alertType msg = [whamlet|
  <div .alert .alert-#{alertType} role=alert>
    #{msg}
    <button type=button .btn-close data-bs-dismiss=alert>
|]

-- Data table widget
data TableColumn a = TableColumn
  { colHeader :: Text
  , colRender :: a -> Widget
  }

tableWidget :: Text -> [TableColumn a] -> [a] -> Widget
tableWidget tableId cols items = [whamlet|
  <table .table id=#{tableId}>
    <thead>
      <tr>
        $forall col <- cols
          <th>#{colHeader col}
    <tbody>
      $forall item <- items
        <tr>
          $forall col <- cols
            <td>^{colRender col item}
|]

-- Example usage
userTable :: [Entity User] -> Widget
userTable users = tableWidget "users-table"
  [ TableColumn "Name"  (toWidget . toHtml . userName . entityVal)
  , TableColumn "Email" (toWidget . toHtml . userEmail . entityVal)
  , TableColumn "Actions" $ \(Entity uid _) -> [whamlet|
      <a href=@{UserR uid} .btn .btn-sm>View
    |]
  ]
  users

-- Modal widget
modalWidget :: Text -> Text -> Widget -> Widget
modalWidget modalId title body = [whamlet|
  <div .modal .fade id=#{modalId} tabindex=-1>
    <div .modal-dialog>
      <div .modal-content>
        <div .modal-header>
          <h5 .modal-title>#{title}
          <button type=button .btn-close data-bs-dismiss=modal>
        <div .modal-body>
          ^{body}
|]
```

---

## ขั้นตอนที่ 375: Image Processing

```haskell
-- Image upload และ processing

import Codec.Picture
import Codec.Picture.Scaling

-- Upload และ resize image
handleImageUpload :: FileInfo -> Handler (Maybe FilePath)
handleImageUpload file = do
  let origName = fileName file
  case imageType (fileContentType file) of
    Nothing -> return Nothing
    Just _  -> do
      let bytes = fileContent file
      case decodeImage bytes of
        Left err -> do
          setMessage $ "Invalid image: " <> toHtml err
          return Nothing
        Right img -> do
          let resized = scaleBilinear 800 600 img
          let path = "static/uploads/" <> T.unpack origName
          liftIO $ savePngFile path (convertRGB8 resized)
          -- Create thumbnail
          let thumb = scaleBilinear 150 150 img
          liftIO $ savePngFile ("static/uploads/thumbs/" <> T.unpack origName)
                               (convertRGB8 thumb)
          return (Just path)
  where
    imageType "image/jpeg" = Just JPEG
    imageType "image/png"  = Just PNG
    imageType "image/gif"  = Just GIF
    imageType _            = Nothing

-- Image serving with cache headers
getImageR :: Text -> Handler ()
getImageR filename = do
  let path = "static/uploads/" <> T.unpack filename
  exists <- liftIO $ doesFileExist path
  unless exists notFound
  
  addHeader "Cache-Control" "public, max-age=31536000"
  addHeader "Vary" "Accept-Encoding"
  
  sendFile "image/jpeg" path
```

---

## ขั้นตอนที่ 376: PDF Generation

```haskell
-- PDF generation ด้วย wkhtmltopdf หรือ pandoc

import System.Process
import System.IO.Temp
import qualified Data.ByteString as BS

-- Generate PDF from HTML
generatePdf :: Text -> IO BS.ByteString
generatePdf html = withSystemTempDirectory "yesod-pdf" $ \tmpDir -> do
  let htmlFile = tmpDir ++ "/content.html"
      pdfFile  = tmpDir ++ "/output.pdf"
  
  writeFile htmlFile (T.unpack html)
  
  callProcess "wkhtmltopdf"
    [ "--page-size", "A4"
    , "--margin-top", "20mm"
    , "--margin-bottom", "20mm"
    , "--margin-left", "15mm"
    , "--margin-right", "15mm"
    , htmlFile
    , pdfFile
    ]
  
  BS.readFile pdfFile

-- Download PDF handler
getPostPdfR :: PostId -> Handler ()
getPostPdfR pid = do
  post   <- runDB $ get404 pid
  author <- runDB $ get404 (postAuthorId post)
  
  let html = T.pack $ renderHtml [shamlet|
        <html>
          <head><title>#{postTitle post}</title>
          <body>
            <h1>#{postTitle post}
            <p>By #{userName author}
            <div>#{postContent post}
      |]
  
  pdfBytes <- liftIO $ generatePdf html
  
  let filename = "post-" <> T.pack (show (fromSqlKey pid)) <> ".pdf"
  addHeader "Content-Disposition" $ "attachment; filename=" <> filename
  sendResponse ("application/pdf", toContent pdfBytes)
```

---

## ขั้นตอนที่ 377: Error Tracking

```haskell
-- Error tracking ด้วย custom error handling

import Control.Exception (SomeException)

-- Custom error handler
instance Yesod App where
  errorHandler NotFound = do
    logWarn "404 Not Found"
    fmap toTypedContent $ defaultLayout [whamlet|
      <div .error-page>
        <h1>404 - Page Not Found
        <p>Sorry, the page you're looking for doesn't exist.
        <a href=@{HomeR}>Return to Home
    |]
  
  errorHandler (InternalError msg) = do
    logError $ "Internal Error: " <> msg
    -- Notify error tracking service (e.g., Sentry)
    app <- getYesod
    liftIO $ notifyErrorService (appConfig app) msg
    
    fmap toTypedContent $ defaultLayout [whamlet|
      <div .error-page>
        <h1>Internal Server Error
        <p>An unexpected error occurred. Please try again later.
    |]
  
  errorHandler other = defaultErrorHandler other

-- Exception logging middleware
exceptionMiddleware :: App -> Middleware
exceptionMiddleware app waiApp req respond = do
  E.catch (waiApp req respond) $ \(e :: SomeException) -> do
    let config = appConfig app
    notifyErrorService config (T.pack (show e))
    respond (responseLBS internalServerError500 [] "Internal Server Error")

-- Notify Sentry (example)
notifyErrorService :: AppConfig -> Text -> IO ()
notifyErrorService config msg = do
  let dsn = configSentryDsn config
  when (isJust dsn) $ do
    -- POST to Sentry API
    putStrLn $ "[Error] " <> T.unpack msg
```

---

## ขั้นตอนที่ 378: Health Checks และ Metrics

```haskell
-- Health checks

data HealthStatus = HealthStatus
  { healthOk       :: Bool
  , healthDatabase :: Bool
  , healthUptime   :: Int
  , healthVersion  :: Text
  } deriving (Generic, ToJSON)

getHealthR :: Handler Value
getHealthR = do
  -- Database check
  dbOk <- (True <$ runDB (rawExecute "SELECT 1" [])) `E.catch`
    \(_ :: SomeException) -> return False
  
  -- Uptime
  app    <- getYesod
  now    <- liftIO getCurrentTime
  uptime <- floor . diffUTCTime now <$> return (appStartTime app)
  
  let status = HealthStatus
        { healthOk       = dbOk
        , healthDatabase = dbOk
        , healthUptime   = uptime
        , healthVersion  = "1.0.0"
        }
  
  if healthOk status
    then return (toJSON status)
    else sendResponseStatus status503 (toJSON status)

-- Metrics endpoint (Prometheus format)
getMetricsR :: Handler Text
getMetricsR = do
  app <- getYesod
  metrics <- liftIO $ readTVarIO (appMetrics app)
  
  return $ T.unlines
    [ "# HELP http_requests_total Total HTTP requests"
    , "# TYPE http_requests_total counter"
    , "http_requests_total " <> T.pack (show (metricsRequests metrics))
    , ""
    , "# HELP active_connections Current active connections"
    , "# TYPE active_connections gauge"
    , "active_connections " <> T.pack (show (metricsConnections metrics))
    ]

-- Metrics middleware
metricsMiddleware :: TVar AppMetrics -> Middleware
metricsMiddleware metricsVar app req respond = do
  atomically $ modifyTVar metricsVar incrementRequests
  app req respond
```

---

## ขั้นตอนที่ 379: Deployment Configuration

```haskell
-- Production deployment

-- warp settings
warpSettings :: AppConfig -> Settings
warpSettings config = setPort (configPort config)
  $ setHost (configHost config)
  $ setOnException handleException
  $ setOnExceptionResponse exceptionResponse
  $ defaultSettings

handleException :: Maybe Request -> SomeException -> IO ()
handleException mReq ex = do
  putStrLn $ "[Exception] " ++ show ex
  case mReq of
    Just req -> putStrLn $ "  Request: " ++ show (rawPathInfo req)
    Nothing  -> return ()

exceptionResponse :: SomeException -> Response
exceptionResponse _ = responseLBS
  internalServerError500
  [("Content-Type", "text/plain")]
  "Internal Server Error"

-- Graceful shutdown
main :: IO ()
main = do
  config <- loadConfig
  
  -- Initialize resources
  pool <- runStderrLoggingT $ createPostgresqlPool (configDbUrl config) 10
  
  -- Run migrations
  runSqlPool (runMigration migrateAll) pool
  
  -- Build app
  let app = App
        { appConfig  = config
        , appConnPool = pool
        }
  
  -- Start server
  let settings = warpSettings config
  putStrLn $ "Starting server on port " ++ show (configPort config)
  
  -- Install signal handler for graceful shutdown
  shutdown <- newEmptyMVar
  installHandler sigTERM (Catch $ putMVar shutdown ()) Nothing
  
  race_
    (runSettings settings (toWaiApp app))
    (takeMVar shutdown >> putStrLn "Shutting down gracefully...")
```

---

## ขั้นตอนที่ 380: โปรเจกต์: Forum Application

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- Forum application ด้วย Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ForumUser
    username Text
    email    Text
    password Text
    role     Text default="member"
    created  UTCTime default=now()
    UniqueUsername username
    UniqueForumEmail email

Category
    name        Text
    description Text
    slug        Text
    UniqueSlug slug

Thread
    title      Text
    categoryId CategoryId
    authorId   ForumUserId
    views      Int default=0
    pinned     Bool default=False
    locked     Bool default=False
    created    UTCTime default=now()
    lastPost   UTCTime default=now()

Reply
    content   Text
    threadId  ThreadId
    authorId  ForumUserId
    created   UTCTime default=now()
    edited    UTCTime Maybe

Reaction
    replyId  ReplyId
    userId   ForumUserId
    emoji    Text
    UniqueReaction replyId userId
|]

-- Routes
mkYesod "ForumApp" [parseRoutes|
/                            HomeR          GET
/categories                  CategoriesR    GET
/categories/#Text            CategoryR      GET
/threads/#ThreadId           ThreadR        GET
/threads/new/#CategoryId     NewThreadR     GET POST
/threads/#ThreadId/reply     ReplyThreadR   POST
/threads/#ThreadId/lock      LockThreadR    POST
/auth                        AuthR          Auth getAuth
/profile/#ForumUserId        ProfileR       GET
/admin                       AdminR         GET
|]

-- Forum home: show categories
getHomeR :: Handler Html
getHomeR = do
  categories <- runDB $ selectList [] [Asc CategoryName]
  -- Get thread counts per category
  catStats <- forM categories $ \(Entity cid cat) -> do
    count  <- runDB $ count [ThreadCategoryId ==. cid]
    latest <- runDB $ selectFirst [ThreadCategoryId ==. cid] [Desc ThreadLastPost]
    return (cat, count, latest)
  
  defaultLayout $ do
    setTitle "Forum"
    [whamlet|
      <h1 .text-3xl .font-bold .mb-8>Forum
      <div .space-y-4>
        $forall (cat, threadCount, mLatest) <- catStats
          <div .bg-white .rounded .shadow .p-6>
            <div .flex .justify-between>
              <div>
                <h2 .text-xl .font-semibold>
                  <a href=@{CategoryR (categorySlug cat)}>#{categoryName cat}
                <p .text-gray-600>#{categoryDescription cat}
              <div .text-right .text-gray-500>
                <p>#{threadCount} threads
                $maybe Entity _ t <- mLatest
                  <p .text-sm>Last: #{show (threadLastPost t)}
    |]

-- Category page: list threads
getCategoryR :: Text -> Handler Html
getCategoryR slug = do
  mCat <- runDB $ getBy (UniqueSlug slug)
  case mCat of
    Nothing -> notFound
    Just (Entity cid cat) -> do
      threads <- runDB $ selectList
        [ThreadCategoryId ==. cid]
        [Desc ThreadPinned, Desc ThreadLastPost]
      
      threadData <- forM threads $ \(Entity tid thread) -> do
        author  <- runDB $ get404 (threadAuthorId thread)
        repCount <- runDB $ count [ReplyThreadId ==. tid]
        return (tid, thread, author, repCount)
      
      defaultLayout $ do
        setTitle (categoryName cat)
        [whamlet|
          <div .flex .justify-between .mb-6>
            <h1 .text-3xl .font-bold>#{categoryName cat}
            <a href=@{NewThreadR cid} .btn .btn-primary>New Thread
          
          <div .space-y-2>
            $forall (tid, thread, author, repCount) <- threadData
              <div .bg-white .rounded .shadow .p-4>
                <div .flex .justify-between>
                  <div>
                    $if threadPinned thread
                      <span .text-yellow-500>📌 
                    $if threadLocked thread
                      <span .text-red-500>🔒 
                    <a href=@{ThreadR tid} .font-semibold>#{threadTitle thread}
                    <span .text-gray-500 .ml-2>by #{forumUserUsername author}
                  <div .text-right .text-gray-500 .text-sm>
                    <p>#{repCount} replies
                    <p>#{threadViews thread} views
        |]

-- Thread page: show posts
getThreadR :: ThreadId -> Handler Html
getThreadR tid = do
  thread   <- runDB $ get404 tid
  author   <- runDB $ get404 (threadAuthorId thread)
  mCat     <- runDB $ get (threadCategoryId thread)
  
  -- Increment view count
  runDB $ update tid [ThreadViews +=. 1]
  
  -- Get replies
  replies <- runDB $ selectList [ReplyThreadId ==. tid] [Asc ReplyCreated]
  replyData <- forM replies $ \(Entity rid reply) -> do
    user <- runDB $ get404 (replyAuthorId reply)
    reactions <- runDB $ selectList [ReactionReplyId ==. rid] []
    return (rid, reply, user, reactions)
  
  mCurrentUser <- getCurrentForumUser
  
  defaultLayout $ do
    setTitle (threadTitle thread)
    [whamlet|
      <nav .breadcrumb .mb-4>
        <a href=@{HomeR}>Forum</a> >
        $maybe cat <- mCat
          <a href=@{CategoryR (categorySlug cat)}>#{categoryName cat}</a> >
        <span>#{threadTitle thread}
      
      <h1 .text-3xl .font-bold .mb-6>#{threadTitle thread}
      
      $forall (rid, reply, user, reactions) <- replyData
        <div .reply .bg-white .rounded .shadow .p-4 .mb-4 id=reply-#{fromSqlKey rid}>
          <div .flex .gap-4>
            <div .user-info .w-32 .text-center>
              <p .font-semibold>#{forumUserUsername user}
              <p .text-sm .text-gray-500>#{forumUserRole user}
            <div .flex-1>
              <div .reply-content>#{replyContent reply}
              <div .reply-meta .text-sm .text-gray-500 .mt-2>
                #{show (replyCreated reply)}
                $maybe _ <- replyEdited reply
                  (edited)
              <div .reactions .mt-2>
                $forall Entity _ reaction <- reactions
                  <span .reaction>#{reactionEmoji reaction}
      
      $maybe _ <- mCurrentUser
        $if not (threadLocked thread)
          <form method=post action=@{ReplyThreadR tid} .mt-6>
            ^{csrfHiddenInput}
            <textarea name=content .w-full .border .rounded .p-2 rows=5
              placeholder="Write your reply...">
            <button type=submit .btn .btn-primary .mt-2>Post Reply
        $else
          <p .text-gray-500 .italic>This thread is locked.
      $nothing
        <p .mt-6>
          <a href=@{AuthR LoginR}>Log in</a> to reply.
    |]

-- Post reply
postReplyThreadR :: ThreadId -> Handler ()
postReplyThreadR tid = do
  uid     <- requireAuthId
  thread  <- runDB $ get404 tid
  when (threadLocked thread) $ permissionDenied "Thread is locked"
  
  content <- runInputPost $ ireq textareaField "content"
  now     <- liftIO getCurrentTime
  
  runDB $ do
    insert $ Reply (unTextarea content) tid uid now Nothing
    update tid [ThreadLastPost =. now]
  
  redirect (ThreadR tid)

-- New thread form
data NewThreadForm = NewThreadForm
  { ntTitle   :: Text
  , ntContent :: Textarea
  }

newThreadForm :: Form NewThreadForm
newThreadForm = renderDivs $ NewThreadForm
  <$> areq textField (bfs ("Title" :: Text)) Nothing
  <*> areq textareaField (bfs ("Content" :: Text)) Nothing

getNewThreadR :: CategoryId -> Handler Html
getNewThreadR cid = do
  _ <- requireAuthId
  cat <- runDB $ get404 cid
  (widget, enctype) <- generateFormPost newThreadForm
  defaultLayout $ do
    setTitle "New Thread"
    [whamlet|
      <h1>New Thread in #{categoryName cat}
      <form method=post action=@{NewThreadR cid} enctype=#{enctype}>
        ^{widget}
        <button type=submit .btn .btn-primary>Create Thread
    |]

postNewThreadR :: CategoryId -> Handler ()
postNewThreadR cid = do
  uid <- requireAuthId
  _ <- runDB $ get404 cid
  ((result, _), _) <- runFormPost newThreadForm
  case result of
    FormSuccess nt -> do
      now <- liftIO getCurrentTime
      tid <- runDB $ do
        tid <- insert $ Thread (ntTitle nt) cid uid 0 False False now now
        insert_ $ Reply (unTextarea (ntContent nt)) tid uid now Nothing
        return tid
      redirect (ThreadR tid)
    _ -> redirect (NewThreadR cid)

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool "host=localhost dbname=forum" 10
  runSqlPool (runMigration migrateAll) pool
  warp 3000 (ForumApp pool)
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 20 เราจะเรียน **Testing, Deployment และ Production Patterns**:
- HSpec, QuickCheck, Integration testing
- Docker containerization
- CI/CD pipeline
- Monitoring และ alerting

---

*[← Part 18](part-18.md) | [Part 20 →](part-20.md)*
