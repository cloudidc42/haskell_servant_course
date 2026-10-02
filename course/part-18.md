# Part 18: Yesod Framework เบื้องต้น
## ขั้นตอนที่ 341-360: Web Application ด้วย Yesod

---

## บทนำ

Yesod เป็น web framework ที่ใช้ Template Haskell สำหรับสร้าง type-safe web applications พร้อม routing, templates, forms, authentication ใน package เดียว

---

## ขั้นตอนที่ 341: Yesod Overview

```haskell
-- Yesod: full-featured web framework

-- Features:
-- - Type-safe routing
-- - Template (Hamlet, Cassius, Julius)
-- - Forms validation
-- - Persistent integration
-- - Authentication
-- - i18n
-- - Sessions

-- Dependencies:
-- yesod
-- yesod-core
-- yesod-persistent
-- yesod-auth
-- yesod-form
-- persistent-postgresql
-- warp

-- ตัวอย่าง HelloWorld Yesod app
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
  <h1>Hello, Yesod!
  <p>Welcome to Haskell web development.
|]

main :: IO ()
main = warp 3000 HelloWorld
```

---

## ขั้นตอนที่ 342: Routes

```haskell
-- Yesod routing ด้วย parseRoutes

mkYesod "App" [parseRoutes|
/                    HomeR        GET
/about               AboutR       GET
/users               UsersR       GET POST
/users/#Int          UserR        GET PUT DELETE
/users/#Int/posts    UserPostsR   GET POST
/search              SearchR      GET
/static              StaticR      Static getStatic
|]

-- Route handlers ต้องมีชื่อตาม pattern:
-- method + RouteR
-- GET /users -> getUsersR :: Handler Html
-- POST /users -> postUsersR :: Handler Html

-- Type-safe URLs
getUsersR :: Handler Html
getUsersR = defaultLayout [whamlet|
  <a href=@{UserR 42}>View User 42</a>
  <a href=@{UserPostsR 42}>User 42's Posts</a>
|]

-- Link ด้วย type-safe URL
-- @{RouteR params} ไม่ compile ถ้า params ไม่ถูกต้อง

-- QueryString parameters
getSearchR :: Handler Html
getSearchR = do
  mQuery <- lookupGetParam "q"
  defaultLayout [whamlet|
    <p>Search: #{fromMaybe "nothing" mQuery}
  |]
```

---

## ขั้นตอนที่ 343: App Foundation

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Foundation where

import Yesod
import Yesod.Static
import Database.Persist.Postgresql
import Data.Text (Text)

-- App foundation type
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: Static
  , appConnPool    :: ConnectionPool
  , appHttpManager :: Manager
  }

data AppSettings = AppSettings
  { appPort     :: Int
  , appDbUrl    :: Text
  , appDebug    :: Bool
  }

-- Static files
staticFiles "static"  -- จาก static/ directory

-- Routing
mkYesod "App" [parseRoutes|
/                    HomeR          GET
/static              StaticR        Static appStatic
/auth                AuthR          Auth   getAuth
/profile             ProfileR       GET
/users               UsersR         GET
/users/#UserId       UserR          GET PUT DELETE
|]

-- Yesod instance
instance Yesod App where
  approot = ApprootMaster appRoot
  defaultLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
      addStylesheet (StaticR css_bootstrap_min_css)
      widget
    withUrlRenderer [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          ^{pageHead pc}
        <body>
          <nav>Navigation here
          <main>
            ^{pageBody pc}
          <footer>Footer here
    |]

-- ต้องการ instance เหล่านี้ด้วย
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage
```

---

## ขั้นตอนที่ 344: Handlers

```haskell
-- Handler: ฟังก์ชันที่จัดการ HTTP requests

import Yesod

-- Basic handler
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Home Page"
  [whamlet|
    <h1>Welcome Home!
    <p>This is the home page.
  |]

-- Handler ที่อ่าน URL parameter
getUserR :: UserId -> Handler Html
getUserR userId = do
  user <- runDB $ get404 userId
  defaultLayout [whamlet|
    <h1>User: #{userName user}
    <p>Email: #{userEmail user}
  |]

-- Handler ที่ return JSON
getUserJsonR :: UserId -> Handler Value
getUserJsonR userId = do
  user <- runDB $ get404 userId
  return $ object
    [ "id"    .= userId
    , "name"  .= userName user
    , "email" .= userEmail user
    ]

-- Select between HTML and JSON based on Accept header
getUserFlexR :: UserId -> Handler TypedContent
getUserFlexR userId = do
  user <- runDB $ get404 userId
  selectRep $ do
    provideRep $ defaultLayout [whamlet|<h1>#{userName user}|]
    provideJson $ object ["name" .= userName user]

-- Error handling
-- get404: throw 404 if not found
-- requireCheckJsonBody: parse JSON body or 400
-- notFound: throw 404
-- invalidArgs: throw 400 with messages

-- sendResponse: send raw response
rawResponse :: Handler ()
rawResponse = sendResponse ("text/plain", toContent ("Hello!" :: Text))
```

---

## ขั้นตอนที่ 345: Hamlet Templates

```haskell
-- Hamlet: Haskell template language

-- เหมือน HTML แต่:
-- - indent-based (no closing tags)
-- - $if, $forall, $case
-- - #{expr}: Haskell expression
-- - @{Route}: type-safe URL
-- - ^{widget}: embed widget

getPageR :: Handler Html
getPageR = do
  let items = ["Apple", "Banana", "Cherry"] :: [Text]
  let user  = Just ("Alice" :: Text)
  defaultLayout [whamlet|
    <header>
      <h1>My Page
      $maybe name <- user
        <p>Welcome, #{name}!
      $nothing
        <p>Please log in.
    
    <ul>
      $forall item <- items
        <li>#{item}
    
    <footer>
      <a href=@{HomeR}>Home</a>
  |]

-- Cassius: CSS
pageStyles :: CssUrl (Route App)
pageStyles = [cassius|
  body
    font-family: Arial, sans-serif
    background-color: #f0f0f0
  
  h1
    color: #333
    font-size: 2em
  
  .container
    max-width: 1200px
    margin: 0 auto
|]

-- Julius: JavaScript
pageScript :: JavascriptUrl (Route App)
pageScript = [julius|
  document.addEventListener('DOMContentLoaded', function() {
    console.log('Page loaded!');
    
    var apiUrl = '@{UsersR}';
    fetch(apiUrl)
      .then(res => res.json())
      .then(users => console.log(users));
  });
|]

-- Widget: combine HTML + CSS + JS
myWidget :: Widget
myWidget = do
  toWidget pageStyles
  toWidget pageScript
  [whamlet|
    <div .my-component>
      <h2>My Component
      <p>Content here
  |]
```

---

## ขั้นตอนที่ 346: Forms

```haskell
-- Yesod Forms: type-safe form handling

import Yesod.Form

-- Define form
data UserForm = UserForm
  { ufName  :: Text
  , ufEmail :: Text
  , ufAge   :: Int
  , ufBio   :: Maybe Textarea
  }

userForm :: Form UserForm
userForm = renderDivs $ UserForm
  <$> areq textField      (bfs ("Name"  :: Text)) Nothing
  <*> areq emailField     (bfs ("Email" :: Text)) Nothing
  <*> areq intField       (bfs ("Age"   :: Text)) Nothing
  <*> aopt textareaField  (bfs ("Bio"   :: Text)) Nothing

-- areq: required field
-- aopt: optional field
-- bfs: bootstrap field settings

-- Handler ที่ใช้ form
getNewUserR :: Handler Html
getNewUserR = do
  (widget, enctype) <- generateFormPost userForm
  defaultLayout [whamlet|
    <h1>Create User
    <form method=post action=@{NewUserR} enctype=#{enctype}>
      ^{widget}
      <button type=submit .btn .btn-primary>Create
  |]

postNewUserR :: Handler Html
postNewUserR = do
  ((result, widget), enctype) <- runFormPost userForm
  case result of
    FormSuccess formData -> do
      let name  = ufName formData
          email = ufEmail formData
          age   = ufAge formData
          bio   = fmap unTextarea (ufBio formData)
      uid <- runDB $ insert (User name email age bio)
      redirect (UserR uid)
    FormFailure errors -> do
      defaultLayout [whamlet|
        <h1>Create User
        <p>Please fix the errors:
        $forall err <- errors
          <p .error>#{err}
        <form method=post action=@{NewUserR} enctype=#{enctype}>
          ^{widget}
          <button type=submit>Try Again
      |]
    FormMissing ->
      redirect NewUserR
```

---

## ขั้นตอนที่ 347: Database Integration ใน Yesod

```haskell
-- Yesod + Persistent

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Yesod
import Database.Persist.Postgresql

-- Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    name  Text
    email Text
    age   Int
    UniqueEmail email
    deriving Show Generic

Post sql=posts
    title   Text
    content Text
    userId  UserId
    published Bool default=False
    deriving Show Generic
|]

-- Yesod.Persist instance
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action (appConnPool master)

-- ใช้ runDB ใน handlers
getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [UserAge >. 18] [Asc UserName]
  defaultLayout [whamlet|
    <h1>Users
    <ul>
      $forall Entity uid user <- users
        <li>
          <a href=@{UserR uid}>#{userName user}
          (#{userAge user} years old)
  |]

getPostsR :: Handler Html
getPostsR = do
  posts <- runDB $ selectList [PostPublished ==. True] [Desc PostId, LimitTo 10]
  defaultLayout [whamlet|
    <h1>Recent Posts
    $forall Entity pid post <- posts
      <article>
        <h2>
          <a href=@{PostR pid}>#{postTitle post}
        <p>#{postContent post}
  |]
```

---

## ขั้นตอนที่ 348: Sessions

```haskell
-- Yesod Sessions: server-side session management

-- เก็บข้อมูลใน session
setUserSession :: UserId -> Handler ()
setUserSession uid = do
  setSession "userId" (toPathPiece uid)
  setSession "loginTime" "2024-01-01"

-- อ่านจาก session
getCurrentUser :: Handler (Maybe UserId)
getCurrentUser = do
  mUid <- lookupSession "userId"
  return $ mUid >>= fromPathPiece

-- ลบ session
clearSession :: Handler ()
clearSession = do
  deleteSession "userId"
  deleteSession "loginTime"

-- ตัวอย่าง: Login/Logout
postLoginR :: Handler ()
postLoginR = do
  email    <- runInputPost $ ireq textField "email"
  password <- runInputPost $ ireq passwordField "password"
  mUser <- runDB $ getBy (UniqueEmail email)
  case mUser of
    Nothing -> do
      setMessage "Invalid credentials"
      redirect LoginR
    Just (Entity uid user) -> do
      valid <- validatePassword password (userPasswordHash user)
      if valid
        then do
          setUserSession uid
          redirect HomeR
        else do
          setMessage "Invalid credentials"
          redirect LoginR

getLogoutR :: Handler ()
getLogoutR = do
  clearSession
  redirect HomeR

-- Flash messages (stored in session)
postActionR :: Handler ()
postActionR = do
  setMessage "Action completed successfully!"
  redirect HomeR

getResultR :: Handler Html
getResultR = do
  mMsg <- getMessage
  defaultLayout [whamlet|
    $maybe msg <- mMsg
      <div .alert .alert-success>#{msg}
    <h1>Result Page
  |]
```

---

## ขั้นตอนที่ 349: Authentication ด้วย yesod-auth

```haskell
-- yesod-auth: authentication plugin system

import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Auth.HashDB

-- Auth instance
instance YesodAuth App where
  type AuthId App = UserId
  
  -- Login route
  loginDest _ = HomeR
  logoutDest _ = HomeR
  
  -- Get user from auth ID
  getAuthId creds = runDB $ do
    x <- getBy $ UniqueEmail $ credsIdent creds
    case x of
      Just (Entity uid _) -> return (Just uid)
      Nothing             -> do
        uid <- insert $ User
          { userName     = credsIdent creds
          , userEmail    = credsIdent creds
          , userAge      = 0
          , userPassword = Nothing
          }
        return (Just uid)
  
  -- Auth plugins
  authPlugins _ = [authEmail, authHashDB (Just . UniqueEmail)]
  
  -- Show login page
  authHttpManager = getHttpManager

-- Email auth instance  
instance YesodAuthEmail App where
  type AuthEmailId App = UserId
  
  addUnverified email verkey = runDB $ do
    insert $ User email email 0 Nothing
  
  sendVerifyEmail email _ verurl = do
    liftIO $ putStrLn $ "Verify URL: " ++ T.unpack verurl
  
  getVerifyKey uid = runDB $ do
    mUser <- get uid
    return (mUser >>= userVerifyKey)
  
  setVerifyKey uid key = runDB $
    update uid [UserVerifyKey =. Just key]
  
  verifyAccount uid = runDB $
    update uid [UserVerified =. True]
  
  getPassword uid = runDB $ do
    mUser <- get uid
    return (mUser >>= userPassword)
  
  setPassword uid hash = runDB $
    update uid [UserPassword =. Just hash]
  
  getEmailCreds email = runDB $ do
    mUser <- getBy (UniqueEmail email)
    return $ fmap (\(Entity uid u) -> 
      EmailCreds uid (userPassword u) (userVerified u) (userVerifyKey u) email) mUser
  
  getEmail uid = runDB $ do
    fmap (fmap userEmail) (get uid)

-- Require auth in handler
requireAuthHandler :: Handler Html
requireAuthHandler = do
  uid <- requireAuthId
  user <- runDB $ get404 uid
  defaultLayout [whamlet|
    <h1>Welcome, #{userName user}!
    <p>You are logged in.
    <a href=@{AuthR LogoutR}>Logout</a>
  |]
```

---

## ขั้นตอนที่ 350: Widgets ขั้นสูง

```haskell
-- Widgets: reusable UI components

-- Simple widget
navbarWidget :: Widget
navbarWidget = do
  currentUser <- handlerToWidget getCurrentUser
  [whamlet|
    <nav .navbar>
      <a href=@{HomeR} .navbar-brand>My App
      <ul .navbar-nav>
        <li><a href=@{UsersR}>Users</a>
        <li><a href=@{PostsR}>Posts</a>
        $maybe _ <- currentUser
          <li><a href=@{AuthR LogoutR}>Logout</a>
        $nothing
          <li><a href=@{AuthR LoginR}>Login</a>
  |]

-- Widget ที่รับ parameters
userCardWidget :: Entity User -> Widget
userCardWidget (Entity uid user) = do
  [whamlet|
    <div .card>
      <div .card-body>
        <h5 .card-title>#{userName user}
        <p .card-text>#{userEmail user}
        <a href=@{UserR uid} .btn .btn-primary>View Profile
  |]

-- Paginator widget
paginatorWidget :: Int -> Int -> Int -> Route App -> Widget
paginatorWidget currentPage totalPages pageSize route = do
  [whamlet|
    <nav .pagination>
      $if currentPage > 1
        <a href=@{route}?page=#{currentPage-1}>Previous</a>
      $forall page <- [1..totalPages]
        $if page == currentPage
          <span .active>#{page}
        $else
          <a href=@{route}?page=#{page}>#{page}
      $if currentPage < totalPages
        <a href=@{route}?page=#{currentPage+1}>Next</a>
  |]

-- ใช้ widgets
getUsersPageR :: Handler Html
getUsersPageR = do
  page <- fromMaybe 1 <$> (>>= readMaybe) <$> lookupGetParam "page"
  let pageSize = 10
  users <- runDB $ selectList [] [Asc UserId, OffsetBy ((page-1)*pageSize), LimitTo pageSize]
  total <- runDB $ count ([] :: [Filter User])
  let totalPages = ceiling (fromIntegral total / fromIntegral pageSize :: Double)
  
  defaultLayout $ do
    setTitle "Users"
    navbarWidget
    [whamlet|
      <h1>Users
      <div .row>
        $forall user <- users
          <div .col-md-4>
            ^{userCardWidget user}
      ^{paginatorWidget page totalPages pageSize UsersR}
    |]
```

---

## ขั้นตอนที่ 351: File Upload ใน Yesod

```haskell
-- File upload

postUploadR :: Handler Html
postUploadR = do
  file <- runInputPost $ ireq fileField "file"
  let fileName = fileName file
  let content  = fileContent file
  
  -- Validate file type
  let mime = fileContentType file
  unless (mime `elem` ["image/jpeg", "image/png", "application/pdf"]) $ do
    setMessage "Invalid file type"
    redirect UploadR
  
  -- Save file
  liftIO $ BS.writeFile ("uploads/" ++ T.unpack fileName) content
  
  setMessage $ "File uploaded: " <> toHtml fileName
  redirect HomeR

-- File ใน form
data UploadForm = UploadForm
  { ufFile :: FileInfo
  , ufName :: Text
  }

uploadForm :: Form UploadForm
uploadForm = renderDivs $ UploadForm
  <$> areq fileField (bfs ("File" :: Text)) Nothing
  <*> areq textField (bfs ("Name" :: Text)) Nothing
```

---

## ขั้นตอนที่ 352: i18n (Internationalization)

```haskell
-- Yesod i18n ด้วย mkMessage

-- messages/en.msg:
-- Welcome: Welcome to our site!
-- Hello name@Text: Hello, #{name}!
-- ItemCount n@Int: You have #{n} items.

-- messages/th.msg:
-- Welcome: ยินดีต้อนรับสู่เว็บไซต์ของเรา!
-- Hello name@Text: สวัสดี, #{name}!
-- ItemCount n@Int: คุณมี #{n} รายการ

-- Load messages
mkMessage "App" "messages" "en"

-- ใช้ใน handlers
getWelcomeR :: Handler Html
getWelcomeR = do
  let name = "Alice" :: Text
  defaultLayout [whamlet|
    <h1>_{MsgWelcome}
    <p>_{MsgHello name}
  |]

-- ใน templates: _{MsgKey}
-- _{MsgItemCount count}

-- รองรับหลายภาษา
instance RenderMessage App AppMessage where
  renderMessage _ langs = renderMessageEnglish langs

-- Set language
setLanguage :: Handler ()
setLanguage = do
  lang <- lookupGetParam "lang"
  case lang of
    Just "th" -> setLanguage' ["th"]
    _         -> setLanguage' ["en"]
  where
    setLanguage' langs = do
      setSession "_LANG" (T.intercalate "," langs)
```

---

## ขั้นตอนที่ 353: AJAX และ JSON API

```haskell
-- Yesod AJAX handlers

-- Return JSON ด้วย jsonToRepJson
getApiUsersR :: Handler Value
getApiUsersR = do
  users <- runDB $ selectList [] [Asc UserId]
  return $ toJSON [entityVal u | u <- users]

-- Accept header routing
getUserFlexR :: UserId -> Handler TypedContent
getUserFlexR uid = do
  user <- runDB $ get404 uid
  selectRep $ do
    provideRep $ defaultLayout [whamlet|
      <h1>#{userName user}
    |]
    provideJson user

-- CORS สำหรับ API
addCorsHeaders :: Handler ()
addCorsHeaders = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
  addHeader "Access-Control-Allow-Headers" "Content-Type, Authorization"

-- OPTIONS handler (preflight)
optionsApiR :: Handler ()
optionsApiR = do
  addCorsHeaders
  sendResponseStatus status200 ()

-- CSRF protection
postApiR :: Handler Value
postApiR = do
  checkCsrfHeaderNamed "X-CSRF-Token"
  body <- requireCheckJsonBody :: Handler CreateUser
  -- process...
  return (toJSON "ok")
```

---

## ขั้นตอนที่ 354: Static Files

```haskell
-- Static file serving ใน Yesod

{-# LANGUAGE TemplateHaskell #-}

import Yesod.Static

-- Generate static file routes
staticFiles "static"
-- สร้าง:
-- StaticR :: StaticRoute -> Route App
-- static/css/style.css  -> StaticR (StaticRoute ["css", "style.css"] [])

-- ใช้ใน templates
addStylesheet (StaticR css_style_css)
addScript (StaticR js_app_js)

-- Configure static settings
getStatic :: Static
getStatic = unsafePerformIO $ 
  staticDevel "static"  -- development mode (no caching)
  -- static "static"    -- production mode (with caching)

-- Cache headers สำหรับ static files
instance Yesod App where
  addStaticContent = addStaticContentExternal
    minifym
    genFileName
    "static/tmp"
    (StaticR . flip StaticRoute [])

-- Custom static directory
mkYesod "App" [parseRoutes|
/static StaticR Static getStatic
|]
```

---

## ขั้นตอนที่ 355: Middleware และ Settings

```haskell
-- Yesod middleware

instance Yesod App where
  -- Yesod middleware
  yesodMiddleware = defaultYesodMiddleware
  
  -- Security headers
  defaultLayout widget = do
    addHeader "X-Frame-Options" "DENY"
    addHeader "X-Content-Type-Options" "nosniff"
    addHeader "X-XSS-Protection" "1; mode=block"
    -- ...
    defaultLayout' widget
  
  -- SSL redirect in production
  isAuthorized _ _ = do
    isSecure <- isSecureConn
    unless isSecure $ redirect secure
    return Authorized

-- Custom error pages
instance Yesod App where
  errorHandler NotFound = fmap toTypedContent $ defaultLayout [whamlet|
    <h1>Page Not Found
    <p>The page you're looking for doesn't exist.
    <a href=@{HomeR}>Go Home</a>
  |]
  
  errorHandler (InternalError msg) = fmap toTypedContent $ defaultLayout [whamlet|
    <h1>Internal Error
    <p>Something went wrong. Please try again later.
  |]
  
  errorHandler other = defaultErrorHandler other

-- Request logging
instance Yesod App where
  messageLoggerSource = appLogger
```

---

## ขั้นตอนที่ 356: Testing Yesod Apps

```haskell
-- Testing ด้วย yesod-test

import Yesod.Test
import Test.Hspec

-- Spec
spec :: Spec
spec = withApp app $ do
  describe "Home page" $ do
    it "returns 200" $ do
      get HomeR
      statusIs 200
    
    it "shows welcome message" $ do
      get HomeR
      bodyContains "Welcome"
  
  describe "User creation" $ do
    it "shows form" $ do
      get NewUserR
      statusIs 200
      htmlAnyContain "form" ""
    
    it "creates user via POST" $ do
      request $ do
        setMethod "POST"
        setUrl NewUserR
        addPostParam "name" "Alice"
        addPostParam "email" "alice@example.com"
        addPostParam "age" "30"
      statusIs 303  -- redirect

-- Authentication testing
authSpec :: Spec
authSpec = withApp app $ do
  describe "Protected pages" $ do
    it "redirects unauthenticated" $ do
      get ProfileR
      statusIs 303
    
    it "shows profile when authenticated" $ do
      -- Create user and log in
      doLogin "alice@example.com" "password123"
      get ProfileR
      statusIs 200

doLogin :: Text -> Text -> YesodExample App ()
doLogin email password = do
  get (AuthR LoginR)
  request $ do
    setMethod "POST"
    setUrl (AuthR loginR)
    addToken
    addPostParam "email" email
    addPostParam "password" password
  statusIs 303
```

---

## ขั้นตอนที่ 357: SEO และ Sitemap

```haskell
-- Sitemap ด้วย yesod-sitemap

import Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
  posts <- runDB $ selectList [PostPublished ==. True] []
  users <- runDB $ selectList [] []
  
  sitemap $ do
    -- Static pages
    yield $ SitemapUrl HomeR Nothing (Just Daily) (Just 1.0)
    yield $ SitemapUrl AboutR Nothing (Just Monthly) (Just 0.5)
    
    -- Dynamic pages
    forM_ posts $ \(Entity pid post) -> do
      yield $ SitemapUrl (PostR pid) 
        (Just (postUpdated post))
        (Just Weekly) 
        (Just 0.8)
    
    forM_ users $ \(Entity uid _) -> do
      yield $ SitemapUrl (UserR uid) Nothing (Just Monthly) (Just 0.6)

-- robots.txt
getRobotsR :: Handler Text
getRobotsR = return $ T.unlines
  [ "User-agent: *"
  , "Allow: /"
  , "Disallow: /admin/"
  , "Sitemap: https://mysite.com/sitemap.xml"
  ]
```

---

## ขั้นตอนที่ 358: Email Sending

```haskell
-- Email sending ด้วย yesod-mail-send หรือ smtp-mail

import Network.Mail.SMTP
import Network.Mail.Mime

-- Email configuration
data EmailConfig = EmailConfig
  { smtpHost :: String
  , smtpPort :: Int
  , smtpUser :: String
  , smtpPass :: String
  , fromAddr :: Text
  }

sendEmail :: EmailConfig -> Text -> [Text] -> Text -> Text -> IO ()
sendEmail config subject to htmlBody textBody = do
  let mail = simpleMail
        (Address Nothing (fromAddr config))
        [Address Nothing addr | addr <- to]
        [] []
        subject
        [ htmlPart htmlBody
        , plainTextPart textBody
        ]
  sendMail (smtpHost config) mail

-- ใน Yesod handler
sendWelcomeEmail :: Text -> Text -> Handler ()
sendWelcomeEmail name email = do
  app <- getYesod
  let config = appEmailConfig app
  liftIO $ sendEmail config
    "Welcome to Our Site!"
    [email]
    ("<h1>Welcome, " <> name <> "!</h1>")
    ("Welcome, " <> name <> "!")

-- Template email
welcomeEmail :: Text -> Text -> Html
welcomeEmail name siteUrl = [shamlet|
  <html>
    <body>
      <h1>Welcome, #{name}!
      <p>Thank you for joining our community.
      <a href=#{siteUrl}>Visit our site</a>
|]
```

---

## ขั้นตอนที่ 359: Background Jobs

```haskell
-- Background jobs ใน Yesod

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

-- Job queue
data Job = Job
  { jobType :: Text
  , jobData :: Value
  , jobId   :: UUID
  } deriving (Show)

type JobQueue = TQueue Job

-- Foundation ที่มี job queue
data App = App
  { appJobQueue :: JobQueue
  , ...
  }

-- Submit job
submitJob :: Text -> Value -> Handler ()
submitJob jobType jobData = do
  queue <- appJobQueue <$> getYesod
  jid   <- liftIO randomIO
  let job = Job jobType jobData jid
  liftIO $ atomically (writeTQueue queue job)

-- Job worker (background thread)
startWorker :: JobQueue -> IO ()
startWorker queue = forever $ do
  job <- atomically (readTQueue queue)
  processJob job
  where
    processJob (Job "send-email" data _) = sendEmailJob data
    processJob (Job "resize-image" data _) = resizeImageJob data
    processJob _ = return ()

-- Start worker in main
main :: IO ()
main = do
  queue <- newTQueueIO
  jobWorker <- async (startWorker queue)
  
  let app = App
        { appJobQueue = queue
        , ...
        }
  
  warp 3000 app
  
  cancel jobWorker
```

---

## ขั้นตอนที่ 360: โปรเจกต์: Blog ด้วย Yesod

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Text (Text)
import Data.Time

-- Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlogUser
    name     Text
    email    Text
    password Text Maybe
    admin    Bool default=False
    UniqueEmail email

BlogPost
    title   Text
    slug    Text
    content Text
    authorId BlogUserId
    published Bool default=False
    created UTCTime default=now()
    UniqueSlug slug

BlogComment
    postId BlogPostId
    author Text
    email  Text
    content Text
    approved Bool default=False
    created UTCTime default=now()
|]

-- Foundation
data App = App
  { appConnPool :: ConnectionPool
  }

-- Routes
mkYesod "App" [parseRoutes|
/                  HomeR     GET
/posts             PostsR    GET
/posts/new         NewPostR  GET POST
/posts/#Text       PostR     GET
/posts/#Text/edit  EditPostR GET POST
/auth              AuthR     Auth getAuth
/admin             AdminR    GET
|]

-- Yesod instances
instance Yesod App where
  defaultLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
      addStylesheetRemote "https://cdn.tailwindcss.com"
      widget
    withUrlRenderer [hamlet|
      $doctype 5
      <html>
        <head>
          <meta charset=utf-8>
          <title>#{pageTitle pc} - Blog
          ^{pageHead pc}
        <body .bg-gray-100>
          <nav .bg-blue-600 .text-white .p-4>
            <div .container .mx-auto .flex .justify-between>
              <a href=@{HomeR} .font-bold .text-xl>Haskell Blog
              <div>
                <a href=@{PostsR} .mr-4>Posts
                <a href=@{AuthR LoginR}>Login
          <main .container .mx-auto .py-8 .px-4>
            ^{pageBody pc}
    |]

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB = defaultRunDB (const appConnPool)

instance YesodAuth App where
  type AuthId App = BlogUserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  getAuthId creds = runDB $ do
    x <- getBy $ UniqueEmail (credsIdent creds)
    return $ fmap entityKey x
  authPlugins _ = [authEmail]
  authHttpManager = error "No manager needed"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Handlers
getHomeR :: Handler Html
getHomeR = do
  posts <- runDB $ selectList [BlogPostPublished ==. True]
    [Desc BlogPostCreated, LimitTo 5]
  defaultLayout $ do
    setTitle "Blog Home"
    [whamlet|
      <h1 .text-3xl .font-bold .mb-6>Latest Posts
      $forall Entity pid post <- posts
        <article .mb-6 .bg-white .rounded .shadow .p-6>
          <h2 .text-2xl .font-semibold>
            <a href=@{PostR (blogPostSlug post)}>#{blogPostTitle post}
          <p .text-gray-600 .mt-2>Published #{show (blogPostCreated post)}
          <a href=@{PostR (blogPostSlug post)} .text-blue-600>Read more
    |]

getPostsR :: Handler Html
getPostsR = do
  posts <- runDB $ selectList [BlogPostPublished ==. True]
    [Desc BlogPostCreated]
  defaultLayout $ do
    setTitle "All Posts"
    [whamlet|
      <h1 .text-3xl .font-bold .mb-6>All Posts
      <ul .space-y-4>
        $forall Entity pid post <- posts
          <li .bg-white .rounded .shadow .p-4>
            <a href=@{PostR (blogPostSlug post)} .text-xl .text-blue-600>
              #{blogPostTitle post}
    |]

getPostR :: Text -> Handler Html
getPostR slug = do
  mPost <- runDB $ getBy (UniqueSlug slug)
  case mPost of
    Nothing -> notFound
    Just (Entity pid post) -> do
      author <- runDB $ get404 (blogPostAuthorId post)
      comments <- runDB $ selectList
        [BlogCommentPostId ==. pid, BlogCommentApproved ==. True]
        [Asc BlogCommentCreated]
      defaultLayout $ do
        setTitle (blogPostTitle post)
        [whamlet|
          <article .bg-white .rounded .shadow .p-8>
            <h1 .text-3xl .font-bold>#{blogPostTitle post}
            <p .text-gray-500 .mb-4>
              By #{blogUserName author} on #{show (blogPostCreated post)}
            <div .prose>#{blogPostContent post}
          
          <section .mt-8>
            <h2 .text-2xl .font-bold .mb-4>Comments
            $forall Entity _ comment <- comments
              <div .bg-gray-50 .rounded .p-4 .mb-4>
                <p .font-semibold>#{blogCommentAuthor comment}
                <p>#{blogCommentContent comment}
        |]

-- New post form
data PostForm = PostForm
  { pfTitle   :: Text
  , pfSlug    :: Text
  , pfContent :: Textarea
  , pfPublish :: Bool
  }

postForm :: Form PostForm
postForm = renderDivs $ PostForm
  <$> areq textField     (bfs ("Title" :: Text))    Nothing
  <*> areq textField     (bfs ("Slug"  :: Text))    Nothing
  <*> areq textareaField (bfs ("Content" :: Text)) Nothing
  <*> areq checkBoxField (bfs ("Publish" :: Text)) Nothing

getNewPostR :: Handler Html
getNewPostR = do
  _ <- requireAuthId
  (widget, enctype) <- generateFormPost postForm
  defaultLayout $ do
    setTitle "New Post"
    [whamlet|
      <h1 .text-3xl .font-bold .mb-6>New Post
      <form method=post action=@{NewPostR} enctype=#{enctype} .space-y-4>
        ^{widget}
        <button type=submit .bg-blue-600 .text-white .px-4 .py-2 .rounded>
          Create Post
    |]

postNewPostR :: Handler Html
postNewPostR = do
  uid <- requireAuthId
  ((result, widget), enctype) <- runFormPost postForm
  case result of
    FormSuccess pf -> do
      now <- liftIO getCurrentTime
      let post = BlogPost
            { blogPostTitle     = pfTitle pf
            , blogPostSlug      = pfSlug pf
            , blogPostContent   = unTextarea (pfContent pf)
            , blogPostAuthorId  = uid
            , blogPostPublished = pfPublish pf
            , blogPostCreated   = now
            }
      pid <- runDB (insert post)
      redirect (PostR (blogPostSlug post))
    _ -> do
      defaultLayout [whamlet|
        <h1>Error in form
        <form method=post action=@{NewPostR} enctype=#{enctype}>
          ^{widget}
          <button type=submit>Try Again
      |]

-- Main
main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool "host=localhost dbname=blog" 5
  runSqlPool (runMigration migrateAll) pool
  warp 3000 (App pool)
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 19 เราจะเรียนเรื่อง **Yesod Advanced Features**:
- Persistent relationships
- Complex forms
- WebSockets ใน Yesod
- Admin interface

---

*[← Part 17](part-17.md) | [Part 19 →](part-19.md)*
