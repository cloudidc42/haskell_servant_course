# Part 15: Servant Framework เบื้องต้น
## ขั้นตอนที่ 281-300: Type-Safe REST API

---

## บทนำ

Servant คือ Haskell library สำหรับสร้าง type-safe REST APIs โดยกำหนด API เป็น type และ Haskell compiler จะตรวจสอบว่า handlers ถูกต้องตาม specification

---

## ขั้นตอนที่ 281: Servant Overview

```haskell
-- Servant ช่วยให้:
-- 1. กำหนด API เป็น Haskell types
-- 2. Compiler ตรวจสอบว่า handlers ตรงกับ API
-- 3. สร้าง client code อัตโนมัติ
-- 4. Generate documentation อัตโนมัติ

-- ตัวอย่าง API definition
-- GET /users           -> [User]
-- GET /users/:id       -> User
-- POST /users          -> User
-- DELETE /users/:id    -> ()

-- ใน Servant:
type UserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  :<|> "users" :> Capture "id" Int :> Delete '[JSON] ()

-- Dependencies ที่ต้องการ
-- servant
-- servant-server
-- wai
-- warp

-- cabal file
-- build-depends:
--   base >=4.7 && <5,
--   servant ^>= 0.20,
--   servant-server ^>= 0.20,
--   wai,
--   warp,
--   aeson,
--   text
```

---

## ขั้นตอนที่ 282: API Types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Proxy
import Servant

-- Basic combinators:
-- :>   (path segment composition)
-- :<|> (alternative routes)

-- Path segments
type GetGreeting = "hello" :> Get '[JSON] String
-- GET /hello -> String (as JSON)

-- Capture: URL parameters
type GetUser = "users" :> Capture "id" Int :> Get '[JSON] User
-- GET /users/42 -> User

-- QueryParam: query string
type SearchUsers = "users" :> QueryParam "name" String :> Get '[JSON] [User]
-- GET /users?name=alice -> [User]

-- QueryParams: multiple values
type GetByIds = "users" :> QueryParams "ids" Int :> Get '[JSON] [User]
-- GET /users?ids=1&ids=2&ids=3 -> [User]

-- QueryFlag: boolean flag
type GetVerbose = "users" :> QueryFlag "verbose" :> Get '[JSON] [UserDetail]
-- GET /users?verbose -> [UserDetail]

-- ReqBody: request body
type CreateUser' = "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
-- POST /users (body: CreateUser) -> User

-- Header: HTTP header
type AuthenticatedGet = "users" :> Header "Authorization" String :> Get '[JSON] [User]
-- GET /users (with Authorization header) -> [User]

-- Composite example
type FullUserAPI =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  :<|> "users" :> Capture "id" Int :> Delete '[JSON] NoContent
```

---

## ขั้นตอนที่ 283: Data Types

```haskell
module Models where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

-- User model
data User = User
  { userId   :: Int
  , userName :: Text
  , userEmail :: Text
  , userAge  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User

-- Create user request
data CreateUser = CreateUser
  { createUserName  :: Text
  , createUserEmail :: Text
  , createUserAge   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON CreateUser
instance FromJSON CreateUser

-- Update user request
data UpdateUser = UpdateUser
  { updateUserName  :: Maybe Text
  , updateUserEmail :: Maybe Text
  , updateUserAge   :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateUser
instance FromJSON UpdateUser

-- Error response
data ApiError = ApiError
  { errorCode    :: Int
  , errorMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ApiError
instance FromJSON ApiError
```

---

## ขั้นตอนที่ 284: Handler Implementation

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers where

import Servant
import Models
import Data.IORef
import qualified Data.Map.Strict as Map

-- Handler monad: Handler a = ExceptT ServerError IO a
-- Handler คือ IO ที่สามารถ throw ServerError ได้

-- Database (mock ด้วย IORef)
type DB = IORef (Map.Map Int User)

-- สร้าง DB
newDB :: IO DB
newDB = newIORef Map.empty

-- Handlers
getUsers :: DB -> Handler [User]
getUsers db = do
  m <- liftIO (readIORef db)
  return (Map.elems m)

getUserById :: DB -> Int -> Handler User
getUserById db uid = do
  m <- liftIO (readIORef db)
  case Map.lookup uid m of
    Nothing   -> throwError err404 { errBody = "User not found" }
    Just user -> return user

createUser :: DB -> CreateUser -> Handler User
createUser db create = do
  m <- liftIO (readIORef db)
  let newId = if Map.null m then 1 else maximum (Map.keys m) + 1
  let user = User
        { userId    = newId
        , userName  = createUserName create
        , userEmail = createUserEmail create
        , userAge   = createUserAge create
        }
  liftIO $ modifyIORef db (Map.insert newId user)
  return user

updateUser :: DB -> Int -> UpdateUser -> Handler User
updateUser db uid update = do
  m <- liftIO (readIORef db)
  case Map.lookup uid m of
    Nothing -> throwError err404 { errBody = "User not found" }
    Just user -> do
      let updated = user
            { userName  = maybe (userName user)  id (updateUserName  update)
            , userEmail = maybe (userEmail user) id (updateUserEmail update)
            , userAge   = maybe (userAge user)   id (updateUserAge   update)
            }
      liftIO $ modifyIORef db (Map.insert uid updated)
      return updated

deleteUser :: DB -> Int -> Handler NoContent
deleteUser db uid = do
  m <- liftIO (readIORef db)
  case Map.lookup uid m of
    Nothing -> throwError err404 { errBody = "User not found" }
    Just _  -> do
      liftIO $ modifyIORef db (Map.delete uid)
      return NoContent
```

---

## ขั้นตอนที่ 285: Server Setup

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import API
import Handlers
import Models

-- Server: ฟังก์ชันที่จัดการ requests
type UserAPI' = 
       "api" :> "v1" :> "users" :> Get '[JSON] [User]
  :<|> "api" :> "v1" :> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "api" :> "v1" :> "users" :> ReqBody '[JSON] CreateUser :> Post '[JSON] User
  :<|> "api" :> "v1" :> "users" :> Capture "id" Int :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User
  :<|> "api" :> "v1" :> "users" :> Capture "id" Int :> Delete '[JSON] NoContent

-- Server type: handlers สำหรับแต่ละ route
userServer :: DB -> Server UserAPI'
userServer db = 
       getUsers db
  :<|> getUserById db
  :<|> createUser db
  :<|> updateUser db
  :<|> deleteUser db

-- Application
app :: DB -> Application
app db = serve (Proxy :: Proxy UserAPI') (userServer db)

-- Run server
main :: IO ()
main = do
  db <- newDB
  putStrLn "Starting server on port 8080..."
  run 8080 (app db)
```

---

## ขั้นตอนที่ 286: Servant Server Errors

```haskell
-- ServerError: standard HTTP errors

-- Built-in errors
err400 :: ServerError  -- Bad Request
err401 :: ServerError  -- Unauthorized  
err403 :: ServerError  -- Forbidden
err404 :: ServerError  -- Not Found
err409 :: ServerError  -- Conflict
err422 :: ServerError  -- Unprocessable Entity
err500 :: ServerError  -- Internal Server Error

-- Customize error
notFound :: Text -> ServerError
notFound msg = err404 { errBody = LBS.fromStrict (encodeUtf8 msg) }

unauthorized :: ServerError
unauthorized = err401 { errHeaders = [("WWW-Authenticate", "Bearer")] }

-- ตัวอย่าง handler ที่ throw error
validateAge :: Int -> Handler Int
validateAge age
  | age < 0   = throwError $ err400 { errBody = "Age cannot be negative" }
  | age > 150 = throwError $ err400 { errBody = "Age is unrealistically high" }
  | otherwise = return age

-- Error ใน Handler monad
checkPermission :: String -> Int -> Handler ()
checkPermission token userId = do
  valid <- liftIO $ validateToken token
  unless valid $ throwError err401
  admin <- liftIO $ isAdmin token
  unless admin $ throwError err403
  where
    validateToken t = return (not (null t))
    isAdmin _       = return True

-- Custom error type
data AppError
  = NotFound     String
  | Unauthorized String
  | BadRequest   String
  | InternalError String

toServerError :: AppError -> ServerError
toServerError (NotFound     msg) = err404 { errBody = encode msg }
toServerError (Unauthorized msg) = err401 { errBody = encode msg }
toServerError (BadRequest   msg) = err400 { errBody = encode msg }
toServerError (InternalError _)  = err500
```

---

## ขั้นตอนที่ 287: Response Types

```haskell
-- Servant response types

-- Get '[JSON] a: ส่ง a เป็น JSON
-- Post '[JSON] a: ส่ง a เป็น JSON หลัง create
-- Put '[JSON] a: ส่ง a เป็น JSON หลัง update
-- Delete '[JSON] NoContent: ไม่มี response body
-- GetNoContent: ไม่มี body (204 No Content)

-- Headers in response
type GetWithHeaders = "resource" :> Get '[JSON] (Headers '[Header "X-Count" Int] [User])

getWithHeaders :: DB -> Handler (Headers '[Header "X-Count" Int] [User])
getWithHeaders db = do
  users <- getUsers db
  return $ addHeader (length users) users

-- Stream response (SSE or streaming)
import Servant.Types.SourceT

type StreamEndpoint = "stream" :> StreamGet NewlineFraming JSON (SourceIO Int)

streamNumbers :: Handler (SourceIO Int)
streamNumbers = return $ source [1..10]

-- Raw response
type RawAPI = "raw" :> Raw

rawHandler :: Server RawAPI
rawHandler = Tagged $ \req sendResponse ->
  sendResponse $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, World!"

-- Multiple content types
type MultiContent = "data" :> Get '[JSON, PlainText, OctetStream] User
```

---

## ขั้นตอนที่ 288: Authentication

```haskell
{-# LANGUAGE DataKinds #-}

import Servant
import Servant.Auth
import Servant.Auth.Server
import Data.Text (Text)

-- JWT Authentication ด้วย servant-auth

-- User claims
data UserClaims = UserClaims
  { claimUserId :: Int
  , claimRole   :: Text
  } deriving (Generic, Show)

instance ToJSON UserClaims
instance FromJSON UserClaims
instance ToJWT UserClaims
instance FromJWT UserClaims

-- Protected API
type ProtectedAPI = Auth '[JWT] UserClaims :> ProtectedEndpoints

type ProtectedEndpoints =
       "profile" :> Get '[JSON] User
  :<|> "settings" :> ReqBody '[JSON] Settings :> Post '[JSON] ()

-- Handler ที่ต้องการ authentication
protectedServer :: Server ProtectedAPI
protectedServer (Authenticated claims) = getProfile claims :<|> saveSettings claims
protectedServer _ = throwAll err401

getProfile :: UserClaims -> Handler User
getProfile claims = do
  let uid = claimUserId claims
  -- fetch user by uid
  return (User uid "Alice" "alice@example.com" 30)

saveSettings :: UserClaims -> Settings -> Handler ()
saveSettings claims settings = do
  -- save settings for user
  return ()

-- API key authentication
data APIKey

instance AuthServerData (AuthProtect "api-key") where
  type AuthServerData (AuthProtect "api-key") = APIKey

apiKeyCheck :: APIKey -> Bool
apiKeyCheck key = True  -- simplified

type APIKeyProtected = AuthProtect "api-key" :> "data" :> Get '[JSON] [Item]

data Settings = Settings { theme :: Text } deriving (Generic, FromJSON, ToJSON)
data Item = Item { itemId :: Int, itemName :: Text } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 289: Middleware

```haskell
-- WAI Middleware: ทำงานระหว่าง server และ client

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Gzip

-- Request logging
loggingApp :: Application -> Application
loggingApp = logStdout  -- หรือ logStdoutDev

-- CORS
corsApp :: Application -> Application
corsApp = cors corsPolicy
  where
    corsPolicy = const $ Just $ simpleCorsResourcePolicy
      { corsOrigins = Nothing  -- allow all
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }

-- Gzip compression
gzipApp :: Application -> Application
gzipApp = gzip def

-- Custom middleware
requestTimer :: Application -> Application
requestTimer app req sendResponse = do
  start <- getCurrentTime
  app req $ \response -> do
    end <- getCurrentTime
    let elapsed = end `diffUTCTime` start
    putStrLn $ "Request took: " ++ show elapsed
    sendResponse response

-- Combine middlewares
withMiddlewares :: Application -> Application
withMiddlewares = loggingApp . corsApp . gzipApp . requestTimer

-- Apply to servant app
main :: IO ()
main = do
  db <- newDB
  let application = withMiddlewares (app db)
  putStrLn "Starting on port 8080..."
  run 8080 application
```

---

## ขั้นตอนที่ 290: Request Validation

```haskell
-- Validation ใน Servant handlers

import Data.Validation

-- ตัวอย่าง: validate CreateUser
validateCreateUser :: CreateUser -> Either [String] CreateUser
validateCreateUser user = do
  validateName  (createUserName user)
  validateEmail (createUserEmail user)
  validateAge   (createUserAge user)
  Right user

validateName :: Text -> Either [String] Text
validateName name
  | T.null name            = Left ["Name cannot be empty"]
  | T.length name > 100    = Left ["Name too long (max 100 chars)"]
  | otherwise              = Right name

validateEmail :: Text -> Either [String] Text
validateEmail email
  | T.null email           = Left ["Email cannot be empty"]
  | not (T.isInfixOf "@" email) = Left ["Invalid email format"]
  | otherwise              = Right email

validateAge :: Int -> Either [String] Int
validateAge age
  | age < 0   = Left ["Age cannot be negative"]
  | age > 150 = Left ["Age seems unrealistic"]
  | otherwise = Right age

-- ใช้ใน handler
createUserValidated :: DB -> CreateUser -> Handler User
createUserValidated db create =
  case validateCreateUser create of
    Left errors -> throwError $ err400
      { errBody = encode (object ["errors" .= errors]) }
    Right validUser -> do
      user <- liftIO $ createUserInDB db validUser
      return user

-- Validation ด้วย Validation monad (accumulate errors)
validateUserV :: CreateUser -> Validation [Text] CreateUser
validateUserV user =
  CreateUser
    <$> validateNameV  (createUserName user)
    <*> validateEmailV (createUserEmail user)
    <*> validateAgeV   (createUserAge user)
```

---

## ขั้นตอนที่ 291: Database Integration

```haskell
-- Servant + Persistent (database ORM)

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- Define database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserDB
    name Text
    email Text
    age Int
    UniqueEmail email
    deriving Show
|]

-- Database config
type DBPool = ConnectionPool

newDBPool :: String -> IO DBPool
newDBPool connStr = runStderrLoggingT $
  createSqlitePool (T.pack connStr) 10

-- Repository pattern
getUserFromDB :: DBPool -> Int -> IO (Maybe User)
getUserFromDB pool uid = runSqlPool pool $ do
  mUserDB <- get (toSqlKey (fromIntegral uid) :: Key UserDB)
  return $ fmap toUser mUserDB
  where
    toUser (Entity k u) = User
      { userId    = fromIntegral (fromSqlKey k)
      , userName  = userDBName u
      , userEmail = userDBEmail u
      , userAge   = userDBAge u
      }

createUserInDB :: DBPool -> CreateUser -> IO User
createUserInDB pool create = runSqlPool pool $ do
  uid <- insert (UserDB (createUserName create) (createUserEmail create) (createUserAge create))
  return User
    { userId    = fromIntegral (fromSqlKey uid)
    , userName  = createUserName create
    , userEmail = createUserEmail create
    , userAge   = createUserAge create
    }

-- Migration
runMigrations :: DBPool -> IO ()
runMigrations pool = runSqlPool pool (runMigration migrateAll)
```

---

## ขั้นตอนที่ 292: Servant Client

```haskell
-- Generate client code จาก API type

import Servant.Client

-- API definition (same as server)
type UserAPI = 
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" Int :> Get '[JSON] User

-- Generate client functions
getUsers  :: ClientM [User]
getUser   :: Int -> ClientM User

getUsers :<|> getUser = client (Proxy :: Proxy UserAPI)

-- Run client
runClient :: ClientM a -> IO (Either ClientError a)
runClient action = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM action env

-- ตัวอย่าง
fetchAllUsers :: IO ()
fetchAllUsers = do
  result <- runClient getUsers
  case result of
    Left  err   -> putStrLn $ "Error: " ++ show err
    Right users -> mapM_ (putStrLn . show) users

fetchUser :: Int -> IO ()
fetchUser uid = do
  result <- runClient (getUser uid)
  case result of
    Left  err  -> putStrLn $ "Error: " ++ show err
    Right user -> print user
```

---

## ขั้นตอนที่ 293: Servant Documentation

```haskell
-- สร้าง documentation อัตโนมัติด้วย servant-docs

import Servant.Docs

-- เพิ่ม documentation ให้ types
instance ToSample User where
  toSamples _ = singleSample (User 1 "Alice" "alice@example.com" 30)

instance ToSample CreateUser where
  toSamples _ = singleSample (CreateUser "Alice" "alice@example.com" 30)

-- เพิ่ม notes สำหรับ endpoints
userDocs :: API
userDocs = extraInfo (Proxy :: Proxy ("users" :> Get '[JSON] [User]))
  (defAction & notes <>~ [DocNote "Get all users" ["Returns list of all users"]])

-- Generate markdown documentation
generateDocs :: IO ()
generateDocs = do
  let docs = markdown $ docs (Proxy :: Proxy UserAPI)
  writeFile "API_DOCS.md" docs

-- Swagger/OpenAPI documentation ด้วย servant-swagger
import Servant.Swagger
import Data.Swagger

generateSwagger :: IO ()
generateSwagger = do
  let swagger = toSwagger (Proxy :: Proxy UserAPI)
  LBS.writeFile "swagger.json" (encode swagger)
```

---

## ขั้นตอนที่ 294: Testing Servant APIs

```haskell
-- Testing ด้วย hspec และ warp

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai (Application)

-- Test ด้วย Wai test framework
spec :: Spec
spec = with (app <$> newDB) $ do
  describe "GET /users" $ do
    it "returns empty list initially" $ do
      get "/users" `shouldRespondWith` "[]"
      { matchStatus = 200 }
    
    it "returns users after creation" $ do
      let body = [json| { "name": "Alice", "email": "alice@example.com", "age": 30 } |]
      post "/users" body
      get "/users" `shouldRespondWith`
        [json| [{ "userId": 1, "userName": "Alice", "userEmail": "alice@example.com", "userAge": 30 }] |]
  
  describe "GET /users/:id" $ do
    it "returns 404 for non-existent user" $ do
      get "/users/999" `shouldRespondWith` 404
    
    it "returns user if exists" $ do
      let body = [json| { "name": "Bob", "email": "bob@example.com", "age": 25 } |]
      post "/users" body
      get "/users/1" `shouldRespondWith` 200

  describe "POST /users" $ do
    it "creates a user" $ do
      let body = [json| { "name": "Charlie", "email": "charlie@example.com", "age": 35 } |]
      post "/users" body `shouldRespondWith` 201
    
    it "returns 400 for invalid data" $ do
      let body = [json| { "name": "", "email": "invalid", "age": -1 } |]
      post "/users" body `shouldRespondWith` 400

main :: IO ()
main = hspec spec
```

---

## ขั้นตอนที่ 295: Full Example: Simple CRUD API

```haskell
-- Full working example

module Main where

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
import Data.IORef
import qualified Data.Map.Strict as Map
import Servant
import Network.Wai.Handler.Warp
import Data.Text (Text)

-- Model
data Product = Product
  { productId    :: Int
  , productName  :: Text
  , productPrice :: Double
  , productStock :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Product
instance FromJSON Product

data NewProduct = NewProduct
  { npName  :: Text
  , npPrice :: Double
  , npStock :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON NewProduct

-- API
type ProductAPI =
       Get '[JSON] [Product]
  :<|> Capture "id" Int :> Get '[JSON] Product
  :<|> ReqBody '[JSON] NewProduct :> Post '[JSON] Product
  :<|> Capture "id" Int :> ReqBody '[JSON] NewProduct :> Put '[JSON] Product
  :<|> Capture "id" Int :> Delete '[JSON] NoContent

type API = "api" :> "products" :> ProductAPI

-- DB
type ProductDB = IORef (Map.Map Int Product)

-- Handlers
server :: ProductDB -> Server API
server db = 
       listProducts
  :<|> getProduct
  :<|> addProduct
  :<|> updateProduct
  :<|> removeProduct
  where
    listProducts :: Handler [Product]
    listProducts = do
      m <- liftIO (readIORef db)
      return (Map.elems m)

    getProduct :: Int -> Handler Product
    getProduct pid = do
      m <- liftIO (readIORef db)
      case Map.lookup pid m of
        Nothing -> throwError err404
        Just p  -> return p

    addProduct :: NewProduct -> Handler Product
    addProduct np = do
      m <- liftIO (readIORef db)
      let pid = if Map.null m then 1 else maximum (Map.keys m) + 1
          product = Product pid (npName np) (npPrice np) (npStock np)
      liftIO $ modifyIORef db (Map.insert pid product)
      return product

    updateProduct :: Int -> NewProduct -> Handler Product
    updateProduct pid np = do
      m <- liftIO (readIORef db)
      case Map.lookup pid m of
        Nothing -> throwError err404
        Just _  -> do
          let product = Product pid (npName np) (npPrice np) (npStock np)
          liftIO $ modifyIORef db (Map.insert pid product)
          return product

    removeProduct :: Int -> Handler NoContent
    removeProduct pid = do
      m <- liftIO (readIORef db)
      case Map.lookup pid m of
        Nothing -> throwError err404
        Just _  -> do
          liftIO $ modifyIORef db (Map.delete pid)
          return NoContent

-- Application
mkApp :: IO Application
mkApp = do
  db <- newIORef Map.empty
  return $ serve (Proxy :: Proxy API) (server db)

main :: IO ()
main = do
  app <- mkApp
  putStrLn "Server running on http://localhost:8080"
  run 8080 app
```

---

## ขั้นตอนที่ 296: cabal.project สำหรับ Servant

```yaml
-- cabal.project
packages: .

-- servant-example.cabal
cabal-version: 2.4
name:          servant-example
version:       0.1.0.0

library
  hs-source-dirs:    src
  exposed-modules:   API, Models, Handlers, Server
  build-depends:
      base         ^>= 4.18
    , servant      ^>= 0.20
    , servant-server ^>= 0.20
    , aeson        ^>= 2.1
    , wai          ^>= 3.2
    , warp         ^>= 3.3
    , text         ^>= 2.0
    , containers   ^>= 0.6
    , mtl          ^>= 2.3
    , transformers ^>= 0.6
  default-language: GHC2021
  ghc-options: -Wall -Wunused-imports

executable servant-example
  main-is:          Main.hs
  hs-source-dirs:   app
  build-depends:    base, servant-example
  default-language: GHC2021

test-suite servant-example-test
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  build-depends:
      base
    , servant-example
    , hspec      ^>= 2.11
    , hspec-wai  ^>= 0.11
    , wai-extra
  default-language: GHC2021
```

---

## ขั้นตอนที่ 297: Environment Configuration

```haskell
-- Configuration สำหรับ Servant app

module Config where

import System.Environment
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

data AppConfig = AppConfig
  { configPort     :: Int
  , configHost     :: String
  , configDBUrl    :: String
  , configLogLevel :: String
  , configDebug    :: Bool
  } deriving (Show)

loadConfig :: IO AppConfig
loadConfig = do
  port     <- getEnvInt    "PORT"      8080
  host     <- getEnvStr    "HOST"      "0.0.0.0"
  dbUrl    <- getEnvStr    "DATABASE_URL" "sqlite:app.db"
  logLevel <- getEnvStr    "LOG_LEVEL"    "INFO"
  debug    <- getEnvBool   "DEBUG"     False
  return (AppConfig port host dbUrl logLevel debug)

getEnvStr :: String -> String -> IO String
getEnvStr key def = fromMaybe def <$> lookupEnv key

getEnvInt :: String -> Int -> IO Int
getEnvInt key def = do
  mval <- lookupEnv key
  return $ case mval >>= readMaybe of
    Just n  -> n
    Nothing -> def

getEnvBool :: String -> Bool -> IO Bool
getEnvBool key def = do
  mval <- lookupEnv key
  return $ case mval of
    Just "true"  -> True
    Just "1"     -> True
    Just "false" -> False
    Just "0"     -> False
    _            -> def

-- ใช้ config
main :: IO ()
main = do
  config <- loadConfig
  putStrLn $ "Starting on " ++ configHost config ++ ":" ++ show (configPort config)
  -- start server with config...
```

---

## ขั้นตอนที่ 298: Pagination

```haskell
-- Pagination ใน Servant

data PaginationParams = PaginationParams
  { page     :: Int
  , pageSize :: Int
  } deriving (Show)

data Paginated a = Paginated
  { items    :: [a]
  , total    :: Int
  , page'    :: Int
  , pageSize' :: Int
  , hasNext  :: Bool
  , hasPrev  :: Bool
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (Paginated a)

-- API ที่รองรับ pagination
type PaginatedAPI = "users"
  :> QueryParam "page"     Int
  :> QueryParam "pageSize" Int
  :> Get '[JSON] (Paginated User)

-- Handler
paginatedUsers :: DB -> Maybe Int -> Maybe Int -> Handler (Paginated User)
paginatedUsers db mPage mPageSize = do
  let p    = fromMaybe 1   mPage
      ps   = fromMaybe 10  mPageSize
      skip = (p - 1) * ps
  
  allUsers <- getUsers db
  let total     = length allUsers
      pageUsers = take ps (drop skip allUsers)
      hasNext   = skip + ps < total
      hasPrev   = p > 1
  
  return Paginated
    { items     = pageUsers
    , total     = total
    , page'     = p
    , pageSize' = ps
    , hasNext   = hasNext
    , hasPrev   = hasPrev
    }

-- Cursor-based pagination (better for large datasets)
type CursorAPI = "users"
  :> QueryParam "cursor" Int  -- last seen id
  :> QueryParam "limit"  Int
  :> Get '[JSON] CursorPage

data CursorPage = CursorPage
  { cpItems  :: [User]
  , cpCursor :: Maybe Int
  } deriving (Generic, ToJSON)
```

---

## ขั้นตอนที่ 299: Rate Limiting

```haskell
-- Rate limiting ใน Servant ด้วย middleware

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Data.Time

data RateLimitConfig = RateLimitConfig
  { maxRequests :: Int
  , windowSeconds :: Int
  }

data RateLimitState = RateLimitState
  { requests :: Map String [(UTCTime)]
  }

newRateLimiter :: IO (TVar RateLimitState)
newRateLimiter = newTVarIO (RateLimitState Map.empty)

checkRateLimit :: TVar RateLimitState -> RateLimitConfig -> String -> IO Bool
checkRateLimit stateVar config clientIp = do
  now <- getCurrentTime
  atomically $ do
    state <- readTVar stateVar
    let window = fromIntegral (windowSeconds config)
        cutoff  = addUTCTime (-window) now
        reqs    = Map.findWithDefault [] clientIp (requests state)
        recent  = filter (> cutoff) reqs
    if length recent >= maxRequests config
      then return False
      else do
        let newReqs = now : recent
        writeTVar stateVar state { requests = Map.insert clientIp newReqs (requests state) }
        return True

-- Middleware
rateLimitMiddleware :: TVar RateLimitState -> RateLimitConfig -> Application -> Application
rateLimitMiddleware stateVar config app req sendResponse = do
  let clientIp = show (remoteHost req)
  allowed <- checkRateLimit stateVar config clientIp
  if allowed
    then app req sendResponse
    else sendResponse $ responseLBS
      status429
      [("Content-Type", "application/json"), ("Retry-After", "60")]
      (encode (object ["error" .= ("Rate limit exceeded" :: Text)]))
```

---

## ขั้นตอนที่ 300: โปรเจกต์: Blog API

```haskell
-- Complete Blog API

module BlogAPI where

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

import Servant
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.IORef
import qualified Data.Map.Strict as Map

-- Models
data Post = Post
  { postId      :: Int
  , postTitle   :: Text
  , postContent :: Text
  , postAuthor  :: Text
  , postCreated :: UTCTime
  , postTags    :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON Post
instance FromJSON Post

data NewPost = NewPost
  { npTitle   :: Text
  , npContent :: Text
  , npAuthor  :: Text
  , npTags    :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON NewPost

data Comment = Comment
  { commentId     :: Int
  , commentPostId :: Int
  , commentText   :: Text
  , commentAuthor :: Text
  , commentDate   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Comment
instance FromJSON Comment

data NewComment = NewComment
  { ncText   :: Text
  , ncAuthor :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON NewComment

-- API
type BlogAPI =
  "api" :> (
       "posts" :> Get '[JSON] [Post]
  :<|> "posts" :> Capture "id" Int :> Get '[JSON] Post
  :<|> "posts" :> ReqBody '[JSON] NewPost :> Post '[JSON] Post
  :<|> "posts" :> Capture "id" Int :> ReqBody '[JSON] NewPost :> Put '[JSON] Post
  :<|> "posts" :> Capture "id" Int :> Delete '[JSON] NoContent
  :<|> "posts" :> Capture "id" Int :> "comments" :> Get '[JSON] [Comment]
  :<|> "posts" :> Capture "id" Int :> "comments" :> ReqBody '[JSON] NewComment :> Post '[JSON] Comment
  :<|> "posts" :> QueryParam "tag" Text :> Get '[JSON] [Post]
  )

-- DB
data BlogDB = BlogDB
  { bPosts    :: IORef (Map.Map Int Post)
  , bComments :: IORef (Map.Map Int [Comment])
  , bNextPost :: IORef Int
  , bNextComment :: IORef Int
  }

newBlogDB :: IO BlogDB
newBlogDB = BlogDB
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef 1
  <*> newIORef 1

-- Handlers
blogServer :: BlogDB -> Server BlogAPI
blogServer db = 
       listPosts
  :<|> getPost
  :<|> createPost
  :<|> editPost
  :<|> deletePost
  :<|> getComments
  :<|> addComment
  :<|> searchByTag
  where
    listPosts :: Handler [Post]
    listPosts = do
      m <- liftIO (readIORef (bPosts db))
      return $ reverse (Map.elems m)

    getPost :: Int -> Handler Post
    getPost pid = do
      m <- liftIO (readIORef (bPosts db))
      maybe (throwError err404) return (Map.lookup pid m)

    createPost :: NewPost -> Handler Post
    createPost np = do
      pid <- liftIO $ atomicModifyIORef' (bNextPost db) (\n -> (n+1, n))
      now <- liftIO getCurrentTime
      let p = Post pid (npTitle np) (npContent np) (npAuthor np) now (npTags np)
      liftIO $ modifyIORef (bPosts db) (Map.insert pid p)
      return p

    editPost :: Int -> NewPost -> Handler Post
    editPost pid np = do
      m <- liftIO (readIORef (bPosts db))
      case Map.lookup pid m of
        Nothing -> throwError err404
        Just old -> do
          let p = old { postTitle = npTitle np, postContent = npContent np }
          liftIO $ modifyIORef (bPosts db) (Map.insert pid p)
          return p

    deletePost :: Int -> Handler NoContent
    deletePost pid = do
      m <- liftIO (readIORef (bPosts db))
      case Map.lookup pid m of
        Nothing -> throwError err404
        Just _  -> do
          liftIO $ modifyIORef (bPosts db) (Map.delete pid)
          return NoContent

    getComments :: Int -> Handler [Comment]
    getComments pid = do
      m <- liftIO (readIORef (bComments db))
      return (Map.findWithDefault [] pid m)

    addComment :: Int -> NewComment -> Handler Comment
    addComment pid nc = do
      cid <- liftIO $ atomicModifyIORef' (bNextComment db) (\n -> (n+1, n))
      now <- liftIO getCurrentTime
      let comment = Comment cid pid (ncText nc) (ncAuthor nc) now
      liftIO $ modifyIORef (bComments db) (Map.insertWith (++) pid [comment])
      return comment

    searchByTag :: Maybe Text -> Handler [Post]
    searchByTag Nothing    = listPosts
    searchByTag (Just tag) = do
      posts <- listPosts
      return $ filter (\p -> tag `elem` postTags p) posts

-- Application
mkBlogApp :: IO Application
mkBlogApp = do
  db <- newBlogDB
  return $ serve (Proxy :: Proxy BlogAPI) (blogServer db)

main :: IO ()
main = do
  app <- mkBlogApp
  putStrLn "Blog API running on http://localhost:8080"
  putStrLn "Endpoints:"
  putStrLn "  GET    /api/posts"
  putStrLn "  GET    /api/posts/:id"
  putStrLn "  POST   /api/posts"
  putStrLn "  PUT    /api/posts/:id"
  putStrLn "  DELETE /api/posts/:id"
  putStrLn "  GET    /api/posts/:id/comments"
  putStrLn "  POST   /api/posts/:id/comments"
  putStrLn "  GET    /api/posts?tag=..."
  run 8080 app
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 16 เราจะเรียนเรื่อง **Servant Advanced Features**:
- Authentication middleware
- Request/Response transformers
- API versioning
- Streaming responses

---

*[← Part 14](part-14.md) | [Part 16 →](part-16.md)*
