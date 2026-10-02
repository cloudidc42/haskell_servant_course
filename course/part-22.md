# Part 22: Production Architecture Patterns
## ขั้นตอนที่ 421-440: Clean Architecture และ DDD

---

## ขั้นตอนที่ 421: Clean Architecture ใน Haskell

```
-- Clean Architecture layers:
-- 1. Domain (Entities, Value Objects)
-- 2. Application (Use Cases)
-- 3. Interface (Adapters, Repositories)
-- 4. Infrastructure (DB, HTTP, External)

src/
├── Domain/
│   ├── User.hs          -- Entity definitions
│   ├── Post.hs
│   └── Errors.hs        -- Domain errors
├── Application/
│   ├── UserService.hs   -- Use cases
│   ├── PostService.hs
│   └── Ports.hs         -- Port interfaces
├── Interface/
│   ├── Web/
│   │   ├── API.hs       -- HTTP adapters
│   │   └── Handlers.hs
│   └── Repository/
│       ├── UserRepo.hs  -- DB adapters
│       └── PostRepo.hs
└── Infrastructure/
    ├── DB.hs            -- Database setup
    ├── Config.hs        -- Configuration
    └── Main.hs          -- Wiring
```

---

## ขั้นตอนที่ 422: Domain Layer

```haskell
-- Domain/User.hs

module Domain.User where

import Data.Text (Text)
import Data.Time (UTCTime)

-- Value Objects
newtype UserId   = UserId   { unUserId :: Int64 }   deriving (Show, Eq, Ord)
newtype Email    = Email    { unEmail  :: Text  }   deriving (Show, Eq, Ord)
newtype Username = Username { unUsername :: Text } deriving (Show, Eq, Ord)

-- Entity
data User = User
  { userId       :: UserId
  , userEmail    :: Email
  , userUsername :: Username
  , userRole     :: Role
  , userCreated  :: UTCTime
  } deriving (Show, Eq)

data Role = RegularUser | Moderator | Admin
  deriving (Show, Eq, Ord)

-- Domain rules
canEditPost :: User -> Post -> Bool
canEditPost user post =
  userId user == postAuthorId post || userRole user >= Moderator

canDeleteUser :: User -> User -> Bool
canDeleteUser actor target =
  userRole actor == Admin && userRole target /= Admin

-- Value Object validation
mkEmail :: Text -> Either EmailError Email
mkEmail raw
  | T.null raw                = Left EmptyEmail
  | not ('@' `T.elem` raw)   = Left InvalidFormat
  | T.length raw > 255       = Left TooLong
  | otherwise                = Right (Email raw)

mkUsername :: Text -> Either UsernameError Username
mkUsername raw
  | T.length raw < 3         = Left TooShort
  | T.length raw > 30        = Left TooLong
  | not (T.all isAlphaNum raw) = Left InvalidChars
  | otherwise                = Right (Username raw)

data EmailError = EmptyEmail | InvalidFormat | TooLong
data UsernameError = TooShort | TooLong | InvalidChars
```

---

## ขั้นตอนที่ 423: Application Ports

```haskell
-- Application/Ports.hs

module Application.Ports where

-- Repository interfaces (ports)
class Monad m => UserRepository m where
  findUserById    :: UserId -> m (Maybe User)
  findUserByEmail :: Email -> m (Maybe User)
  createUser      :: NewUser -> m (Either CreateUserError User)
  updateUser      :: UserId -> UserUpdate -> m (Either UpdateUserError User)
  deleteUser      :: UserId -> m (Either DeleteUserError ())
  listUsers       :: UserFilter -> Pagination -> m [User]

class Monad m => PostRepository m where
  findPostById    :: PostId -> m (Maybe Post)
  createPost      :: NewPost -> m (Either CreatePostError Post)
  updatePost      :: PostId -> PostUpdate -> m (Either UpdatePostError Post)
  deletePost      :: PostId -> m (Either DeletePostError ())
  listPosts       :: PostFilter -> Pagination -> m [Post]
  countPosts      :: PostFilter -> m Int

-- Service interfaces
class Monad m => EmailService m where
  sendWelcomeEmail    :: Email -> Username -> m ()
  sendPasswordReset   :: Email -> ResetToken -> m ()
  sendNotification    :: Email -> Notification -> m ()

class Monad m => CacheService m where
  cacheGet :: Text -> m (Maybe Text)
  cacheSet :: Text -> Text -> Int -> m ()  -- key, value, ttl
  cacheDel :: Text -> m ()

class Monad m => FileStorage m where
  uploadFile  :: FileName -> ByteString -> m (Either StorageError FileUrl)
  downloadFile :: FileUrl -> m (Either StorageError ByteString)
  deleteFile  :: FileUrl -> m (Either StorageError ())
```

---

## ขั้นตอนที่ 424: Application Use Cases

```haskell
-- Application/UserService.hs

module Application.UserService where

import Domain.User
import Application.Ports

-- Register new user
data RegisterUserCommand = RegisterUserCommand
  { regEmail    :: Text
  , regUsername :: Text
  , regPassword :: Text
  }

data RegisterUserError
  = EmailAlreadyTaken
  | UsernameAlreadyTaken
  | InvalidEmail EmailError
  | InvalidUsername UsernameError
  | PasswordTooWeak
  deriving Show

registerUser
  :: (UserRepository m, EmailService m, Monad m)
  => RegisterUserCommand
  -> m (Either RegisterUserError User)
registerUser cmd = do
  -- Validate inputs
  email <- case mkEmail (regEmail cmd) of
    Left err -> return (Left (InvalidEmail err))
    Right e  -> return (Right e)
  
  username <- case mkUsername (regUsername cmd) of
    Left err -> return (Left (InvalidUsername err))
    Right u  -> return (Right u)
  
  -- Check uniqueness
  case (email, username) of
    (Right e, Right u) -> do
      mExistingEmail <- findUserByEmail e
      case mExistingEmail of
        Just _ -> return (Left EmailAlreadyTaken)
        Nothing -> do
          result <- createUser (NewUser e u (regPassword cmd))
          case result of
            Left err   -> return (Left (mapCreateError err))
            Right user -> do
              sendWelcomeEmail e u
              return (Right user)
    _ -> return (Left (head [e | Left e <- [email, username]]))

-- Get user profile
getUserProfile
  :: (UserRepository m, CacheService m, Monad m)
  => UserId
  -> m (Either UserError UserProfile)
getUserProfile uid = do
  -- Try cache first
  let cacheKey = "user:" <> T.pack (show (unUserId uid))
  mCached <- cacheGet cacheKey
  
  case mCached >>= decode . encodeUtf8 of
    Just profile -> return (Right profile)
    Nothing -> do
      mUser <- findUserById uid
      case mUser of
        Nothing   -> return (Left UserNotFound)
        Just user -> do
          let profile = userToProfile user
          cacheSet cacheKey (decodeUtf8 (encode profile)) 300
          return (Right profile)
```

---

## ขั้นตอนที่ 425: Infrastructure Adapters

```haskell
-- Interface/Repository/UserRepo.hs

module Interface.Repository.UserRepo where

import Domain.User
import Application.Ports
import Database.Persist.Postgresql

-- PostgreSQL adapter
newtype PgUserRepository m a = PgUserRepository
  { runPgUserRepo :: ReaderT ConnectionPool m a
  } deriving (Functor, Applicative, Monad, MonadReader ConnectionPool)

instance MonadIO m => UserRepository (PgUserRepository m) where
  findUserById uid = do
    pool <- ask
    mRow <- liftIO $ runSqlPool (get (toDbKey uid)) pool
    return (fmap rowToUser mRow)
  
  findUserByEmail email = do
    pool <- ask
    mRow <- liftIO $ runSqlPool
      (getBy (UniqueEmail (unEmail email))) pool
    return (fmap (rowToUser . entityVal) mRow)
  
  createUser newUser = do
    pool <- ask
    result <- liftIO $ runSqlPool (insertUnique (userToRow newUser)) pool
    case result of
      Nothing  -> return (Left DuplicateUser)
      Just key -> findUserById (fromDbKey key) >>= \case
        Nothing   -> return (Left DatabaseError)
        Just user -> return (Right user)
  
  listUsers filter' pag = do
    pool <- ask
    rows <- liftIO $ runSqlPool
      (selectList
        (buildFilters filter')
        [Asc DbUserId, LimitTo (pageSize pag), OffsetBy (pageOffset pag)])
      pool
    return (map (rowToUser . entityVal) rows)

-- Domain ↔ Database mapping
rowToUser :: DbUser -> User
rowToUser row = User
  { userId       = UserId (fromSqlKey (dbUserId row))
  , userEmail    = Email (dbUserEmail row)
  , userUsername = Username (dbUserUsername row)
  , userRole     = parseRole (dbUserRole row)
  , userCreated  = dbUserCreated row
  }

userToRow :: NewUser -> DbUser
userToRow nu = DbUser
  { dbUserEmail    = unEmail (nuEmail nu)
  , dbUserUsername = unUsername (nuUsername nu)
  , dbUserPassword = nuPasswordHash nu
  , dbUserRole     = "user"
  , dbUserCreated  = nuCreated nu
  }
```

---

## ขั้นตอนที่ 426: Event Sourcing

```haskell
-- Event Sourcing pattern

-- Domain Events
data UserEvent
  = UserRegistered    { evUserId :: UserId, evEmail :: Email, evAt :: UTCTime }
  | UserEmailChanged  { evUserId :: UserId, evOldEmail :: Email, evNewEmail :: Email, evAt :: UTCTime }
  | UserRoleChanged   { evUserId :: UserId, evRole :: Role, evAt :: UTCTime }
  | UserDeleted       { evUserId :: UserId, evAt :: UTCTime }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Event Store
class Monad m => EventStore m where
  appendEvent :: StreamId -> ExpectedVersion -> DomainEvent -> m (Either EventStoreError ())
  loadEvents  :: StreamId -> m [DomainEvent]
  loadFrom    :: StreamId -> EventNumber -> m [DomainEvent]

data ExpectedVersion = Any | Version Int | NoStream

-- Aggregate
data UserAggregate = UserAggregate
  { aggId      :: UserId
  , aggVersion :: Int
  , aggState   :: Maybe UserState
  }

data UserState = UserState
  { stEmail    :: Email
  , stUsername :: Username
  , stRole     :: Role
  , stDeleted  :: Bool
  }

-- Apply events to state
applyEvent :: Maybe UserState -> UserEvent -> Maybe UserState
applyEvent Nothing (UserRegistered uid email _) =
  Just $ UserState email (Username "") RegularUser False
applyEvent (Just st) (UserEmailChanged _ _ newEmail _) =
  Just st { stEmail = newEmail }
applyEvent (Just st) (UserRoleChanged _ role _) =
  Just st { stRole = role }
applyEvent (Just _) (UserDeleted _ _) = Nothing
applyEvent st _ = st

-- Reconstruct from events
loadAggregate :: EventStore m => UserId -> m (Maybe UserAggregate)
loadAggregate uid = do
  events <- loadEvents (userStreamId uid)
  if null events
    then return Nothing
    else do
      let state = foldl' (\s e -> applyEvent s e) Nothing
                  (map fromDomainEvent events)
      return $ Just $ UserAggregate
        { aggId = uid
        , aggVersion = length events
        , aggState = state
        }

-- Command handler
handleRegisterUser
  :: EventStore m
  => RegisterUserCommand
  -> m (Either RegisterError UserAggregate)
handleRegisterUser cmd = do
  let uid = generateId
  mExisting <- loadAggregate uid
  case mExisting of
    Just _  -> return (Left UserAlreadyExists)
    Nothing -> do
      now <- getCurrentTime
      let event = UserRegistered uid (Email (regEmail cmd)) now
      result <- appendEvent (userStreamId uid) NoStream (toDomainEvent event)
      case result of
        Left err -> return (Left (StoreError err))
        Right () -> loadAggregate uid >>= return . maybe (Left NotFound) Right
```

---

## ขั้นตอนที่ 427: CQRS Pattern

```haskell
-- CQRS: Command Query Responsibility Segregation

-- Commands (write side)
data Command
  = RegisterUser RegisterUserCommand
  | ChangeEmail  ChangeEmailCommand
  | DeleteUser   DeleteUserCommand
  | CreatePost   CreatePostCommand
  | PublishPost  PublishPostCommand

-- Command handler
class Monad m => CommandHandler m where
  handleCommand :: Command -> m (Either CommandError CommandResult)

-- Queries (read side)
data Query
  = GetUser         UserId
  | ListUsers       UserFilter Pagination
  | GetUserProfile  UserId
  | SearchUsers     Text

-- Query handler
class Monad m => QueryHandler m where
  handleQuery :: Query -> m (Either QueryError QueryResult)

-- Read models (denormalized for performance)
data UserReadModel = UserReadModel
  { urmId          :: Int64
  , urmEmail       :: Text
  , urmUsername    :: Text
  , urmPostCount   :: Int
  , urmFollowers   :: Int
  , urmCreated     :: UTCTime
  }

-- Projection: update read model from events
projectUserEvent :: UserEvent -> DB ()
projectUserEvent (UserRegistered uid email at) = do
  insert_ $ UserReadModel
    (unUserId uid) (unEmail email) "" 0 0 at
projectUserEvent (UserEmailChanged uid _ newEmail _) = do
  updateWhere
    [UserReadModelId ==. unUserId uid]
    [UserReadModelEmail =. unEmail newEmail]
projectUserEvent (UserDeleted uid _) = do
  deleteWhere [UserReadModelId ==. unUserId uid]
projectUserEvent _ = return ()

-- Event bus for projections
runProjections :: EventStore m => m ()
runProjections = do
  events <- getAllNewEvents
  mapM_ applyToReadModel events
```

---

## ขั้นตอนที่ 428: Hexagonal Architecture

```haskell
-- Hexagonal (Ports & Adapters) Architecture

-- Core domain (no external dependencies)
module Core where

-- Pure domain logic
processOrder :: Order -> [Item] -> Either OrderError ProcessedOrder
processOrder order items = do
  validateOrder order
  calculateTotal items
  applyDiscounts order
  return (ProcessedOrder order items total)

-- Application layer: orchestrates use cases
module Application where

import Core

-- Port interfaces
class Monad m => OrderPort m where
  saveOrder    :: ProcessedOrder -> m OrderId
  loadOrder    :: OrderId -> m (Maybe Order)

class Monad m => PaymentPort m where
  processPayment :: OrderId -> Money -> m (Either PaymentError TransactionId)
  refundPayment  :: TransactionId -> m (Either PaymentError ())

class Monad m => NotificationPort m where
  sendOrderConfirmation :: Email -> Order -> m ()
  sendShippingUpdate    :: Email -> TrackingInfo -> m ()

-- Use case
checkout
  :: (OrderPort m, PaymentPort m, NotificationPort m)
  => CheckoutRequest
  -> m (Either CheckoutError OrderConfirmation)
checkout req = do
  let order = buildOrder req
  case processOrder order (reqItems req) of
    Left err -> return (Left (DomainError err))
    Right processedOrder -> do
      orderId <- saveOrder processedOrder
      payResult <- processPayment orderId (orderTotal processedOrder)
      case payResult of
        Left err -> return (Left (PaymentFailed err))
        Right txId -> do
          sendOrderConfirmation (reqEmail req) order
          return (Right (OrderConfirmation orderId txId))

-- Adapters
module Adapter.PostgresOrderPort where
-- PostgreSQL implementation

module Adapter.StripePaymentPort where
-- Stripe implementation

module Adapter.SendGridNotificationPort where
-- SendGrid implementation
```

---

## ขั้นตอนที่ 429: Domain-Driven Design

```haskell
-- DDD Aggregates และ Bounded Contexts

-- Bounded Context: User Management
module Context.UserManagement where

-- Aggregate Root
data UserAggregate = UserAggregate
  { user    :: User
  , events  :: [UserEvent]  -- uncommitted events
  }

-- Factory method
createUser :: RegisterUserCommand -> Either ValidationError UserAggregate
createUser cmd = do
  email    <- validateEmail (regEmail cmd)
  username <- validateUsername (regUsername cmd)
  password <- validatePassword (regPassword cmd)
  return $ UserAggregate
    { user   = User Nothing email username RegularUser
    , events = [UserRegistered email username]
    }

-- Business methods
changeEmail :: Email -> UserAggregate -> Either DomainError UserAggregate
changeEmail newEmail agg
  | userDeleted (user agg) = Left UserIsDeleted
  | userEmail (user agg) == newEmail = Left SameEmail
  | otherwise = Right agg
      { user = (user agg) { userEmail = newEmail }
      , events = UserEmailChanged (userEmail (user agg)) newEmail : events agg
      }

promote :: Admin -> UserAggregate -> Either DomainError UserAggregate
promote admin agg
  | userRole (user agg) == Admin = Left AlreadyAdmin
  | userRole (user admin) /= Admin = Left NotAuthorized
  | otherwise = Right agg
      { user   = (user agg) { userRole = Admin }
      , events = UserPromoted (adminId admin) : events agg
      }

-- Bounded Context: Content Management
module Context.ContentManagement where

-- Different User concept in this context
data ContentAuthor = ContentAuthor
  { authorId   :: UserId     -- reference, not full aggregate
  , authorName :: Text
  }

-- Aggregate Root  
data PostAggregate = PostAggregate
  { post   :: Post
  , events :: [PostEvent]
  }
```

---

## ขั้นตอนที่ 430: Repository Pattern Advanced

```haskell
-- Repository ขั้นสูง

-- Generic repository
class Repository f m where
  type Id f
  find   :: Id f -> m (Maybe (f (Id f)))
  findAll :: Filter f -> Pagination -> m [f (Id f)]
  save   :: f (Id f) -> m (Id f)
  update :: Id f -> Update f -> m (Either UpdateError (f (Id f)))
  delete :: Id f -> m (Either DeleteError ())
  count  :: Filter f -> m Int

-- Unit of Work pattern
class UnitOfWork m where
  begin    :: m Transaction
  commit   :: Transaction -> m (Either TransactionError ())
  rollback :: Transaction -> m ()
  withTransaction :: m a -> m (Either TransactionError a)

-- Specification pattern
data Spec a = Spec { isSatisfiedBy :: a -> Bool, toSQL :: (Text, [SqlParam]) }

activeUsers :: Spec User
activeUsers = Spec
  { isSatisfiedBy = \u -> not (userDeleted u) && userVerified u
  , toSQL = ("deleted = false AND verified = true", [])
  }

adultUsers :: Int -> Spec User
adultUsers minAge = Spec
  { isSatisfiedBy = \u -> userAge u >= minAge
  , toSQL = ("age >= ?", [SqlInt minAge])
  }

(&&.) :: Spec a -> Spec a -> Spec a
s1 &&. s2 = Spec
  { isSatisfiedBy = \x -> isSatisfiedBy s1 x && isSatisfiedBy s2 x
  , toSQL = let (sql1, p1) = toSQL s1
                (sql2, p2) = toSQL s2
            in ("(" <> sql1 <> " AND " <> sql2 <> ")", p1 ++ p2)
  }

-- Usage
findAdultActiveUsers :: UserRepository m => m [User]
findAdultActiveUsers = findBySpec (activeUsers &&. adultUsers 18)
```

---

## ขั้นตอนที่ 431: Saga Pattern

```haskell
-- Saga: manage distributed transactions

-- Order Saga: Create Order -> Reserve Inventory -> Process Payment -> Confirm

data SagaState
  = OrderCreated    OrderId
  | InventoryReserved OrderId ReservationId
  | PaymentProcessed OrderId TransactionId
  | OrderConfirmed  OrderId
  | CompensatingPayment OrderId
  | CompensatingInventory OrderId ReservationId
  | SagaFailed OrderId Text

-- Saga steps
data SagaStep m a = SagaStep
  { action      :: m (Either Error a)     -- forward action
  , compensation :: a -> m (Either Error ()) -- rollback
  }

-- Run saga
runSaga :: Monad m => [SagaStep m a] -> m (Either Error [a])
runSaga steps = go [] steps
  where
    go completed [] = return (Right (reverse completed))
    go completed (step:remaining) = do
      result <- action step
      case result of
        Left err -> do
          -- Compensate in reverse order
          mapM_ (\(s, r) -> compensation s r) (zip steps (reverse completed))
          return (Left err)
        Right a -> go (a:completed) remaining

-- Order saga implementation
orderSaga :: OrderId -> CreateOrderSaga
orderSaga oid = runSaga
  [ SagaStep
      { action = createOrderStep oid
      , compensation = cancelOrder
      }
  , SagaStep
      { action = reserveInventoryStep oid
      , compensation = releaseReservation
      }
  , SagaStep
      { action = processPaymentStep oid
      , compensation = refundPayment
      }
  , SagaStep
      { action = confirmOrderStep oid
      , compensation = \_ -> return (Right ())  -- terminal, no compensation
      }
  ]
```

---

## ขั้นตอนที่ 432: Functional Domain Modeling

```haskell
-- Functional Domain Modeling: Types as Documentation

-- State machine for Order
data OrderState
  = Pending
  | Confirmed  ConfirmationDetails
  | Shipped    ShippingDetails
  | Delivered  DeliveryDetails
  | Cancelled  CancellationReason

-- Type-safe state transitions
confirmOrder :: Order Pending -> ConfirmationDetails -> Order Confirmed
confirmOrder order details = order { orderState = Confirmed details }

shipOrder :: Order Confirmed -> ShippingDetails -> Order Shipped
shipOrder order details = order { orderState = Shipped details }

-- Can't ship a cancelled order (type error)
-- shipOrder :: Order Cancelled -> ... doesn't type check

-- Smart constructors with validation
data Email' = Email' Text  -- private constructor

mkEmail' :: Text -> Validated Email'
mkEmail' raw
  | isValidEmail raw = Valid (Email' raw)
  | otherwise        = Invalid ["Invalid email format"]

-- Railway-oriented programming
(&&&) :: Validated a -> (a -> Validated b) -> Validated b
(Valid a)   &&& f = f a
(Invalid e) &&& _ = Invalid e

validateRegistration :: Text -> Text -> Text -> Validated Registration
validateRegistration email username password =
  mkEmail' email
  &&& \e -> mkUsername' username
  >>= \u -> mkPassword' password
  >>= \p -> Valid (Registration e u p)
```

---

## ขั้นตอนที่ 433: Observability

```haskell
-- Distributed tracing ด้วย OpenTelemetry

import OpenTelemetry.Trace

-- Span creation
withSpan :: Text -> [(Text, AttributeValue)] -> IO a -> IO a
withSpan spanName attributes action = do
  tracer <- getTracer "my-app"
  inSpan tracer spanName (defaultSpanArguments { attributes = Map.fromList attributes }) $ \span -> do
    action

-- Trace an HTTP request
traceRequest :: Request -> IO Response -> IO Response
traceRequest req action = withSpan "http.request"
  [ ("http.method", toAttribute (decodeUtf8 (requestMethod req)))
  , ("http.url",    toAttribute (decodeUtf8 (rawPathInfo req)))
  ] action

-- Metrics with counters and histograms
data Telemetry = Telemetry
  { telRequestCount    :: Counter
  , telRequestDuration :: Histogram
  , telActiveConns     :: UpDownCounter
  , telErrors          :: Counter
  }

recordRequest :: Telemetry -> Text -> Int -> Double -> IO ()
recordRequest tel path status duration = do
  add (telRequestCount tel) 1
    [ ("http.route",  path)
    , ("http.status", T.pack (show status))
    ]
  record (telRequestDuration tel) duration
    [ ("http.route", path) ]
  when (status >= 500) $
    add (telErrors tel) 1 [("http.route", path)]

-- Correlation IDs
withCorrelationId :: Text -> IO a -> IO a
withCorrelationId corrId action = do
  ctx <- Context.current
  let newCtx = Context.setValue corrIdKey corrId ctx
  Context.with newCtx action

-- Structured log with trace context
logWithTrace :: Text -> LogLevel -> Text -> IO ()
logWithTrace component level msg = do
  span <- getCurrentSpan
  let traceId = maybe "" showTraceId (spanTraceId span)
      spanId  = maybe "" showSpanId  (spanId span)
  logJSON $ object
    [ "message"   .= msg
    , "level"     .= show level
    , "component" .= component
    , "trace_id"  .= traceId
    , "span_id"   .= spanId
    ]
```

---

## ขั้นตอนที่ 434: Configuration as Code

```haskell
-- Infrastructure as Code ด้วย Haskell

import Pulumi  -- หรือ custom DSL

-- Define infrastructure
infrastructure :: Resource ()
infrastructure = do
  -- VPC
  vpc <- newVpc "main" VpcConfig
    { vpcCidr = "10.0.0.0/16"
    , vpcEnableDns = True
    }
  
  -- Subnets
  publicSubnet <- newSubnet "public" SubnetConfig
    { subnetVpcId = vpcId vpc
    , subnetCidr  = "10.0.1.0/24"
    , subnetPublic = True
    }
  
  privateSubnet <- newSubnet "private" SubnetConfig
    { subnetVpcId = vpcId vpc
    , subnetCidr  = "10.0.2.0/24"
    , subnetPublic = False
    }
  
  -- RDS
  db <- newRdsInstance "postgres" RdsConfig
    { rdsEngine        = Postgres14
    , rdsInstanceClass = "db.t3.micro"
    , rdsMultiAz       = True
    , rdsSubnetGroup   = [privateSubnet]
    }
  
  -- ECS
  cluster <- newEcsCluster "app" EcsConfig
    { ecsVpc     = vpc
    , ecsSubnets = [privateSubnet]
    }
  
  service <- newEcsService "api" EcsServiceConfig
    { ecsCluster     = cluster
    , ecsImage       = "myregistry/myapp:latest"
    , ecsDesiredCount = 3
    , ecsCpu         = 256
    , ecsMemory      = 512
    }
  
  return ()
```

---

## ขั้นตอนที่ 435: Testing Strategies

```haskell
-- Testing pyramid ใน Haskell

-- 1. Unit tests
unit_spec :: Spec
unit_spec = do
  describe "Domain.User" $ do
    it "validates email" $ do
      mkEmail "valid@example.com" `shouldSatisfy` isRight
      mkEmail "invalid-email"     `shouldSatisfy` isLeft
    
    it "validates username" $ do
      mkUsername "alice"     `shouldSatisfy` isRight
      mkUsername "ab"        `shouldSatisfy` isLeft  -- too short
      mkUsername "toolongusername12345678901234" `shouldSatisfy` isLeft

-- 2. Integration tests (with real DB)
integration_spec :: Spec
integration_spec = do
  pool <- runIO (createTestPool)
  
  describe "UserRepository" $ do
    it "creates and retrieves user" $ do
      uid <- runSqlPool (insert testUser) pool
      mUser <- runSqlPool (get uid) pool
      fmap userName mUser `shouldBe` Just "Alice"

-- 3. Contract tests
contract_spec :: Spec
contract_spec = do
  describe "API contracts" $ do
    it "GET /users returns expected schema" $ do
      response <- getUsers
      response `shouldMatchSchema` usersResponseSchema
    
    it "POST /users validates input" $ do
      response <- createUser invalidInput
      responseStatus response `shouldBe` 400

-- 4. Property tests
property_spec :: Spec
property_spec = do
  describe "Serialization" $ do
    prop "JSON round-trip for User" $ \user ->
      (decode (encode user) :: Maybe User) == Just user
    
    prop "Pagination is correct" $ \(Positive n) ->
      let pages = paginate n 10
      in all (\p -> length p <= 10) pages

-- 5. Snapshot tests
snapshot_spec :: Spec
snapshot_spec = do
  describe "Response bodies" $ do
    it "matches user response snapshot" $ do
      response <- getUser 1
      matchSnapshot "user_response" (encode response)
```

---

## ขั้นตอนที่ 436: API Gateway Pattern

```haskell
-- API Gateway ด้วย Servant

type GatewayAPI =
  -- Route to User Service
  "users"  :> Raw
  -- Route to Post Service
  :<|> "posts" :> Raw
  -- Route to Search Service
  :<|> "search" :> Raw
  -- Direct gateway routes
  :<|> "health" :> Get '[JSON] HealthStatus

-- Gateway server
gatewayServer :: GatewayConfig -> Server GatewayAPI
gatewayServer cfg =
  proxyTo (userServiceUrl cfg)
  :<|> proxyTo (postServiceUrl cfg)
  :<|> proxyTo (searchServiceUrl cfg)
  :<|> healthHandler

proxyTo :: Text -> Tagged Handler Application
proxyTo url = Tagged $ \req respond -> do
  -- Forward request to upstream service
  manager <- newManager defaultManagerSettings
  upstreamReq <- parseRequest (T.unpack url <> rawPathInfo req)
  let proxyReq = upstreamReq
        { requestHeaders = requestHeaders req
        , requestBody    = requestBody req
        , method         = requestMethod req
        }
  response <- httpLbs proxyReq manager
  respond (responseLBS
    (responseStatus response)
    (Map.toList (responseHeaders response))
    (responseBody response))

-- Rate limiting per route
rateLimitedGateway :: RateLimiter -> Middleware
rateLimitedGateway limiter app req respond = do
  let key = decodeUtf8 (rawPathInfo req) <> ":" <> clientIp req
  allowed <- checkRateLimit limiter key
  if allowed
    then app req respond
    else respond (responseLBS status429 [] "Rate limit exceeded")
```

---

## ขั้นตอนที่ 437: Service Mesh Integration

```haskell
-- Service Mesh patterns ใน Haskell

-- Service discovery
class ServiceDiscovery m where
  register   :: ServiceInfo -> m ()
  deregister :: ServiceId -> m ()
  discover   :: ServiceName -> m [ServiceInstance]
  watch      :: ServiceName -> (ServiceChange -> m ()) -> m ()

-- Health check registration
registerWithConsul :: ConsulConfig -> ServiceInfo -> IO ()
registerWithConsul cfg info = do
  let endpoint = consulEndpoint cfg <> "/v1/agent/service/register"
  httpPut endpoint (encode info)

-- Load balancing
data LoadBalancer = RoundRobin | Random | LeastConnections

selectInstance :: LoadBalancer -> [ServiceInstance] -> IO ServiceInstance
selectInstance RoundRobin instances = do
  counter <- readIORef roundRobinCounter
  modifyIORef' roundRobinCounter (+1)
  return (instances !! (counter `mod` length instances))

selectInstance Random instances = do
  idx <- randomRIO (0, length instances - 1)
  return (instances !! idx)

-- Retry logic
withRetry :: RetryPolicy -> IO a -> IO (Either RetryError a)
withRetry policy action = go (maxRetries policy) []
  where
    go 0 errs = return (Left (AllRetriesFailed errs))
    go n errs = do
      result <- E.try action
      case result of
        Right a -> return (Right a)
        Left err -> do
          let delay = calcDelay policy n
          threadDelay delay
          go (n-1) (err:errs)

exponentialBackoff :: Int -> RetryPolicy
exponentialBackoff baseMs = RetryPolicy
  { maxRetries = 5
  , calcDelay  = \attempt -> baseMs * 2^attempt * 1000
  , shouldRetry = \_ -> True
  }
```

---

## ขั้นตอนที่ 438: Data Pipeline

```haskell
-- ETL/Data Pipeline ด้วย Haskell

import Conduit

-- Extract: read data from source
extractFromDb :: ConnectionPool -> Source IO Row
extractFromDb pool = do
  rows <- liftIO $ runSqlPool (selectList [] [Asc RowId]) pool
  mapM_ yield rows

-- Transform: process data
transformRow :: Conduit Row IO ProcessedRow
transformRow = mapMC $ \row -> do
  let cleaned = cleanData row
  enriched <- enrichWithExternalData cleaned
  return enriched

cleanData :: Row -> Row
cleanData row = row
  { rowName  = T.strip (rowName row)
  , rowEmail = T.toLower (rowEmail row)
  }

enrichWithExternalData :: Row -> IO ProcessedRow
enrichWithExternalData row = do
  geo <- lookupGeolocation (rowIpAddress row)
  return ProcessedRow
    { prRow    = row
    , prGeo    = geo
    , prScored = computeScore row
    }

-- Load: write to destination
loadToWarehouse :: Sink ProcessedRow IO ()
loadToWarehouse = mapM_C $ \row -> do
  insertIntoWarehouse row

-- Run pipeline
runPipeline :: ConnectionPool -> IO PipelineStats
runPipeline pool = do
  counter <- newIORef 0
  errors  <- newIORef []
  
  runConduit $
    extractFromDb pool
    .| transformRow
    .| filterC isValid
    .| tapC (\_ -> modifyIORef' counter (+1))
    .| catchC (loadToWarehouse) (\e -> modifyIORef' errors (e:))
  
  count <- readIORef counter
  errs  <- readIORef errors
  return PipelineStats
    { psProcessed = count
    , psErrors    = length errs
    , psErrorList = errs
    }
```

---

## ขั้นตอนที่ 439: Machine Learning Integration

```haskell
-- ML model integration ใน Haskell

-- Call Python/ML service via HTTP
data PredictionRequest = PredictionRequest
  { prFeatures :: [Double]
  } deriving (Generic, ToJSON)

data PredictionResponse = PredictionResponse
  { prPrediction  :: Text
  , prConfidence  :: Double
  , prProbabilities :: Map Text Double
  } deriving (Generic, FromJSON)

classify :: Text -> Handler PredictionResponse
classify text = do
  let features = extractFeatures text
  manager <- liftIO (newManager defaultManagerSettings)
  req <- parseRequest "http://ml-service:5000/predict"
  let body = RequestBodyLBS (encode (PredictionRequest features))
  let reqWithBody = req
        { method = "POST"
        , requestBody = body
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- liftIO $ httpLbs reqWithBody manager
  case decode (responseBody resp) of
    Nothing  -> throwError err500
    Just pred -> return pred

-- Feature extraction
extractFeatures :: Text -> [Double]
extractFeatures text =
  [ fromIntegral (T.length text)
  , fromIntegral (wordCount text)
  , sentimentScore text
  , readabilityScore text
  ]

-- Recommendation system
data RecommendRequest = RecommendRequest
  { rrUserId :: UserId
  , rrLimit  :: Int
  }

getRecommendations :: UserId -> Handler [Post]
getRecommendations uid = do
  -- Get user history
  history <- runDB $ selectList [ViewUserId ==. uid] [Desc ViewAt, LimitTo 10]
  let postIds = map (viewPostId . entityVal) history
  
  -- Call recommendation service
  recs <- callRecommendService (RecommendRequest uid 10)
  
  -- Fetch posts
  posts <- runDB $ selectList [PostId `in_` recs] []
  return (map entityVal posts)
```

---

## ขั้นตอนที่ 440: โปรเจกต์: E-Commerce Platform

```haskell
-- Complete E-Commerce platform architecture

-- Domain Model
module Commerce.Domain where

-- Products
data Product = Product
  { productId       :: ProductId
  , productName     :: Text
  , productPrice    :: Money
  , productStock    :: Quantity
  , productCategory :: CategoryId
  , productImages   :: [ImageUrl]
  , productStatus   :: ProductStatus
  }

data ProductStatus = Active | Inactive | OutOfStock

-- Orders
data Order = Order
  { orderId      :: OrderId
  , orderUser    :: UserId
  , orderItems   :: NonEmpty OrderItem
  , orderStatus  :: OrderStatus
  , orderTotal   :: Money
  , orderCreated :: UTCTime
  }

data OrderStatus
  = Cart
  | Pending Payment
  | Processing
  | Shipped Tracking
  | Delivered
  | Cancelled CancellationReason
  | Refunded RefundDetails

data OrderItem = OrderItem
  { itemProduct  :: ProductId
  , itemQuantity :: Quantity
  , itemPrice    :: Money  -- price at time of order
  }

-- API routes
type CommerceAPI =
  -- Public
  "products" :> ProductsAPI
  :<|> "categories" :> CategoriesAPI
  :<|> "search" :> SearchAPI
  -- Authenticated
  :<|> Auth '[JWT] UserClaims :>
    ( "cart"     :> CartAPI
    :<|> "orders"  :> OrdersAPI
    :<|> "account" :> AccountAPI
    )
  -- Admin
  :<|> Auth '[JWT] AdminClaims :>
    ( "admin" :> AdminAPI )

-- Services
class Monad m => ProductService m where
  listProducts    :: ProductFilter -> Pagination -> m [Product]
  getProduct      :: ProductId -> m (Maybe Product)
  searchProducts  :: Text -> SearchOptions -> m SearchResults
  checkStock      :: ProductId -> Quantity -> m StockStatus

class Monad m => OrderService m where
  createCart      :: UserId -> m Cart
  addToCart       :: CartId -> ProductId -> Quantity -> m (Either CartError Cart)
  removeFromCart  :: CartId -> ProductId -> m (Either CartError Cart)
  checkout        :: CartId -> CheckoutDetails -> m (Either CheckoutError Order)
  getOrders       :: UserId -> Pagination -> m [Order]
  cancelOrder     :: OrderId -> UserId -> m (Either CancelError Order)

-- Application
checkoutUseCase
  :: (OrderService m, ProductService m, PaymentPort m, NotificationPort m)
  => UserId -> CartId -> PaymentDetails -> ShippingAddress
  -> m (Either CheckoutError OrderConfirmation)
checkoutUseCase userId cartId payment shipping = do
  -- Validate cart
  cart <- getCart cartId
  when (cartUserId cart /= userId) $ return (Left UnauthorizedCart)
  
  -- Check stock
  stockResults <- mapM (\item -> checkStock (itemProductId item) (itemQuantity item))
    (cartItems cart)
  
  let outOfStock = filter ((== OutOfStock) . fst) (zip stockResults (cartItems cart))
  unless (null outOfStock) $
    return (Left (ItemsOutOfStock (map snd outOfStock)))
  
  -- Calculate total
  let total = sum [itemPrice item * fromIntegral (itemQuantity item) | item <- cartItems cart]
  
  -- Process payment
  payResult <- processPayment total payment
  case payResult of
    Left err -> return (Left (PaymentFailed err))
    Right txId -> do
      -- Create order
      order <- createOrderFromCart cart shipping txId
      
      -- Send confirmation
      sendOrderConfirmation (userId) order
      
      return (Right (OrderConfirmation (orderId order) txId total))
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 23 เราจะเรียน **Advanced Concurrency และ Distributed Systems**:
- STM ขั้นสูง
- Actor Model ด้วย Distributed Haskell
- Consensus algorithms
- Distributed locks

---

*[← Part 21](part-21.md) | [Part 23 →](part-23.md)*
