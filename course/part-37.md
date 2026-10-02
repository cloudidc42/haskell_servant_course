# Part 37: Functional Patterns in Production
## ขั้นตอนที่ 721-740

---

## ขั้นตอนที่ 721: Domain Modeling with Types

```haskell
-- Rich domain model ด้วย type system

-- Make illegal states unrepresentable
newtype Email = Email { getEmail :: Text }
  deriving (Show, Eq, Ord)

mkEmail :: Text -> Either Text Email
mkEmail t
  | "@" `T.isInfixOf` t && "." `T.isInfixOf` t = Right (Email t)
  | otherwise = Left ("Invalid email: " <> t)

newtype PositiveInt = PositiveInt { getPositiveInt :: Int }
  deriving (Show, Eq, Ord)

mkPositiveInt :: Int -> Either Text PositiveInt
mkPositiveInt n
  | n > 0     = Right (PositiveInt n)
  | otherwise = Left ("Must be positive: " <> T.pack (show n))

-- Payment domain
data PaymentStatus
  = Pending
  | Processing { processingId :: TransactionId }
  | Completed  { transactionId :: TransactionId, completedAt :: UTCTime }
  | Failed     { failureReason :: Text, failedAt :: UTCTime }
  | Refunded   { refundId :: TransactionId, refundedAt :: UTCTime }

-- Business rules in types
data Order
  = DraftOrder     { draftItems :: [OrderItem] }
  | ConfirmedOrder { items :: [OrderItem], confirmedAt :: UTCTime }
  | PaidOrder      { items :: [OrderItem], payment :: Payment, paidAt :: UTCTime }
  | ShippedOrder   { items :: [OrderItem], payment :: Payment, tracking :: TrackingCode }

-- Only confirmed orders can be paid
payOrder :: ConfirmedOrder -> Payment -> IO PaidOrder
payOrder order payment = do
  result <- processPayment payment
  now    <- getCurrentTime
  return PaidOrder
    { items    = confirmedItems order
    , payment  = payment
    , paidAt   = now
    }
```

---

## ขั้นตอนที่ 722: Event-Driven Architecture

```haskell
-- Event-driven domain design

-- Domain events
data DomainEvent
  = UserRegistered   { ureUserId :: UserId, ureEmail :: Email, ureAt :: UTCTime }
  | OrderPlaced      { opOrderId :: OrderId, opItems :: [OrderItem], opAt :: UTCTime }
  | PaymentReceived  { prOrderId :: OrderId, prAmount :: Money, prAt :: UTCTime }
  | ItemShipped      { isOrderId :: OrderId, isTracking :: Text, isAt :: UTCTime }
  | PasswordReset    { prUserId :: UserId, prAt :: UTCTime }

-- Event store
class EventStore m where
  appendEvent   :: DomainEvent -> m EventId
  getEvents     :: StreamId -> m [DomainEvent]
  subscribeToEvents :: (DomainEvent -> m ()) -> m ()

-- Event handlers
handleUserRegistered :: (MonadEmail m, MonadDB m) => DomainEvent -> m ()
handleUserRegistered (UserRegistered uid email _) = do
  sendWelcomeEmail email
  createUserProfile uid
handleUserRegistered _ = return ()

handleOrderPlaced :: (MonadNotify m, MonadInventory m) => DomainEvent -> m ()
handleOrderPlaced (OrderPlaced orderId items _) = do
  reserveInventory items
  notifyWarehouse orderId
handleOrderPlaced _ = return ()

-- Event bus
data EventBus = EventBus
  { ebPublish    :: DomainEvent -> IO ()
  , ebSubscribe  :: [EventType] -> (DomainEvent -> IO ()) -> IO ()
  }

-- Event sourcing: rebuild state from events
rebuildUserState :: [DomainEvent] -> Maybe User
rebuildUserState = foldl' apply Nothing
  where
    apply _ (UserRegistered uid email ts) = Just (User uid email ts Active)
    apply (Just u) (PasswordReset _ ts)   = Just u { userUpdatedAt = ts }
    apply state _                         = state
```

---

## ขั้นตอนที่ 723: CQRS Pattern

```haskell
-- Command Query Responsibility Segregation

-- Commands (write side)
data Command
  = RegisterUser  { ruEmail :: Email, ruPassword :: Text }
  | PlaceOrder    { poUserId :: UserId, poItems :: [OrderItem] }
  | CancelOrder   { coOrderId :: OrderId, coReason :: Text }
  | UpdateProfile { upUserId :: UserId, upName :: Text }

-- Command handlers
class CommandHandler m where
  handleCommand :: Command -> m (Either DomainError [DomainEvent])

-- Queries (read side — optimized read models)
data Query
  = GetUser     { guUserId :: UserId }
  | ListOrders  { loUserId :: UserId, loStatus :: Maybe OrderStatus }
  | SearchUsers { suQuery :: Text, suLimit :: Int, suOffset :: Int }

-- Query handlers (read from denormalized views)
class QueryHandler m where
  handleQuery :: Query -> m QueryResult

-- Command validation
validateCommand :: Command -> Validation [ValidationError] Command
validateCommand (RegisterUser email pass) = RegisterUser
  <$> validateEmail email
  <*> validatePassword pass
validateCommand cmd = Success cmd

-- Write model aggregate
data UserAggregate = UserAggregate
  { uaId       :: UserId
  , uaEmail    :: Email
  , uaVersion  :: Int
  , uaEvents   :: [DomainEvent]
  }

applyCommand :: UserAggregate -> Command -> Either DomainError UserAggregate
applyCommand agg (UpdateProfile uid name)
  | uaId agg /= uid = Left (UnauthorizedError "Wrong user")
  | otherwise = Right agg
      { uaEvents = ProfileUpdated uid name Now : uaEvents agg }
applyCommand agg _ = Right agg
```

---

## ขั้นตอนที่ 724: Hexagonal Architecture

```haskell
-- Hexagonal (Ports and Adapters) Architecture

-- Core domain (no external dependencies)
module Domain.User where

data User = User
  { userId   :: UserId
  , userEmail :: Email
  , userName  :: Text
  }

data UserError
  = UserNotFound UserId
  | DuplicateEmail Email
  | ValidationError [Text]

-- Port: what the domain needs
class UserRepository m where
  findUser   :: UserId -> m (Maybe User)
  saveUser   :: User -> m (Either UserError ())
  deleteUser :: UserId -> m Bool
  findByEmail :: Email -> m (Maybe User)

-- Domain service
registerUser
  :: (UserRepository m, MonadIO m)
  => Email -> Text -> m (Either UserError User)
registerUser email name = do
  existing <- findByEmail email
  case existing of
    Just _  -> return (Left (DuplicateEmail email))
    Nothing -> do
      uid  <- generateUserId
      let user = User uid email name
      result <- saveUser user
      return (result $> user)

-- Adapter: PostgreSQL implementation
module Adapter.Postgres.User where

instance UserRepository (ReaderT ConnectionPool IO) where
  findUser uid = ask >>= \pool ->
    runSqlPool (selectFirst [UserId ==. uid] []) pool
    >>= return . fmap entityToUser
  
  saveUser user = ask >>= \pool ->
    E.try @SomeException (runSqlPool (insert_ (userToEntity user)) pool)
    >>= return . either (Left . toError) Right
  
  findByEmail email = ask >>= \pool ->
    runSqlPool (selectFirst [UserEmail ==. email] []) pool
    >>= return . fmap entityToUser
```

---

## ขั้นตอนที่ 725: Error Handling Patterns

```haskell
-- Comprehensive error handling

-- Error hierarchy
data AppError
  = NotFound        { nfEntity :: Text, nfId :: Text }
  | Unauthorized    { uaMessage :: Text }
  | Forbidden       { fbMessage :: Text }
  | ValidationError { veErrors :: [FieldError] }
  | ConflictError   { ceMessage :: Text }
  | ServiceError    { seService :: Text, seMessage :: Text }
  | InternalError   { ieMessage :: Text, ieCause :: SomeException }

data FieldError = FieldError
  { feField   :: Text
  , feMessage :: Text
  }

-- Error conversion
toHttpStatus :: AppError -> Status
toHttpStatus NotFound {}       = status404
toHttpStatus Unauthorized {}   = status401
toHttpStatus Forbidden {}      = status403
toHttpStatus ValidationError{} = status422
toHttpStatus ConflictError {}  = status409
toHttpStatus ServiceError {}   = status502
toHttpStatus InternalError {}  = status500

-- Error accumulation with Validation
data Validation e a = Failure e | Success a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure e <*> Failure f = Failure (e <> f)  -- accumulates!
  Failure e <*> _         = Failure e
  _ <*> Failure e         = Failure e

-- Validate form data
validateForm :: CreateUserForm -> Validation [FieldError] CreateUserCmd
validateForm form = CreateUserCmd
  <$> validateField "email" (validateEmail (formEmail form))
  <*> validateField "name"  (validateName  (formName form))
  <*> validateField "age"   (validateAge   (formAge form))

validateField :: Text -> Either Text a -> Validation [FieldError] a
validateField field (Left err) = Failure [FieldError field err]
validateField _ (Right v)      = Success v
```

---

## ขั้นตอนที่ 726: Repository Pattern

```haskell
-- Repository pattern ที่สะอาดและ testable

-- Generic repository
class Monad m => Repository m entity where
  type EntityId entity :: *
  
  find   :: EntityId entity -> m (Maybe entity)
  save   :: entity -> m (EntityId entity)
  update :: EntityId entity -> entity -> m Bool
  delete :: EntityId entity -> m Bool
  
  -- Query extensions
  findWhere :: [FilterExpr entity] -> PaginationArgs -> m [entity]
  count     :: [FilterExpr entity] -> m Int

-- Filter expression DSL
data FilterExpr entity where
  Eq  :: (entity -> a) -> a -> FilterExpr entity
  Gt  :: Ord a => (entity -> a) -> a -> FilterExpr entity
  Lt  :: Ord a => (entity -> a) -> a -> FilterExpr entity
  In  :: (entity -> a) -> [a] -> FilterExpr entity
  And :: FilterExpr entity -> FilterExpr entity -> FilterExpr entity
  Or  :: FilterExpr entity -> FilterExpr entity -> FilterExpr entity
  Not :: FilterExpr entity -> FilterExpr entity

-- Apply filter in-memory (for testing)
applyFilter :: FilterExpr entity -> entity -> Bool
applyFilter (Eq f v) e     = f e == v
applyFilter (Gt f v) e     = f e > v
applyFilter (Lt f v) e     = f e < v
applyFilter (In f vs) e    = f e `elem` vs
applyFilter (And a b) e    = applyFilter a e && applyFilter b e
applyFilter (Or a b) e     = applyFilter a e || applyFilter b e
applyFilter (Not a) e      = not (applyFilter a e)

-- In-memory repository for tests
data InMemoryRepo entity = InMemoryRepo
  { imrData    :: TVar (Map (EntityId entity) entity)
  , imrCounter :: TVar Int
  }

instance (Ord (EntityId entity), Enum (EntityId entity))
  => Repository (ReaderT (InMemoryRepo entity) IO) entity where
  find uid = do
    repo <- ask
    fmap (Map.lookup uid) (liftIO (readTVarIO (imrData repo)))
  save entity = do
    repo <- ask
    liftIO $ atomically $ do
      n <- readTVar (imrCounter repo)
      let uid = toEnum n
      modifyTVar (imrData repo) (Map.insert uid entity)
      writeTVar  (imrCounter repo) (n + 1)
      return uid
```

---

## ขั้นตอนที่ 727: Saga Pattern

```haskell
-- Saga pattern สำหรับ distributed transactions

data SagaStep a = SagaStep
  { ssAction      :: a -> IO (Either SagaError SagaState)
  , ssCompensation :: SagaState -> IO ()
  , ssName        :: Text
  }

data Saga a = Saga
  { sagaId    :: SagaId
  , sagaSteps :: [SagaStep a]
  }

-- Execute saga with automatic rollback
executeSaga :: Saga a -> a -> IO (Either SagaError ())
executeSaga saga input = go [] (sagaSteps saga)
  where
    go completed [] = return (Right ())
    go completed (step : remaining) = do
      logInfo ("Executing step: " <> ssName step)
      result <- ssAction step input
      case result of
        Left err -> do
          logError ("Step failed: " <> T.pack (show err))
          -- Compensate all completed steps in reverse
          mapM_ (compensate) (reverse completed)
          return (Left err)
        Right state -> go ((step, state) : completed) remaining
    
    compensate (step, state) = do
      logInfo ("Compensating: " <> ssName step)
      E.catch (ssCompensation step state)
              (\e -> logError ("Compensation failed: " <> T.pack (show e)))

-- Order fulfillment saga
orderSaga :: Saga OrderRequest
orderSaga = Saga
  { sagaId    = "order-fulfillment"
  , sagaSteps =
      [ SagaStep reserveInventory  cancelReservation "reserve_inventory"
      , SagaStep chargePayment     refundPayment     "charge_payment"
      , SagaStep updateOrder       cancelOrder       "update_order"
      , SagaStep notifyWarehouse   (\_ -> return ()) "notify_warehouse"
      ]
  }
```

---

## ขั้นตอนที่ 728: Circuit Breaker Pattern

```haskell
-- Circuit breaker implementation

data CircuitState = Closed | Open { openUntil :: UTCTime } | HalfOpen

data CircuitBreaker = CircuitBreaker
  { cbName         :: Text
  , cbState        :: TVar CircuitState
  , cbFailures     :: TVar Int
  , cbSuccesses    :: TVar Int
  , cbThreshold    :: Int
  , cbResetTimeout :: NominalDiffTime
  }

newCircuitBreaker :: Text -> Int -> NominalDiffTime -> IO CircuitBreaker
newCircuitBreaker name threshold timeout = CircuitBreaker name
  <$> newTVarIO Closed
  <*> newTVarIO 0
  <*> newTVarIO 0
  <*> pure threshold
  <*> pure timeout

-- Execute with circuit breaker
callWithBreaker :: CircuitBreaker -> IO a -> IO (Either CircuitBreakerError a)
callWithBreaker cb action = do
  now   <- getCurrentTime
  state <- readTVarIO (cbState cb)
  
  case state of
    Open until -> if now > until
      then transitionToHalfOpen cb >> executeHalfOpen cb action
      else return (Left CircuitOpen)
    
    HalfOpen -> executeHalfOpen cb action
    
    Closed -> do
      result <- E.try @SomeException action
      case result of
        Left err -> do
          recordFailure cb
          return (Left (UnderlyingError err))
        Right val -> do
          recordSuccess cb
          return (Right val)

recordFailure :: CircuitBreaker -> IO ()
recordFailure cb = do
  n <- atomically $ do
    n <- stateTVar (cbFailures cb) (\f -> (f+1, f+1))
    writeTVar (cbSuccesses cb) 0
    return n
  
  when (n >= cbThreshold cb) $ do
    now <- getCurrentTime
    atomically $ writeTVar (cbState cb)
      (Open (addUTCTime (cbResetTimeout cb) now))
```

---

## ขั้นตอนที่ 729: Retry Pattern

```haskell
-- Retry with exponential backoff and jitter

data RetryConfig = RetryConfig
  { rcMaxAttempts :: Int
  , rcBaseDelay   :: Int   -- microseconds
  , rcMaxDelay    :: Int   -- microseconds
  , rcJitter      :: Bool
  }

defaultRetry :: RetryConfig
defaultRetry = RetryConfig 3 100000 30000000 True  -- 0.1s base, 30s max

-- Retry action
withRetry :: RetryConfig -> IO a -> IO a
withRetry cfg action = go 0
  where
    go attempt = do
      result <- E.try @SomeException action
      case result of
        Right val -> return val
        Left err
          | attempt >= rcMaxAttempts cfg - 1 -> throwIO err
          | otherwise -> do
              delay <- computeDelay cfg attempt
              logInfo ("Attempt " <> show attempt <> " failed, retrying in " <> show delay <> "μs")
              threadDelay delay
              go (attempt + 1)

computeDelay :: RetryConfig -> Int -> IO Int
computeDelay cfg attempt = do
  let exponential = rcBaseDelay cfg * (2 ^ attempt)
  let capped      = min exponential (rcMaxDelay cfg)
  if rcJitter cfg
    then do
      jitter <- randomRIO (0, capped `div` 2)
      return (capped + jitter)
    else return capped

-- Retry specific errors
withRetryOn :: Exception e => RetryConfig -> (e -> Bool) -> IO a -> IO a
withRetryOn cfg shouldRetry action = go 0
  where
    go attempt = do
      result <- E.try action
      case result of
        Right val -> return val
        Left err
          | shouldRetry err && attempt < rcMaxAttempts cfg - 1 -> do
              delay <- computeDelay cfg attempt
              threadDelay delay
              go (attempt + 1)
          | otherwise -> throwIO err
```

---

## ขั้นตอนที่ 730: Bulkhead Pattern

```haskell
-- Bulkhead: isolate failures

data Bulkhead = Bulkhead
  { bhSemaphore :: QSem
  , bhQueue     :: TBQueue (IO ())
  , bhMaxQueue  :: Int
  }

newBulkhead :: Int -> Int -> IO Bulkhead
newBulkhead maxConcurrent maxQueue = Bulkhead
  <$> newQSem maxConcurrent
  <*> newTBQueueIO (fromIntegral maxQueue)
  <*> pure maxQueue

-- Execute within bulkhead
withBulkhead :: Bulkhead -> IO a -> IO (Either BulkheadError a)
withBulkhead bh action = do
  queueSize <- atomically $ lengthTBQueue (bhQueue bh)
  
  if queueSize >= bhMaxQueue bh
    then return (Left BulkheadFull)
    else do
      resultVar <- newEmptyTMVarIO
      let wrapped = do
            waitQSem (bhSemaphore bh)
            result <- E.try @SomeException action
            atomically (putTMVar resultVar result)
            signalQSem (bhSemaphore bh)
      
      atomically $ writeTBQueue (bhQueue bh) wrapped
      forkIO wrapped  -- start processing
      
      result <- atomically (takeTMVar resultVar)
      case result of
        Left err  -> return (Left (BulkheadError err))
        Right val -> return (Right val)

-- Worker pool
runWorkers :: Bulkhead -> Int -> IO ()
runWorkers bh n = replicateM_ n $ forkIO $ forever $ do
  action <- atomically (readTBQueue (bhQueue bh))
  action
```

---

## ขั้นตอนที่ 731: Observer Pattern

```haskell
-- Observer / publish-subscribe pattern

data Subject a = Subject
  { sObservers :: TVar [Observer a]
  , sValue     :: TVar a
  }

newtype Observer a = Observer { notify :: a -> IO () }

newSubject :: a -> IO (Subject a)
newSubject initial = Subject
  <$> newTVarIO []
  <*> newTVarIO initial

subscribe :: Subject a -> Observer a -> IO ()
subscribe subject observer =
  atomically $ modifyTVar (sObservers subject) (observer :)

unsubscribe :: Subject a -> Observer a -> IO ()
unsubscribe subject observer =
  atomically $ modifyTVar (sObservers subject) (filter ((/= observer)))

-- Update and notify
updateValue :: Subject a -> a -> IO ()
updateValue subject newVal = do
  (oldVal, observers) <- atomically $ do
    oldVal <- readTVar (sValue subject)
    writeTVar (sValue subject) newVal
    observers <- readTVar (sObservers subject)
    return (oldVal, observers)
  
  forM_ observers $ \obs ->
    forkIO (notify obs newVal)

-- Reactive variables
reactive :: a -> [Observer a] -> IO (Subject a)
reactive initial observers = do
  subject <- newSubject initial
  mapM_ (subscribe subject) observers
  return subject

-- Derived computations
derived :: Subject a -> (a -> b) -> IO (Subject b)
derived source transform = do
  initial    <- readTVarIO (sValue source)
  derived'   <- newSubject (transform initial)
  subscribe source (Observer (\a -> updateValue derived' (transform a)))
  return derived'
```

---

## ขั้นตอนที่ 732: Strategy Pattern

```haskell
-- Strategy pattern ด้วย type classes

-- Sorting strategies
class SortStrategy s where
  sortWith :: Ord a => s -> [a] -> [a]

data QuickSort = QuickSort
data MergeSort = MergeSort
data BubbleSort = BubbleSort

instance SortStrategy QuickSort where
  sortWith _ []     = []
  sortWith _ [x]    = [x]
  sortWith s (x:xs) = sortWith s smaller ++ [x] ++ sortWith s larger
    where smaller = filter (<= x) xs
          larger  = filter (> x) xs

instance SortStrategy MergeSort where
  sortWith _ []  = []
  sortWith _ [x] = [x]
  sortWith s xs  = merge (sortWith s left) (sortWith s right)
    where (left, right) = splitAt (length xs `div` 2) xs
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
                                           else y : merge (x:xs) ys

-- Caching strategies
class CacheStrategy s k v where
  cacheGet  :: s k v -> k -> IO (Maybe v)
  cachePut  :: s k v -> k -> v -> IO ()
  cacheEvict :: s k v -> IO ()

data LRUCache k v = LRUCache { lruMap :: TVar (Map k (v, Int)), lruMax :: Int }
data TTLCache k v = TTLCache { ttlMap :: TVar (Map k (v, UTCTime)), ttlSeconds :: Int }
```

---

## ขั้นตอนที่ 733: Decorator Pattern

```haskell
-- Decorator pattern ด้วย type classes

-- Base interface
class Logger m where
  logMessage :: LogLevel -> Text -> m ()

-- Concrete implementation
newtype ConsoleLogger m a = ConsoleLogger { runConsoleLogger :: m a }

instance MonadIO m => Logger (ConsoleLogger m) where
  logMessage level msg = liftIO $ putStrLn $
    "[" <> show level <> "] " <> T.unpack msg

-- Decorators
data TimestampLogger m a = TimestampLogger { innerLogger :: m a }

instance (Logger m, MonadIO m) => Logger (TimestampLogger m) where
  logMessage level msg = do
    now <- liftIO getCurrentTime
    let timestamped = T.pack (show now) <> " " <> msg
    logMessage level timestamped  -- delegate to inner

data FilterLogger m a = FilterLogger { filterLevel :: LogLevel, innerFilter :: m a }

instance Logger m => Logger (FilterLogger m) where
  logMessage level msg = do
    when (level >= filterLevel thisLogger) $
      logMessage level msg  -- delegate to inner

-- Function decoration
decorate :: (a -> b) -> (a -> a) -> (b -> b) -> a -> b
decorate f pre post = post . f . pre

-- Memoize decorator
memoize :: (Ord a, MonadIO m) => (a -> m b) -> m (a -> m b)
memoize f = do
  cache <- liftIO (newTVarIO Map.empty)
  return $ \a -> do
    cached <- liftIO (Map.lookup a <$> readTVarIO cache)
    case cached of
      Just v  -> return v
      Nothing -> do
        v <- f a
        liftIO $ atomically $ modifyTVar cache (Map.insert a v)
        return v
```

---

## ขั้นตอนที่ 734: Command Pattern

```haskell
-- Command pattern with undo/redo

data Command' = Command'
  { cmdExecute :: IO ()
  , cmdUndo    :: IO ()
  , cmdName    :: Text
  }

data CommandHistory = CommandHistory
  { chUndo  :: TVar [Command']
  , chRedo  :: TVar [Command']
  }

newHistory :: IO CommandHistory
newHistory = CommandHistory <$> newTVarIO [] <*> newTVarIO []

execute :: CommandHistory -> Command' -> IO ()
execute history cmd = do
  cmdExecute cmd
  atomically $ do
    modifyTVar (chUndo history) (cmd :)
    writeTVar  (chRedo history) []  -- clear redo stack

undo :: CommandHistory -> IO Bool
undo history = do
  undos <- readTVarIO (chUndo history)
  case undos of
    []        -> return False
    (cmd:rest) -> do
      cmdUndo cmd
      atomically $ do
        writeTVar  (chUndo history) rest
        modifyTVar (chRedo history) (cmd :)
      return True

redo :: CommandHistory -> IO Bool
redo history = do
  redos <- readTVarIO (chRedo history)
  case redos of
    []        -> return False
    (cmd:rest) -> do
      cmdExecute cmd
      atomically $ do
        writeTVar  (chRedo history) rest
        modifyTVar (chUndo history) (cmd :)
      return True

-- Example commands
insertTextCmd :: IORef Text -> Int -> Text -> Command'
insertTextCmd ref pos text = Command'
  { cmdExecute = modifyIORef ref (\t -> insertAt pos text t)
  , cmdUndo    = modifyIORef ref (\t -> deleteAt pos (T.length text) t)
  , cmdName    = "insert_text"
  }
```

---

## ขั้นตอนที่ 735: Event Sourcing Advanced

```haskell
-- Advanced event sourcing patterns

-- Snapshot for performance
data Snapshot a = Snapshot
  { snVersion :: Int
  , snState   :: a
  , snCreatedAt :: UTCTime
  }

-- Aggregate with snapshotting
class EventSourced a where
  type Event' a :: *
  apply :: a -> Event' a -> a
  initial :: a

reconstruct :: EventSourced a => Maybe (Snapshot a) -> [Event' a] -> a
reconstruct Nothing      events = foldl' apply initial events
reconstruct (Just snap)  events = foldl' apply (snState snap) events

-- Repository with snapshots
loadAggregate :: (EventSourced a, EventStore m) => AggregateId -> m a
loadAggregate aggId = do
  snapshot <- loadSnapshot aggId
  let fromVersion = fmap ((+1) . snVersion) snapshot
  events   <- loadEventsAfter aggId fromVersion
  return (reconstruct snapshot events)

saveAggregate :: (EventSourced a, EventStore m) => AggregateId -> a -> [Event' a] -> m ()
saveAggregate aggId _ newEvents = do
  appendEvents aggId newEvents
  -- Snapshot every 50 events
  version <- getEventCount aggId
  when (version `mod` 50 == 0) $ do
    state <- loadAggregate aggId
    saveSnapshot aggId Snapshot
      { snVersion   = version
      , snState     = state
      , snCreatedAt = getCurrentTime
      }

-- Projections (read models)
data Projection event state = Projection
  { pInitial :: state
  , pApply   :: state -> event -> state
  }

runProjection :: Projection event state -> [event] -> state
runProjection p = foldl' (pApply p) (pInitial p)
```

---

## ขั้นตอนที่ 736: Functional Data Structures

```haskell
-- Persistent (immutable) data structures

-- Finger tree for O(1) amortized operations
data FingerTree a
  = Empty
  | Single a
  | Deep (Digit a) (FingerTree (Node a)) (Digit a)

data Digit a = One a | Two a a | Three a a a | Four a a a a
data Node  a = Node2 a a | Node3 a a a

-- O(1) cons and snoc
cons :: a -> FingerTree a -> FingerTree a
cons a Empty      = Single a
cons a (Single b) = Deep (One a) Empty (One b)
cons a (Deep (Four b c d e) m sf) =
  let m' = cons (Node3 b c d) m
  in Deep (Two a e) m' sf
cons a (Deep pr m sf) = Deep (consDigit a pr) m sf

-- Weight-balanced binary search tree
data WBTree a
  = Leaf
  | Node { wbtLeft  :: WBTree a
         , wbtRight :: WBTree a
         , wbtValue :: a
         , wbtSize  :: Int
         }

size :: WBTree a -> Int
size Leaf         = 0
size (Node _ _ _ s) = s

node' :: WBTree a -> a -> WBTree a -> WBTree a
node' l v r = Node l r v (1 + size l + size r)

-- Balanced insert
insert' :: Ord a => a -> WBTree a -> WBTree a
insert' x Leaf = node' Leaf x Leaf
insert' x n@(Node l r v _)
  | x < v    = balance (insert' x l) v r
  | x > v    = balance l v (insert' x r)
  | otherwise = n
```

---

## ขั้นตอนที่ 737: Functional Configuration

```haskell
-- Pure configuration management

-- Configuration as a product type
data Config = Config
  { cfgServer   :: ServerConfig
  , cfgDatabase :: DatabaseConfig
  , cfgCache    :: CacheConfig
  , cfgAuth     :: AuthConfig
  }

-- Layered configuration loading
loadConfig :: IO Config
loadConfig = do
  defaults <- loadDefaults
  envVars  <- loadFromEnv
  files    <- loadFromFile "config.yaml"
  
  return (mergeConfigs [defaults, files, envVars])

-- Merge layers (later wins)
mergeConfigs :: [Config] -> Config
mergeConfigs = foldl1 merge
  where
    merge c1 c2 = Config
      { cfgServer   = mergeServer   (cfgServer c1)   (cfgServer c2)
      , cfgDatabase = mergeDatabase (cfgDatabase c1) (cfgDatabase c2)
      , cfgCache    = mergeCache    (cfgCache c1)     (cfgCache c2)
      , cfgAuth     = mergeAuth     (cfgAuth c1)       (cfgAuth c2)
      }

-- Validate configuration
validateConfig :: Config -> Validation [Text] Config
validateConfig cfg = Config
  <$> validateServer   (cfgServer cfg)
  <*> validateDatabase (cfgDatabase cfg)
  <*> validateCache    (cfgCache cfg)
  <*> validateAuth     (cfgAuth cfg)

-- Config with defaults lens
-- Quickly override individual fields for testing
testConfig :: Config
testConfig = defaultConfig
  & (field @"cfgServer" . field @"port") .~ 9999
  & (field @"cfgDatabase" . field @"url") .~ "sqlite://test.db"
```

---

## ขั้นตอนที่ 738: Batch Processing

```haskell
-- Efficient batch processing patterns

-- Batched IO (DataLoader pattern)
data BatchLoader k v = BatchLoader
  { blBatch  :: [k] -> IO (Map k v)
  , blCache  :: TVar (Map k (Either Text v))
  , blQueue  :: TVar [(k, MVar (Either Text v))]
  }

-- Request a single value (batched automatically)
loadOne :: BatchLoader k v -> k -> IO (Either Text v)
loadOne loader key = do
  cached <- Map.lookup key <$> readTVarIO (blCache loader)
  case cached of
    Just v  -> return v
    Nothing -> do
      resultVar <- newEmptyMVar
      atomically $ modifyTVar (blQueue loader) ((key, resultVar) :)
      takeMVar resultVar

-- Flush batch
flushBatch :: Ord k => BatchLoader k v -> IO ()
flushBatch loader = do
  pending <- atomically $ do
    q <- readTVar (blQueue loader)
    writeTVar (blQueue loader) []
    return q
  
  unless (null pending) $ do
    let keys    = map fst pending
    let results = blBatch loader keys
    
    values <- results
    
    forM_ pending $ \(key, var) -> do
      let result = maybe (Left "Not found") Right (Map.lookup key values)
      putMVar var result
    
    atomically $ modifyTVar (blCache loader) (Map.union (Map.map Right values))

-- Process large datasets in parallel chunks
processInChunks :: Int -> [a] -> (a -> IO b) -> IO [b]
processInChunks chunkSize items process = do
  let chunks = chunksOf chunkSize items
  concat <$> forM chunks (mapConcurrently process)
```

---

## ขั้นตอนที่ 739: DSL Design Patterns

```haskell
-- Embedded DSL patterns

-- SQL-like DSL
data Select = Select
  { selColumns :: [Text]
  , selFrom    :: Text
  , selJoins   :: [Join]
  , selWhere   :: Maybe Condition
  , selGroupBy :: [Text]
  , selHaving  :: Maybe Condition
  , selOrderBy :: [(Text, Order)]
  , selLimit   :: Maybe Int
  , selOffset  :: Maybe Int
  }

data Join = Join { joinType :: JoinType, joinTable :: Text, joinOn :: Condition }
data Order = Asc | Desc

-- Fluent builder
select :: [Text] -> Select
select cols = Select cols "" [] Nothing [] Nothing [] Nothing Nothing

from :: Text -> Select -> Select
from t s = s { selFrom = t }

where' :: Condition -> Select -> Select
where' c s = s { selWhere = Just c }

limit :: Int -> Select -> Select
limit n s = s { selLimit = Just n }

orderBy :: [(Text, Order)] -> Select -> Select
orderBy ord s = s { selOrderBy = ord }

-- Compile to SQL
compile :: Select -> (Text, [PersistValue])
compile sel = (sql, params)
  where
    sql = T.unwords . catMaybes $
      [ Just "SELECT"
      , Just (T.intercalate ", " (selColumns sel))
      , Just "FROM"
      , Just (selFrom sel)
      , fmap (\c -> "WHERE " <> compileCond c) (selWhere sel)
      , if null (selOrderBy sel) then Nothing
        else Just ("ORDER BY " <> T.intercalate ", " (map compileOrd (selOrderBy sel)))
      , fmap (\n -> "LIMIT " <> T.pack (show n)) (selLimit sel)
      ]
    params = maybe [] condParams (selWhere sel)
```

---

## ขั้นตอนที่ 740: โปรเจกต์: E-Commerce Backend

```haskell
-- Complete e-commerce backend ด้วย functional patterns

-- Product catalog
module Catalog where

data Product = Product
  { prodId     :: ProductId
  , prodName   :: Text
  , prodPrice  :: Money
  , prodStock  :: PositiveInt
  , prodTags   :: [Tag]
  , prodStatus :: ProductStatus
  }

data ProductStatus = Available | OutOfStock | Discontinued

-- Order management
module Orders where

data OrderState
  = DraftOrder   { items :: [OrderItem] }
  | PendingOrder { items :: [OrderItem], userId :: UserId }
  | PaidOrder    { items :: [OrderItem], payment :: Payment }
  | FulfilledOrder { items :: [OrderItem], shipment :: Shipment }
  | CancelledOrder { reason :: Text }

-- Type-safe transitions
confirmOrder :: DraftOrder -> UserId -> IO PendingOrder
confirmOrder (DraftOrder items) uid = do
  validateItems items
  return (PendingOrder items uid)

payOrder :: PendingOrder -> Payment -> IO PaidOrder
payOrder (PendingOrder items _) payment = do
  receipt <- processPayment payment
  return (PaidOrder items payment)

-- Shopping cart (event sourced)
data CartEvent
  = ItemAdded   ProductId Int
  | ItemRemoved ProductId
  | CartCleared

type Cart = Map ProductId Int

applyCartEvent :: Cart -> CartEvent -> Cart
applyCartEvent cart (ItemAdded pid qty)  = Map.insertWith (+) pid qty cart
applyCartEvent cart (ItemRemoved pid)    = Map.delete pid cart
applyCartEvent _    CartCleared          = Map.empty
```

---

*[← Part 36](part-36.md) | [Part 38 →](part-38.md)*
