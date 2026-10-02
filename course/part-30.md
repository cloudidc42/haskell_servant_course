# Part 30: Microservices Architecture ด้วย Haskell
## ขั้นตอนที่ 581-600

---

## ขั้นตอนที่ 581: Microservices Design Patterns

```haskell
-- Microservices ด้วย Haskell

-- Service boundaries
-- Each service: single responsibility, own database

-- ┌──────────────┐   ┌──────────────┐   ┌──────────────┐
-- │  User Service │   │  Post Service│   │Comment Service│
-- │              │   │              │   │              │
-- │  PostgreSQL  │   │  PostgreSQL  │   │  MongoDB     │
-- └──────────────┘   └──────────────┘   └──────────────┘
--        │                  │                  │
-- ────────────────────────────────────────────────────────
--                     API Gateway / gRPC

-- Service definition
data ServiceConfig = ServiceConfig
  { serviceName    :: Text
  , servicePort    :: Int
  , serviceHost    :: Text
  , serviceVersion :: Text
  , serviceDeps    :: [ServiceDependency]
  }

data ServiceDependency = ServiceDependency
  { depName    :: Text
  , depBaseUrl :: Text
  , depTimeout :: Int  -- milliseconds
  }

-- Service registry
class ServiceRegistry m where
  register   :: ServiceInfo -> m ()
  deregister :: ServiceId -> m ()
  discover   :: ServiceName -> m [ServiceInfo]
  healthCheck :: ServiceId -> m ServiceHealth

data ServiceInfo = ServiceInfo
  { siId      :: ServiceId
  , siName    :: ServiceName
  , siAddress :: Text
  , siPort    :: Int
  , siTags    :: [Text]
  , siMeta    :: Map Text Text
  }
```

---

## ขั้นตอนที่ 582: gRPC ด้วย Haskell

```haskell
-- gRPC server ด้วย grpc-haskell

import Network.GRPC.HighLevel.Generated
import Network.GRPC.LowLevel

-- Proto file (users.proto):
-- service UserService {
--   rpc GetUser (GetUserRequest) returns (User) {}
--   rpc ListUsers (ListUsersRequest) returns (stream User) {}
--   rpc CreateUser (CreateUserRequest) returns (User) {}
-- }

-- Generated types (from proto-lens)
import Proto.Users
import Proto.Users_Fields

-- Server implementation
userServiceImpl :: ServiceServer UserService
userServiceImpl = UserService
  { userServiceGetUser    = getUser
  , userServiceListUsers  = listUsers
  , userServiceCreateUser = createUser
  }

getUser :: ServerRequest 'Normal GetUserRequest User
        -> IO (ServerResponse 'Normal User)
getUser (ServerNormalRequest _meta req) = do
  let uid = req ^. userId
  mUser <- runDB $ get (UserId (fromIntegral uid))
  case mUser of
    Nothing -> return $ ServerNormalResponse
      (defMessage & Proto.Users_Fields.error .~ "User not found")
      mempty StatusNotFound ""
    Just user -> return $ ServerNormalResponse
      (userToProto user) mempty StatusOk ""

listUsers :: ServerRequest 'ServerStreaming ListUsersRequest User
          -> IO (ServerResponse 'ServerStreaming User)
listUsers (ServerWriterRequest _meta req writer) = do
  users <- runDB $ selectList [] [Asc UserId]
  forM_ users $ \(Entity _ u) -> do
    sendReply writer (userToProto u)
  return (ServerWriterResponse mempty StatusOk "")

-- Start gRPC server
main :: IO ()
main = do
  let opts = ServerOptions
        { serverPort = 50051
        , serverMaxRecvMsgSize = 4096
        }
  withGRPCServer opts $ \server ->
    runServer server userServiceImpl
```

---

## ขั้นตอนที่ 583: Service Mesh Integration

```haskell
-- Istio/Envoy integration

-- Distributed tracing headers (Zipkin/Jaeger)
propagateTrace :: Request -> Request
propagateTrace req = req { requestHeaders = traceHeaders ++ requestHeaders req }
  where
    traceHeaders =
      [ ("x-request-id",      fromMaybe newId (getHeader "x-request-id"))
      , ("x-b3-traceid",      fromMaybe (generateTraceId) (getHeader "x-b3-traceid"))
      , ("x-b3-spanid",       generateSpanId)
      , ("x-b3-parentspanid", fromMaybe "" (getHeader "x-b3-spanid"))
      , ("x-b3-sampled",      "1")
      ]
    getHeader h = lookup h (requestHeaders req)

-- Health check endpoints (required by k8s)
type HealthAPI
  = "health" :> "live"  :> Get '[JSON] LivenessResponse
  :<|> "health" :> "ready" :> Get '[JSON] ReadinessResponse

data LivenessResponse = LivenessResponse
  { lrStatus :: Text }
  deriving (Generic, ToJSON)

data ReadinessResponse = ReadinessResponse
  { rrStatus :: Text
  , rrChecks :: Map Text CheckResult
  }
  deriving (Generic, ToJSON)

-- Liveness: is the app alive (not deadlocked)?
getLive :: Handler LivenessResponse
getLive = return LivenessResponse { lrStatus = "ok" }

-- Readiness: can the app serve traffic?
getReady :: Handler ReadinessResponse
getReady = do
  dbOk    <- checkDatabase
  cacheOk <- checkCache
  return ReadinessResponse
    { rrStatus = if all isOk [dbOk, cacheOk] then "ok" else "degraded"
    , rrChecks = Map.fromList
        [ ("database", dbOk)
        , ("cache",    cacheOk)
        ]
    }
```

---

## ขั้นตอนที่ 584: Event-Driven Microservices

```haskell
-- Event-driven communication

-- Domain events
data UserEvent
  = UserCreated User
  | UserUpdated UserId UserUpdate
  | UserDeleted UserId
  deriving (Generic, ToJSON, FromJSON)

data PostEvent
  = PostPublished Post
  | PostDeleted PostId
  deriving (Generic, ToJSON, FromJSON)

-- Event envelope
data Event payload = Event
  { eventId        :: EventId
  , eventType      :: Text
  , eventPayload   :: payload
  , eventTimestamp :: UTCTime
  , eventVersion   :: Int
  , eventSource    :: Text  -- service that emitted
  } deriving (Generic, ToJSON, FromJSON)

-- Publish event
class EventPublisher m where
  publish :: ToJSON payload => Event payload -> m ()

-- Kafka publisher
instance EventPublisher KafkaM where
  publish event = do
    producer <- ask
    let topic   = eventType event
    let payload = BSL.toStrict (encode event)
    liftIO $ produceMessage producer
      ProducerRecord
        { prTopic   = TopicName topic
        , prKey     = Just (T.encodeUtf8 (eventId event))
        , prValue   = Just payload
        }

-- Event handler
class EventHandler m where
  handleEvent :: FromJSON payload => Event payload -> m ()

-- Handler registration
data EventRegistry m = EventRegistry
  { handlers :: Map Text (Value -> m ())
  }

registerHandler :: Text -> (Event payload -> m ()) -> EventRegistry m -> EventRegistry m
registerHandler eventType handler registry =
  registry { handlers = Map.insert eventType
    (\v -> case fromJSON v of
      Success e -> handler e
      Error _   -> return ())
    (handlers registry) }
```

---

## ขั้นตอนที่ 585: API Gateway

```haskell
-- API Gateway pattern

data GatewayConfig = GatewayConfig
  { gcRoutes   :: [RouteConfig]
  , gcMiddleware :: [MiddlewareConfig]
  , gcRateLimit  :: RateLimitConfig
  , gcAuth       :: AuthConfig
  }

data RouteConfig = RouteConfig
  { rcPrefix     :: Text        -- /api/users
  , rcBackend    :: BackendConfig
  , rcTransforms :: [Transform]  -- request/response transforms
  , rcTimeout    :: Int          -- ms
  }

data BackendConfig = BackendConfig
  { bcUrl     :: Text
  , bcBalancer :: LoadBalancerStrategy
  , bcRetry    :: RetryConfig
  }

-- Gateway proxy handler
proxyHandler :: GatewayConfig -> Application
proxyHandler config req respond = do
  let path = T.decodeUtf8 (rawPathInfo req)
  
  case findRoute config path of
    Nothing -> respond (responseLBS status404 [] "Not Found")
    Just route -> do
      -- Apply request transforms
      req' <- applyRequestTransforms (rcTransforms route) req
      
      -- Rate limiting
      ok <- checkRateLimit (gcRateLimit config) req
      unless ok $ respond (responseLBS status429 [] "Rate Limited")
      
      -- Authentication
      authResult <- authenticate (gcAuth config) req
      case authResult of
        AuthFailed -> respond (responseLBS status401 [] "Unauthorized")
        AuthOk claims -> do
          -- Forward to backend
          let backendUrl = bcUrl (rcBackend route)
          result <- proxyRequest backendUrl req'
          
          -- Apply response transforms
          resp' <- applyResponseTransforms (rcTransforms route) result
          respond resp'

-- Service discovery integration
resolveBackend :: ServiceRegistry m => Text -> m (Maybe Text)
resolveBackend serviceName = do
  instances <- discover serviceName
  case instances of
    []    -> return Nothing
    (i:_) -> return (Just (siAddress i <> ":" <> T.pack (show (siPort i))))
```

---

## ขั้นตอนที่ 586: Distributed Configuration

```haskell
-- Centralized configuration ด้วย Consul/etcd

import Network.Consul

-- Consul client
getConfig :: Text -> IO (Maybe Text)
getConfig key = do
  client <- createConsulClient "localhost" 8500
  mVal   <- kvGet client key
  return (kvValue =<< mVal)

-- Watch for config changes
watchConfig :: Text -> (Text -> IO ()) -> IO ()
watchConfig key handler = do
  client <- createConsulClient "localhost" 8500
  go (Just 0)
  where
    go mIndex = do
      (mVal, newIndex) <- kvGetWatch client key mIndex
      case mVal of
        Just v  -> handler (kvValue v)
        Nothing -> return ()
      go (Just newIndex)

-- Dynamic configuration reload
data DynamicConfig = DynamicConfig
  { dcConfig :: TVar AppConfig
  , dcWatchers :: [Thread]
  }

startConfigWatcher :: IO DynamicConfig
startConfigWatcher = do
  initialConfig <- loadConfig
  configVar     <- newTVarIO initialConfig
  
  watcher <- forkIO $ watchConfig "app/config" $ \rawConfig ->
    case eitherDecode (BSL.fromStrict (T.encodeUtf8 rawConfig)) of
      Left err  -> putStrLn $ "Config parse error: " ++ err
      Right cfg -> atomically $ writeTVar configVar cfg
  
  return DynamicConfig
    { dcConfig   = configVar
    , dcWatchers = [watcher]
    }

-- Use dynamic config
readConfig :: DynamicConfig -> IO AppConfig
readConfig = readTVarIO . dcConfig
```

---

## ขั้นตอนที่ 587: Distributed Tracing

```haskell
-- OpenTelemetry tracing

import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core

-- Initialize tracer
initTracing :: IO TracerProvider
initTracing = do
  let exporter = otlpExporter "localhost" 4317
  let sampler  = parentBased alwaysOn
  createTracerProvider [SimpleSpanProcessor exporter] sampler

-- Create spans
withSpan :: TracerProvider -> Text -> IO a -> IO a
withSpan provider spanName action = do
  let tracer = getTracer provider "my-service" "1.0.0"
  inSpan tracer spanName defaultSpanArguments action

-- Servant middleware for auto-tracing
tracingMiddleware :: TracerProvider -> Middleware
tracingMiddleware provider app req respond = do
  let spanName = T.decodeUtf8 (requestMethod req) <> " " <> T.decodeUtf8 (rawPathInfo req)
  withSpan provider spanName $ do
    -- Add HTTP attributes
    currentSpan <- getCurrentSpan
    addAttribute currentSpan "http.method"  (T.decodeUtf8 (requestMethod req))
    addAttribute currentSpan "http.path"    (T.decodeUtf8 (rawPathInfo req))
    addAttribute currentSpan "http.host"    (T.decodeUtf8 (fromMaybe "" (requestHeaderHost req)))
    
    app req $ \resp -> do
      addAttribute currentSpan "http.status" (statusCode (responseStatus resp))
      respond resp

-- Propagate trace context
propagateContext :: Request -> IO SpanContext
propagateContext req = do
  let traceParent = lookup "traceparent" (requestHeaders req)
  case traceParent of
    Nothing -> newRootContext
    Just tp -> extractContext tp
```

---

## ขั้นตอนที่ 588: Saga Pattern

```haskell
-- Saga pattern สำหรับ distributed transactions

-- Choreography-based saga
data OrderSaga
  = OrderCreated Order
  | PaymentProcessed OrderId PaymentResult
  | InventoryReserved OrderId
  | ShipmentCreated OrderId ShipmentId
  | OrderCompleted OrderId
  | OrderFailed OrderId FailureReason

-- Each service reacts to events
handleOrderCreated :: Order -> IO ()
handleOrderCreated order = do
  result <- processPayment (orderPayment order)
  case result of
    PaymentSuccess -> publish (PaymentProcessed (orderId order) result)
    PaymentFailure -> publish (OrderFailed (orderId order) PaymentFailed)

handlePaymentProcessed :: OrderId -> PaymentResult -> IO ()
handlePaymentProcessed oid _ = do
  ok <- reserveInventory oid
  if ok
    then publish (InventoryReserved oid)
    else do
      refundPayment oid  -- compensation!
      publish (OrderFailed oid InventoryUnavailable)

-- Orchestration-based saga
data SagaStep m a = SagaStep
  { stepAction      :: m a
  , stepCompensation :: a -> m ()
  }

newtype Saga m a = Saga { runSaga :: [SagaStep m Value] -> m (Either SagaError a) }

executeSaga :: Monad m => [SagaStep m Value] -> m (Either SagaError [Value])
executeSaga steps = go [] steps
  where
    go completed [] = return (Right completed)
    go completed (step:rest) = do
      result <- stepAction step
      -- (simplified - real impl wraps in Maybe/Either)
      let newCompleted = result : completed
      r <- go newCompleted rest
      case r of
        Right vals -> return (Right vals)
        Left err   -> do
          -- Compensate completed steps in reverse
          forM_ (zip completed steps) $ \(result, s) ->
            stepCompensation s result
          return (Left err)
```

---

## ขั้นตอนที่ 589: Service-to-Service Auth

```haskell
-- Service-to-service authentication

-- mTLS: mutual TLS
-- Both client and server verify each other's certificates

data ServiceAuth = ServiceAuth
  { saCert    :: X509Certificate
  , saKey     :: PrivateKey
  , saCaChain :: [X509Certificate]
  }

-- Create client with client certificate
createAuthenticatedClient :: ServiceAuth -> IO Manager
createAuthenticatedClient auth = do
  let tlsSettings = TLSSettings
        { tlsClientCertFile = saCert auth
        , tlsClientKeyFile  = saKey auth
        , tlsCaFile         = saCaChain auth
        }
  newManager (mkManagerSettings (TLSSettings tlsSettings) Nothing)

-- JWT service tokens
generateServiceToken :: ServiceId -> Text -> IO Text
generateServiceToken serviceId secret = do
  now <- getCurrentTime
  let claims = JWTClaims
        { issuer  = "my-service-1"
        , subject = serviceId
        , audience = ["my-service-2"]
        , expiry   = addUTCTime 300 now  -- 5 minutes
        }
  return $ signJWT secret claims

-- Validate service token
validateServiceToken :: Text -> Text -> IO (Either AuthError ServiceId)
validateServiceToken secret token = do
  now <- getCurrentTime
  case verifyJWT secret token of
    Left err -> return (Left (InvalidToken err))
    Right claims ->
      if expiryTime claims > now
        then return (Right (subject claims))
        else return (Left TokenExpired)

-- API key rotation
rotateApiKeys :: ServiceConfig -> IO ()
rotateApiKeys config = do
  newKey <- generateApiKey
  updateServiceConfig config { apiKey = newKey }
  -- Grace period: accept both old and new key
  scheduleRevocation (oldApiKey config) (addUTCTime 3600 now)
```

---

## ขั้นตอนที่ 590: Kubernetes Deployment

```yaml
# k8s deployment สำหรับ Haskell service

# deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: user-service
spec:
  replicas: 3
  selector:
    matchLabels:
      app: user-service
  template:
    metadata:
      labels:
        app: user-service
    spec:
      containers:
      - name: user-service
        image: myregistry/user-service:1.0.0
        ports:
        - containerPort: 8080
        env:
        - name: PORT
          value: "8080"
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: db-secrets
              key: url
        resources:
          requests:
            cpu: "100m"
            memory: "128Mi"
          limits:
            cpu: "500m"
            memory: "512Mi"
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        # Graceful shutdown
        lifecycle:
          preStop:
            exec:
              command: ["sleep", "5"]
      terminationGracePeriodSeconds: 30
```

```haskell
-- Haskell code สำหรับ k8s deployment

-- Graceful shutdown handler
setupGracefulShutdown :: IO () -> IO ()
setupGracefulShutdown cleanupAction = do
  -- Handle SIGTERM (from k8s)
  installHandler sigTERM (Catch cleanupAction) Nothing
  -- Handle SIGINT (Ctrl+C)
  installHandler sigINT  (Catch cleanupAction) Nothing

-- Startup probe: wait for dependencies
waitForDependencies :: [Dependency] -> IO ()
waitForDependencies deps = do
  forM_ deps $ \dep -> do
    ok <- checkDependency dep
    unless ok $ do
      threadDelay 1000000  -- 1 second
      waitForDependencies [dep]  -- retry
```

---

## ขั้นตอนที่ 591: Circuit Breaker สำหรับ Services

```haskell
-- Per-service circuit breaker

data ServiceCircuitBreaker = ServiceCircuitBreaker
  { scbService    :: Text
  , scbBreaker    :: CircuitBreaker
  , scbSuccessRate :: TVar Double
  , scbLatency     :: TVar Double
  }

-- Adaptive circuit breaker based on error rate
adaptiveCircuitBreaker :: ServiceCircuitBreaker -> IO a -> IO (Either ServiceError a)
adaptiveCircuitBreaker scb action = do
  successRate <- readTVarIO (scbSuccessRate scb)
  if successRate < 0.5  -- open if <50% success
    then return (Left ServiceUnavailable)
    else do
      start  <- getCurrentTime
      result <- callWithBreaker (scbBreaker scb) action
      end    <- getCurrentTime
      
      let latency = realToFrac (diffUTCTime end start) * 1000
      
      -- Update stats
      atomically $ do
        modifyTVar (scbLatency scb) (\l -> 0.9 * l + 0.1 * latency)
        case result of
          Left _  -> modifyTVar (scbSuccessRate scb) (\r -> 0.9 * r + 0.1 * 0.0)
          Right _ -> modifyTVar (scbSuccessRate scb) (\r -> 0.9 * r + 0.1 * 1.0)
      
      return result

-- Bulkhead pattern: isolate failing services
data Bulkhead = Bulkhead
  { bhSemaphore :: TSemaphore
  , bhQueue     :: TBQueue (IO ())
  }

withBulkhead :: Bulkhead -> IO a -> IO (Either BulkheadError a)
withBulkhead bulkhead action = do
  acquired <- atomically $ tryAcquireN (bhSemaphore bulkhead) 1
  if acquired
    then do
      result <- E.try action
      atomically $ releaseN (bhSemaphore bulkhead) 1
      return (bimap BulkheadActionFailed id result)
    else return (Left BulkheadFull)
```

---

## ขั้นตอนที่ 592: Event Sourcing across Services

```haskell
-- Cross-service event sourcing

-- Global event store
data GlobalEvent = GlobalEvent
  { geId        :: EventId
  , geAggregate :: AggregateId
  , geType      :: Text
  , gePayload   :: Value
  , geService   :: ServiceId
  , geTimestamp :: UTCTime
  , geVersion   :: Int
  }

-- Event streaming (Kafka topics per aggregate type)
topicForAggregate :: AggregateType -> TopicName
topicForAggregate UserAggregate    = "users-events"
topicForAggregate OrderAggregate   = "orders-events"
topicForAggregate ProductAggregate = "products-events"

-- Consumer group per service
consumeEvents :: ServiceId -> AggregateType -> (GlobalEvent -> IO ()) -> IO ()
consumeEvents serviceId aggType handler = do
  let groupId = "service-" <> serviceId
  let topic   = topicForAggregate aggType
  
  consumer <- newConsumer defaultConfig
    { configGroupId = groupId
    , configAutoOffsetReset = "earliest"
    }
  
  subscribe consumer [topic]
  
  forever $ do
    msgs <- pollMessages consumer (Timeout 1000)
    forM_ msgs $ \msg -> do
      case eitherDecode (messageValue msg) of
        Left err -> logError $ "Bad event: " <> T.pack err
        Right ev -> handler ev
      commitOffsets consumer [topicPartitionOffset msg]

-- Event replay
replayEvents :: AggregateId -> IO [GlobalEvent]
replayEvents aggId = do
  conn <- getKafkaAdminClient
  let topic     = topicForAggregate (aggregateType aggId)
  let partition = partitionForAggregate aggId
  
  consumer <- newConsumer defaultConfig { configGroupId = "replay-" <> newUUID }
  assignPartitions consumer [(topic, partition, Offset 0)]
  
  go [] (HighWatermark 0)
  where
    go events hwm = do
      msgs <- pollMessages consumer (Timeout 100)
      if null msgs
        then return (reverse events)
        else do
          let new = [e | msg <- msgs, Right e <- [eitherDecode (messageValue msg)]]
          go (new ++ events) hwm
```

---

## ขั้นตอนที่ 593: Data Consistency Patterns

```haskell
-- Data consistency ใน microservices

-- Eventually consistent with idempotency
data IdempotentOperation = IdempotentOperation
  { ioKey     :: Text       -- unique operation key
  , ioExpiry  :: UTCTime
  }

-- Check and record operation atomically
withIdempotency :: RedisConn -> Text -> IO a -> IO (Maybe a)
withIdempotency redis key action = do
  ok <- liftIO $ Redis.runRedis redis $ do
    result <- Redis.setnx (T.encodeUtf8 key) "1"
    case result of
      Right True -> do
        Redis.expire (T.encodeUtf8 key) 86400  -- 24h
        return True
      _ -> return False
  if ok
    then Just <$> action
    else return Nothing  -- duplicate operation

-- Two-phase commit (simplified)
data TwoPCState = Preparing | Prepared | Committed | Aborted

twoPhaseCommit :: [Participant] -> Transaction -> IO Bool
twoPhaseCommit participants tx = do
  -- Phase 1: Prepare
  prepareResults <- forM participants $ \p -> do
    result <- sendPrepare p tx
    return (p, result)
  
  let allPrepared = all (== Prepared . snd) prepareResults
  
  if allPrepared
    then do
      -- Phase 2: Commit
      forM_ participants $ \p -> sendCommit p tx
      return True
    else do
      -- Phase 2: Abort
      forM_ participants $ \p -> sendAbort p tx
      return False

-- Outbox pattern: atomic event publishing
insertWithOutbox :: Entity -> DomainEvent -> DB ()
insertWithOutbox entity event = do
  insert_ entity
  insert_ OutboxEntry
    { oeEventType = eventType event
    , oePayload   = encode event
    , oePublished = False
    , oeCreatedAt = getCurrentTime
    }
-- Outbox worker publishes events and marks as published
```

---

## ขั้นตอนที่ 594: Service Health & SLO

```haskell
-- SLO (Service Level Objective) monitoring

data SLO = SLO
  { sloName        :: Text
  , sloTarget      :: Double  -- e.g., 0.999 = 99.9%
  , sloWindow      :: NominalDiffTime  -- rolling window
  , sloMeasurement :: IO Double  -- returns current value 0-1
  }

-- Availability SLO
availabilitySLO :: IORef RequestStats -> SLO
availabilitySLO statsRef = SLO
  { sloName        = "availability"
  , sloTarget      = 0.999  -- 99.9%
  , sloWindow      = 86400  -- 24 hours
  , sloMeasurement = do
      stats <- readIORef statsRef
      let total  = rsTotal stats
      let errors = rsErrors stats
      return (if total == 0 then 1.0
              else 1.0 - (fromIntegral errors / fromIntegral total))
  }

-- Error budget calculation
calculateErrorBudget :: SLO -> IO ErrorBudget
calculateErrorBudget slo = do
  current <- sloMeasurement slo
  let budget      = 1.0 - sloTarget slo
  let consumed    = max 0 (sloTarget slo - current)
  let remaining   = budget - consumed
  let percentage  = remaining / budget * 100
  return ErrorBudget
    { ebTotal     = budget
    , ebConsumed  = consumed
    , ebRemaining = remaining
    , ebPercent   = percentage
    , ebBurnRate  = if budget == 0 then 0 else consumed / budget
    }

-- SLO dashboard
sloReport :: [SLO] -> IO Text
sloReport slos = do
  rows <- forM slos $ \slo -> do
    current <- sloMeasurement slo
    budget  <- calculateErrorBudget slo
    let status = if current >= sloTarget slo then "✓" else "✗"
    return $ T.intercalate "|"
      [ sloName slo
      , T.pack (show (sloTarget slo * 100)) <> "%"
      , T.pack (show (current * 100)) <> "%"
      , T.pack (show (ebPercent budget)) <> "% budget left"
      , status
      ]
  return (T.unlines rows)
```

---

## ขั้นตอนที่ 595: Canary Deployments

```haskell
-- Canary deployment strategy

data TrafficSplit = TrafficSplit
  { tsCanaryVersion :: Text
  , tsCanaryPercent :: Int  -- 0-100
  , tsStableVersion :: Text
  }

-- Route traffic to canary based on percentage
canaryMiddleware :: TVar TrafficSplit -> Middleware
canaryMiddleware splitVar app req respond = do
  split <- readTVarIO splitVar
  r <- randomRIO (0, 99 :: Int)
  
  let backendUrl = if r < tsCanaryPercent split
        then "http://canary-service:8080"
        else "http://stable-service:8080"
  
  proxyToBackend backendUrl app req respond

-- Progressive rollout
progressiveRollout :: Text -> IO ()
progressiveRollout version = do
  splitVar <- newTVarIO (TrafficSplit version 0 "stable")
  
  forM_ [5, 10, 25, 50, 75, 100] $ \percent -> do
    -- Check metrics before increasing traffic
    ok <- checkCanaryHealth version
    if ok
      then do
        putStrLn $ "Increasing canary to " ++ show percent ++ "%"
        atomically $ modifyTVar splitVar (\s -> s { tsCanaryPercent = percent })
        threadDelay (5 * 60 * 1000000)  -- wait 5 minutes
      else do
        putStrLn "Canary unhealthy, rolling back!"
        atomically $ modifyTVar splitVar (\s -> s { tsCanaryPercent = 0 })
        return ()

-- Feature flags integration
data FeatureFlag = FeatureFlag
  { ffName       :: Text
  , ffEnabled    :: Bool
  , ffRollout    :: Maybe Int  -- percentage for gradual rollout
  , ffConditions :: [FlagCondition]  -- user segment conditions
  }

isFeatureEnabled :: FeatureFlag -> UserId -> IO Bool
isFeatureEnabled flag uid = case ffRollout flag of
  Nothing  -> return (ffEnabled flag)
  Just pct -> do
    let hash = hashUserId uid
    return (hash `mod` 100 < pct && ffEnabled flag)
```

---

## ขั้นตอนที่ 596: Database per Service

```haskell
-- Database patterns สำหรับ microservices

-- Each service owns its data
-- User Service: PostgreSQL (relational)
-- Search Service: Elasticsearch (full-text)
-- Session Service: Redis (key-value)
-- Analytics Service: ClickHouse (columnar)

-- CQRS per service
-- Command side: PostgreSQL (ACID)
-- Query side: Elasticsearch/Redis (optimized reads)

-- Read model sync (event-driven)
syncReadModel :: UserEvent -> IO ()
syncReadModel (UserCreated user) = do
  -- Index in Elasticsearch
  indexDocument esClient "users" (userId user) (toJSON user)
  -- Cache in Redis
  Redis.setex redis (userCacheKey (userId user)) 3600 (encode user)

syncReadModel (UserUpdated uid update) = do
  -- Update Elasticsearch
  updateDocument esClient "users" uid update
  -- Invalidate Redis cache
  Redis.del redis [userCacheKey uid]

-- Cross-service data joining (AVOID in production)
-- Instead: denormalize into each service's own DB
-- Or: use API composition at gateway level

-- API Composition
data UserPost = UserPost
  { upUser :: UserSummary
  , upPost :: PostSummary
  }

getUserPosts :: UserId -> IO [UserPost]
getUserPosts uid = do
  user  <- fetchFromUserService uid
  posts <- fetchFromPostService uid
  return [UserPost user p | p <- posts]
```

---

## ขั้นตอนที่ 597: Chaos Engineering

```haskell
-- Chaos engineering ใน testing

-- Inject failures in tests
data ChaosConfig = ChaosConfig
  { ccFailureRate  :: Double  -- 0.0 - 1.0
  , ccLatencyMs    :: Maybe Int
  , ccErrorTypes   :: [SomeException]
  }

-- Chaos proxy for HTTP
chaosMiddleware :: ChaosConfig -> Middleware
chaosMiddleware config app req respond = do
  -- Inject random failure
  r <- randomRIO (0.0, 1.0 :: Double)
  when (r < ccFailureRate config) $ do
    err <- randomChoice (ccErrorTypes config)
    E.throwIO err
  
  -- Inject latency
  case ccLatencyMs config of
    Nothing  -> return ()
    Just ms  -> do
      jitter <- randomRIO (0, ms `div` 2)
      threadDelay ((ms + jitter) * 1000)
  
  app req respond

-- Database chaos
chaosDB :: ChaosConfig -> DB a -> DB a
chaosDB config action = do
  r <- liftIO (randomRIO (0.0, 1.0 :: Double))
  if r < ccFailureRate config
    then liftIO $ E.throwIO (DBError "Injected failure")
    else action

-- Resilience testing
testCircuitBreaker :: IO ()
testCircuitBreaker = do
  breaker <- newCircuitBreaker 5 30  -- 5 failures, 30s timeout
  
  -- Trigger failures to open circuit
  replicateM_ 5 $ do
    result <- callWithBreaker breaker (E.throwIO TestException)
    print result
  
  -- Circuit should now be open
  result <- callWithBreaker breaker (return "success")
  assert (isLeft result) "Circuit should be open"
  
  -- Wait for timeout
  threadDelay (30 * 1000000)
  
  -- Circuit should be half-open
  result2 <- callWithBreaker breaker (return "success")
  assert (isRight result2) "Circuit should close after success"
```

---

## ขั้นตอนที่ 598: Service Mesh Observability

```haskell
-- Observability ใน microservices

-- Distributed tracing ด้วย OpenTelemetry
data ServiceSpan = ServiceSpan
  { ssTraceId :: TraceId
  , ssSpanId  :: SpanId
  , ssService :: Text
  , ssOp      :: Text
  , ssTags    :: Map Text Value
  , ssLogs    :: [LogEntry]
  , ssStart   :: UTCTime
  , ssEnd     :: Maybe UTCTime
  }

-- Correlation ID propagation
withCorrelationId :: Text -> IO a -> IO a
withCorrelationId cid action = do
  let ctx = CorrelationContext cid
  withContext ctx action

-- Structured logging with trace context
data LogEntry = LogEntry
  { leLevel     :: LogLevel
  , leMessage   :: Text
  , leTimestamp :: UTCTime
  , leTraceId   :: Maybe TraceId
  , leSpanId    :: Maybe SpanId
  , leService   :: Text
  , leAttrs     :: Map Text Value
  }

logWithContext :: MonadIO m => LogLevel -> Text -> Map Text Value -> m ()
logWithContext level msg attrs = do
  mCtx      <- getTraceContext
  timestamp <- liftIO getCurrentTime
  let entry = LogEntry
        { leLevel     = level
        , leMessage   = msg
        , leTimestamp = timestamp
        , leTraceId   = fmap tctTraceId mCtx
        , leSpanId    = fmap tctSpanId mCtx
        , leService   = "my-service"
        , leAttrs     = attrs
        }
  liftIO $ BSL.putStr (encode entry <> "\n")

-- Metrics aggregation across services
-- Service A: request_count{service="user-service", method="GET", path="/users"}
-- Service B: request_count{service="post-service", method="GET", path="/posts"}
-- Aggregated: sum(request_count) by (service)
```

---

## ขั้นตอนที่ 599: gRPC Streaming

```haskell
-- Bidirectional gRPC streaming

-- Proto:
-- service ChatService {
--   rpc Chat (stream ChatMessage) returns (stream ChatMessage) {}
-- }

-- Handler
chatHandler :: BidiStreamHandler ChatMessage ChatMessage IO ()
chatHandler inStream outStream = do
  -- Read messages concurrently with writing
  race_
    (processIncoming inStream outStream)
    (sendHeartbeat outStream)
  where
    processIncoming inS outS = do
      msg <- recvMsg inS
      case msg of
        Nothing   -> return ()
        Just chatMsg -> do
          let response = processChatMessage chatMsg
          sendMsg outS response
          processIncoming inS outS
    
    sendHeartbeat outS = forever $ do
      threadDelay 30000000  -- 30 seconds
      sendMsg outS heartbeatMsg

-- Server-side streaming
type LiveUpdates = ServerStreaming UpdateRequest Update

sendLiveUpdates :: UpdateRequest -> ServerStreamHandler Update IO ()
sendLiveUpdates req outStream = do
  let entityId = reqEntityId req
  
  -- Subscribe to updates
  chan <- subscribeToUpdates entityId
  
  -- Stream updates to client
  forever $ do
    update <- readChan chan
    sendMsg outStream update

-- Client-side streaming (collect then process)
processUploads :: ClientStreamHandler UploadChunk UploadResult IO ()
processUploads inStream = do
  chunks <- collectAll inStream
  let combined = BS.concat (map uploadData chunks)
  result <- processUpload combined
  return (mkUploadResult result)

collectAll :: ClientStream msg -> IO [msg]
collectAll stream = do
  msg <- recvMsg stream
  case msg of
    Nothing -> return []
    Just m  -> (m:) <$> collectAll stream
```

---

## ขั้นตอนที่ 600: โปรเจกต์: Complete Microservices System

```haskell
-- Complete microservices system architecture

-- Service catalog
data Services = Services
  { userService    :: UserServiceClient
  , postService    :: PostServiceClient
  , commentService :: CommentServiceClient
  , notifyService  :: NotificationServiceClient
  , searchService  :: SearchServiceClient
  }

-- Service clients (gRPC or HTTP)
data UserServiceClient = UserServiceClient
  { getUser    :: UserId -> IO (Either ServiceError User)
  , createUser :: CreateUserReq -> IO (Either ServiceError User)
  , updateUser :: UserId -> UpdateUserReq -> IO (Either ServiceError User)
  , deleteUser :: UserId -> IO (Either ServiceError ())
  }

-- API Gateway aggregates services
data AggregatedPost = AggregatedPost
  { apPost     :: Post
  , apAuthor   :: User
  , apComments :: [Comment]
  , apLikeCount :: Int
  }

getAggregatedPost :: Services -> PostId -> IO (Either ServiceError AggregatedPost)
getAggregatedPost services pid = do
  -- Concurrent fetching!
  (postResult, commentsResult) <- concurrently
    (postService services `getPost` pid)
    (commentService services `getComments` pid)
  
  case (postResult, commentsResult) of
    (Left err, _) -> return (Left err)
    (_, Left err) -> return (Left err)
    (Right post, Right comments) -> do
      authorResult <- userService services `getUser` (postAuthorId post)
      case authorResult of
        Left err -> return (Left err)
        Right author -> do
          likeCount <- postService services `getLikeCount` pid
          return $ Right AggregatedPost
            { apPost      = post
            , apAuthor    = author
            , apComments  = comments
            , apLikeCount = fromRight 0 likeCount
            }

-- Health check all services
checkAllServices :: Services -> IO (Map Text ServiceHealth)
checkAllServices services = Map.fromList <$> mapConcurrently check allServices
  where
    allServices =
      [ ("user-service",    checkUserService)
      , ("post-service",    checkPostService)
      , ("comment-service", checkCommentService)
      , ("notify-service",  checkNotifyService)
      , ("search-service",  checkSearchService)
      ]
    check (name, checkFn) = (name,) <$> checkFn services
```

---

*[← Part 29](part-29.md) | [Part 31 →](part-31.md)*
