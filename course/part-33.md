# Part 33: Production Observability
## ขั้นตอนที่ 641-660

---

## ขั้นตอนที่ 641: Structured Logging

```haskell
-- Structured logging ด้วย co-log

import Colog

-- Log message types
data AppMsg
  = AppMsg    { msgLevel :: Severity, msgText :: Text }
  | HttpMsg   { httpMethod :: Text, httpPath :: Text, httpStatus :: Int, httpDuration :: Double }
  | DbMsg     { dbQuery :: Text, dbDuration :: Double, dbRows :: Int }
  | ErrorMsg  { errMsg :: Text, errStack :: [Text] }

-- Structured log action
mkLogger :: Handle -> LogAction IO AppMsg
mkLogger handle = LogAction $ \msg ->
  let json = encode (toJSON msg)
  in BSL.hPutStrLn handle json

instance ToJSON AppMsg where
  toJSON (AppMsg level text) = object
    [ "level"   .= show level
    , "message" .= text
    , "ts"      .= currentTimestamp
    ]
  toJSON (HttpMsg method path status dur) = object
    [ "level"    .= ("info" :: Text)
    , "type"     .= ("http_request" :: Text)
    , "method"   .= method
    , "path"     .= path
    , "status"   .= status
    , "duration_ms" .= dur
    ]
  toJSON (DbMsg query dur rows) = object
    [ "level"       .= ("debug" :: Text)
    , "type"        .= ("db_query" :: Text)
    , "query"       .= query
    , "duration_ms" .= dur
    , "rows"        .= rows
    ]

-- Context-aware logging with ReaderT
type App = ReaderT AppEnv IO

data AppEnv = AppEnv
  { envLogger    :: LogAction IO AppMsg
  , envRequestId :: Maybe Text
  }

logMsg :: AppMsg -> App ()
logMsg msg = do
  env <- ask
  liftIO $ unLogAction (envLogger env) msg

-- Add context to log
withRequestId :: Text -> App a -> App a
withRequestId reqId = local (\env -> env { envRequestId = Just reqId })
```

---

## ขั้นตอนที่ 642: Log Correlation

```haskell
-- Distributed log correlation

import System.Random (randomIO)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)

-- Trace context
data TraceContext = TraceContext
  { tcTraceId  :: Text
  , tcSpanId   :: Text
  , tcParentId :: Maybe Text
  , tcFlags    :: Word8
  }

-- Generate new trace context
newTraceContext :: IO TraceContext
newTraceContext = do
  traceId <- toText <$> nextRandom
  spanId  <- toText <$> nextRandom
  return TraceContext
    { tcTraceId  = traceId
    , tcSpanId   = spanId
    , tcParentId = Nothing
    , tcFlags    = 1  -- sampled
    }

-- Child span
childSpan :: TraceContext -> IO TraceContext
childSpan parent = do
  spanId <- toText <$> nextRandom
  return parent
    { tcSpanId   = spanId
    , tcParentId = Just (tcSpanId parent)
    }

-- Log with trace context
data CorrelatedMsg = CorrelatedMsg
  { cmMsg     :: AppMsg
  , cmTrace   :: TraceContext
  , cmService :: Text
  , cmHost    :: Text
  }

instance ToJSON CorrelatedMsg where
  toJSON cm = object
    [ "trace_id"   .= tcTraceId (cmTrace cm)
    , "span_id"    .= tcSpanId (cmTrace cm)
    , "parent_id"  .= tcParentId (cmTrace cm)
    , "service"    .= cmService cm
    , "host"       .= cmHost cm
    , "payload"    .= toJSON (cmMsg cm)
    ]

-- Propagate trace via HTTP headers
injectTraceHeaders :: TraceContext -> RequestHeaders -> RequestHeaders
injectTraceHeaders ctx headers =
  [ ("X-Trace-Id",  encodeUtf8 (tcTraceId ctx))
  , ("X-Span-Id",   encodeUtf8 (tcSpanId ctx))
  ] ++ maybeToList (fmap (\p -> ("X-Parent-Span-Id", encodeUtf8 p)) (tcParentId ctx))
  ++ headers

extractTraceHeaders :: RequestHeaders -> Maybe TraceContext
extractTraceHeaders headers = do
  traceId <- lookup "X-Trace-Id"  headers
  spanId  <- lookup "X-Span-Id"   headers
  return TraceContext
    { tcTraceId  = decodeUtf8 traceId
    , tcSpanId   = decodeUtf8 spanId
    , tcParentId = fmap decodeUtf8 (lookup "X-Parent-Span-Id" headers)
    , tcFlags    = 1
    }
```

---

## ขั้นตอนที่ 643: OpenTelemetry Tracing

```haskell
-- OpenTelemetry integration

import OpenTelemetry.Trace
import OpenTelemetry.Trace.Core
import OpenTelemetry.Context

-- Initialize tracer
initTracer :: Text -> IO TracerProvider
initTracer serviceName = do
  let config = TracerProviderConfig
        { tpcExporter    = otlpExporter "http://otel-collector:4317"
        , tpcSampler     = alwaysOnSampler
        , tpcResource    = mkResource [("service.name", toAttribute serviceName)]
        }
  createTracerProvider config

-- Instrument a function
withSpan :: Tracer -> Text -> [(Text, Attribute)] -> IO a -> IO a
withSpan tracer spanName attrs action = do
  let spanArgs = defaultSpanArguments
        { attributes = Map.fromList attrs }
  
  inSpan tracer spanName spanArgs $ \span -> do
    result <- action
    return result

-- Servant middleware for auto-instrumentation
traceMiddleware :: TracerProvider -> Application -> Application
traceMiddleware provider app request respond = do
  tracer <- getTracer provider "servant"
  
  let traceId   = extractTraceId request
  let spanName  = T.pack (requestMethod request) <> " " <> T.pack (rawPathInfo request)
  
  withSpan tracer spanName
    [ ("http.method",      toAttribute (requestMethod request))
    , ("http.url",         toAttribute (rawPathInfo request))
    , ("http.user_agent",  toAttribute (fromMaybe "" (lookupHeader "User-Agent" request)))
    ]
    $ do
      start <- getMonotonicTime
      app request $ \response -> do
        end <- getMonotonicTime
        let duration = end - start
        addEvent "request.complete"
          [ ("http.status_code", toAttribute (responseStatus response))
          , ("duration_ms",      toAttribute duration)
          ]
        respond response
```

---

## ขั้นตอนที่ 644: Custom Metrics with Prometheus

```haskell
-- Prometheus metrics

import Prometheus
import Prometheus.Metric.GHC

-- Define metrics
data AppMetrics = AppMetrics
  { amRequestsTotal    :: Counter
  , amRequestDuration  :: Histogram
  , amActiveRequests   :: Gauge
  , amCacheHits        :: Counter
  , amCacheMisses      :: Counter
  , amDbQueryDuration  :: Histogram
  , amErrors           :: Counter
  }

initMetrics :: IO AppMetrics
initMetrics = AppMetrics
  <$> registerIO (counter (Info "http_requests_total" "Total HTTP requests"))
  <*> registerIO (histogram
        (Info "http_request_duration_seconds" "HTTP request duration")
        defaultBuckets)
  <*> registerIO (gauge (Info "http_active_requests" "Active HTTP requests"))
  <*> registerIO (counter (Info "cache_hits_total" "Cache hits"))
  <*> registerIO (counter (Info "cache_misses_total" "Cache misses"))
  <*> registerIO (histogram
        (Info "db_query_duration_seconds" "DB query duration")
        [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0])
  <*> registerIO (counter (Info "errors_total" "Total errors"))

-- Record metrics middleware
metricsMiddleware :: AppMetrics -> Application -> Application
metricsMiddleware metrics app request respond = do
  incGauge (amActiveRequests metrics)
  start <- getPOSIXTime
  
  app request $ \response -> do
    end <- getPOSIXTime
    let duration = fromRational (toRational (end - start))
    
    countAdd 1 (amRequestsTotal metrics)
    observe duration (amRequestDuration metrics)
    decGauge (amActiveRequests metrics)
    
    when (statusCode (responseStatus response) >= 500) $
      countAdd 1 (amErrors metrics)
    
    respond response

-- Expose metrics endpoint
metricsServer :: IO ()
metricsServer = do
  ghcMetrics <- registerGHCMetrics
  serveHttpTextMetrics 9090 ["metrics"]
```

---

## ขั้นตอนที่ 645: Health Checks

```haskell
-- Comprehensive health checks

data HealthStatus = Healthy | Degraded | Unhealthy
  deriving (Show, Eq, Ord)

data ComponentHealth = ComponentHealth
  { chName    :: Text
  , chStatus  :: HealthStatus
  , chMessage :: Text
  , chLatency :: Maybe Double  -- ms
  }

-- Health check functions
checkDatabase :: ConnectionPool -> IO ComponentHealth
checkDatabase pool = do
  start <- getMonotonicTime
  result <- E.try @SomeException $ runSqlPool (rawSql "SELECT 1" []) pool
  end   <- getMonotonicTime
  let latency = (end - start) * 1000
  
  return $ case result of
    Right _ -> ComponentHealth "database" Healthy "OK" (Just latency)
    Left e  -> ComponentHealth "database" Unhealthy (T.pack (show e)) (Just latency)

checkCache :: Redis.Connection -> IO ComponentHealth
checkCache conn = do
  start  <- getMonotonicTime
  result <- E.try @SomeException $ Redis.runRedis conn Redis.ping
  end    <- getMonotonicTime
  let latency = (end - start) * 1000
  
  return $ case result of
    Right (Right Redis.Pong) -> ComponentHealth "cache" Healthy "OK" (Just latency)
    Right (Left err)         -> ComponentHealth "cache" Degraded (T.pack (show err)) (Just latency)
    Left e                   -> ComponentHealth "cache" Unhealthy (T.pack (show e)) Nothing

checkExternalApi :: Text -> IO ComponentHealth
checkExternalApi url = do
  start  <- getMonotonicTime
  result <- E.try @SomeException $ httpGet url
  end    <- getMonotonicTime
  let latency = (end - start) * 1000
  
  return $ case result of
    Right (status, _)
      | statusCode status < 500 -> ComponentHealth "external_api" Healthy "OK" (Just latency)
      | otherwise               -> ComponentHealth "external_api" Degraded "5xx response" (Just latency)
    Left e -> ComponentHealth "external_api" Unhealthy (T.pack (show e)) Nothing

-- Aggregate health
aggregateHealth :: [ComponentHealth] -> HealthStatus
aggregateHealth components =
  let statuses = map chStatus components
  in if Unhealthy `elem` statuses then Unhealthy
     else if Degraded `elem` statuses then Degraded
     else Healthy

-- Health endpoint
healthHandler :: AppEnv -> Handler HealthResponse
healthHandler env = do
  checks <- liftIO $ mapConcurrently id
    [ checkDatabase  (envDbPool env)
    , checkCache     (envRedis env)
    , checkExternalApi "https://api.example.com/ping"
    ]
  
  let overall = aggregateHealth checks
  
  when (overall == Unhealthy) $
    throwError err503 { errBody = encode (errorResponse checks) }
  
  return HealthResponse
    { hrStatus     = overall
    , hrComponents = checks
    , hrVersion    = envVersion env
    , hrUptime     = envStartTime env
    }
```

---

## ขั้นตอนที่ 646: Alerting System

```haskell
-- Alerting rules and notifications

data AlertConfig = AlertConfig
  { acRules    :: [AlertRule]
  , acChannels :: [AlertChannel]
  }

data AlertRule = AlertRule
  { arName      :: Text
  , arCondition :: MetricsSnapshot -> Bool
  , arSeverity  :: AlertSeverity
  , arMessage   :: MetricsSnapshot -> Text
  , arCooldown  :: NominalDiffTime
  }

data AlertSeverity = Page | Warning | Info

data AlertChannel
  = SlackChannel { scWebhook :: Text, scChannel :: Text }
  | EmailChannel { ecAddresses :: [Text] }
  | PagerDuty    { pdKey :: Text }

-- Alert rules
highErrorRate :: AlertRule
highErrorRate = AlertRule
  { arName      = "high_error_rate"
  , arCondition = \snap -> snapshotErrorRate snap > 0.05
  , arSeverity  = Page
  , arMessage   = \snap -> "Error rate " <> formatPercent (snapshotErrorRate snap) <> " > 5%"
  , arCooldown  = 300  -- 5 minutes
  }

highLatency :: AlertRule
highLatency = AlertRule
  { arName      = "high_p99_latency"
  , arCondition = \snap -> snapshotP99Latency snap > 2000  -- 2 seconds
  , arSeverity  = Warning
  , arMessage   = \snap -> "P99 latency " <> formatMs (snapshotP99Latency snap) <> " > 2s"
  , arCooldown  = 600  -- 10 minutes
  }

-- Alert manager
data AlertManager = AlertManager
  { amConfig    :: AlertConfig
  , amHistory   :: TVar (Map Text UTCTime)  -- last alert time per rule
  }

checkAndAlert :: AlertManager -> MetricsSnapshot -> IO ()
checkAndAlert mgr snap = do
  now <- getCurrentTime
  forM_ (acRules (amConfig mgr)) $ \rule -> do
    when (arCondition rule snap) $ do
      lastAlert <- Map.lookup (arName rule) <$> readTVarIO (amHistory mgr)
      let shouldAlert = case lastAlert of
            Nothing -> True
            Just t  -> diffUTCTime now t > arCooldown rule
      
      when shouldAlert $ do
        let msg = arMessage rule snap
        forM_ (acChannels (amConfig mgr)) (sendAlert (arSeverity rule) msg)
        atomically $ modifyTVar (amHistory mgr) (Map.insert (arName rule) now)

sendAlert :: AlertSeverity -> Text -> AlertChannel -> IO ()
sendAlert sev msg (SlackChannel webhook chan) =
  postSlackMessage webhook chan (formatSlackAlert sev msg)
sendAlert sev msg (EmailChannel addrs) =
  sendEmail addrs ("Alert: " <> severityLabel sev) msg
sendAlert _ msg (PagerDuty key) =
  triggerPagerDuty key msg
```

---

## ขั้นตอนที่ 647: Distributed Tracing

```haskell
-- Advanced distributed tracing

-- Span with timing
data Span = Span
  { spanId        :: Text
  , spanTraceId   :: Text
  , spanName      :: Text
  , spanStart     :: UTCTime
  , spanEnd       :: Maybe UTCTime
  , spanTags      :: Map Text Text
  , spanLogs      :: [(UTCTime, Map Text Text)]
  , spanStatus    :: SpanStatus
  , spanParentId  :: Maybe Text
  }

data SpanStatus = OK | Error Text

-- Span builder
buildSpan :: Text -> TraceContext -> IO Span
buildSpan name ctx = do
  now   <- getCurrentTime
  sid   <- toText <$> nextRandom
  return Span
    { spanId       = sid
    , spanTraceId  = tcTraceId ctx
    , spanName     = name
    , spanStart    = now
    , spanEnd      = Nothing
    , spanTags     = Map.empty
    , spanLogs     = []
    , spanStatus   = OK
    , spanParentId = Just (tcSpanId ctx)
    }

-- Finish span
finishSpan :: Span -> IO Span
finishSpan span = do
  now <- getCurrentTime
  return span { spanEnd = Just now }

-- Trace a computation
traced :: Text -> TraceContext -> IO a -> IO (a, Span)
traced name ctx action = do
  span  <- buildSpan name ctx
  result <- E.try @SomeException action
  now   <- getCurrentTime
  let finalSpan = span
        { spanEnd    = Just now
        , spanStatus = case result of
            Right _ -> OK
            Left e  -> Error (T.pack (show e))
        }
  case result of
    Right v -> return (v, finalSpan)
    Left e  -> throwIO e

-- Export to Jaeger
exportToJaeger :: [Span] -> IO ()
exportToJaeger spans = do
  let payload = encode (map toJaegerSpan spans)
  post "http://jaeger:14268/api/traces" payload
  where
    toJaegerSpan s = object
      [ "traceId"      .= spanTraceId s
      , "spanId"       .= spanId s
      , "operationName" .= spanName s
      , "startTime"    .= (utcTimeToPOSIXSeconds (spanStart s) * 1e6)
      , "duration"     .= maybe 0 (\e -> diffUTCTime e (spanStart s) * 1e6) (spanEnd s)
      , "tags"         .= [object ["key" .= k, "value" .= v] | (k,v) <- Map.toList (spanTags s)]
      ]
```

---

## ขั้นตอนที่ 648: Log Aggregation

```haskell
-- Centralized log shipping

import Network.HTTP.Simple
import Data.Aeson

-- Elasticsearch log shipper
data ElasticsearchConfig = ElasticsearchConfig
  { escHost  :: Text
  , escPort  :: Int
  , escIndex :: Text
  }

-- Ship logs to Elasticsearch
shipToElasticsearch :: ElasticsearchConfig -> [LogEntry] -> IO ()
shipToElasticsearch cfg logs = do
  let bulk = BSL.intercalate "\n"
        [ encode (indexAction (escIndex cfg))
          <> "\n" <> encode log
        | log <- logs
        ]
  
  let url = mconcat
        [ "http://", escHost cfg, ":", T.pack (show (escPort cfg))
        , "/", escIndex cfg, "/_bulk"
        ]
  
  response <- httpLBS (setRequestBodyLBS bulk (parseRequest_ url))
  unless (responseStatus response == ok200) $
    putStrLn $ "Log shipping failed: " ++ show (responseStatus response)

indexAction :: Text -> Value
indexAction idx = object
  [ "_index" .= idx
  , "_type"  .= ("_doc" :: Text)
  ]

-- Buffered log shipper
data LogBuffer = LogBuffer
  { lbBuffer  :: TVar [LogEntry]
  , lbSize    :: Int
  , lbFlushAt :: Int
  , lbShipper :: [LogEntry] -> IO ()
  }

addToBuffer :: LogBuffer -> LogEntry -> IO ()
addToBuffer buf entry = do
  entries <- atomically $ do
    modifyTVar (lbBuffer buf) (entry :)
    entries <- readTVar (lbBuffer buf)
    if length entries >= lbFlushAt buf
      then do
        writeTVar (lbBuffer buf) []
        return entries
      else return []
  
  unless (null entries) $
    lbShipper buf (reverse entries)

-- Flush buffer periodically
flushLoop :: LogBuffer -> IO ()
flushLoop buf = forever $ do
  threadDelay (5 * 1000000)  -- 5 seconds
  entries <- atomically $ do
    entries <- readTVar (lbBuffer buf)
    writeTVar (lbBuffer buf) []
    return entries
  unless (null entries) $
    lbShipper buf (reverse entries)
```

---

## ขั้นตอนที่ 649: SLO Tracking

```haskell
-- Service Level Objectives (SLO) tracking

data Slo = Slo
  { sloName      :: Text
  , sloTarget    :: Double  -- e.g., 0.999 for 99.9%
  , sloWindow    :: NominalDiffTime
  , sloMetric    :: IO Double  -- current value
  , sloErrorBudget :: IO Double
  }

-- Availability SLO
availabilitySlo :: MetricsService -> Slo
availabilitySlo metrics = Slo
  { sloName     = "availability"
  , sloTarget   = 0.999  -- 99.9%
  , sloWindow   = 30 * 24 * 3600  -- 30 days
  , sloMetric   = getAvailability metrics (30 * 24 * 3600)
  , sloErrorBudget = do
      avail <- getAvailability metrics (30 * 24 * 3600)
      return ((avail - 0.999) / (1 - 0.999))
  }

-- Latency SLO
latencySlo :: MetricsService -> Slo
latencySlo metrics = Slo
  { sloName     = "latency_p99"
  , sloTarget   = 0.99  -- 99% requests under 500ms
  , sloWindow   = 24 * 3600  -- 1 day
  , sloMetric   = getP99Compliance metrics 500 (24 * 3600)
  , sloErrorBudget = computeErrorBudget metrics
  }

-- SLO Dashboard
data SloDashboard = SloDashboard
  { sdSlos       :: [Slo]
  , sdPeriod     :: NominalDiffTime
  }

generateSloReport :: SloDashboard -> IO SloReport
generateSloReport dashboard = do
  statuses <- forM (sdSlos dashboard) $ \slo -> do
    current <- sloMetric slo
    budget  <- sloErrorBudget slo
    return SloStatus
      { ssName          = sloName slo
      , ssCurrent       = current
      , ssTarget        = sloTarget slo
      , ssErrorBudget   = budget
      , ssStatus        = if current >= sloTarget slo then Meeting else Violating
      , ssBurnRate      = (1 - current) / (1 - sloTarget slo)
      }
  
  return SloReport
    { srStatuses  = statuses
    , srGeneratedAt = getCurrentTime
    }
```

---

## ขั้นตอนที่ 650: On-Call Runbook System

```haskell
-- On-call runbook automation

data Runbook = Runbook
  { rbName        :: Text
  , rbTrigger     :: AlertRule
  , rbSteps       :: [RunbookStep]
  , rbOnComplete  :: RunbookResult -> IO ()
  }

data RunbookStep
  = AutoStep  { asName :: Text, asAction :: IO StepResult }
  | ManualStep { msName :: Text, msInstructions :: Text }
  | CheckStep { csName :: Text, csCheck :: IO Bool, csMessage :: Text }
  | EscalateStep { esLevel :: Int, esMessage :: Text }

data StepResult = StepSuccess Text | StepFailure Text

-- Execute runbook
executeRunbook :: Runbook -> IO RunbookResult
executeRunbook runbook = do
  results <- forM (rbSteps runbook) executeStep
  let success = all stepSucceeded results
  let result  = RunbookResult (rbName runbook) success results
  rbOnComplete runbook result
  return result

executeStep :: RunbookStep -> IO (Text, StepResult)
executeStep (AutoStep name action) = do
  result <- E.try @SomeException action
  return (name, case result of
    Right r -> r
    Left e  -> StepFailure (T.pack (show e)))

executeStep (CheckStep name check msg) = do
  ok <- check
  return (name, if ok then StepSuccess msg else StepFailure msg)

executeStep (ManualStep name instructions) = do
  putStrLn $ T.unpack $ "MANUAL: " <> name
  putStrLn $ T.unpack instructions
  putStrLn "Press Enter when done..."
  _ <- getLine
  return (name, StepSuccess "Manual step completed")

executeStep (EscalateStep level msg) = do
  putStrLn $ "ESCALATING to level " ++ show level ++ ": " ++ T.unpack msg
  -- Send PagerDuty escalation
  return ("escalate", StepSuccess "Escalated")
```

---

## ขั้นตอนที่ 651: Distributed Configuration

```haskell
-- Configuration management ด้วย Consul/etcd

import Network.Consul.Client
import Data.Aeson

-- Configuration source
data ConfigSource
  = ConsulSource  { csHost :: Text, csPrefix :: Text }
  | EtcdSource    { esHost :: Text, esPrefix :: Text }
  | EnvSource     { esPrefix :: Text }
  | FileSource    { fsPath :: FilePath }

-- Dynamic configuration with hot-reload
data DynamicConfig = DynamicConfig
  { dcValues  :: TVar (Map Text Value)
  , dcWatcher :: IO ()  -- background watcher thread
  }

initDynamicConfig :: ConfigSource -> IO DynamicConfig
initDynamicConfig (ConsulSource host prefix) = do
  values    <- loadAllFromConsul host prefix
  valuesVar <- newTVarIO values
  
  let watcher = watchConsul host prefix valuesVar
  
  watcherThread <- forkIO (forever watcher)
  
  return DynamicConfig
    { dcValues  = valuesVar
    , dcWatcher = return ()
    }

watchConsul :: Text -> Text -> TVar (Map Text Value) -> IO ()
watchConsul host prefix valuesVar = do
  threadDelay 5000000  -- poll every 5 seconds
  newValues <- loadAllFromConsul host prefix
  atomically $ writeTVar valuesVar newValues

-- Get config value with type-safe parsing
getConfig :: FromJSON a => DynamicConfig -> Text -> IO (Maybe a)
getConfig cfg key = do
  values <- readTVarIO (dcValues cfg)
  return $ do
    val <- Map.lookup key values
    case fromJSON val of
      Success v -> Just v
      Error   _ -> Nothing

-- Feature flags
data FeatureFlags = FeatureFlags
  { ffEnableNewDashboard :: Bool
  , ffMaxUploadSize      :: Int
  , ffBetaUsers          :: [Text]
  }

isFeatureEnabled :: FeatureFlags -> Text -> UserId -> Bool
isFeatureEnabled flags "new_dashboard" uid =
  ffEnableNewDashboard flags || userEmail uid `elem` ffBetaUsers flags
isFeatureEnabled _ _ _ = False
```

---

## ขั้นตอนที่ 652: Service Discovery

```haskell
-- Service discovery pattern

data ServiceRegistry = ServiceRegistry
  { srServices :: TVar (Map Text [ServiceInstance])
  }

data ServiceInstance = ServiceInstance
  { siId      :: Text
  , siHost    :: Text
  , siPort    :: Int
  , siHealth  :: HealthStatus
  , siMeta    :: Map Text Text
  , siLastSeen :: UTCTime
  }

-- Register service
registerService :: ServiceRegistry -> Text -> ServiceInstance -> IO ()
registerService registry name instance' = atomically $
  modifyTVar (srServices registry) $
    Map.insertWith (++) name [instance']

-- Discover services (round-robin load balancing)
data LoadBalancer = LoadBalancer
  { lbRegistry :: ServiceRegistry
  , lbCounters :: TVar (Map Text Int)  -- current index per service
  }

nextInstance :: LoadBalancer -> Text -> IO (Maybe ServiceInstance)
nextInstance lb serviceName = do
  instances <- Map.findWithDefault [] serviceName
    <$> readTVarIO (srServices (lbRegistry lb))
  
  let healthy = filter ((== Healthy) . siHealth) instances
  
  if null healthy
    then return Nothing
    else do
      idx <- atomically $ do
        counters <- readTVar (lbCounters lb)
        let current = fromMaybe 0 (Map.lookup serviceName counters)
        let next    = (current + 1) `mod` length healthy
        writeTVar (lbCounters lb) (Map.insert serviceName next counters)
        return current
      return (Just (healthy !! (idx `mod` length healthy)))

-- Health check loop
healthCheckLoop :: ServiceRegistry -> IO ()
healthCheckLoop registry = forever $ do
  threadDelay 10000000  -- 10 seconds
  services <- readTVarIO (srServices registry)
  
  forM_ (Map.toList services) $ \(name, instances) -> do
    updatedInstances <- forM instances $ \inst -> do
      health <- checkInstanceHealth inst
      return inst { siHealth = health }
    
    atomically $ modifyTVar (srServices registry) (Map.insert name updatedInstances)
```

---

## ขั้นตอนที่ 653: Incident Management

```haskell
-- Incident management workflow

data Incident = Incident
  { incId          :: UUID
  , incTitle       :: Text
  , incDescription :: Text
  , incSeverity    :: Severity
  , incStatus      :: IncidentStatus
  , incCreatedAt   :: UTCTime
  , incResolvedAt  :: Maybe UTCTime
  , incTimeline    :: [IncidentEvent]
  , incAssignee    :: Maybe UserId
  }

data IncidentStatus = Open | Investigating | Resolved | Closed

data IncidentEvent = IncidentEvent
  { ieTime    :: UTCTime
  , ieActor   :: Text
  , ieAction  :: Text
  , ieDetails :: Text
  }

-- Incident lifecycle
createIncident :: IncidentRequest -> IO Incident
createIncident req = do
  now <- getCurrentTime
  uid <- nextRandom
  return Incident
    { incId          = uid
    , incTitle       = irTitle req
    , incDescription = irDescription req
    , incSeverity    = irSeverity req
    , incStatus      = Open
    , incCreatedAt   = now
    , incResolvedAt  = Nothing
    , incTimeline    = [IncidentEvent now "system" "created" "Incident created"]
    , incAssignee    = Nothing
    }

-- Post-incident review
data PostMortem = PostMortem
  { pmIncident     :: Incident
  , pmSummary      :: Text
  , pmTimeline     :: [TimelineItem]
  , pmImpact       :: Text
  , pmRootCause    :: Text
  , pmContributing :: [Text]
  , pmRemediation  :: [RemediationItem]
  , pmFollowUps    :: [ActionItem]
  }

data RemediationItem = RemediationItem
  { riAction   :: Text
  , riOwner    :: Text
  , riDeadline :: Day
  , riStatus   :: ActionStatus
  }

generatePostMortemReport :: PostMortem -> Text
generatePostMortemReport pm = T.unlines
  [ "# Post-Mortem: " <> incTitle (pmIncident pm)
  , ""
  , "## Summary"
  , pmSummary pm
  , ""
  , "## Impact"
  , pmImpact pm
  , ""
  , "## Root Cause"
  , pmRootCause pm
  , ""
  , "## Timeline"
  , T.unlines (map formatTimelineItem (pmTimeline pm))
  , ""
  , "## Remediation Actions"
  , T.unlines (map formatRemediation (pmRemediation pm))
  ]
```

---

## ขั้นตอนที่ 654: Capacity Planning

```haskell
-- Capacity planning tools

data CapacityMetrics = CapacityMetrics
  { cmCpuUsage     :: Double  -- 0-100%
  , cmMemoryUsage  :: Double  -- 0-100%
  , cmDiskUsage    :: Double  -- 0-100%
  , cmNetworkIn    :: Double  -- Mbps
  , cmNetworkOut   :: Double  -- Mbps
  , cmRequestRate  :: Double  -- req/s
  , cmConnections  :: Int
  }

-- Growth forecasting
forecastCapacity :: [CapacityMetrics] -> Int -> [CapacityMetrics]
forecastCapacity historical days =
  let n        = length historical
      cpuTrend = linearTrend (map cmCpuUsage historical)
      memTrend = linearTrend (map cmMemoryUsage historical)
      reqTrend = linearTrend (map cmRequestRate historical)
  in [ CapacityMetrics
        { cmCpuUsage    = clamp 0 100 (fst cpuTrend + snd cpuTrend * fromIntegral (n + i))
        , cmMemoryUsage = clamp 0 100 (fst memTrend + snd memTrend * fromIntegral (n + i))
        , cmRequestRate = max 0 (fst reqTrend + snd reqTrend * fromIntegral (n + i))
        , cmDiskUsage   = 50  -- placeholder
        , cmNetworkIn   = 100 -- placeholder
        , cmNetworkOut  = 100 -- placeholder
        , cmConnections = 0   -- placeholder
        }
     | i <- [1..days]
     ]
  where clamp lo hi x = max lo (min hi x)

-- When will we hit capacity?
estimateCapacityDate :: [CapacityMetrics] -> Double -> Maybe Day
estimateCapacityDate historical threshold =
  let cpuTrend = linearTrend (map cmCpuUsage historical)
  in if snd cpuTrend <= 0
     then Nothing  -- not growing
     else
       let daysToFull = (threshold - fst cpuTrend) / snd cpuTrend
           n          = fromIntegral (length historical)
       in Just (addDays (floor (daysToFull - n)) today)
```

---

## ขั้นตอนที่ 655: Disaster Recovery

```haskell
-- Disaster recovery procedures

data RecoveryConfig = RecoveryConfig
  { rcRpo         :: NominalDiffTime  -- Recovery Point Objective
  , rcRto         :: NominalDiffTime  -- Recovery Time Objective
  , rcBackupBucket :: Text
  , rcPrimaryDb   :: Text
  , rcStandbyDb   :: Text
  }

-- Database backup
backupDatabase :: RecoveryConfig -> IO BackupResult
backupDatabase cfg = do
  timestamp <- formatTime defaultTimeLocale "%Y%m%d_%H%M%S" <$> getCurrentTime
  let filename = "backup_" <> T.pack timestamp <> ".sql.gz"
  
  -- pg_dump and compress
  exitCode <- runProcess (proc "pg_dump"
    [ T.unpack (rcPrimaryDb cfg)
    , "|", "gzip", "-9"
    , ">", "/tmp/" ++ T.unpack filename
    ])
  
  case exitCode of
    ExitSuccess -> do
      -- Upload to S3
      uploadToS3 (rcBackupBucket cfg) filename
      return (BackupSuccess filename)
    ExitFailure code ->
      return (BackupFailed ("pg_dump failed with code " <> T.pack (show code)))

-- Point-in-time recovery test
testRecovery :: RecoveryConfig -> UTCTime -> IO RecoveryTestResult
testRecovery cfg targetTime = do
  start <- getCurrentTime
  
  -- Find appropriate backup
  mBackup <- findNearestBackup cfg targetTime
  
  case mBackup of
    Nothing -> return RecoveryTestFailed { rtfReason = "No backup found" }
    Just backup -> do
      -- Restore to test instance
      restoreToTest backup cfg
      end <- getCurrentTime
      
      let duration = diffUTCTime end start
      return RecoveryTestSuccess
        { rtsBackupAge = diffUTCTime targetTime (backupTime backup)
        , rtsDuration  = duration
        , rtsRtoMet    = duration <= rcRto cfg
        , rtsRpoMet    = backupAge backup <= rcRpo cfg
        }
```

---

## ขั้นตอนที่ 656: Cost Optimization

```haskell
-- Cost monitoring and optimization

data ResourceCost = ResourceCost
  { rcCompute  :: Double  -- USD/hour
  , rcMemory   :: Double  -- USD/GB/hour
  , rcStorage  :: Double  -- USD/GB/month
  , rcNetwork  :: Double  -- USD/GB
  , rcDatabase :: Double  -- USD/hour
  }

-- Cost analysis per service
analyzeServiceCost :: [ServiceMetrics] -> ResourceCost -> ServiceCostReport
analyzeServiceCost metrics rates = ServiceCostReport
  { scrDailyCost   = computeDailyCost metrics rates
  , scrMonthlyCost = computeMonthlyCost metrics rates
  , scrWastedCost  = computeWaste metrics rates
  , scrOptimizations = suggestOptimizations metrics
  }

computeWaste :: [ServiceMetrics] -> ResourceCost -> Double
computeWaste metrics rates =
  let avgCpu  = avg (map smCpuUsage metrics)
      avgMem  = avg (map smMemoryUsage metrics)
      wastedCpu = max 0 (0.8 - avgCpu / 100)  -- waste if < 80% util
      wastedMem = max 0 (0.7 - avgMem / 100)  -- waste if < 70% util
  in wastedCpu * rcCompute rates + wastedMem * rcMemory rates

suggestOptimizations :: [ServiceMetrics] -> [Optimization]
suggestOptimizations metrics =
  concat
    [ [ DownscaleCompute | avgCpu < 30 ]
    , [ DownscaleMemory  | avgMem < 40 ]
    , [ RightSizeInstances | maxCpu < 60 && maxMem < 70 ]
    , [ EnableAutoScaling | variation > 0.5 ]
    , [ UseSpotInstances | avgCpu < 50 && not criticalService ]
    ]
  where
    avgCpu  = avg (map smCpuUsage metrics)
    avgMem  = avg (map smMemoryUsage metrics)
    maxCpu  = maximum (map smCpuUsage metrics)
    maxMem  = maximum (map smMemoryUsage metrics)
    variation = stdDev (map smCpuUsage metrics) / avgCpu
    criticalService = any smIsCritical metrics
```

---

## ขั้นตอนที่ 657: Deployment Verification

```haskell
-- Deployment verification and rollback

data DeploymentVerifier = DeploymentVerifier
  { dvChecks    :: [VerificationCheck]
  , dvTimeout   :: Int  -- seconds
  , dvThreshold :: Double  -- success threshold 0-1
  }

data VerificationCheck
  = SmokeTest    { stName :: Text, stCheck :: IO Bool }
  | MetricCheck  { mcName :: Text, mcCheck :: IO Bool }
  | CanaryCheck  { ccName :: Text, ccPercentile :: Double, ccThreshold :: Double }

-- Verify deployment
verifyDeployment :: DeploymentVerifier -> IO VerificationResult
verifyDeployment verifier = do
  results <- withTimeout (dvTimeout verifier) $
    mapConcurrently runCheck (dvChecks verifier)
  
  let passed   = length (filter fst results)
  let total    = length results
  let passRate = fromIntegral passed / fromIntegral total
  
  return VerificationResult
    { vrPassed    = passRate >= dvThreshold verifier
    , vrPassRate  = passRate
    , vrDetails   = results
    }

runCheck :: VerificationCheck -> IO (Bool, Text)
runCheck (SmokeTest name check) = do
  ok <- check
  return (ok, if ok then name <> " passed" else name <> " failed")

runCheck (MetricCheck name check) = do
  ok <- check
  return (ok, if ok then name <> " within threshold" else name <> " exceeded threshold")

-- Blue-green deployment
data BlueGreen = BlueGreen
  { bgBlue     :: ServiceVersion
  , bgGreen    :: ServiceVersion
  , bgActive   :: TVar Color
  , bgRouter   :: LoadBalancer
  }

data Color = Blue | Green

switchTraffic :: BlueGreen -> Color -> IO ()
switchTraffic bg newActive = do
  atomically $ writeTVar (bgActive bg) newActive
  putStrLn $ "Switched to " ++ show newActive
```

---

## ขั้นตอนที่ 658: Audit Logging

```haskell
-- Audit logging for compliance

data AuditEvent = AuditEvent
  { aeId         :: UUID
  , aeTimestamp  :: UTCTime
  , aeUserId     :: UserId
  , aeAction     :: Text
  , aeResource   :: Text
  , aeResourceId :: Text
  , aeBefore     :: Maybe Value
  , aeAfter      :: Maybe Value
  , aeIpAddress  :: Text
  , aeUserAgent  :: Text
  , aeResult     :: AuditResult
  }

data AuditResult = AuditSuccess | AuditDenied Text | AuditFailed Text

-- Audit logger
class AuditLogger m where
  logAudit :: AuditEvent -> m ()

instance AuditLogger IO where
  logAudit event = do
    -- Write to append-only audit log
    let json = encode event
    BSL.appendFile "/var/log/audit.jsonl" (json <> "\n")
    -- Also write to database
    saveAuditToDb event

-- Middleware
auditMiddleware :: (UserId -> AuditEvent -> IO ()) -> Application -> Application
auditMiddleware logger app request respond = do
  let uid    = extractUserId request
  let action = T.pack (requestMethod request)
  let path   = T.pack (rawPathInfo request)
  
  start  <- getCurrentTime
  bodyVar <- newIORef Nothing
  
  app request $ \response -> do
    uid' <- uid
    now  <- getCurrentTime
    
    event <- mkAuditEvent uid' action path now
    logger uid' event
    
    respond response

-- Query audit log
queryAuditLog :: AuditQuery -> IO [AuditEvent]
queryAuditLog q = do
  events <- readAuditDb
  return $ filter (matchesQuery q) events
  where
    matchesQuery q e =
      maybe True (== aeUserId e) (aqUserId q) &&
      maybe True (`T.isInfixOf` aeAction e) (aqAction q) &&
      maybe True (>= aeTimestamp e) (aqFrom q) &&
      maybe True (<= aeTimestamp e) (aqTo q)
```

---

## ขั้นตอนที่ 659: Compliance Reporting

```haskell
-- Compliance checks and reporting (GDPR, SOC2)

data ComplianceFramework = GDPR | SOC2 | HIPAA | PCI_DSS

data ComplianceCheck = ComplianceCheck
  { ccFramework   :: ComplianceFramework
  , ccControl     :: Text
  , ccDescription :: Text
  , ccCheck       :: IO ComplianceStatus
  }

data ComplianceStatus = Pass | Fail Text | NotApplicable

-- GDPR checks
gdprChecks :: [ComplianceCheck]
gdprChecks =
  [ ComplianceCheck GDPR "data_encryption"
      "Personal data must be encrypted at rest"
      checkEncryptionAtRest
  
  , ComplianceCheck GDPR "right_to_erasure"
      "Must support deletion of personal data"
      checkErasureSupport
  
  , ComplianceCheck GDPR "data_retention"
      "Personal data must not be kept longer than necessary"
      checkDataRetentionPolicies
  
  , ComplianceCheck GDPR "consent_tracking"
      "Must track user consent"
      checkConsentTracking
  
  , ComplianceCheck GDPR "breach_notification"
      "Must be able to detect and report breaches within 72h"
      checkBreachDetection
  ]

-- Run compliance checks
runComplianceAudit :: [ComplianceCheck] -> IO ComplianceReport
runComplianceAudit checks = do
  results <- forM checks $ \check -> do
    status <- ccCheck check
    return (check, status)
  
  let passed = length (filter ((== Pass) . snd) results)
  let failed = length (filter (isFail . snd) results)
  
  return ComplianceReport
    { crChecks     = results
    , crPassCount  = passed
    , crFailCount  = failed
    , crScore      = fromIntegral passed / fromIntegral (length checks)
    , crGeneratedAt = getCurrentTime
    }
  where isFail (Fail _) = True
        isFail _        = False
```

---

## ขั้นตอนที่ 660: โปรเจกต์: Complete Observability Stack

```haskell
-- Complete observability stack

data ObservabilityStack = ObservabilityStack
  { osLogger     :: LogAction IO AppMsg
  , osTracer     :: TracerProvider
  , osMetrics    :: AppMetrics
  , osAlerts     :: AlertManager
  , osHealth     :: HealthChecker
  }

initObservability :: ObservabilityConfig -> IO ObservabilityStack
initObservability cfg = do
  -- Initialize all components
  logger   <- initStructuredLogger (ocLogLevel cfg)
  tracer   <- initTracer (ocServiceName cfg)
  metrics  <- initMetrics
  alerts   <- initAlertManager (ocAlertConfig cfg)
  health   <- initHealthChecker (ocHealthConfig cfg)
  
  -- Register GHC runtime metrics
  registerGHCMetrics
  
  -- Start background workers
  forkIO $ flushLoop (ocLogBuffer cfg)
  forkIO $ collectMetricsLoop metrics
  forkIO $ checkAlertsLoop alerts metrics
  forkIO $ healthCheckLoop health
  
  return ObservabilityStack
    { osLogger  = logger
    , osTracer  = tracer
    , osMetrics = metrics
    , osAlerts  = alerts
    , osHealth  = health
    }

-- Unified middleware
observabilityMiddleware :: ObservabilityStack -> Application -> Application
observabilityMiddleware obs app =
  logRequestMiddleware (osLogger obs) $
  traceMiddleware (osTracer obs) $
  metricsMiddleware (osMetrics obs) $
  app

-- Status endpoint
statusEndpoint :: ObservabilityStack -> Handler StatusResponse
statusEndpoint obs = do
  health  <- liftIO $ runHealthChecks (osHealth obs)
  metrics <- liftIO $ collectSnapshot (osMetrics obs)
  
  return StatusResponse
    { srHealth  = health
    , srMetrics = metrics
    , srVersion = "1.0.0"
    }
```

---

*[← Part 32](part-32.md) | [Part 34 →](part-34.md)*
