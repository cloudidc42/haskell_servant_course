# Part 23: Advanced Concurrency และ Distributed Systems
## ขั้นตอนที่ 441-460

---

## ขั้นตอนที่ 441: STM ขั้นสูง

```haskell
-- Software Transactional Memory ขั้นสูง

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue

-- Composable transactions
transferMoney :: TVar Int -> TVar Int -> Int -> STM (Either TransferError ())
transferMoney fromAcc toAcc amount = do
  from <- readTVar fromAcc
  if from < amount
    then return (Left InsufficientFunds)
    else do
      writeTVar fromAcc (from - amount)
      modifyTVar toAcc (+amount)
      return (Right ())

-- Retry: block until condition
waitForFunds :: TVar Int -> Int -> STM ()
waitForFunds acc amount = do
  balance <- readTVar acc
  when (balance < amount) retry  -- retry ทำให้ transaction รอจนกว่า TVar จะเปลี่ยน

-- OrElse: try alternatives
checkEither :: TVar (Maybe a) -> TVar (Maybe a) -> STM a
checkEither tv1 tv2 =
  readValue tv1 `orElse` readValue tv2
  where
    readValue tv = do
      mVal <- readTVar tv
      case mVal of
        Nothing -> retry
        Just v  -> return v

-- Multi-producer, multi-consumer queue
data WorkQueue a = WorkQueue
  { wqQueue  :: TBQueue a       -- bounded queue
  , wqActive :: TVar Int         -- active workers
  , wqDone   :: TVar Bool        -- shutdown signal
  }

newWorkQueue :: Int -> Int -> STM (WorkQueue a)
newWorkQueue size maxWorkers = WorkQueue
  <$> newTBQueue (fromIntegral size)
  <*> newTVar 0
  <*> newTVar False

submitWork :: WorkQueue a -> a -> IO Bool
submitWork q item = atomically $ do
  done <- readTVar (wqDone q)
  if done
    then return False
    else do
      writeTBQueue (wqQueue q) item
      return True

processWork :: WorkQueue a -> (a -> IO ()) -> IO ()
processWork q handler = do
  atomically $ modifyTVar (wqActive q) (+1)
  go
  atomically $ modifyTVar (wqActive q) (subtract 1)
  where
    go = do
      mItem <- atomically $ do
        done <- readTVar (wqDone q)
        if done
          then return Nothing
          else do
            mItem <- tryReadTBQueue (wqQueue q)
            case mItem of
              Nothing -> if done then return Nothing else retry
              Just i  -> return (Just i)
      case mItem of
        Nothing   -> return ()
        Just item -> handler item >> go
```

---

## ขั้นตอนที่ 442: Distributed Actor Model

```haskell
-- Actor Model ด้วย Cloud Haskell (distributed-process)

import Control.Distributed.Process
import Control.Distributed.Process.Node

-- Actor (Process)
data ActorMessage
  = Ping ProcessId
  | Pong ProcessId
  | Stop

pingActor :: Process ()
pingActor = do
  self <- getSelfPid
  say "Ping actor started"
  receive (self) -- wait for first message
  where
    receive self = do
      msg <- expect :: Process ActorMessage
      case msg of
        Ping sender -> do
          say "Got Ping, sending Pong"
          send sender (Pong self)
          receive self
        Stop -> say "Stopping"
        _    -> receive self

pongActor :: ProcessId -> Process ()
pongActor pingPid = do
  self <- getSelfPid
  send pingPid (Ping self)
  go 5
  where
    go 0 = do
      send pingPid Stop
      say "Done"
    go n = do
      Pong _ <- expect
      say $ "Got Pong " ++ show (5 - n + 1)
      self <- getSelfPid
      send pingPid (Ping self)
      go (n-1)

-- Start actors
main :: IO ()
main = do
  node <- newLocalNode initRemoteTable
  runProcess node $ do
    pingPid <- spawnLocal pingActor
    spawnLocal (pongActor pingPid)
    liftIO (threadDelay 1000000)
```

---

## ขั้นตอนที่ 443: Distributed Locks

```haskell
-- Distributed locks ด้วย Redis

import Database.Redis

data DistributedLock = DistributedLock
  { lockKey    :: ByteString
  , lockValue  :: ByteString  -- unique identifier
  , lockTtl    :: Integer     -- seconds
  }

acquireLock :: Connection -> ByteString -> Integer -> IO (Maybe DistributedLock)
acquireLock conn key ttl = do
  value <- generateUniqueId
  result <- runRedis conn $ do
    r <- setOpts key value SetOpts
      { setSeconds   = Just ttl
      , setMilliseconds = Nothing
      , setCondition = Just Nx  -- Only if not exists
      }
    return r
  case result of
    Right Ok -> return $ Just $ DistributedLock key value ttl
    _        -> return Nothing

releaseLock :: Connection -> DistributedLock -> IO Bool
releaseLock conn lock = do
  -- Lua script: atomic check-and-delete
  let script = "if redis.call('get', KEYS[1]) == ARGV[1] then \
               \  return redis.call('del', KEYS[1]) \
               \else \
               \  return 0 \
               \end"
  result <- runRedis conn $ eval script [lockKey lock] [lockValue lock]
  return $ result == Right (Integer 1)

withDistributedLock :: Connection -> ByteString -> Integer -> IO a -> IO (Either LockError a)
withDistributedLock conn key ttl action = do
  mLock <- acquireLock conn key ttl
  case mLock of
    Nothing -> return (Left LockAcquisitionFailed)
    Just lock -> do
      result <- E.try action
      released <- releaseLock conn lock
      case result of
        Left err -> return (Left (ActionFailed err))
        Right v  -> return (Right v)

-- Lock with retry
acquireLockWithRetry :: Connection -> ByteString -> Integer -> Int -> IO (Maybe DistributedLock)
acquireLockWithRetry conn key ttl maxRetries = go maxRetries
  where
    go 0 = return Nothing
    go n = do
      mLock <- acquireLock conn key ttl
      case mLock of
        Just lock -> return (Just lock)
        Nothing   -> do
          threadDelay 100000  -- 100ms
          go (n-1)
```

---

## ขั้นตอนที่ 444: Consensus Algorithm

```haskell
-- Raft-like consensus ใน Haskell

data NodeRole = Follower | Candidate | Leader

data RaftState = RaftState
  { rsRole        :: NodeRole
  , rsTerm        :: Int
  , rsVotedFor    :: Maybe NodeId
  , rsLog         :: [LogEntry]
  , rsCommitIndex :: Int
  , rsLastApplied :: Int
  , rsVotesReceived :: Set NodeId
  }

data LogEntry = LogEntry
  { leIndex   :: Int
  , leTerm    :: Int
  , leCommand :: ByteString
  }

data RaftMessage
  = RequestVote     { rvTerm :: Int, rvCandidateId :: NodeId, rvLastLogIndex :: Int, rvLastLogTerm :: Int }
  | RequestVoteResp { rvrTerm :: Int, rvrGranted :: Bool }
  | AppendEntries   { aeTerm :: Int, aeLeaderId :: NodeId, aePrevLogIndex :: Int, aePrevLogTerm :: Int, aeEntries :: [LogEntry], aeLeaderCommit :: Int }
  | AppendEntriesResp { aerTerm :: Int, aerSuccess :: Bool, aerMatchIndex :: Int }

-- Process message
handleMessage :: NodeId -> [NodeId] -> RaftMessage -> State RaftState [RaftAction]
handleMessage self peers msg = do
  state <- get
  case msg of
    RequestVote term candidateId lastLogIdx lastLogTerm -> do
      if term > rsTerm state
        then do
          put state { rsTerm = term, rsRole = Follower, rsVotedFor = Nothing }
          grantVoteIfValid candidateId lastLogIdx lastLogTerm
        else if term < rsTerm state
          then return [ReplyWith (RequestVoteResp (rsTerm state) False)]
          else grantVoteIfValid candidateId lastLogIdx lastLogTerm
    
    AppendEntries term leaderId prevLogIdx prevLogTerm entries leaderCommit -> do
      if term < rsTerm state
        then return [ReplyWith (AppendEntriesResp (rsTerm state) False 0)]
        else do
          put state { rsTerm = term, rsRole = Follower }
          appendLogEntries prevLogIdx prevLogTerm entries leaderCommit
    
    _ -> return []

data RaftAction
  = ReplyWith RaftMessage
  | SendTo NodeId RaftMessage
  | Broadcast RaftMessage
  | ApplyCommand ByteString
  | StartElectionTimer
  | ResetElectionTimer
```

---

## ขั้นตอนที่ 445: Message Passing Patterns

```haskell
-- Message passing patterns

-- Request-Reply
data Request req resp = Request
  { reqPayload :: req
  , reqReply   :: MVar (Either Error resp)
  }

sendRequest :: Chan (Request req resp) -> req -> IO (Either Error resp)
sendRequest chan payload = do
  replyMVar <- newEmptyMVar
  writeChan chan (Request payload replyMVar)
  takeMVar replyMVar

handleRequests :: Chan (Request req resp) -> (req -> IO (Either Error resp)) -> IO ()
handleRequests chan handler = forever $ do
  Request payload reply <- readChan chan
  result <- handler payload
  putMVar reply result

-- Publish-Subscribe
data Topic = Topic Text

data PubSub = PubSub
  { subscribers :: TVar (Map Topic [Chan ByteString])
  }

subscribe :: PubSub -> Topic -> IO (Chan ByteString)
subscribe ps topic = do
  chan <- newChan
  atomically $ modifyTVar (subscribers ps) $
    Map.insertWith (++) topic [chan]
  return chan

publish :: PubSub -> Topic -> ByteString -> IO ()
publish ps topic msg = do
  subs <- readTVarIO (subscribers ps)
  let chans = fromMaybe [] (Map.lookup topic subs)
  mapM_ (\c -> writeChan c msg) chans

unsubscribe :: PubSub -> Topic -> Chan ByteString -> IO ()
unsubscribe ps topic chan = atomically $ modifyTVar (subscribers ps) $
  Map.adjust (filter (/= chan)) topic

-- Event streaming
streamEvents :: [Event] -> Chan Event -> IO ()
streamEvents events chan = mapM_ (writeChan chan) events

processEventStream :: Chan Event -> (Event -> IO ()) -> IO ()
processEventStream chan handler = forever $ do
  event <- readChan chan
  handler event
```

---

## ขั้นตอนที่ 446: Distributed Cache

```haskell
-- Distributed Cache ด้วย Redis Cluster

import Database.Redis
import Data.Hashable

-- Consistent hashing
data HashRing = HashRing
  { ringNodes   :: [(Int, Text)]  -- (hash, nodeId)
  , ringVnodes  :: Int
  }

buildRing :: [Text] -> Int -> HashRing
buildRing nodes vnodes = HashRing
  { ringNodes = sortBy (compare `on` fst)
      [ (hash (node <> T.pack (show i)), node)
      | node <- nodes
      , i    <- [0..vnodes-1]
      ]
  , ringVnodes = vnodes
  }

getNode :: HashRing -> Text -> Text
getNode ring key =
  let h = hash key
      nodes = ringNodes ring
  in case dropWhile (\(n,_) -> n < h) nodes of
    []         -> snd (head nodes)  -- wrap around
    ((_,n):_)  -> n

-- Multi-level cache
data MultiCache = MultiCache
  { l1Cache :: IORef (HashMap Text CacheEntry)  -- L1: in-process
  , l2Cache :: Connection                         -- L2: Redis
  }

data CacheEntry = CacheEntry
  { ceValue   :: ByteString
  , ceExpires :: UTCTime
  }

getCached :: MultiCache -> Text -> IO (Maybe ByteString)
getCached cache key = do
  -- Try L1
  l1 <- readIORef (l1Cache cache)
  now <- getCurrentTime
  case HashMap.lookup key l1 >>= \e -> if ceExpires e > now then Just (ceValue e) else Nothing of
    Just v -> return (Just v)
    Nothing -> do
      -- Try L2 (Redis)
      result <- runRedis (l2Cache cache) $ get (encodeUtf8 key)
      case result of
        Right (Just v) -> do
          -- Populate L1
          expiry <- addUTCTime 60 <$> getCurrentTime
          atomicModifyIORef' (l1Cache cache) $
            \m -> (HashMap.insert key (CacheEntry v expiry) m, ())
          return (Just v)
        _ -> return Nothing

setCached :: MultiCache -> Text -> ByteString -> Int -> IO ()
setCached cache key value ttl = do
  -- Write to L2
  runRedis (l2Cache cache) $ setex (encodeUtf8 key) (fromIntegral ttl) value
  -- Invalidate L1
  atomicModifyIORef' (l1Cache cache) $
    \m -> (HashMap.delete key m, ())
```

---

## ขั้นตอนที่ 447: Task Scheduling

```haskell
-- Task scheduling ด้วย cron-like scheduler

import Control.Concurrent.Cron

-- Define scheduled tasks
type CronSchedule = Text

data ScheduledTask = ScheduledTask
  { taskName     :: Text
  , taskSchedule :: CronSchedule
  , taskAction   :: IO ()
  , taskTimeout  :: Int  -- seconds
}

-- Cron expressions
everyMinute    :: CronSchedule
everyMinute    = "* * * * *"

everyHour :: CronSchedule
everyHour = "0 * * * *"

everyDay :: CronSchedule
everyDay = "0 0 * * *"

everyMonday :: CronSchedule
everyMonday = "0 9 * * 1"

-- Task scheduler
data TaskScheduler = TaskScheduler
  { tsJobs    :: TVar [JobState]
  , tsRunning :: TVar (Map Text Async)
  }

data JobState = JobState
  { jsTask      :: ScheduledTask
  , jsNextRun   :: UTCTime
  , jsLastRun   :: Maybe UTCTime
  , jsLastError :: Maybe Text
  }

runScheduler :: [ScheduledTask] -> IO ()
runScheduler tasks = do
  now <- getCurrentTime
  jobs <- forM tasks $ \t -> do
    nextRun <- nextRunTime (taskSchedule t) now
    return $ JobState t nextRun Nothing Nothing
  
  scheduler <- TaskScheduler
    <$> newTVarIO jobs
    <*> newTVarIO Map.empty
  
  forever $ do
    now <- getCurrentTime
    jobs <- readTVarIO (tsJobs scheduler)
    
    forM_ jobs $ \job -> do
      when (jsNextRun job <= now) $ do
        let task = jsTask job
        -- Run task in background
        a <- async $ timeout (taskTimeout task * 1000000) (taskAction task)
        atomically $ modifyTVar (tsRunning scheduler) $
          Map.insert (taskName task) a
        
        -- Update next run time
        nextRun <- nextRunTime (taskSchedule task) now
        atomically $ modifyTVar (tsJobs scheduler) $
          map (\j -> if taskName (jsTask j) == taskName task
                     then j { jsNextRun = nextRun, jsLastRun = Just now }
                     else j)
    
    threadDelay 60000000  -- check every minute

-- Built-in tasks
cleanupExpiredSessions :: IO ()
cleanupExpiredSessions = do
  putStrLn "Cleaning up expired sessions..."
  -- DB cleanup

sendDailyDigest :: IO ()
sendDailyDigest = do
  putStrLn "Sending daily digest emails..."

recalculateStats :: IO ()
recalculateStats = do
  putStrLn "Recalculating statistics..."

-- Application startup
main :: IO ()
main = do
  let tasks =
        [ ScheduledTask "cleanup-sessions" "*/30 * * * *" cleanupExpiredSessions 30
        , ScheduledTask "daily-digest"     "0 8 * * *"    sendDailyDigest 60
        , ScheduledTask "recalc-stats"     "0 * * * *"    recalculateStats 120
        ]
  
  void $ forkIO (runScheduler tasks)
  warp 3000 myApp
```

---

## ขั้นตอนที่ 448: gRPC สำหรับ Microservices

```haskell
-- gRPC ด้วย grpc-haskell

{-# LANGUAGE OverloadedStrings #-}

import Network.GRPC.HighLevel.Generated
import Network.GRPC.LowLevel

-- Proto definition (generated from .proto)
-- service UserService {
--   rpc GetUser (GetUserRequest) returns (GetUserResponse) {}
--   rpc ListUsers (ListUsersRequest) returns (stream UserInfo) {}
--   rpc CreateUser (CreateUserRequest) returns (UserInfo) {}
-- }

-- Server implementation
userServiceServer :: UserServiceServer
userServiceServer = UserServiceServer
  { getUser = handleGetUser
  , listUsers = handleListUsers
  , createUser = handleCreateUser
  }

handleGetUser :: ServerRequest 'Normal GetUserRequest GetUserResponse
             -> IO (ServerResponse 'Normal GetUserResponse)
handleGetUser (ServerNormalRequest _metadata req) = do
  let uid = getUserId req
  mUser <- fetchUser uid
  case mUser of
    Nothing -> return $ ServerNormalResponse
      (def :: GetUserResponse)
      [("grpc-status", "5")]  -- NOT_FOUND
      StatusNotFound
      "User not found"
    Just user -> return $ ServerNormalResponse
      (def { getUserId = uid, getUserName = userName user })
      mempty
      StatusOk
      ""

handleListUsers :: ServerRequest 'ServerStreaming ListUsersRequest UserInfo
               -> IO (ServerResponse 'ServerStreaming UserInfo)
handleListUsers (ServerWriterRequest _meta req sendFn) = do
  users <- fetchAllUsers (listPage req) (listPageSize req)
  forM_ users $ \user -> sendFn
    (def { userInfoId = userId user, userInfoName = userName user })
  return $ ServerWriterResponse mempty StatusOk ""

-- Client
callGetUser :: Client -> Int64 -> IO (Either ClientError GetUserResponse)
callGetUser client uid = do
  let req = def { getUserId = uid }
  withGRPCClient (clientConfig client) $ \c -> do
    UserServiceClient{..} <- userServiceClient c
    getUser (ClientNormalRequest req 10 mempty)
```

---

## ขั้นตอนที่ 449: WebRTC Signaling Server

```haskell
-- WebRTC signaling server ด้วย WebSocket

import Network.WebSockets

data SignalMessage
  = Offer     { offerSdp :: Text, offerTo :: Text }
  | Answer    { answerSdp :: Text, answerTo :: Text }
  | IceCandidate { candidate :: Text, to :: Text }
  | Join      Text  -- room name
  | Leave     Text
  deriving (Generic, FromJSON, ToJSON)

data RoomState = RoomState
  { roomConnections :: TVar (Map Text Connection)
  }

signalingServer :: IORef (Map Text RoomState) -> ServerApp
signalingServer rooms pending = do
  conn   <- acceptRequest pending
  peerId <- generateId
  
  forkPingThread conn 30
  
  finally
    (handleMessages conn peerId rooms)
    (cleanup peerId rooms)

handleMessages :: Connection -> Text -> IORef (Map Text RoomState) -> IO ()
handleMessages conn peerId rooms = forever $ do
  msgData <- receiveData conn :: IO Text
  case decode (encodeUtf8 msgData) of
    Nothing -> return ()
    Just msg -> case msg of
      Join roomName -> joinRoom conn peerId roomName rooms
      Leave roomName -> leaveRoom peerId roomName rooms
      Offer sdp target -> forwardToTarget target msg rooms
      Answer sdp target -> forwardToTarget target msg rooms
      IceCandidate cand target -> forwardToTarget target msg rooms

joinRoom :: Connection -> Text -> Text -> IORef (Map Text RoomState) -> IO ()
joinRoom conn peerId roomName rooms = do
  atomicModifyIORef' rooms $ \m ->
    let room = fromMaybe (RoomState (unsafePerformIO (newTVarIO Map.empty))) (Map.lookup roomName m)
    in (Map.insert roomName room m, ())
  
  roomMap <- readIORef rooms
  case Map.lookup roomName roomMap of
    Nothing -> return ()
    Just room -> atomically $
      modifyTVar (roomConnections room) (Map.insert peerId conn)

forwardToTarget :: Text -> SignalMessage -> IORef (Map Text RoomState) -> IO ()
forwardToTarget target msg rooms = do
  roomMap <- readIORef rooms
  let allConns = Map.unions (map (unsafePerformIO . readTVarIO . roomConnections) (Map.elems roomMap))
  case Map.lookup target allConns of
    Nothing -> return ()
    Just conn -> sendTextData conn (decodeUtf8 (encode msg))
```

---

## ขั้นตอนที่ 450: Distributed Tracing

```haskell
-- Distributed tracing implementation

data TraceContext = TraceContext
  { traceId      :: TraceId
  , spanId       :: SpanId
  , parentSpanId :: Maybe SpanId
  , baggage      :: Map Text Text
  }

data Span = Span
  { spanTraceContext :: TraceContext
  , spanName         :: Text
  , spanStart        :: UTCTime
  , spanEnd          :: Maybe UTCTime
  , spanTags         :: Map Text Text
  , spanLogs         :: [SpanLog]
  }

data SpanLog = SpanLog
  { logTime    :: UTCTime
  , logMessage :: Text
  , logFields  :: Map Text Text
  }

-- Tracer
data Tracer = Tracer
  { tracerName    :: Text
  , tracerSampler :: Sampler
  , tracerReporter :: SpanReporter
  }

data Sampler = AlwaysSample | NeverSample | ProbabilisticSample Double

-- Start span
startSpan :: Tracer -> Text -> Maybe TraceContext -> IO Span
startSpan tracer name mParent = do
  tid  <- maybe generateTraceId (traceId . spanTraceContext) <$> pure mParent
  sid  <- generateSpanId
  now  <- getCurrentTime
  return Span
    { spanTraceContext = TraceContext tid sid (fmap (spanId . spanTraceContext) mParent) Map.empty
    , spanName  = name
    , spanStart = now
    , spanEnd   = Nothing
    , spanTags  = Map.empty
    , spanLogs  = []
    }

finishSpan :: Tracer -> Span -> IO ()
finishSpan tracer span = do
  now <- getCurrentTime
  let finished = span { spanEnd = Just now }
  report (tracerReporter tracer) finished

-- Propagation headers
injectContext :: TraceContext -> Request -> Request
injectContext ctx req = req
  { requestHeaders = requestHeaders req ++
    [ ("X-Trace-Id",      encodeUtf8 (showTraceId (traceId ctx)))
    , ("X-Span-Id",       encodeUtf8 (showSpanId (spanId ctx)))
    , ("X-Parent-Span-Id", maybe "" (encodeUtf8 . showSpanId) (parentSpanId ctx))
    ]
  }

extractContext :: Request -> Maybe TraceContext
extractContext req = do
  tid <- lookup "X-Trace-Id" (requestHeaders req) >>= parseTraceId . decodeUtf8
  sid <- lookup "X-Span-Id"  (requestHeaders req) >>= parseSpanId . decodeUtf8
  let pid = lookup "X-Parent-Span-Id" (requestHeaders req) >>= parseSpanId . decodeUtf8
  return (TraceContext tid sid pid Map.empty)
```

---

## ขั้นตอนที่ 451: Database Sharding

```haskell
-- Database sharding strategy

data ShardKey = ShardKey Text  -- e.g., userId

data ShardConfig = ShardConfig
  { shardCount :: Int
  , shardPools :: Vector ConnectionPool
  }

getShardFor :: ShardConfig -> ShardKey -> ConnectionPool
getShardFor config (ShardKey key) =
  let idx = hash key `mod` shardCount config
  in shardPools config Vector.! idx

-- Run query on specific shard
runOnShard :: ShardConfig -> ShardKey -> SqlPersistM a -> IO a
runOnShard config key action =
  let pool = getShardFor config key
  in runSqlPool action pool

-- Run query across all shards
runOnAllShards :: ShardConfig -> SqlPersistM a -> IO [a]
runOnAllShards config action =
  forConcurrently (Vector.toList (shardPools config)) $ \pool ->
    runSqlPool action pool

-- Fan-out query
searchAllShards :: ShardConfig -> Text -> IO [User]
searchAllShards config query = do
  results <- runOnAllShards config $
    selectList [UserName `like` ("%" <> query <> "%")] []
  return (concatMap (map entityVal) results)

-- Consistent sharding with virtual nodes
data VirtualNode = VirtualNode
  { vnHash       :: Int
  , vnShardIndex :: Int
  }

buildVirtualNodes :: Int -> Int -> [VirtualNode]
buildVirtualNodes shards vnPerShard =
  [ VirtualNode (hash (show shard <> "-" <> show vn)) shard
  | shard <- [0..shards-1]
  , vn    <- [0..vnPerShard-1]
  ]

lookupVirtualNode :: [VirtualNode] -> ShardKey -> Int
lookupVirtualNode nodes (ShardKey key) =
  let h = hash key
      sorted = sortBy (compare `on` vnHash) nodes
  in case dropWhile (\n -> vnHash n < h) sorted of
    []    -> vnShardIndex (head sorted)
    (n:_) -> vnShardIndex n
```

---

## ขั้นตอนที่ 452: Circuit Breaker Pattern

```haskell
-- Circuit Breaker สำหรับ distributed systems

data CircuitBreakerState
  = Closed   { failures :: Int, lastFailure :: Maybe UTCTime }
  | Open     { openedAt :: UTCTime }
  | HalfOpen

data CircuitBreaker = CircuitBreaker
  { cbState          :: TVar CircuitBreakerState
  , cbThreshold      :: Int     -- failures before opening
  , cbTimeout        :: Int     -- seconds before half-open
  , cbSuccessThreshold :: Int   -- successes to close from half-open
  , cbHalfOpenSuccess :: TVar Int
  }

newCircuitBreaker :: Int -> Int -> Int -> IO CircuitBreaker
newCircuitBreaker threshold timeout successThreshold = CircuitBreaker
  <$> newTVarIO (Closed 0 Nothing)
  <*> pure threshold
  <*> pure timeout
  <*> pure successThreshold
  <*> newTVarIO 0

call :: CircuitBreaker -> IO a -> IO (Either CircuitBreakerError a)
call cb action = do
  state <- readTVarIO (cbState cb)
  now   <- getCurrentTime
  
  case state of
    Open openedAt -> do
      if diffUTCTime now openedAt > fromIntegral (cbTimeout cb)
        then do
          atomically $ writeTVar (cbState cb) HalfOpen
          tryAction cb action
        else return (Left CircuitOpen)
    
    HalfOpen -> tryAction cb action
    Closed _ _ -> tryAction cb action

tryAction :: CircuitBreaker -> IO a -> IO (Either CircuitBreakerError a)
tryAction cb action = do
  result <- E.try action
  case result of
    Left (e :: SomeException) -> do
      recordFailure cb
      return (Left (ActionFailed (T.pack (show e))))
    Right v -> do
      recordSuccess cb
      return (Right v)

recordFailure :: CircuitBreaker -> IO ()
recordFailure cb = do
  now <- getCurrentTime
  atomically $ modifyTVar (cbState cb) $ \case
    Closed failures _ ->
      let newFails = failures + 1
      in if newFails >= cbThreshold cb
         then Open now
         else Closed newFails (Just now)
    _ -> Open now

recordSuccess :: CircuitBreaker -> IO ()
recordSuccess cb = atomically $ do
  state <- readTVar (cbState cb)
  case state of
    HalfOpen -> do
      successes <- readTVar (cbHalfOpenSuccess cb)
      let newSuccesses = successes + 1
      writeTVar (cbHalfOpenSuccess cb) newSuccesses
      when (newSuccesses >= cbSuccessThreshold cb) $ do
        writeTVar (cbState cb) (Closed 0 Nothing)
        writeTVar (cbHalfOpenSuccess cb) 0
    _ -> writeTVar (cbState cb) (Closed 0 Nothing)
```

---

## ขั้นตอนที่ 453: Service Registry

```haskell
-- Service Registry และ Discovery

data ServiceInfo = ServiceInfo
  { siName     :: Text
  , siHost     :: Text
  , siPort     :: Int
  , siHealth   :: Text   -- health check URL
  , siMetadata :: Map Text Text
  , siStatus   :: ServiceStatus
  }

data ServiceStatus = Healthy | Degraded | Unhealthy
  deriving (Show, Eq)

-- In-memory registry
data ServiceRegistry = ServiceRegistry
  { srServices :: TVar (Map Text [ServiceInfo])
  }

registerService :: ServiceRegistry -> ServiceInfo -> IO ()
registerService reg info = atomically $
  modifyTVar (srServices reg) $
    Map.insertWith (++) (siName info) [info]

deregisterService :: ServiceRegistry -> Text -> Text -> Int -> IO ()
deregisterService reg name host port = atomically $
  modifyTVar (srServices reg) $
    Map.adjust (filter (\si -> siHost si /= host || siPort si /= port)) name

discoverService :: ServiceRegistry -> Text -> IO [ServiceInfo]
discoverService reg name = do
  services <- readTVarIO (srServices reg)
  return $ filter (\si -> siStatus si == Healthy)
           (fromMaybe [] (Map.lookup name services))

-- Health check
checkHealth :: ServiceInfo -> IO ServiceStatus
checkHealth info = do
  result <- E.try $ do
    manager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 5000000 })
    req <- parseRequest (T.unpack (siHealth info))
    resp <- httpNoBody req manager
    return (responseStatus resp)
  case result of
    Left (_ :: SomeException) -> return Unhealthy
    Right status
      | statusCode status == 200 -> return Healthy
      | statusCode status >= 500 -> return Unhealthy
      | otherwise                -> return Degraded

-- Periodic health checks
runHealthChecker :: ServiceRegistry -> Int -> IO ()
runHealthChecker reg intervalSeconds = forever $ do
  services <- readTVarIO (srServices reg)
  
  -- Check all services
  updated <- forM (Map.toList services) $ \(name, instances) -> do
    updatedInstances <- forM instances $ \si -> do
      status <- checkHealth si
      return si { siStatus = status }
    return (name, updatedInstances)
  
  atomically $ writeTVar (srServices reg) (Map.fromList updated)
  
  threadDelay (intervalSeconds * 1000000)
```

---

## ขั้นตอนที่ 454: Event Streaming

```haskell
-- Event streaming ด้วย Kafka

import Kafka.Consumer
import Kafka.Producer

-- Producer
data KafkaProducer = KafkaProducer
  { kpProducer :: Producer
  , kpTopic    :: TopicName
  }

publishEvent :: KafkaProducer -> DomainEvent -> IO (Either KafkaError ())
publishEvent kp event = do
  let key = eventKey event
      val = encode event
  err <- produceMessage (kpProducer kp)
    ProducerRecord
      { prTopic     = kpTopic kp
      , prPartition = UnassignedPartition
      , prKey       = Just (encodeUtf8 key)
      , prValue     = Just (BSL.toStrict val)
      }
  return (maybe (Right ()) Left err)

-- Consumer
data KafkaConsumer' = KafkaConsumer'
  { kcConsumer :: Consumer
  , kcTopics   :: [TopicName]
  }

consumeEvents :: KafkaConsumer' -> (DomainEvent -> IO ()) -> IO ()
consumeEvents kc handler = do
  subscribeToTopics (kcConsumer kc) (kcTopics kc) >>= throwOnErr
  forever $ do
    msgs <- pollMessages (kcConsumer kc) 1000
    forM_ msgs $ \msg ->
      case crValue msg of
        Nothing -> putStrLn "Empty message"
        Just bs -> case decode (BSL.fromStrict bs) of
          Nothing    -> putStrLn "Invalid event"
          Just event -> handler event
    commitAllOffsets OffsetCommit (kcConsumer kc) >>= throwOnErr

-- Event sourcing with Kafka
appendToEventLog :: KafkaProducer -> AggregateId -> DomainEvent -> IO ()
appendToEventLog kp aggId event = do
  let msg = EventMessage aggId event
  publishEvent kp (toDomainEvent msg) >>= throwOnErr

loadEventLog :: KafkaConsumer' -> AggregateId -> IO [DomainEvent]
loadEventLog kc aggId = do
  -- Read events from beginning for this aggregate's partition
  events <- readAllMessages (kcConsumer kc) (getPartitionFor aggId)
  return (filter (\e -> eventAggregateId e == aggId) events)
```

---

## ขั้นตอนที่ 455: Reactive Programming

```haskell
-- Reactive programming ด้วย FRP

import Reactive.Banana
import Reactive.Banana.Frameworks

-- Event types
type UserInput = Text
type ServerResponse = Either Error Text

-- Reactive network
createNetwork :: EventSource UserInput -> EventSource ServerResponse -> MomentIO ()
createNetwork userInput serverResponse = do
  -- Create events from sources
  eUserInput    <- fromAddHandler (addHandler userInput)
  eServerResp   <- fromAddHandler (addHandler serverResponse)
  
  -- Behaviors (time-varying values)
  bInput <- stepper "" eUserInput
  bLoading <- accumB False $
    (const True  <$ eUserInput)  `union`
    (const False <$ eServerResp)
  
  -- Derived events
  let eRequest = eUserInput `whenB` (not <$> bLoading)
      eSuccess = filterRight eServerResp
      eError   = filterLeft  eServerResp
  
  -- Side effects
  reactimate $ sendRequest <$> eRequest
  reactimate $ updateUI    <$> eSuccess
  reactimate $ showError   <$> eError
  reactimate $ (putStrLn "Loading...") <$ eUserInput

-- Higher-order events
switchNetwork :: Event (Event a) -> MomentIO (Event a)
switchNetwork = fmap switchE . execute . fmap (\e -> do
  (fire, handler) <- newEvent
  reactimate (fire <$> e)
  return handler)
```

---

## ขั้นตอนที่ 456: Protocol Buffers

```haskell
-- Protocol Buffers ด้วย proto-lens หรือ protobuf

{-# LANGUAGE DeriveGeneric #-}

import Data.ProtoLens
import Proto.User

-- Generated from .proto:
-- message User {
--   int64 id = 1;
--   string name = 2;
--   string email = 3;
--   repeated string roles = 4;
-- }

-- Serialize
serializeUser :: User -> ByteString
serializeUser = encodeMessage

-- Deserialize
deserializeUser :: ByteString -> Either Text User
deserializeUser bs = case decodeMessage bs of
  Left err   -> Left (T.pack err)
  Right user -> Right user

-- Use in API
protoHandler :: ByteString -> IO ByteString
protoHandler body = do
  case deserializeUser body of
    Left err -> return (encodeMessage (errorResponse err))
    Right req -> do
      user <- processUser req
      return (serializeUser user)

-- JSON ↔ Proto conversion
userToProto :: DomainUser -> ProtoUser
userToProto user = defMessage
  & Proto.User.id    .~ fromIntegral (unUserId (userId user))
  & Proto.User.name  .~ unUsername (userUsername user)
  & Proto.User.email .~ unEmail (userEmail user)

protoToUser :: ProtoUser -> Maybe DomainUser
protoToUser proto = do
  email <- mkEmail (proto ^. Proto.User.email)
  name  <- mkUsername (proto ^. Proto.User.name)
  return DomainUser
    { userId       = UserId (fromIntegral (proto ^. Proto.User.id))
    , userEmail    = email
    , userUsername = name
    }
```

---

## ขั้นตอนที่ 457: Chaos Engineering

```haskell
-- Chaos Engineering: ทดสอบ resilience

data ChaosConfig = ChaosConfig
  { chaosEnabled        :: Bool
  , chaosLatencyP       :: Double  -- probability of adding latency
  , chaosLatencyMs      :: Int
  , chaosErrorP         :: Double  -- probability of error
  , chaosErrorStatus    :: Int
  , chaosNetworkDropP   :: Double  -- probability of dropping request
  }

-- Chaos middleware
chaosMiddleware :: ChaosConfig -> Middleware
chaosMiddleware cfg app req respond
  | not (chaosEnabled cfg) = app req respond
  | otherwise = do
    -- Simulate random latency
    shouldDelay <- randomRIO (0.0, 1.0) >>= \p -> return (p < chaosLatencyP cfg)
    when shouldDelay $ threadDelay (chaosLatencyMs cfg * 1000)
    
    -- Simulate random errors
    shouldError <- randomRIO (0.0, 1.0) >>= \p -> return (p < chaosErrorP cfg)
    when shouldError $ respond (responseLBS
      (Status (chaosErrorStatus cfg) "Chaos Error")
      [] "Chaos Engineering Error")
    
    -- Simulate network drops
    shouldDrop <- randomRIO (0.0, 1.0) >>= \p -> return (p < chaosNetworkDropP cfg)
    unless shouldDrop $ app req respond

-- Resilience testing
testResilienceWith :: ChaosConfig -> Server API -> IO TestResults
testResilienceWith chaos server = do
  let app = chaosMiddleware chaos (serve (Proxy :: Proxy API) server)
  
  successes <- newIORef 0
  failures  <- newIORef 0
  latencies <- newIORef []
  
  replicateConcurrently_ 100 $ do
    start <- getMonotonicTime
    result <- E.try $ doRequest app
    end   <- getMonotonicTime
    
    let latency = end - start
    atomicModifyIORef' latencies (\ls -> (latency:ls, ()))
    
    case result of
      Right _ -> atomicModifyIORef' successes (+1) >> return ()
      Left _  -> atomicModifyIORef' failures  (+1) >> return ()
  
  s <- readIORef successes
  f <- readIORef failures
  ls <- readIORef latencies
  
  return TestResults
    { trSuccessRate   = fromIntegral s / fromIntegral (s + f)
    , trP99Latency    = percentile 0.99 ls
    , trP95Latency    = percentile 0.95 ls
    , trMeanLatency   = sum ls / fromIntegral (length ls)
    }
```

---

## ขั้นตอนที่ 458: Blue-Green Deployment

```haskell
-- Blue-Green deployment ด้วย load balancer

data DeploymentColor = Blue | Green

data BlueGreenConfig = BlueGreenConfig
  { bgcBlueUrl    :: Text   -- Blue deployment URL
  , bgcGreenUrl   :: Text   -- Green deployment URL
  , bgcActiveSlot :: TVar DeploymentColor
  , bgcWeight     :: TVar (Int, Int)  -- (blue%, green%)
  }

-- Traffic router
routeTraffic :: BlueGreenConfig -> Middleware
routeTraffic cfg app req respond = do
  active <- readTVarIO (bgcActiveSlot cfg)
  (blueW, greenW) <- readTVarIO (bgcWeight cfg)
  
  -- Select slot based on weights
  r <- randomRIO (0, blueW + greenW - 1)
  let targetSlot = if r < blueW then Blue else Green
  
  -- Proxy to target
  let targetUrl = case targetSlot of
        Blue  -> bgcBlueUrl cfg
        Green -> bgcGreenUrl cfg
  
  proxyToUrl targetUrl req respond

-- Deployment switch
switchToGreen :: BlueGreenConfig -> IO ()
switchToGreen cfg = do
  -- Gradually shift traffic
  forM_ [(100, 0), (80, 20), (60, 40), (40, 60), (20, 80), (0, 100)] $ \(b, g) -> do
    atomically $ writeTVar (bgcWeight cfg) (b, g)
    threadDelay 30000000  -- Wait 30s between steps
    -- Check health of green
    healthOk <- checkGreenHealth cfg
    unless healthOk $ do
      -- Rollback on failure
      atomically $ writeTVar (bgcWeight cfg) (100, 0)
      throwIO RollbackRequired
  
  atomically $ writeTVar (bgcActiveSlot cfg) Green
  putStrLn "Deployment complete: Green is now active"

checkGreenHealth :: BlueGreenConfig -> IO Bool
checkGreenHealth cfg = do
  let healthUrl = bgcGreenUrl cfg <> "/health"
  result <- E.try $ do
    manager <- newManager defaultManagerSettings
    req <- parseRequest (T.unpack healthUrl)
    resp <- httpNoBody req manager
    return (statusCode (responseStatus resp) == 200)
  return $ either (const False) id (result :: Either SomeException Bool)
```

---

## ขั้นตอนที่ 459: Advanced Logging

```haskell
-- Structured logging ด้วย co-log

import Colog

-- Log message types
data AppLog
  = DatabaseQuery  Text Int           -- query, duration_ms
  | HttpRequest    Text Int Int       -- method, status, duration
  | UserAction     UserId Text        -- user, action
  | BusinessEvent  Text Value         -- event_name, data
  | SystemWarning  Text               -- message
  | CriticalError  Text SomeException -- message, exception

-- Action with logging
type AppMonad = ReaderT (LogAction IO AppLog) IO

logAction :: LogAction IO AppLog
logAction = cmap formatLog logTextStdout
  where
    formatLog (DatabaseQuery q d) =
      "[DB] " <> q <> " (" <> T.pack (show d) <> "ms)"
    formatLog (HttpRequest m s d) =
      "[HTTP] " <> m <> " " <> T.pack (show s) <> " " <> T.pack (show d) <> "ms"
    formatLog (UserAction uid action) =
      "[USER:" <> T.pack (show (unUserId uid)) <> "] " <> action
    formatLog (BusinessEvent name _) =
      "[EVENT] " <> name
    formatLog (SystemWarning msg) =
      "[WARN] " <> msg
    formatLog (CriticalError msg e) =
      "[ERROR] " <> msg <> ": " <> T.pack (show e)

-- Logging decorator
withDbLogging :: Text -> DB a -> AppMonad a
withDbLogging query action = do
  logger <- ask
  start  <- liftIO getMonotonicTime
  result <- liftIO (runDb action)
  end    <- liftIO getMonotonicTime
  let duration = round ((end - start) * 1000) :: Int
  liftIO $ unLogAction logger (DatabaseQuery query duration)
  return result

-- Audit log
auditLog :: UserId -> Text -> Value -> AppMonad ()
auditLog uid action details = do
  now <- liftIO getCurrentTime
  let entry = AuditEntry uid action details now
  liftIO $ insertAuditEntry entry
  logger <- ask
  liftIO $ unLogAction logger (UserAction uid action)
```

---

## ขั้นตอนที่ 460: โปรเจกต์: Real-time Collaborative Editor

```haskell
-- Real-time collaborative editor ด้วย CRDT

-- Operational Transformation
data Operation
  = Insert { opPos :: Int, opChar :: Char, opUser :: UserId, opSeq :: Int }
  | Delete { opPos :: Int, opUser :: UserId, opSeq :: Int }

-- Transform operation against another
transform :: Operation -> Operation -> (Operation, Operation)
transform op1 op2 = case (op1, op2) of
  -- Both insert
  (Insert p1 c1 u1 s1, Insert p2 c2 u2 s2)
    | p1 < p2   -> (op1, Insert (p2+1) c2 u2 s2)
    | p1 > p2   -> (Insert (p1+1) c1 u1 s1, op2)
    | otherwise -> -- same position: tie-break by userId
      if u1 < u2
      then (op1, Insert (p2+1) c2 u2 s2)
      else (Insert (p1+1) c1 u1 s1, op2)
  -- ...other cases

-- Document state
data Document = Document
  { docId       :: DocumentId
  , docContent  :: Text
  , docVersion  :: Int
  , docClients  :: TVar (Map ClientId Connection)
  , docPending  :: TVar [[Operation]]  -- pending operations per client
  }

-- WebSocket handler for editor
editorHandler :: Document -> Connection -> IO ()
editorHandler doc conn = do
  clientId <- newClientId
  atomically $ modifyTVar (docClients doc) (Map.insert clientId conn)
  
  -- Send current state
  sendTextData conn (encode (DocumentState (docContent doc) (docVersion doc)))
  
  finally
    (forever $ do
      msg <- receiveData conn :: IO Text
      case decode (encodeUtf8 msg) of
        Nothing -> return ()
        Just op -> do
          -- Apply operation
          atomically $ do
            let content  = docContent doc
                content' = applyOp op content
            doc { docContent = content' }
          
          -- Broadcast to other clients
          clients <- readTVarIO (docClients doc)
          forM_ (Map.elems (Map.delete clientId clients)) $ \c ->
            sendTextData c msg
    )
    (atomically $ modifyTVar (docClients doc) (Map.delete clientId))

applyOp :: Operation -> Text -> Text
applyOp (Insert pos char _ _) text =
  let (before, after) = T.splitAt pos text
  in before <> T.singleton char <> after
applyOp (Delete pos _ _) text =
  let (before, after) = T.splitAt pos text
  in before <> T.drop 1 after
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 24 เราจะเรียน **Security เชิงลึก**:
- OWASP Top 10 ใน Haskell
- JWT Advanced
- OAuth 2.0 / OpenID Connect
- API Security best practices

---

*[← Part 22](part-22.md) | [Part 24 →](part-24.md)*
