# Part 42: Distributed Systems
## ขั้นตอนที่ 821-840

---

## ขั้นตอนที่ 821: Consensus Algorithms

```haskell
-- Raft consensus algorithm

module Raft where

import Control.Concurrent.STM
import Data.Map.Strict (Map)

-- Node state
data RaftState
  = Follower
  | Candidate
  | Leader

-- Raft node
data RaftNode = RaftNode
  { rnId          :: NodeId
  , rnState       :: TVar RaftState
  , rnCurrentTerm :: TVar Int
  , rnVotedFor    :: TVar (Maybe NodeId)
  , rnLog         :: TVar [LogEntry]
  , rnCommitIndex :: TVar Int
  , rnLastApplied :: TVar Int
  -- Leader state
  , rnNextIndex   :: TVar (Map NodeId Int)
  , rnMatchIndex  :: TVar (Map NodeId Int)
  -- Config
  , rnPeers       :: [NodeId]
  , rnElectionTimeout :: Int  -- milliseconds
  }

data LogEntry = LogEntry
  { leTerm    :: Int
  , leIndex   :: Int
  , leCommand :: ByteString
  } deriving (Show, Eq)

-- Vote request
data RequestVote = RequestVote
  { rvTerm         :: Int
  , rvCandidateId  :: NodeId
  , rvLastLogIndex :: Int
  , rvLastLogTerm  :: Int
  }

data RequestVoteResponse = RequestVoteResponse
  { rvrTerm        :: Int
  , rvrVoteGranted :: Bool
  }

-- Handle vote request
handleRequestVote :: RaftNode -> RequestVote -> STM RequestVoteResponse
handleRequestVote node req = do
  currentTerm <- readTVar (rnCurrentTerm node)
  votedFor    <- readTVar (rnVotedFor node)
  log'        <- readTVar (rnLog node)
  
  -- Update term if necessary
  when (rvTerm req > currentTerm) $ do
    writeTVar (rnCurrentTerm node) (rvTerm req)
    writeTVar (rnState node) Follower
    writeTVar (rnVotedFor node) Nothing
  
  currentTerm' <- readTVar (rnCurrentTerm node)
  votedFor'    <- readTVar (rnVotedFor node)
  
  let logUpToDate = isLogUpToDate log' (rvLastLogIndex req) (rvLastLogTerm req)
  let canVote = rvTerm req >= currentTerm' &&
                (isNothing votedFor' || votedFor' == Just (rvCandidateId req)) &&
                logUpToDate
  
  when canVote (writeTVar (rnVotedFor node) (Just (rvCandidateId req)))
  
  return RequestVoteResponse
    { rvrTerm        = currentTerm'
    , rvrVoteGranted = canVote
    }

isLogUpToDate :: [LogEntry] -> Int -> Int -> Bool
isLogUpToDate [] _ _ = True
isLogUpToDate log lastIdx lastTerm =
  let lastEntry = last log
  in leTerm lastEntry < lastTerm ||
     (leTerm lastEntry == lastTerm && leIndex lastEntry <= lastIdx)
```

---

## ขั้นตอนที่ 822: Distributed Storage

```haskell
-- Distributed key-value store with consistent hashing

module DistributedKV where

import Data.Hashable (hash)
import qualified Data.Map.Strict as Map

-- Consistent hashing ring
data HashRing = HashRing
  { hrRing         :: Map Int NodeId  -- hash -> node
  , hrReplication  :: Int
  , hrVnodes       :: Int             -- virtual nodes per physical node
  }

-- Add node to ring
addNode :: NodeId -> HashRing -> HashRing
addNode node ring =
  let vnodes = [hash (show node ++ "#" ++ show i) | i <- [0..hrVnodes ring - 1]]
      newEntries = Map.fromList (zip vnodes (repeat node))
  in ring { hrRing = Map.union (hrRing ring) newEntries }

-- Find responsible nodes for a key
getNodes :: HashRing -> Text -> Int -> [NodeId]
getNodes ring key n =
  let keyHash = hash key
      ring'   = hrRing ring
      -- Find n successors in the ring (with wrap-around)
      successors = take n $ nub $ map snd $
        dropWhile (\(h, _) -> h < keyHash) (Map.toAscList ring')
        ++ Map.toAscList ring'
  in successors

-- Distributed KV operations
data DistKVConfig = DistKVConfig
  { dkvRing    :: HashRing
  , dkvNodes   :: Map NodeId KVClient
  , dkvW       :: Int  -- write quorum
  , dkvR       :: Int  -- read quorum
  }

-- Write with quorum
distKVPut :: DistKVConfig -> Text -> ByteString -> IO (Either DistError ())
distKVPut cfg key value = do
  let nodes   = getNodes (dkvRing cfg) key (dkvW cfg + 1)
  results <- mapConcurrently (\n -> kvPut (dkvNodes cfg Map.! n) key value) nodes
  let successes = length (filter isRight results)
  if successes >= dkvW cfg
    then return (Right ())
    else return (Left (QuorumNotMet successes (dkvW cfg)))

-- Read with quorum and conflict resolution (last-write-wins)
distKVGet :: DistKVConfig -> Text -> IO (Either DistError (Maybe ByteString))
distKVGet cfg key = do
  let nodes   = getNodes (dkvRing cfg) key (dkvR cfg + 1)
  results <- mapConcurrently (\n -> kvGetWithVersion (dkvNodes cfg Map.! n) key) nodes
  let successes = rights results
  if length successes >= dkvR cfg
    then return (Right (resolveConflicts successes))
    else return (Left (QuorumNotMet (length successes) (dkvR cfg)))

resolveConflicts :: [(Maybe ByteString, VectorClock)] -> Maybe ByteString
resolveConflicts [] = Nothing
resolveConflicts vs = fst (maximumBy (comparing (compareVClock . snd)) vs)
```

---

## ขั้นตอนที่ 823: Message Queues

```haskell
-- Message queue system

module MessageQueue where

import Database.Redis

-- Redis-based queue
data Queue = Queue
  { qConn   :: Connection
  , qName   :: Text
  , qGroup  :: Text  -- consumer group
  }

-- Publish message
enqueue :: Queue -> ByteString -> IO ()
enqueue q msg = do
  runRedis (qConn q) $ do
    void $ xadd (encodeUtf8 (qName q)) "*" [("data", msg)]

-- Consume messages (reliable with XREADGROUP)
consume :: Queue -> Text -> Int -> IO [(Text, ByteString)]
consume q consumer batchSize = do
  results <- runRedis (qConn q) $ do
    xreadGroup
      (encodeUtf8 (qGroup q))
      (encodeUtf8 consumer)
      [XReadOpts (Just batchSize) (Just 5000)]  -- block 5s
      [(encodeUtf8 (qName q), ">")]  -- ">" = new messages only
  
  return $ case results of
    Right (Just [(_, msgs)]) -> [(decodeUtf8 msgId, snd (head fields)) | (msgId, fields) <- msgs]
    _                        -> []

-- Acknowledge processed message
ack :: Queue -> Text -> IO ()
ack q msgId = do
  runRedis (qConn q) $ do
    void $ xack (encodeUtf8 (qName q)) (encodeUtf8 (qGroup q)) [encodeUtf8 msgId]

-- Dead letter queue
data DLQueue = DLQueue
  { dlqMain    :: Queue
  , dlqDead    :: Queue
  , dlqMaxRetry :: Int
  }

processWithRetry :: DLQueue -> (ByteString -> IO ()) -> IO ()
processWithRetry dlq processor = forever $ do
  messages <- consume (dlqMain dlq) "worker" 10
  forM_ messages $ \(msgId, payload) -> do
    retries <- getRetryCount dlq msgId
    if retries >= dlqMaxRetry dlq
      then do
        enqueue (dlqDead dlq) payload
        ack (dlqMain dlq) msgId
      else catch
        (do processor payload
            ack (dlqMain dlq) msgId)
        (\e -> incrementRetry dlq msgId >> logError e)
```

---

## ขั้นตอนที่ 824: Two-Phase Commit

```haskell
-- Distributed transaction: Two-Phase Commit

module TwoPhaseCommit where

data TransactionCoordinator = TransactionCoordinator
  { tcId           :: TransactionId
  , tcParticipants :: [Participant]
  , tcState        :: TVar TransactionState
  , tcLog          :: TransactionLog
  }

data TransactionState
  = TxPreparing
  | TxPrepared
  | TxCommitting
  | TxCommitted
  | TxAborting
  | TxAborted

data Participant = Participant
  { pId       :: ParticipantId
  , pEndpoint :: Text
  , pClient   :: ParticipantClient
  }

-- Phase 1: Prepare
prepare :: TransactionCoordinator -> IO Bool
prepare tc = do
  logTxState (tcLog tc) (tcId tc) TxPreparing
  atomically (writeTVar (tcState tc) TxPreparing)
  
  -- Ask all participants to prepare
  results <- mapConcurrently (\p -> preparePart p (tcId tc)) (tcParticipants tc)
  
  let allPrepared = all isReady results
  
  if allPrepared
    then do
      logTxState (tcLog tc) (tcId tc) TxPrepared
      atomically (writeTVar (tcState tc) TxPrepared)
      return True
    else do
      -- Abort if any participant voted NO
      abort tc
      return False

-- Phase 2: Commit
commit :: TransactionCoordinator -> IO ()
commit tc = do
  logTxState (tcLog tc) (tcId tc) TxCommitting
  atomically (writeTVar (tcState tc) TxCommitting)
  
  -- Commit at all participants
  -- Must retry until all succeed (fault tolerance)
  let commitWithRetry p = retrying defaultRetryPolicy (\_ r -> return (isLeft r)) $
        \_ -> commitPart p (tcId tc)
  
  mapConcurrently commitWithRetry (tcParticipants tc)
  
  logTxState (tcLog tc) (tcId tc) TxCommitted
  atomically (writeTVar (tcState tc) TxCommitted)

-- Abort
abort :: TransactionCoordinator -> IO ()
abort tc = do
  logTxState (tcLog tc) (tcId tc) TxAborting
  atomically (writeTVar (tcState tc) TxAborting)
  
  mapConcurrently (\p -> abortPart p (tcId tc)) (tcParticipants tc)
  
  logTxState (tcLog tc) (tcId tc) TxAborted
  atomically (writeTVar (tcState tc) TxAborted)
```

---

## ขั้นตอนที่ 825: CRDT Data Structures

```haskell
-- Conflict-free Replicated Data Types

-- G-Counter (grow-only counter)
newtype GCounter = GCounter (Map NodeId Int) deriving (Show, Eq)

gcIncrement :: NodeId -> GCounter -> GCounter
gcIncrement node (GCounter counts) =
  GCounter (Map.insertWith (+) node 1 counts)

gcValue :: GCounter -> Int
gcValue (GCounter counts) = sum (Map.elems counts)

gcMerge :: GCounter -> GCounter -> GCounter
gcMerge (GCounter a) (GCounter b) =
  GCounter (Map.unionWith max a b)

-- PN-Counter (increment/decrement counter)
data PNCounter = PNCounter
  { pncPositive :: GCounter
  , pncNegative :: GCounter
  } deriving (Show)

pncIncrement :: NodeId -> PNCounter -> PNCounter
pncIncrement n pnc = pnc { pncPositive = gcIncrement n (pncPositive pnc) }

pncDecrement :: NodeId -> PNCounter -> PNCounter
pncDecrement n pnc = pnc { pncNegative = gcIncrement n (pncNegative pnc) }

pncValue :: PNCounter -> Int
pncValue pnc = gcValue (pncPositive pnc) - gcValue (pncNegative pnc)

pncMerge :: PNCounter -> PNCounter -> PNCounter
pncMerge a b = PNCounter
  { pncPositive = gcMerge (pncPositive a) (pncPositive b)
  , pncNegative = gcMerge (pncNegative a) (pncNegative b)
  }

-- OR-Set (observed-remove set)
data ORSet a = ORSet
  { osAdded   :: Map a (Set UniqueTag)
  , osRemoved :: Map a (Set UniqueTag)
  } deriving (Show)

osAdd :: (Ord a) => a -> UniqueTag -> ORSet a -> ORSet a
osAdd item tag os =
  os { osAdded = Map.insertWith Set.union item (Set.singleton tag) (osAdded os) }

osRemove :: (Ord a) => a -> ORSet a -> ORSet a
osRemove item os =
  case Map.lookup item (osAdded os) of
    Nothing   -> os
    Just tags -> os { osRemoved = Map.insertWith Set.union item tags (osRemoved os) }

osContains :: (Ord a) => a -> ORSet a -> Bool
osContains item os =
  let added   = fromMaybe Set.empty (Map.lookup item (osAdded os))
      removed = fromMaybe Set.empty (Map.lookup item (osRemoved os))
  in not (Set.null (added `Set.difference` removed))

osMerge :: (Ord a) => ORSet a -> ORSet a -> ORSet a
osMerge a b = ORSet
  { osAdded   = Map.unionWith Set.union (osAdded a)   (osAdded b)
  , osRemoved = Map.unionWith Set.union (osRemoved a) (osRemoved b)
  }
```

---

## ขั้นตอนที่ 826: Service Discovery

```haskell
-- Service discovery and health checking

module ServiceDiscovery where

import Network.HTTP.Client

-- Service registry
data ServiceRegistry = ServiceRegistry
  { srConsulClient :: ConsulClient
  , srLocalCache   :: TVar (Map Text [ServiceInstance])
  , srWatchers     :: TVar (Map Text [ServiceInstance -> IO ()])
  }

data ServiceInstance = ServiceInstance
  { siId       :: Text
  , siName     :: Text
  , siAddress  :: Text
  , siPort     :: Int
  , siHealth   :: HealthStatus
  , siMetadata :: Map Text Text
  }

data HealthStatus = Healthy | Warning | Critical deriving (Eq, Ord)

-- Register service
registerService :: ServiceRegistry -> ServiceInstance -> IO ()
registerService sr si = do
  -- Register with Consul
  registerWithConsul (srConsulClient sr) si
  
  -- Set up health check
  registerHealthCheck (srConsulClient sr) si

-- Discover healthy instances
discoverService :: ServiceRegistry -> Text -> IO [ServiceInstance]
discoverService sr serviceName = do
  -- Check cache first
  cache <- readTVarIO (srLocalCache sr)
  case Map.lookup serviceName cache of
    Just instances -> return (filter ((== Healthy) . siHealth) instances)
    Nothing        -> do
      instances <- queryConsul (srConsulClient sr) serviceName
      atomically (modifyTVar (srLocalCache sr) (Map.insert serviceName instances))
      return (filter ((== Healthy) . siHealth) instances)

-- Load balancer
data LoadBalancer = LoadBalancer
  { lbStrategy :: LbStrategy
  , lbCounter  :: TVar Int  -- for round-robin
  }

data LbStrategy = RoundRobin | Random | LeastConnections

selectInstance :: LoadBalancer -> [ServiceInstance] -> IO (Maybe ServiceInstance)
selectInstance lb [] = return Nothing
selectInstance lb instances = case lbStrategy lb of
  RoundRobin -> do
    n <- atomically (stateTVar (lbCounter lb) (\n -> (n, (n+1) `mod` length instances)))
    return (Just (instances !! n))
  Random     -> do
    idx <- randomRIO (0, length instances - 1)
    return (Just (instances !! idx))
  LeastConnections ->
    return (Just (minimumBy (comparing siActiveConnections) instances))
```

---

## ขั้นตอนที่ 827: Distributed Tracing

```haskell
-- Distributed tracing with OpenTelemetry

module Tracing where

import OpenTelemetry.Trace

-- Trace context
data TraceContext = TraceContext
  { tcTraceId  :: TraceId
  , tcSpanId   :: SpanId
  , tcFlags    :: Word8
  } deriving (Show, Eq)

-- Propagate context via HTTP headers
injectContext :: TraceContext -> RequestHeaders -> RequestHeaders
injectContext ctx headers =
  headers ++
  [ ("traceparent", encodeTraceparent ctx)
  , ("tracestate",  encodeTracestate ctx)
  ]

extractContext :: RequestHeaders -> Maybe TraceContext
extractContext headers = do
  tp <- lookup "traceparent" headers
  parseTraceparent (decodeUtf8 tp)

-- W3C traceparent format: "00-{traceId}-{spanId}-{flags}"
encodeTraceparent :: TraceContext -> ByteString
encodeTraceparent ctx = encodeUtf8 $ T.intercalate "-"
  [ "00"
  , encodeHex (tcTraceId ctx)
  , encodeHex (tcSpanId ctx)
  , T.pack (printf "%02x" (tcFlags ctx))
  ]

-- Span operations
withSpan :: Text -> TraceContext -> Map Text AttributeValue -> IO a -> IO (a, Span)
withSpan name parent attrs action = do
  span' <- createSpan name parent attrs
  result <- action `onException` recordException span'
  endSpan span'
  return (result, span')

-- Baggage propagation
data Baggage = Baggage (Map Text Text)

injectBaggage :: Baggage -> RequestHeaders -> RequestHeaders
injectBaggage (Baggage items) headers =
  let encoded = encodeUtf8 $ T.intercalate "," $
        [ k <> "=" <> v | (k, v) <- Map.toList items ]
  in headers ++ [("baggage", encoded)]
```

---

## ขั้นตอนที่ 828: Distributed Locks

```haskell
-- Distributed locking

module DistributedLock where

import Database.Redis

-- Redis-based distributed lock (Redlock algorithm simplified)
data RedisLock = RedisLock
  { rlConn      :: Connection
  , rlKey       :: Text
  , rlValue     :: Text  -- unique token for this lock holder
  , rlTtlMs     :: Int
  }

-- Acquire lock
acquireLock :: Connection -> Text -> Int -> IO (Maybe RedisLock)
acquireLock conn key ttlMs = do
  token <- generateToken
  result <- runRedis conn $ do
    setOpts (encodeUtf8 key) (encodeUtf8 token)
      SetOpts { setSeconds = Nothing
              , setMilliseconds = Just (fromIntegral ttlMs)
              , setCondition = Just Nx  -- NX = only set if not exists
              }
  return $ case result of
    Right (Just "OK") -> Just RedisLock
      { rlConn  = conn
      , rlKey   = key
      , rlValue = token
      , rlTtlMs = ttlMs
      }
    _ -> Nothing

-- Release lock (only if we own it)
releaseLock :: RedisLock -> IO Bool
releaseLock lock = do
  result <- runRedis (rlConn lock) $ do
    -- Atomic check-and-delete using Lua script
    eval
      "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('del', KEYS[1]) else return 0 end"
      [encodeUtf8 (rlKey lock)]
      [encodeUtf8 (rlValue lock)]
  return $ case result of
    Right (Integer 1) -> True
    _                 -> False

-- Extend lock TTL (heartbeat)
extendLock :: RedisLock -> IO Bool
extendLock lock = do
  result <- runRedis (rlConn lock) $ do
    eval
      "if redis.call('get', KEYS[1]) == ARGV[1] then return redis.call('pexpire', KEYS[1], ARGV[2]) else return 0 end"
      [encodeUtf8 (rlKey lock)]
      [encodeUtf8 (rlValue lock), encodeUtf8 (T.pack (show (rlTtlMs lock)))]
  return $ case result of
    Right (Integer 1) -> True
    _                 -> False

-- Use lock safely
withDistributedLock :: Connection -> Text -> Int -> IO a -> IO (Maybe a)
withDistributedLock conn key ttlMs action = do
  mLock <- acquireLock conn key ttlMs
  case mLock of
    Nothing   -> return Nothing
    Just lock -> do
      result <- action `finally` releaseLock lock
      return (Just result)
```

---

## ขั้นตอนที่ 829: Eventual Consistency

```haskell
-- Eventual consistency patterns

-- Vector clocks for causality tracking
newtype VectorClock = VectorClock (Map NodeId Int)
  deriving (Show, Eq)

incrementVC :: NodeId -> VectorClock -> VectorClock
incrementVC node (VectorClock vc) =
  VectorClock (Map.insertWith (+) node 1 vc)

mergeVC :: VectorClock -> VectorClock -> VectorClock
mergeVC (VectorClock a) (VectorClock b) =
  VectorClock (Map.unionWith max a b)

-- Happens-before relation
happensBefore :: VectorClock -> VectorClock -> Bool
happensBefore (VectorClock a) (VectorClock b) =
  all (\k -> fromMaybe 0 (Map.lookup k a) <= fromMaybe 0 (Map.lookup k b)) (Map.keys a)
  && a /= b

concurrent :: VectorClock -> VectorClock -> Bool
concurrent a b = not (happensBefore a b) && not (happensBefore b a)

-- Anti-entropy (Gossip protocol)
data GossipState = GossipState
  { gsNodeId   :: NodeId
  , gsData     :: TVar (Map Text (ByteString, VectorClock))
  , gsPeers    :: [NodeId]
  }

-- Gossip round
gossipRound :: GossipState -> NodeClient -> IO ()
gossipRound state peerClient = do
  ourData <- readTVarIO (gsData state)
  
  -- Get peer's data summary
  peerSummary <- getPeerSummary peerClient
  
  -- Find data peer is missing
  let toSend = Map.differenceWith
        (\(v, vc) (_, pvc) -> if happensBefore pvc vc then Just (v, vc) else Nothing)
        ourData peerSummary
  
  -- Send missing data
  unless (Map.null toSend) (sendGossip peerClient toSend)
  
  -- Request what we're missing
  let toRequest = Map.differenceWith
        (\(_, pvc) (_, vc) -> if happensBefore vc pvc then Just () else Nothing)
        peerSummary ourData
  
  unless (Map.null toRequest) $ do
    peerData <- requestData peerClient (Map.keys toRequest)
    mergeGossip state peerData
```

---

## ขั้นตอนที่ 830: Distributed Scheduler

```haskell
-- Distributed task scheduler

module DistScheduler where

import Data.Time (UTCTime, addUTCTime, getCurrentTime)

-- Task definition
data Task = Task
  { taskId       :: TaskId
  , taskName     :: Text
  , taskSchedule :: Schedule
  , taskPayload  :: ByteString
  , taskHandler  :: Text  -- handler name
  }

data Schedule
  = RunAt UTCTime
  | RunEvery Int  -- seconds
  | RunCron Text  -- cron expression

-- Task instance (one execution)
data TaskInstance = TaskInstance
  { tiId       :: InstanceId
  , tiTaskId   :: TaskId
  , tiScheduledAt :: UTCTime
  , tiStatus   :: TaskStatus
  , tiWorker   :: Maybe WorkerId
  }

data TaskStatus = Pending | Running | Completed | Failed | Skipped

-- Distributed scheduler coordinator
data Scheduler = Scheduler
  { schStore  :: TaskStore
  , schWorkers :: TVar (Map WorkerId WorkerInfo)
  , schLock   :: DistributedLockClient
  }

-- Acquire work (atomic claim)
claimTask :: Scheduler -> WorkerId -> IO (Maybe TaskInstance)
claimTask sch worker = do
  -- Use distributed lock to avoid double-claiming
  withDistributedLock (schLock sch) "task-claim" 5000 $ do
    pending <- getPendingTasks (schStore sch)
    case pending of
      []     -> return Nothing
      (ti:_) -> do
        now <- getCurrentTime
        updateTaskInstance (schStore sch) ti { tiStatus = Running, tiWorker = Just worker }
        return (Just ti)

-- Worker heartbeat
workerHeartbeat :: Scheduler -> WorkerId -> IO ()
workerHeartbeat sch worker = do
  now <- getCurrentTime
  atomically (modifyTVar (schWorkers sch) (Map.insert worker (WorkerInfo worker now)))

-- Detect dead workers and reschedule their tasks
reapDeadWorkers :: Scheduler -> IO ()
reapDeadWorkers sch = do
  now <- getCurrentTime
  workers <- readTVarIO (schWorkers sch)
  let deadWorkers = Map.filter (isDeadWorker now) workers
  forM_ (Map.keys deadWorkers) $ \workerId -> do
    runningTasks <- getRunningTasksForWorker (schStore sch) workerId
    forM_ runningTasks $ \ti ->
      updateTaskInstance (schStore sch) ti { tiStatus = Pending, tiWorker = Nothing }
    atomically (modifyTVar (schWorkers sch) (Map.delete workerId))

isDeadWorker :: UTCTime -> WorkerInfo -> Bool
isDeadWorker now wi = now > addUTCTime 30 (wiLastHeartbeat wi)
```

---

## ขั้นตอนที่ 831: Network Protocols

```haskell
-- Custom binary protocol

module Protocol where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

-- Message frame
data Frame = Frame
  { fMagic    :: Word16  -- 0xDEAD
  , fVersion  :: Word8   -- protocol version
  , fType     :: MessageType
  , fPayload  :: ByteString
  , fChecksum :: Word32
  }

data MessageType
  = MsgRequest
  | MsgResponse
  | MsgEvent
  | MsgHeartbeat
  deriving (Show, Eq, Enum)

-- Encode frame
encodeFrame :: Frame -> BL.ByteString
encodeFrame frame = runPut $ do
  putWord16be (fMagic frame)
  putWord8    (fVersion frame)
  putWord8    (fromIntegral $ fromEnum (fType frame))
  putWord32be (fromIntegral $ BS.length (fPayload frame))
  putByteString (fPayload frame)
  putWord32be (crc32 (fPayload frame))

-- Decode frame
decodeFrame :: Get Frame
decodeFrame = do
  magic    <- getWord16be
  unless (magic == 0xDEAD) (fail "Invalid magic bytes")
  version  <- getWord8
  msgType  <- toEnum . fromIntegral <$> getWord8
  len      <- getWord32be
  payload  <- getByteString (fromIntegral len)
  checksum <- getWord32be
  unless (checksum == crc32 payload) (fail "Checksum mismatch")
  return Frame
    { fMagic    = magic
    , fVersion  = version
    , fType     = msgType
    , fPayload  = payload
    , fChecksum = checksum
    }

-- Frame-based connection
data Connection' = Connection'
  { connSend    :: Frame -> IO ()
  , connReceive :: IO Frame
  , connClose   :: IO ()
  }

openConnection :: HostName -> Int -> IO Connection'
openConnection host port = do
  sock <- connectTCP host port
  return Connection'
    { connSend    = \f -> sendAll sock (BL.toStrict (encodeFrame f))
    , connReceive = runGet decodeFrame . BL.fromStrict <$> receiveFrame sock
    , connClose   = close sock
    }
```

---

## ขั้นตอนที่ 832: Fault Tolerance

```haskell
-- Fault tolerance patterns

-- Supervision tree
data Supervisor = Supervisor
  { supId       :: Text
  , supChildren :: TVar [Worker']
  , supStrategy :: RestartStrategy
  , supMaxRetries :: Int
  , supWindow   :: Int  -- seconds
  }

data RestartStrategy
  = OneForOne    -- restart only failed child
  | OneForAll    -- restart all children when one fails
  | RestForOne   -- restart failed child and all children started after it

data Worker' = Worker'
  { wId      :: Text
  , wAction  :: IO ()
  , wThread  :: TVar (Maybe ThreadId)
  , wRestarts :: TVar [(UTCTime)]
  }

-- Start worker with monitoring
startWorker :: Supervisor -> Worker' -> IO ()
startWorker sup w = do
  tid <- forkIO (workerLoop sup w)
  atomically (writeTVar (wThread w) (Just tid))

workerLoop :: Supervisor -> Worker' -> IO ()
workerLoop sup w = do
  result <- try (wAction w)
  case result of
    Right () -> return ()  -- graceful exit
    Left (e :: SomeException) -> do
      logError ("Worker " <> wId w <> " failed: " <> tshow e)
      
      -- Check restart limit
      now      <- getCurrentTime
      restarts <- atomically (readTVar (wRestarts w))
      let recent = filter (\t -> now < addUTCTime (fromIntegral (supWindow sup)) t) restarts
      
      if length recent >= supMaxRetries sup
        then logError ("Worker " <> wId w <> " exceeded restart limit, giving up")
        else do
          atomically (writeTVar (wRestarts w) (now : recent))
          
          -- Apply restart strategy
          case supStrategy sup of
            OneForOne -> restartWorker sup w
            OneForAll -> restartAllChildren sup
            RestForOne -> restartFromChild sup w
```

---

## ขั้นตอนที่ 833: Backpressure

```haskell
-- Backpressure mechanisms

-- Bounded queue with backpressure
data BoundedQueue a = BoundedQueue
  { bqQueue  :: TQueue a
  , bqSize   :: TVar Int
  , bqLimit  :: Int
  }

newBoundedQueue :: Int -> IO (BoundedQueue a)
newBoundedQueue limit = do
  q    <- newTQueueIO
  size <- newTVarIO 0
  return (BoundedQueue q size limit)

-- Enqueue with backpressure (blocks when full)
bqEnqueue :: BoundedQueue a -> a -> IO ()
bqEnqueue bq item = atomically $ do
  size <- readTVar (bqSize bq)
  check (size < bqLimit bq)  -- STM retry if full
  writeTQueue (bqQueue bq) item
  writeTVar (bqSize bq) (size + 1)

-- Enqueue with timeout
bqEnqueueTimeout :: BoundedQueue a -> a -> Int -> IO Bool
bqEnqueueTimeout bq item timeoutMs = do
  result <- timeout (timeoutMs * 1000) (bqEnqueue bq item)
  return (isJust result)

-- Dequeue
bqDequeue :: BoundedQueue a -> IO a
bqDequeue bq = atomically $ do
  item <- readTQueue (bqQueue bq)
  modifyTVar (bqSize bq) (subtract 1)
  return item

-- Adaptive rate limiter
data AdaptiveRateLimiter = AdaptiveRateLimiter
  { arlTokens    :: TVar Double
  , arlMaxTokens :: Double
  , arlLastRefill :: TVar UTCTime
  , arlRate       :: TVar Double  -- tokens per second (adjustable)
  }

-- Decrease rate on queue pressure
adaptRate :: AdaptiveRateLimiter -> BoundedQueue a -> IO ()
adaptRate arl bq = do
  qSize  <- readTVarIO (bqSize bq)
  qLimit <- return (bqLimit bq)
  let pressure = fromIntegral qSize / fromIntegral qLimit
  
  atomically $ modifyTVar (arlRate arl) $ \rate ->
    let newRate = rate * (1 - 0.1 * pressure)  -- reduce by up to 10%
    in max 1 (min 1000 newRate)
```

---

## ขั้นตอนที่ 834: Health Monitoring

```haskell
-- Distributed health monitoring

-- Health check types
data HealthCheck = HealthCheck
  { hcName     :: Text
  , hcAction   :: IO HealthStatus
  , hcTimeout  :: Int  -- milliseconds
  , hcInterval :: Int  -- seconds
  }

data HealthStatus
  = Healthy    { hsMessage :: Maybe Text }
  | Degraded   { hsMessage :: Text, hsDegradation :: Double }
  | Unhealthy  { hsMessage :: Text, hsError :: Maybe SomeException }

-- System health report
data SystemHealth = SystemHealth
  { shTimestamp :: UTCTime
  , shStatus    :: OverallStatus
  , shChecks    :: Map Text HealthStatus
  , shDuration  :: Double  -- ms
  }

data OverallStatus = AllHealthy | SomeDegraded | CriticalFailure

-- Run health checks with circuit breaker
runHealthChecks :: [HealthCheck] -> IO SystemHealth
runHealthChecks checks = do
  start  <- getMonotonicTime
  status <- forConcurrently checks $ \hc -> do
    result <- timeout (hcTimeout hc * 1000) (hcAction hc)
    let status = case result of
          Nothing -> Unhealthy "Health check timed out" Nothing
          Just s  -> s
    return (hcName hc, status)
  
  end <- getMonotonicTime
  let checkMap = Map.fromList status
  let overall  = computeOverall (Map.elems checkMap)
  
  return SystemHealth
    { shTimestamp = getCurrentTime
    , shStatus    = overall
    , shChecks    = checkMap
    , shDuration  = (end - start) * 1000
    }

computeOverall :: [HealthStatus] -> OverallStatus
computeOverall statuses
  | any isUnhealthy statuses = CriticalFailure
  | any isDegraded  statuses = SomeDegraded
  | otherwise                = AllHealthy

-- Prometheus metrics for health
healthToMetrics :: SystemHealth -> [PrometheusMetric]
healthToMetrics health =
  [ Gauge "service_health" (if shStatus health == AllHealthy then 1 else 0) []
  , Gauge "health_check_duration_ms" (shDuration health) []
  ] ++
  [ Gauge "health_check_status" (statusToNum st) [("check", name)]
  | (name, st) <- Map.toList (shChecks health)
  ]
```

---

## ขั้นตอนที่ 835: Configuration Management

```haskell
-- Distributed configuration management

module ConfigManagement where

-- Hot-reloadable config
data DynamicConfig a = DynamicConfig
  { dcValue    :: TVar a
  , dcVersion  :: TVar Int
  , dcWatchers :: TVar [a -> IO ()]
  }

newDynamicConfig :: a -> IO (DynamicConfig a)
newDynamicConfig initial = do
  value    <- newTVarIO initial
  version  <- newTVarIO 0
  watchers <- newTVarIO []
  return (DynamicConfig value version watchers)

-- Watch for config changes
watchConfig :: DynamicConfig a -> (a -> IO ()) -> IO ()
watchConfig dc callback = do
  atomically (modifyTVar (dcWatchers dc) (callback :))
  current <- readTVarIO (dcValue dc)
  callback current  -- notify immediately with current value

-- Update config and notify watchers
updateConfig :: DynamicConfig a -> a -> IO ()
updateConfig dc newValue = do
  watchers <- atomically $ do
    writeTVar (dcValue dc) newValue
    modifyTVar (dcVersion dc) (+1)
    readTVar (dcWatchers dc)
  mapConcurrently_ (\w -> w newValue) watchers

-- Pull from remote config source (Consul KV, etcd, etc.)
configPoller :: Text -> DynamicConfig Value -> IO ()
configPoller key dc = forever $ do
  newValue <- fetchRemoteConfig key
  currentVersion <- readTVarIO (dcVersion dc)
  remoteVersion  <- getRemoteVersion key
  when (remoteVersion > currentVersion) (updateConfig dc newValue)
  threadDelay (5 * 1000000)  -- poll every 5 seconds

-- Feature flags
data FeatureFlag = FeatureFlag
  { ffName     :: Text
  , ffEnabled  :: Bool
  , ffRollout  :: Double  -- 0-100% rollout
  , ffTargeting :: [TargetRule]
  }

data TargetRule
  = TargetUser   Text
  | TargetGroup  Text
  | TargetCountry Text
  | TargetPct    Double

isFeatureEnabled :: FeatureFlag -> UserId -> Map Text Text -> Bool
isFeatureEnabled ff uid context
  | not (ffEnabled ff) = False
  | otherwise =
      any (evaluateRule uid context) (ffTargeting ff) ||
      hashToPercent uid <= ffRollout ff
```

---

## ขั้นตอนที่ 836: gRPC Services

```haskell
-- gRPC service implementation

module GrpcService where

import Network.GRPC.HighLevel.Generated

-- Proto-generated types (conceptual)
data GetUserRequest  = GetUserRequest  { gurUserId :: Int32 }
data GetUserResponse = GetUserResponse { gurUser :: Maybe UserProto }
data UserProto       = UserProto { upId :: Int32, upName :: Text, upEmail :: Text }

-- gRPC server
userServiceServer :: UserServiceServer
userServiceServer = UserServiceServer
  { getUser         = handleGetUser
  , listUsers       = handleListUsers
  , createUser      = handleCreateUser
  , streamUserEvents = handleStreamUserEvents
  }

-- Unary RPC
handleGetUser :: ServerRequest 'Normal GetUserRequest GetUserResponse -> IO (ServerResponse 'Normal GetUserResponse)
handleGetUser (ServerNormalRequest _meta req) = do
  let userId = gurUserId req
  mUser <- getUserFromDb userId
  return $ ServerNormalResponse
    (GetUserResponse (toProto <$> mUser))
    mempty
    StatusOk
    ""

-- Server streaming RPC
handleStreamUserEvents :: ServerRequest 'ServerStreaming WatchEventsRequest UserEvent -> IO (ServerResponse 'ServerStreaming UserEvent)
handleStreamUserEvents (ServerWriterRequest _meta req send) = do
  let userId = wreqUserId req
  
  -- Subscribe to event stream
  events <- subscribeUserEvents userId
  
  -- Stream events until disconnected
  forM_ events $ \event -> do
    sent <- send (toProtoEvent event)
    unless sent (throwIO ClientDisconnected)
  
  return (ServerWriterResponse mempty StatusOk "")

-- gRPC client
userServiceClient :: Channel -> UserServiceClient
userServiceClient chan = buildUserServiceClient chan defaultConfig

getUser' :: UserServiceClient -> Int32 -> IO (Maybe UserProto)
getUser' client uid = do
  response <- getUser client (ClientNormalRequest (GetUserRequest uid) 5 mempty)
  case response of
    ClientNormalResponse resp _ _ StatusOk _ -> return (gurUser resp)
    ClientErrorResponse err                  -> throwIO (GrpcError err)
```

---

## ขั้นตอนที่ 837: Pub/Sub System

```haskell
-- In-memory pub/sub system

module PubSub where

import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

type Topic     = Text
type MessageId = Text
type SubscriberId = Text

data PubSubBroker = PubSubBroker
  { psbTopics      :: TVar (Map Topic (Seq Message))
  , psbSubscribers :: TVar (Map Topic (Map SubscriberId (TQueue Message)))
  }

data Message = Message
  { msgId      :: MessageId
  , msgTopic   :: Topic
  , msgPayload :: ByteString
  , msgMeta    :: Map Text Text
  }

-- Create broker
newBroker :: IO PubSubBroker
newBroker = PubSubBroker <$> newTVarIO Map.empty <*> newTVarIO Map.empty

-- Publish message
publish :: PubSubBroker -> Topic -> ByteString -> Map Text Text -> IO MessageId
publish broker topic payload meta = do
  msgId' <- generateMessageId
  let msg = Message msgId' topic payload meta
  
  atomically $ do
    -- Store in topic history
    modifyTVar (psbTopics broker) (Map.insertWith (flip (<>)) topic (Seq.singleton msg))
    
    -- Deliver to subscribers
    subs <- readTVar (psbSubscribers broker)
    case Map.lookup topic subs of
      Nothing -> return ()
      Just subQueues -> mapM_ (`writeTQueue` msg) (Map.elems subQueues)
  
  return msgId'

-- Subscribe to topic
subscribe :: PubSubBroker -> Topic -> SubscriberId -> IO (TQueue Message)
subscribe broker topic subId = do
  q <- newTQueueIO
  atomically (modifyTVar (psbSubscribers broker)
    (Map.insertWith Map.union topic (Map.singleton subId q)))
  return q

-- Unsubscribe
unsubscribe :: PubSubBroker -> Topic -> SubscriberId -> IO ()
unsubscribe broker topic subId =
  atomically (modifyTVar (psbSubscribers broker)
    (Map.adjust (Map.delete subId) topic))
```

---

## ขั้นตอนที่ 838: Actor Model

```haskell
-- Actor model implementation

module Actor where

import Control.Concurrent.STM
import Control.Concurrent (forkIO)

-- Actor reference
newtype ActorRef a = ActorRef (TQueue a)

-- Actor state
data ActorState s msg = ActorState
  { asSelf     :: ActorRef msg
  , asState    :: TVar s
  , asChildren :: TVar [SomeActorRef]
  , asParent   :: Maybe SomeActorRef
  }

-- Create and spawn actor
spawnActor :: s -> (ActorState s msg -> msg -> IO s) -> IO (ActorRef msg)
spawnActor initialState handler = do
  q    <- newTQueueIO
  sv   <- newTVarIO initialState
  kids <- newTVarIO []
  
  let actorState = ActorState (ActorRef q) sv kids Nothing
  
  forkIO $ do
    let loop = do
          msg   <- atomically (readTQueue q)
          state <- readTVarIO sv
          newState <- handler actorState msg
          atomically (writeTVar sv newState)
          loop
    loop `catch` \(e :: SomeException) -> do
      logError ("Actor crashed: " <> tshow e)
      case asParent actorState of
        Nothing -> return ()
        Just parent -> notifyCrash parent (ActorRef q) e
  
  return (ActorRef q)

-- Send message to actor
send :: ActorRef msg -> msg -> IO ()
send (ActorRef q) msg = atomically (writeTQueue q msg)

-- Ask pattern (request-response)
ask :: ActorRef (msg, TMVar resp) -> msg -> IO resp
ask ref msg = do
  reply <- newEmptyTMVarIO
  send ref (msg, reply)
  atomically (takeTMVar reply)
```

---

## ขั้นตอนที่ 839: Data Replication

```haskell
-- Data replication patterns

-- Primary-replica replication
data ReplicationConfig = ReplicationConfig
  { rcPrimary    :: NodeId
  , rcReplicas   :: [NodeId]
  , rcSyncMode   :: SyncMode
  , rcBinlogPos  :: TVar BinlogPosition
  }

data SyncMode = Synchronous | SemiSynchronous | Asynchronous

-- Binlog event
data BinlogEvent = BinlogEvent
  { blePos       :: BinlogPosition
  , bleTimestamp :: UTCTime
  , bleOp        :: ReplicationOp
  }

data ReplicationOp
  = ROInsert Text [(Text, Value)]  -- table, columns/values
  | ROUpdate Text [(Text, Value)] [(Text, Value)]  -- table, where, set
  | RODelete Text [(Text, Value)]  -- table, where

-- Apply binlog to replica
applyBinlog :: Connection -> BinlogEvent -> IO ()
applyBinlog conn event = case bleOp event of
  ROInsert table cols -> do
    let sql = buildInsert table cols
    execute conn sql []
  ROUpdate table where' set -> do
    let sql = buildUpdate table where' set
    execute conn sql []
  RODelete table where' -> do
    let sql = buildDelete table where'
    execute conn sql []

-- Replication lag monitor
monitorReplicationLag :: ReplicationConfig -> IO ()
monitorReplicationLag cfg = forever $ do
  primaryPos <- getPrimaryBinlogPos (rcPrimary cfg)
  forM_ (rcReplicas cfg) $ \replica -> do
    replicaPos <- getReplicaBinlogPos replica
    let lag = primaryPos - replicaPos
    recordMetric ("replication_lag_" <> tshow replica) lag
  threadDelay (5 * 1000000)
```

---

## ขั้นตอนที่ 840: โปรเจกต์: Distributed Key-Value Store

```haskell
-- Complete distributed key-value store

module DistributedKVStore where

import Control.Concurrent.Async
import qualified Data.Map.Strict as Map

-- Main server
data KVServer = KVServer
  { kvsNodeId     :: NodeId
  , kvsPort       :: Int
  , kvsState      :: TVar KVState
  , kvsRaft       :: RaftNode
  , kvsPeers      :: [Peer]
  }

data KVState = KVState
  { ksData        :: Map Text (ByteString, Version)
  , ksVersion     :: Version
  }

data Peer = Peer
  { peerId     :: NodeId
  , peerAddr   :: (Text, Int)
  , peerClient :: Maybe PeerClient
  }

-- Start KV server
startKVServer :: KVServerConfig -> IO KVServer
startKVServer cfg = do
  state    <- newTVarIO (KVState Map.empty 0)
  raftNode <- initRaftNode (kvcNodeId cfg) (kvcPeers cfg)
  
  let server = KVServer
        { kvsNodeId = kvcNodeId cfg
        , kvsPort   = kvcPort cfg
        , kvsState  = state
        , kvsRaft   = raftNode
        , kvsPeers  = kvcPeers cfg
        }
  
  -- Start services
  concurrently_
    (startHttpServer server)
    (concurrently_ (startRaftLoop raftNode) (startPeerSync server))
  
  return server

-- Client operations
kvGet :: KVServer -> Text -> IO (Maybe ByteString)
kvGet server key = do
  state <- readTVarIO (kvsState server)
  return (fst <$> Map.lookup key (ksData state))

kvPut :: KVServer -> Text -> ByteString -> IO Version
kvPut server key value = do
  -- Replicate via Raft consensus
  cmd <- serializeCommand (PutCmd key value)
  success <- appendToRaft (kvsRaft server) cmd
  unless success (throwIO (ConsensusError "Failed to replicate"))
  
  version <- atomically $ do
    state <- readTVar (kvsState server)
    let v = ksVersion state + 1
    writeTVar (kvsState server) state
      { ksData    = Map.insert key (value, v) (ksData state)
      , ksVersion = v
      }
    return v
  
  return version

main :: IO ()
main = do
  cfg <- loadConfig
  server <- startKVServer cfg
  putStrLn ("KV Server started on port " ++ show (kvsPort server))
  waitForever
```

---

*[← Part 41](part-41.md) | [Part 43 →](part-43.md)*
