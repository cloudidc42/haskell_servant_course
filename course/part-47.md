# Part 47: Advanced Concurrency Patterns
## ขั้นตอนที่ 921-940

---

## ขั้นตอนที่ 921: STM Advanced Patterns

```haskell
-- Advanced STM patterns

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

-- Priority queue using STM
data PriorityQueue a = PQ
  { pqQueues :: TArray Int (TQueue a)
  , pqLevels :: Int
  }

newPQ :: Int -> STM (PriorityQueue a)
newPQ levels = do
  qs <- newArray (0, levels-1) =<< newTQueue  -- placeholder
  forM_ [0..levels-1] $ \i -> do
    q <- newTQueue
    writeArray qs i q
  return (PQ qs levels)

enqueue :: PriorityQueue a -> Int -> a -> STM ()
enqueue pq priority item = do
  q <- readArray (pqQueues pq) (min priority (pqLevels pq - 1))
  writeTQueue q item

dequeue :: PriorityQueue a -> STM a
dequeue pq = go 0
  where
    go i
      | i >= pqLevels pq = retry  -- all queues empty, retry
      | otherwise = do
          q <- readArray (pqQueues pq) i
          tryReadTQueue q >>= \case
            Just v  -> return v
            Nothing -> go (i + 1)

-- Transactional semaphore
data TSemaphore = TSemaphore (TVar Int)

newTSemaphore :: Int -> STM TSemaphore
newTSemaphore n = TSemaphore <$> newTVar n

acquireTS :: TSemaphore -> STM ()
acquireTS (TSemaphore var) = do
  n <- readTVar var
  check (n > 0)
  writeTVar var (n - 1)

releaseTS :: TSemaphore -> STM ()
releaseTS (TSemaphore var) = modifyTVar' var (+1)

withTS :: TSemaphore -> IO a -> IO a
withTS sem action = do
  atomically (acquireTS sem)
  action `finally` atomically (releaseTS sem)
```

---

## ขั้นตอนที่ 922: Lock-Free Data Structures

```haskell
-- Lock-free data structures using IORef + CAS

import Data.IORef
import Control.Monad (unless)

-- Lock-free Treiber stack
data LFStack a = LFStack (IORef (Maybe (a, LFStack' a)))
type LFStack' a = (a, IORef (Maybe (a, LFStack' a)))

newLFStack :: IO (LFStack a)
newLFStack = LFStack <$> newIORef Nothing

pushLF :: LFStack a -> a -> IO ()
pushLF (LFStack ref) x = do
  old <- readIORef ref
  let new = Just (x, (x, ref))  -- simplified
  atomicWriteIORef ref new

-- CAS loop using atomicModifyIORef'
casLoop :: IORef a -> (a -> (Bool, a)) -> IO Bool
casLoop ref f = do
  old <- readIORef ref
  let (success, new) = f old
  if success
    then atomicModifyIORef' ref (\curr -> if curr == old then (new, True) else (curr, False))
    else return False

-- Lock-free counter
data Counter = Counter (IORef Int)

newCounter :: IO Counter
newCounter = Counter <$> newIORef 0

increment :: Counter -> IO Int
increment (Counter ref) = atomicModifyIORef' ref (\n -> (n+1, n+1))

decrement :: Counter -> IO Int
decrement (Counter ref) = atomicModifyIORef' ref (\n -> (n-1, n-1))

getCount :: Counter -> IO Int
getCount (Counter ref) = readIORef ref

-- Lock-free hashmap (simplified using STM Map)
data LFMap k v = LFMap (TVar (Map k v))

newLFMap :: IO (LFMap k v)
newLFMap = LFMap <$> newTVarIO Map.empty

insertLF :: Ord k => LFMap k v -> k -> v -> IO ()
insertLF (LFMap var) k v = atomically (modifyTVar' var (Map.insert k v))

lookupLF :: Ord k => LFMap k v -> k -> IO (Maybe v)
lookupLF (LFMap var) k = Map.lookup k <$> readTVarIO var

deleteLF :: Ord k => LFMap k v -> k -> IO ()
deleteLF (LFMap var) k = atomically (modifyTVar' var (Map.delete k))
```

---

## ขั้นตอนที่ 923: Work-Stealing Scheduler

```haskell
-- Work-stealing thread pool

import Control.Concurrent
import Control.Concurrent.STM
import Data.Array.IO

data WorkStealingPool = WSPool
  { wsWorkers   :: Int
  , wsQueues    :: Array Int (TQueue (IO ()))  -- per-worker deques
  , wsActive    :: TVar Int
  }

newWSPool :: Int -> IO WorkStealingPool
newWSPool n = do
  queues <- listArray (0, n-1) <$> replicateM n (newTQueueIO)
  active <- newTVarIO 0
  return (WSPool n queues active)

-- Submit task to a specific worker queue
submitTask :: WorkStealingPool -> IO () -> IO ()
submitTask pool task = do
  -- Round-robin assignment
  i <- (mod) <$> randomRIO (0, maxBound) <*> pure (wsWorkers pool)
  atomically (writeTQueue (wsQueues pool ! i) task)

-- Worker logic with stealing
workerLoop :: WorkStealingPool -> Int -> IO ()
workerLoop pool workerId = forever $ do
  -- Try own queue first
  task <- atomically $ do
    let myQueue = wsQueues pool ! workerId
    tryReadTQueue myQueue >>= \case
      Just t  -> return (Just t)
      Nothing -> trySteal pool workerId  -- steal from others

  case task of
    Just t  -> t `catch` (\(e :: SomeException) -> logError e)
    Nothing -> threadDelay 100  -- backoff

trySteal :: WorkStealingPool -> Int -> STM (Maybe (IO ()))
trySteal pool myId = do
  let n = wsWorkers pool
  go ((myId + 1) `mod` n) 0
  where
    go i 0 | i == myId = return Nothing
    go i count = do
      let q = wsQueues pool ! i
      tryReadTQueue q >>= \case
        Just t  -> return (Just t)
        Nothing -> go ((i + 1) `mod` wsWorkers pool) (count + 1)
    go _ _ = return Nothing

startPool :: WorkStealingPool -> IO ()
startPool pool = forM_ [0..wsWorkers pool - 1] $ \i ->
  forkIO (workerLoop pool i)
```

---

## ขั้นตอนที่ 924: Actor Model

```haskell
-- Actor model implementation

import Control.Concurrent
import Control.Concurrent.STM

data ActorRef msg = ActorRef
  { arMailbox :: TBQueue msg
  , arId      :: Int
  }

data ActorSystem = ActorSystem
  { asActors :: TVar (Map Int SomeActor)
  , asNextId :: TVar Int
  }

data SomeActor = forall s msg. SomeActor
  { aState   :: TVar s
  , aBehavior :: s -> msg -> IO (s, [IO ()])  -- state -> msg -> (newState, effects)
  }

-- Spawn an actor
spawn :: ActorSystem -> s -> (s -> msg -> IO (s, [IO ()])) -> IO (ActorRef msg)
spawn system initState behavior = do
  mailbox <- newTBQueueIO 1000
  actorId <- atomically (stateTVar (asNextId system) (\n -> (n, n+1)))
  stateVar <- newTVarIO initState
  
  let actor = SomeActor stateVar (\s m -> behavior s m)
      ref = ActorRef mailbox actorId
  
  atomically (modifyTVar' (asActors system) (Map.insert actorId actor))
  forkIO (actorLoop stateVar behavior mailbox)
  return ref

actorLoop :: TVar s -> (s -> msg -> IO (s, [IO ()])) -> TBQueue msg -> IO ()
actorLoop stateVar behavior mailbox = forever $ do
  msg    <- atomically (readTBQueue mailbox)
  state  <- readTVarIO stateVar
  (newState, effects) <- behavior state msg
  atomically (writeTVar stateVar newState)
  sequence_ effects

-- Send message
(!) :: ActorRef msg -> msg -> IO ()
ref ! msg = atomically (writeTBQueue (arMailbox ref) msg)

-- Ask pattern (request-response)
ask :: ActorRef (msg, TMVar a) -> msg -> IO a
ask ref msg = do
  replyVar <- newEmptyTMVarIO
  ref ! (msg, replyVar)
  atomically (readTMVar replyVar)

-- Counter actor example
data CounterMsg
  = Increment
  | Decrement
  | GetCount (TMVar Int)

counterActor :: Int -> CounterMsg -> IO (Int, [IO ()])
counterActor n Increment     = return (n+1, [])
counterActor n Decrement     = return (n-1, [])
counterActor n (GetCount r)  = return (n, [atomically (putTMVar r n)])
```

---

## ขั้นตอนที่ 925: Async Computation Patterns

```haskell
-- Advanced async patterns

import Control.Concurrent.Async

-- Parallel map with bounded concurrency
parMapN :: Int -> (a -> IO b) -> [a] -> IO [b]
parMapN n f xs = do
  sem <- newQSemN n
  results <- forM xs $ \x -> async $ do
    waitQSemN sem 1
    result <- f x
    signalQSemN sem 1
    return result
  mapM wait results

-- Timeout with result
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
  resultVar <- newEmptyMVar
  tid <- forkIO $ action >>= putMVar resultVar
  threadDelay micros
  tryTakeMVar resultVar <* killThread tid

-- Retry with exponential backoff
retryWithBackoff :: Int -> Int -> IO (Either e a) -> IO (Either e a)
retryWithBackoff 0 _ action = action
retryWithBackoff n delay action = action >>= \case
  Right a -> return (Right a)
  Left _  -> do
    threadDelay delay
    retryWithBackoff (n-1) (delay * 2) action

-- Pipeline stage
data Stage a b = Stage
  { stageFunc  :: a -> IO b
  , stageConc  :: Int
  }

runPipeline :: [Stage a a] -> [a] -> IO [a]
runPipeline [] xs = return xs
runPipeline (stage:stages) xs = do
  results <- parMapN (stageConc stage) (stageFunc stage) xs
  runPipeline stages results

-- Fan-out / fan-in
fanOut :: (a -> [IO b]) -> a -> IO [b]
fanOut f x = mapConcurrently id (f x)

fanIn :: [IO a] -> IO [a]
fanIn = mapConcurrently id

-- Circuit breaker
data CircuitState = Closed | Open | HalfOpen deriving (Show, Eq)

data CircuitBreaker = CircuitBreaker
  { cbState    :: TVar CircuitState
  , cbFailures :: TVar Int
  , cbThreshold :: Int
  , cbTimeout  :: Int
  }

callWithBreaker :: CircuitBreaker -> IO a -> IO (Either BreakerError a)
callWithBreaker cb action = do
  state <- readTVarIO (cbState cb)
  case state of
    Open -> return (Left CircuitOpen)
    _ -> do
      result <- try action
      case result of
        Right v -> do
          atomically (writeTVar (cbState cb) Closed >> writeTVar (cbFailures cb) 0)
          return (Right v)
        Left (e :: SomeException) -> do
          failures <- atomically $ do
            n <- readTVar (cbFailures cb) >>= \n -> writeTVar (cbFailures cb) (n+1) >> return (n+1)
            when (n >= cbThreshold cb) (writeTVar (cbState cb) Open)
            return n
          return (Left (CircuitTripped failures))
```

---

## ขั้นตอนที่ 926: Reactive Programming

```haskell
-- Reactive programming with FRP concepts

-- Event stream
data Event a = Event { runEvent :: IO (a, Event a) }

-- Signal (continuous value)
newtype Signal a = Signal { runSignal :: IO a }

instance Functor Signal where
  fmap f (Signal io) = Signal (f <$> io)

instance Applicative Signal where
  pure a = Signal (return a)
  Signal f <*> Signal a = Signal (f <*> a)

-- Create signal from IORef
refSignal :: IORef a -> Signal a
refSignal ref = Signal (readIORef ref)

-- Combine signals
combineSignals :: Signal a -> Signal b -> Signal (a, b)
combineSignals sa sb = (,) <$> sa <*> sb

-- Sample signal at intervals
sampleEvery :: Int -> Signal a -> IO (Event a)
sampleEvery micros sig = do
  let loop = do
        v <- runSignal sig
        threadDelay micros
        rest <- loop
        return (v, rest)
  Event <$> loop

-- Map over event stream
mapE :: (a -> b) -> Event a -> IO (Event b)
mapE f (Event next) = return $ Event $ do
  (v, rest) <- next
  rest' <- mapE f rest
  return (f v, rest')

-- Filter event stream
filterE :: (a -> Bool) -> Event a -> IO (Event (Maybe a))
filterE p (Event next) = return $ Event $ do
  (v, rest) <- next
  rest' <- filterE p rest
  return (if p v then Just v else Nothing, rest')

-- Fold over event stream
foldE :: (b -> a -> b) -> b -> Event a -> IO (Signal b)
foldE f init events = do
  var <- newIORef init
  _ <- forkIO $ go var events
  return (refSignal var)
  where
    go var (Event next) = do
      (v, rest) <- next
      modifyIORef' var (`f` v)
      go var rest
```

---

## ขั้นตอนที่ 927: Software Transactional Memory Advanced

```haskell
-- Advanced STM patterns

-- Composable synchronization
data Barrier = Barrier (TVar Int) Int

newBarrier :: Int -> IO Barrier
newBarrier n = Barrier <$> newTVarIO 0 <*> pure n

-- Wait at barrier until all participants arrive
waitBarrier :: Barrier -> IO ()
waitBarrier (Barrier var n) = do
  atomically $ do
    count <- readTVar var
    writeTVar var (count + 1)
    check (count + 1 >= n)  -- block until n reached
  atomically (writeTVar var 0)  -- reset for reuse

-- Transactional channel with backpressure
data BackPressureChan a = BPChan (TBQueue a) (TVar Int) Int

newBPChan :: Int -> IO (BackPressureChan a)
newBPChan capacity = BPChan <$> newTBQueueIO capacity <*> newTVarIO 0 <*> pure capacity

writeBPChan :: BackPressureChan a -> a -> STM ()
writeBPChan (BPChan q _ _) = writeTBQueue q

readBPChan :: BackPressureChan a -> STM a
readBPChan (BPChan q _ _) = readTBQueue q

-- STM-based LRU cache
data LRUCache k v = LRUCache
  { lruMap   :: TVar (Map k (v, Int))  -- value with access time
  , lruTime  :: TVar Int
  , lruCap   :: Int
  }

newLRU :: Int -> IO (LRUCache k v)
newLRU cap = LRUCache <$> newTVarIO Map.empty <*> newTVarIO 0 <*> pure cap

lookupLRU :: Ord k => LRUCache k v -> k -> IO (Maybe v)
lookupLRU cache key = atomically $ do
  m <- readTVar (lruMap cache)
  case Map.lookup key m of
    Nothing       -> return Nothing
    Just (v, _)   -> do
      t <- readTVar (lruTime cache)
      writeTVar (lruTime cache) (t + 1)
      modifyTVar' (lruMap cache) (Map.insert key (v, t+1))
      return (Just v)

insertLRU :: Ord k => LRUCache k v -> k -> v -> IO ()
insertLRU cache key val = atomically $ do
  m <- readTVar (lruMap cache)
  t <- readTVar (lruTime cache)
  writeTVar (lruTime cache) (t + 1)
  let m' = Map.insert key (val, t+1) m
  -- Evict LRU if over capacity
  let m'' = if Map.size m' > lruCap cache
              then Map.deleteMin m'  -- not truly LRU, simplified
              else m'
  writeTVar (lruMap cache) m''
```

---

## ขั้นตอนที่ 928: Concurrent Data Pipelines

```haskell
-- High-performance concurrent data pipeline

import Control.Concurrent.Chan
import Control.Concurrent.MVar

-- Bounded channel
data BoundedChan a = BChan
  { bcChan     :: Chan a
  , bcSem      :: QSemN
  }

newBChan :: Int -> IO (BoundedChan a)
newBChan capacity = BChan <$> newChan <*> newQSemN capacity

writeBChan :: BoundedChan a -> a -> IO ()
writeBChan (BChan chan sem) x = do
  waitQSemN sem 1
  writeChan chan x

readBChan :: BoundedChan a -> IO a
readBChan (BChan chan sem) = do
  x <- readChan chan
  signalQSemN sem 1
  return x

-- Pipeline stage with backpressure
data PipeStage a b = PipeStage
  { psInput    :: BoundedChan a
  , psOutput   :: BoundedChan b
  , psWorkers  :: Int
  }

runStage :: PipeStage a b -> (a -> IO b) -> IO ()
runStage stage transform = do
  forM_ [1..psWorkers stage] $ \_ ->
    forkIO $ forever $ do
      item   <- readBChan (psInput stage)
      result <- transform item
      writeBChan (psOutput stage) result

-- Connect stages
connectStages :: [a -> IO a] -> [Int] -> IO (BoundedChan a, BoundedChan a)
connectStages transforms concurrencies = do
  let n = length transforms
  chans <- replicateM (n+1) (newBChan 100)
  forM_ (zip3 transforms concurrencies (zip chans (tail chans))) $ \(f, c, (inp, out)) ->
    let stage = PipeStage inp out c
    in runStage stage f
  return (head chans, last chans)

-- Aggregator
data Aggregator a b = Aggregator
  { aggInput  :: BoundedChan a
  , aggOutput :: MVar b
  , aggFunc   :: b -> a -> b
  , aggInit   :: b
  }

runAggregator :: Aggregator a b -> IO ()
runAggregator agg = do
  state <- newMVar (aggInit agg)
  forever $ do
    item <- readBChan (aggInput agg)
    modifyMVar_ state (\s -> return (aggFunc agg s item))
```

---

## ขั้นตอนที่ 929: Green Threads & Fibers

```haskell
-- Lightweight cooperative threading (fiber-like)

import Control.Monad.Cont

-- Cooperative coroutine
type Coroutine r a = ContT r IO a

yield :: Coroutine () ()
yield = ContT $ \k -> do
  forkIO (k ())  -- schedule continuation
  threadDelay 0  -- yield current thread

-- Fiber type
data Fiber a = Fiber { runFiber :: IO (FiberResult a) }

data FiberResult a
  = FiberDone a
  | FiberSuspended (Fiber a)
  | FiberFailed SomeException

-- Create fiber from IO action
fiber :: IO a -> IO (Fiber a)
fiber action = do
  var <- newEmptyMVar
  forkIO $ try action >>= putMVar var
  return $ Fiber $ do
    result <- tryTakeMVar var
    case result of
      Nothing         -> return (FiberSuspended (Fiber (tryTakeMVar var >>= \case
        Nothing -> return (FiberSuspended undefined)  -- simplified
        Just (Right v)  -> return (FiberDone v)
        Just (Left e)   -> return (FiberFailed e))))
      Just (Right v)  -> return (FiberDone v)
      Just (Left e)   -> return (FiberFailed e)

-- Fiber scheduler
data Scheduler = Scheduler
  { scQueue   :: TQueue (IO ())
  , scActive  :: TVar Int
  }

newScheduler :: IO Scheduler
newScheduler = Scheduler <$> newTQueueIO <*> newTVarIO 0

schedule :: Scheduler -> IO () -> IO ()
schedule sc task = do
  atomically $ do
    writeTQueue (scQueue sc) task
    modifyTVar' (scActive sc) (+1)

runScheduler :: Scheduler -> Int -> IO ()
runScheduler sc threads = do
  forM_ [1..threads] $ \_ ->
    forkIO $ forever $ do
      task <- atomically (readTQueue (scQueue sc))
      task
      atomically (modifyTVar' (scActive sc) (subtract 1))
  atomically $ do
    n <- readTVar (scActive sc)
    check (n == 0)  -- wait until all done
```

---

## ขั้นตอนที่ 930: Parallel Strategies

```haskell
-- Evaluation strategies for parallelism

import Control.Parallel.Strategies
import Control.DeepSeq

-- Custom strategies
parChunks :: NFData a => Int -> Strategy [a]
parChunks n xs = do
  let chunks = chunksOf n xs
  chunks' <- parList rdeepseq chunks
  return (concat chunks')

-- Strategy combinator
both :: Strategy a -> Strategy b -> Strategy (a, b)
both sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

-- Parallel fold
parFold :: NFData a => (a -> a -> a) -> a -> [a] -> a
parFold f z xs = runEval $ do
  let n = length xs
      (left, right) = splitAt (n `div` 2) xs
  l <- rpar (foldl' f z left)
  r <- rseq (foldl' f z right)
  rseq (f l r)
  return (f l r)

-- Parallel quicksort with cutoff
parQuicksort :: (Ord a, NFData a) => Int -> [a] -> [a]
parQuicksort _ [] = []
parQuicksort threshold xs
  | length xs < threshold = sort xs
  | otherwise = runEval $ do
      let pivot = xs !! (length xs `div` 2)
          lts   = filter (<  pivot) xs
          gts   = filter (>  pivot) xs
          eqs   = filter (== pivot) xs
      lts' <- rpar (parQuicksort threshold lts)
      gts' <- rseq (parQuicksort threshold gts)
      rseq lts'
      return (lts' ++ eqs ++ gts')

-- Parallel matrix multiply
parMatMul :: Int -> [[Double]] -> [[Double]] -> [[Double]]
parMatMul threshold a b = runEval $
  parList rdeepseq [map (dot row) (transpose b) | row <- a]
  where dot x y = sum (zipWith (*) x y)
```

---

## ขั้นตอนที่ 931: Message Passing Concurrency

```haskell
-- CSP-style message passing

import Control.Concurrent.MVar

-- Channel type
data Channel a = Channel
  { chSend :: a -> IO ()
  , chRecv :: IO a
  }

-- Create synchronous channel (rendezvous)
newSyncChan :: IO (Channel a)
newSyncChan = do
  mvar <- newEmptyMVar
  return Channel
    { chSend = putMVar mvar
    , chRecv = takeMVar mvar
    }

-- Create buffered channel
newBuffChan :: Int -> IO (Channel a)
newBuffChan n = do
  q <- newTBQueueIO n
  return Channel
    { chSend = atomically . writeTBQueue q
    , chRecv = atomically (readTBQueue q)
    }

-- Select over channels (like Go select)
data SelectOp a
  = forall msg. Recv (Channel msg) (msg -> IO a)
  | forall msg. Send (Channel msg) msg (IO a)

select :: [SelectOp a] -> IO a
select ops = do
  results <- newEmptyMVar
  tids <- forM ops $ \op -> forkIO $ case op of
    Recv chan k -> chRecv chan >>= k >>= putMVar results
    Send chan msg k -> chSend chan msg >> k >>= putMVar results
  result <- takeMVar results
  mapM_ killThread tids
  return result

-- Process communication example
producer :: Channel Int -> IO ()
producer chan = forM_ [1..100] $ \i -> do
  chSend chan i
  threadDelay 10000  -- 10ms

consumer :: Channel Int -> IO ()
consumer chan = forever $ do
  n <- chRecv chan
  putStrLn ("Received: " ++ show n)
```

---

## ขั้นตอนที่ 932: Concurrent Collections

```haskell
-- Thread-safe concurrent collections

-- Concurrent hashmap (using STM Map)
newtype ConcMap k v = ConcMap (TVar (HashMap k v))

newCMap :: IO (ConcMap k v)
newCMap = ConcMap <$> newTVarIO HashMap.empty

cInsert :: (Eq k, Hashable k) => ConcMap k v -> k -> v -> IO ()
cInsert (ConcMap var) k v = atomically (modifyTVar' var (HashMap.insert k v))

cLookup :: (Eq k, Hashable k) => ConcMap k v -> k -> IO (Maybe v)
cLookup (ConcMap var) k = HashMap.lookup k <$> readTVarIO var

cDelete :: (Eq k, Hashable k) => ConcMap k v -> k -> IO ()
cDelete (ConcMap var) k = atomically (modifyTVar' var (HashMap.delete k))

cModify :: (Eq k, Hashable k) => ConcMap k v -> k -> (Maybe v -> v) -> IO ()
cModify (ConcMap var) k f = atomically $
  modifyTVar' var (\m -> HashMap.insert k (f (HashMap.lookup k m)) m)

-- Concurrent set
newtype ConcSet a = ConcSet (TVar (HashSet a))

newCSet :: IO (ConcSet a)
newCSet = ConcSet <$> newTVarIO HashSet.empty

cSetInsert :: (Eq a, Hashable a) => ConcSet a -> a -> IO ()
cSetInsert (ConcSet var) x = atomically (modifyTVar' var (HashSet.insert x))

cSetMember :: (Eq a, Hashable a) => ConcSet a -> a -> IO Bool
cSetMember (ConcSet var) x = HashSet.member x <$> readTVarIO var

-- Concurrent counter group
data CounterGroup k = CG (ConcMap k Int)

newCG :: IO (CounterGroup k)
newCG = CG <$> newCMap

increment' :: (Eq k, Hashable k) => CounterGroup k -> k -> IO ()
increment' (CG m) k = cModify m k (maybe 1 (+1))

getCount' :: (Eq k, Hashable k) => CounterGroup k -> k -> IO Int
getCount' (CG m) k = fromMaybe 0 <$> cLookup m k

topN :: CounterGroup Text -> Int -> IO [(Text, Int)]
topN (CG (ConcMap var)) n = do
  m <- readTVarIO var
  return $ take n $ sortBy (flip compare `on` snd) (HashMap.toList m)
```

---

## ขั้นตอนที่ 933: Async Error Handling

```haskell
-- Robust async error handling

import Control.Exception
import Control.Concurrent.Async

-- Supervisor that restarts failed workers
data Supervisor = Supervisor
  { supWorkers  :: TVar (Map Int (Async ()))
  , supTasks    :: Map Int (IO ())
  , supPolicy   :: RestartPolicy
  }

data RestartPolicy
  = AlwaysRestart
  | RestartN Int    -- restart up to N times
  | NoRestart

newSupervisor :: RestartPolicy -> IO Supervisor
newSupervisor policy = do
  workers <- newTVarIO Map.empty
  return (Supervisor workers Map.empty policy)

-- Start a supervised task
supervise :: Supervisor -> Int -> IO () -> IO ()
supervise sup taskId task = do
  a <- async (task `catch` handleError)
  atomically (modifyTVar' (supWorkers sup) (Map.insert taskId a))
  where
    handleError (e :: SomeException) = case supPolicy sup of
      AlwaysRestart -> supervise sup taskId task
      NoRestart     -> return ()
      RestartN n | n > 0 -> supervise (sup { supPolicy = RestartN (n-1) }) taskId task
      _ -> return ()

-- Wait with timeout and fallback
withFallback :: Int -> IO a -> IO a -> IO a
withFallback micros primary fallback =
  race (threadDelay micros >> return Nothing) (Just <$> primary) >>= \case
    Right (Just v) -> return v
    _              -> fallback

-- Exception accumulation
data AccumException = AccumException [SomeException]
  deriving (Typeable)
instance Exception AccumException
instance Show AccumException where
  show (AccumException es) = "Multiple exceptions: " ++ show (length es)

-- Run all, accumulate exceptions
runAll :: [IO ()] -> IO ()
runAll actions = do
  results <- mapConcurrently (try @SomeException) actions
  let errors = [e | Left e <- results]
  unless (null errors) (throwIO (AccumException errors))
```

---

## ขั้นตอนที่ 934: Rate Limiting & Throttling

```haskell
-- Rate limiting patterns

-- Token bucket algorithm
data TokenBucket = TokenBucket
  { tbTokens     :: TVar Double
  , tbMaxTokens  :: Double
  , tbFillRate   :: Double   -- tokens per second
  , tbLastFill   :: TVar UTCTime
  }

newTokenBucket :: Double -> Double -> IO TokenBucket
newTokenBucket maxTokens fillRate = do
  tokens <- newTVarIO maxTokens
  now    <- getCurrentTime
  lastFill <- newTVarIO now
  return (TokenBucket tokens maxTokens fillRate lastFill)

-- Consume tokens (blocking)
consume :: TokenBucket -> Double -> IO ()
consume tb cost = do
  fillTokens tb
  atomically $ do
    tokens <- readTVar (tbTokens tb)
    check (tokens >= cost)  -- block until enough tokens
    writeTVar (tbTokens tb) (tokens - cost)

-- Fill tokens based on elapsed time
fillTokens :: TokenBucket -> IO ()
fillTokens tb = do
  now  <- getCurrentTime
  last <- readTVarIO (tbLastFill tb)
  let elapsed  = realToFrac (diffUTCTime now last) :: Double
      newTokens = elapsed * tbFillRate tb
  atomically $ do
    tokens <- readTVar (tbTokens tb)
    let filled = min (tbMaxTokens tb) (tokens + newTokens)
    writeTVar (tbTokens tb) filled
  writeIORef (tbLastFill tb) now  -- simplified

-- Rate limiter for API calls
data RateLimiter = RateLimiter
  { rlBucket  :: TokenBucket
  , rlWindow  :: TVar [(UTCTime, Int)]  -- sliding window
  }

withRateLimit :: RateLimiter -> IO a -> IO a
withRateLimit rl action = do
  consume (rlBucket rl) 1
  action

-- Fixed window rate limiter
data WindowLimiter = WindowLimiter
  { wlWindow  :: Int         -- window size in seconds
  , wlMax     :: Int         -- max requests per window
  , wlCounts  :: TVar (Map Int Int)  -- window -> count
  }

checkWindowLimit :: WindowLimiter -> IO Bool
checkWindowLimit wl = do
  now <- getCurrentTime
  let windowId = floor (realToFrac (utcTimeToPOSIXSeconds now)) `div` wlWindow wl :: Int
  atomically $ do
    counts <- readTVar (wlCounts wl)
    let count = fromMaybe 0 (Map.lookup windowId counts)
    if count >= wlMax wl
      then return False
      else do
        modifyTVar' (wlCounts wl) (Map.insertWith (+) windowId 1)
        return True
```

---

## ขั้นตอนที่ 935: Concurrent Logging

```haskell
-- High-performance concurrent logging

import System.IO
import Control.Concurrent.STM

data LogLevel = DEBUG | INFO | WARN | ERROR | FATAL deriving (Show, Eq, Ord)

data LogRecord = LogRecord
  { lrLevel     :: LogLevel
  , lrTimestamp :: UTCTime
  , lrMessage   :: Text
  , lrContext   :: Map Text Text
  }

data Logger = Logger
  { lgQueue  :: TBQueue LogRecord
  , lgLevel  :: LogLevel
  }

newLogger :: LogLevel -> IO Logger
newLogger level = do
  q <- newTBQueueIO 10000  -- buffer up to 10000 records
  return (Logger q level)

-- Non-blocking log (drops if queue full)
logNB :: Logger -> LogLevel -> Text -> IO ()
logNB lg level msg
  | level < lgLevel lg = return ()  -- fast path
  | otherwise = do
      now <- getCurrentTime
      let record = LogRecord level now msg Map.empty
      void $ atomically (tryWriteTBQueue (lgQueue lg) record)

-- Blocking log
logB :: Logger -> LogLevel -> Text -> IO ()
logB lg level msg
  | level < lgLevel lg = return ()
  | otherwise = do
      now <- getCurrentTime
      let record = LogRecord level now msg Map.empty
      atomically (writeTBQueue (lgQueue lg) record)

-- Log processor (async writer)
startLogProcessor :: Logger -> Handle -> IO ThreadId
startLogProcessor lg handle = forkIO $ forever $ do
  record <- atomically (readTBQueue (lgQueue lg))
  let line = formatRecord record
  hPutStrLn handle (T.unpack line)
  hFlush handle

formatRecord :: LogRecord -> Text
formatRecord r = T.unwords
  [ showTime (lrTimestamp r)
  , "[" <> T.pack (show (lrLevel r)) <> "]"
  , lrMessage r
  ]

-- Structured logging with context
withContext :: Map Text Text -> Logger -> Logger
withContext ctx lg = lg  -- simplified: would wrap enqueue

class HasLogger m where
  getLogger :: m Logger

logInfo :: HasLogger m => MonadIO m => Text -> m ()
logInfo msg = do
  lg <- getLogger
  liftIO (logB lg INFO msg)
```

---

## ขั้นตอนที่ 936: Connection Pool

```haskell
-- Generic connection pool

import Data.Pool (Pool, createPool, withResource)
import qualified Data.Pool as Pool

-- Pool configuration
data PoolConfig = PoolConfig
  { poolSize       :: Int
  , poolIdleTimeout :: NominalDiffTime
  , poolMaxLifetime :: NominalDiffTime
  }

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
  { poolSize        = 10
  , poolIdleTimeout = 600
  , poolMaxLifetime = 3600
  }

-- Database connection pool
makeDbPool :: PoolConfig -> DatabaseUrl -> IO (Pool Connection)
makeDbPool cfg url = createPool
  (connectDatabase url)  -- create
  closeConnection        -- destroy
  1                      -- stripes
  (poolIdleTimeout cfg)
  (poolSize cfg)

-- Use connection from pool
withDb :: Pool Connection -> (Connection -> IO a) -> IO a
withDb = withResource

-- HTTP connection pool
data HttpPool = HttpPool
  { hpPool    :: Pool Manager
  , hpBaseUrl :: Text
  }

newHttpPool :: Int -> Text -> IO HttpPool
newHttpPool n baseUrl = do
  pool <- createPool
    (newManager defaultManagerSettings)
    (\_ -> return ())  -- no cleanup needed
    1 300 n
  return (HttpPool pool baseUrl)

httpGet :: HttpPool -> Text -> IO (Response BSL.ByteString)
httpGet hp path = withResource (hpPool hp) $ \mgr -> do
  let url = T.unpack (hpBaseUrl hp <> path)
  request <- parseRequest url
  httpLbs request mgr

-- Pool metrics
data PoolMetrics = PoolMetrics
  { pmActive  :: Int
  , pmIdle    :: Int
  , pmWaiting :: Int
  }

getPoolMetrics :: Pool a -> IO PoolMetrics
getPoolMetrics pool = do
  stats <- Pool.stats pool
  return (PoolMetrics (Pool.activeConnections stats) (Pool.idleConnections stats) 0)
```

---

## ขั้นตอนที่ 937: Broadcast & Pub/Sub

```haskell
-- Broadcast and pub/sub patterns

-- Broadcast channel
data Broadcast a = Broadcast
  { bcTopics    :: TVar (Map Text (Set (TQueue a)))
  , bcAll       :: TVar (Set (TQueue a))
  }

newBroadcast :: IO (Broadcast a)
newBroadcast = Broadcast <$> newTVarIO Map.empty <*> newTVarIO Set.empty

subscribe :: Broadcast a -> Maybe Text -> IO (TQueue a)
subscribe bc topic = do
  q <- newTQueueIO
  atomically $ case topic of
    Nothing -> modifyTVar' (bcAll bc) (Set.insert q)
    Just t  -> modifyTVar' (bcTopics bc) (Map.insertWith Set.union t (Set.singleton q))
  return q

unsubscribe :: Broadcast a -> Maybe Text -> TQueue a -> IO ()
unsubscribe bc topic q = atomically $ case topic of
  Nothing -> modifyTVar' (bcAll bc) (Set.delete q)
  Just t  -> modifyTVar' (bcTopics bc) (Map.adjust (Set.delete q) t)

publish :: Broadcast a -> Maybe Text -> a -> IO ()
publish bc topic msg = atomically $ do
  allSubs  <- readTVar (bcAll bc)
  topicSubs <- case topic of
    Nothing -> return Set.empty
    Just t  -> fromMaybe Set.empty . Map.lookup t <$> readTVar (bcTopics bc)
  forM_ (Set.toList (Set.union allSubs topicSubs)) (`writeTQueue` msg)

-- Event bus
data EventBus = EventBus (Broadcast Event)

data Event = Event { evType :: Text, evPayload :: Value }

newEventBus :: IO EventBus
newEventBus = EventBus <$> newBroadcast

on :: EventBus -> Text -> (Event -> IO ()) -> IO ()
on (EventBus bc) topic handler = do
  q <- subscribe bc (Just topic)
  void $ forkIO $ forever $ do
    ev <- atomically (readTQueue q)
    handler ev `catch` (\(e :: SomeException) -> logError e)

emit :: EventBus -> Text -> Value -> IO ()
emit (EventBus bc) evType payload =
  publish bc (Just evType) (Event evType payload)
```

---

## ขั้นตอนที่ 938: Thread-Local Storage

```haskell
-- Thread-local storage patterns

import System.IO.Unsafe (unsafePerformIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Global thread-local map
{-# NOINLINE threadLocalMap #-}
threadLocalMap :: MVar (Map ThreadId (Map Text Dynamic))
threadLocalMap = unsafePerformIO (newMVar Map.empty)

setThreadLocal :: Typeable a => Text -> a -> IO ()
setThreadLocal key val = do
  tid <- myThreadId
  modifyMVar_ threadLocalMap $ \m ->
    let inner  = fromMaybe Map.empty (Map.lookup tid m)
        inner' = Map.insert key (toDyn val) inner
    in return (Map.insert tid inner' m)

getThreadLocal :: Typeable a => Text -> IO (Maybe a)
getThreadLocal key = do
  tid <- myThreadId
  m   <- readMVar threadLocalMap
  case Map.lookup tid m of
    Nothing    -> return Nothing
    Just inner -> return $ Map.lookup key inner >>= fromDynamic

-- Request context using thread locals
data RequestCtx = RequestCtx
  { rcRequestId :: Text
  , rcUserId    :: Maybe Int
  , rcStartTime :: UTCTime
  }

setRequestCtx :: RequestCtx -> IO ()
setRequestCtx = setThreadLocal "request_ctx"

getRequestCtx :: IO (Maybe RequestCtx)
getRequestCtx = getThreadLocal "request_ctx"

withRequestCtx :: RequestCtx -> IO a -> IO a
withRequestCtx ctx action = do
  setRequestCtx ctx
  result <- action
  -- cleanup thread locals
  tid <- myThreadId
  modifyMVar_ threadLocalMap (return . Map.delete tid)
  return result
```

---

## ขั้นตอนที่ 939: Concurrent Testing

```haskell
-- Testing concurrent code

import Test.Hspec
import Control.Concurrent.STM

-- Test STM transactions
spec_stm :: Spec
spec_stm = describe "STM" $ do
  it "atomic increment" $ do
    var <- newTVarIO (0 :: Int)
    let increment' = atomically (modifyTVar' var (+1))
    replicateM_ 1000 (forkIO increment')
    threadDelay 100000  -- wait for completion
    val <- readTVarIO var
    val `shouldBe` 1000

  it "no lost updates" $ do
    var <- newTVarIO (0 :: Int)
    -- concurrent read-modify-write via STM
    let update = atomically $ do
          v <- readTVar var
          writeTVar var (v + 1)
    asyncs <- replicateM 100 (async update)
    mapM_ wait asyncs
    val <- readTVarIO var
    val `shouldBe` 100

-- Linearizability test
linearizabilityTest :: IO ()
linearizabilityTest = do
  stack <- newLFStack
  
  -- Interleave pushes and pops
  let pushes = forM_ [1..100] (pushLF stack)
      pops   = replicateM 100 (popLF stack)
  
  (_, results) <- concurrently pushes pops
  let values = catMaybes results
  
  -- All non-Nothing results should be unique
  let unique = length (nub values)
  unique `shouldBe` length values

-- Race detector test
raceTest :: IO ()
raceTest = do
  ref <- newIORef ([] :: [Int])
  
  -- These should not race with proper synchronization
  mutex <- newMVar ()
  let safeAppend x = withMVar mutex $ \_ ->
        modifyIORef' ref (x:)
  
  asyncs <- mapM (\i -> async (safeAppend i)) [1..100]
  mapM_ wait asyncs
  
  result <- readIORef ref
  length result `shouldBe` 100
```

---

## ขั้นตอนที่ 940: โปรเจกต์: High-Performance Concurrent Server

```haskell
-- High-performance concurrent server with all patterns

module ConcurrentServer where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Pool (Pool)

-- Server state
data ServerState = ServerState
  { ssConnections  :: TVar Int
  , ssRequests     :: TVar Int
  , ssErrors       :: TVar Int
  , ssDb           :: Pool Connection
  , ssCache        :: LRUCache Text Value
  , ssRateLimiter  :: RateLimiter
  , ssEventBus     :: EventBus
  , ssBroadcast    :: Broadcast Notification
  , ssWorkerPool   :: WorkStealingPool
  }

-- Initialize server
initServer :: Config -> IO ServerState
initServer cfg = do
  connections <- newTVarIO 0
  requests    <- newTVarIO 0
  errors      <- newTVarIO 0
  db          <- makeDbPool (cfgPoolConfig cfg) (cfgDbUrl cfg)
  cache       <- newLRU (cfgCacheSize cfg)
  rateLimiter <- newRateLimiter (cfgRateLimit cfg)
  eventBus    <- newEventBus
  broadcast   <- newBroadcast
  workerPool  <- newWSPool (cfgWorkers cfg)
  
  startPool workerPool
  
  return ServerState
    { ssConnections = connections
    , ssRequests    = requests
    , ssErrors      = errors
    , ssDb          = db
    , ssCache       = cache
    , ssRateLimiter = rateLimiter
    , ssEventBus    = eventBus
    , ssBroadcast   = broadcast
    , ssWorkerPool  = workerPool
    }

-- Handle request with all safety measures
handleRequest :: ServerState -> Request -> IO Response
handleRequest ss req = do
  -- Rate limiting
  allowed <- checkWindowLimit (ssRateLimiter ss)
  unless allowed (throwIO TooManyRequests)
  
  -- Track metrics
  atomically (modifyTVar' (ssRequests ss) (+1))
  
  -- Try cache first
  cached <- lookupLRU (ssCache ss) (reqCacheKey req)
  case cached of
    Just v  -> return (Response 200 v)
    Nothing -> do
      -- Fetch from DB
      result <- withDb (ssDb ss) $ \conn -> fetchData conn req
      insertLRU (ssCache ss) (reqCacheKey req) result
      
      -- Emit event
      emit (ssEventBus ss) "request.handled" (toJSON req)
      
      return (Response 200 result)
  `catch` \(e :: SomeException) -> do
    atomically (modifyTVar' (ssErrors ss) (+1))
    return (Response 500 (toJSON (show e)))

main :: IO ()
main = do
  cfg   <- loadConfig
  state <- initServer cfg
  let logger = mkLogger cfg
  
  logInfo logger "Server starting..."
  
  -- Start websocket broadcaster
  startBroadcaster state
  
  -- Start HTTP server
  run (cfgPort cfg) (app state)
```

---

*[← Part 46](part-46.md) | [Part 48 →](part-48.md)*
