# Part 14: Concurrency และ Parallelism
## ขั้นตอนที่ 261-280: Async, STM, Parallel Programming

---

## บทนำ

Haskell มีเครื่องมือที่ทรงพลังสำหรับ concurrent และ parallel programming: lightweight threads, STM transactions, และ async APIs

---

## ขั้นตอนที่ 261: Lightweight Threads

```haskell
import Control.Concurrent
import Control.Concurrent.Async

-- forkIO: สร้าง lightweight thread
-- Haskell threads เป็น M:N (many-to-many)
-- ใช้ heap เริ่มต้น 1KB เท่านั้น

-- simple threads
main :: IO ()
main = do
  -- สร้าง thread
  tid <- forkIO $ do
    putStrLn "Thread 1"
    threadDelay 1000000  -- 1 second
    putStrLn "Thread 1 done"
  
  -- Main thread ยังทำงานต่อ
  putStrLn "Main thread"
  
  -- รอ thread ด้วย MVar
  done <- newEmptyMVar
  forkIO $ do
    threadDelay 500000
    putStrLn "Thread 2 done"
    putMVar done ()
  
  takeMVar done  -- รอจนกว่า Thread 2 จะเสร็จ
  putStrLn "All done"

-- kill thread
killExample :: IO ()
killExample = do
  tid <- forkIO $ do
    threadDelay 10000000
    putStrLn "This won't print"
  killThread tid
  putStrLn "Thread killed"

-- Thread scheduling: cooperative preemption
-- ทุก 10ms GHC จะ check thread switching points
```

---

## ขั้นตอนที่ 262: Async API

```haskell
import Control.Concurrent.Async

-- async: run IO action in background, get Async handle
asyncExample :: IO ()
asyncExample = do
  a <- async $ do
    threadDelay 1000000
    return 42
  
  -- Do other work...
  putStrLn "Working..."
  
  -- Wait for result
  result <- wait a
  putStrLn $ "Got: " ++ show result

-- cancel: cancel async action
cancelExample :: IO ()
cancelExample = do
  a <- async $ do
    threadDelay 10000000
    putStrLn "Won't print"
  cancel a
  putStrLn "Cancelled"

-- withAsync: automatic cleanup
withAsyncExample :: IO ()
withAsyncExample = withAsync (threadDelay 1000000 >> return 42) $ \a -> do
  result <- wait a
  print result
-- Automatically cancelled if we leave the block

-- concurrently: run two actions in parallel
parallelExample :: IO ()
parallelExample = do
  (r1, r2) <- concurrently
    (do threadDelay 500000; return "first")
    (do threadDelay 300000; return "second")
  putStrLn $ "Results: " ++ r1 ++ ", " ++ r2

-- race: first to finish wins
raceExample :: IO ()
raceExample = do
  result <- race
    (do threadDelay 200000; return "fast")
    (do threadDelay 500000; return "slow")
  case result of
    Left  r -> putStrLn $ "First: " ++ r
    Right r -> putStrLn $ "Second: " ++ r

-- mapConcurrently: parallel map
processAll :: [Int] -> IO [Int]
processAll = mapConcurrently $ \n -> do
  threadDelay 100000  -- simulated work
  return (n * 2)
```

---

## ขั้นตอนที่ 263: STM (Software Transactional Memory)

```haskell
import Control.Concurrent.STM

-- STM: composable atomic transactions
-- atomically :: STM a -> IO a

-- TVar: mutable variable in STM
counterExample :: IO ()
counterExample = do
  counter <- newTVarIO (0 :: Int)
  
  -- Spawn 10 threads that each increment 100 times
  threads <- replicateM 10 $ async $ atomically $
    replicateM_ 100 (modifyTVar counter (+1))
  
  mapM_ wait threads
  
  result <- readTVarIO counter
  putStrLn $ "Counter: " ++ show result  -- Should be 1000

-- STM is composable: combine transactions safely
transferFunds :: TVar Int -> TVar Int -> Int -> STM ()
transferFunds from to amount = do
  fromBalance <- readTVar from
  if fromBalance < amount
    then retry  -- rerun transaction when state changes
    else do
      modifyTVar from (subtract amount)
      modifyTVar to   (+amount)

-- atomically makes it all-or-nothing
doTransfer :: Int -> IO Bool
doTransfer amount = do
  alice <- newTVarIO 100
  bob   <- newTVarIO 50
  
  result <- atomically $ do
    aliceBalance <- readTVar alice
    if aliceBalance >= amount
      then do transferFunds alice bob amount; return True
      else return False
  
  aliceFinal <- readTVarIO alice
  bobFinal   <- readTVarIO bob
  putStrLn $ "Alice: " ++ show aliceFinal ++ ", Bob: " ++ show bobFinal
  return result
```

---

## ขั้นตอนที่ 264: STM Channels

```haskell
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TQueue

-- TChan: unbounded channel
-- TQueue: unbounded FIFO queue
-- TBQueue: bounded FIFO queue

-- Producer-consumer ด้วย TQueue
data Job = Job { jobId :: Int, jobData :: String } deriving (Show)

worker :: TQueue Job -> IO ()
worker queue = forever $ do
  job <- atomically (readTQueue queue)  -- blocks if empty
  putStrLn $ "Processing: " ++ show (jobId job)
  threadDelay 100000  -- simulated work

producer :: TQueue Job -> [Job] -> IO ()
producer queue jobs = forM_ jobs $ \job -> do
  atomically (writeTQueue queue job)
  threadDelay 50000

-- ตัวอย่าง: Thread pool ด้วย TQueue
threadPool :: Int -> IO (TQueue (IO ()), IO ())
threadPool n = do
  queue <- newTQueueIO
  workers <- replicateM n $ async $ forever $ do
    action <- atomically (readTQueue queue)
    action
  let shutdown = do
        mapM_ cancel workers
  return (queue, shutdown)

submitJob :: TQueue (IO ()) -> IO () -> IO ()
submitJob queue action = atomically (writeTQueue queue action)

-- TBQueue: bounded (blocks producer if full)
boundedExample :: IO ()
boundedExample = do
  queue <- atomically (newTBQueue 10)  -- max 10 items
  
  -- Producer (will block when queue is full)
  async $ forM_ [1..100] $ \n -> do
    atomically (writeTBQueue queue n)
  
  -- Consumer
  replicateM_ 100 $ do
    n <- atomically (readTBQueue queue)
    print n
```

---

## ขั้นตอนที่ 265: STM Advanced Patterns

```haskell
-- retry: ทำ transaction ซ้ำเมื่อ state เปลี่ยน

-- waitUntil: รอจนกว่า condition เป็น True
waitUntilTrue :: TVar Bool -> STM ()
waitUntilTrue var = do
  b <- readTVar var
  unless b retry  -- retry ถ้า False

-- orElse: ลอง transaction แรก ถ้า retry ลองอีก transaction
firstAvailable :: TVar (Maybe a) -> TVar (Maybe a) -> STM a
firstAvailable v1 v2 =
  readAndTake v1 `orElse` readAndTake v2
  where
    readAndTake var = do
      mVal <- readTVar var
      case mVal of
        Nothing -> retry
        Just v  -> do
          writeTVar var Nothing
          return v

-- ตัวอย่าง: Semaphore
data Semaphore = Semaphore (TVar Int)

newSemaphore :: Int -> IO Semaphore
newSemaphore n = Semaphore <$> newTVarIO n

acquire :: Semaphore -> STM ()
acquire (Semaphore var) = do
  n <- readTVar var
  if n > 0
    then writeTVar var (n - 1)
    else retry

release :: Semaphore -> STM ()
release (Semaphore var) = modifyTVar var (+1)

withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore sem action = do
  atomically (acquire sem)
  result <- action `finally` atomically (release sem)
  return result

-- Rate limiter
data RateLimiter = RateLimiter
  { _limit :: Int
  , _tokens :: TVar Int
  }

newRateLimiter :: Int -> IO RateLimiter
newRateLimiter n = RateLimiter n <$> newTVarIO n

checkRate :: RateLimiter -> STM Bool
checkRate rl = do
  tokens <- readTVar (_tokens rl)
  if tokens > 0
    then do modifyTVar (_tokens rl) (subtract 1); return True
    else return False
```

---

## ขั้นตอนที่ 266: Exception Handling ใน Concurrent Code

```haskell
import Control.Exception
import Control.Concurrent.Async

-- Exception propagation ใน async
asyncException :: IO ()
asyncException = do
  a <- async (throwIO (userError "Something failed"))
  result <- waitCatch a  -- Either SomeException a
  case result of
    Left err  -> putStrLn $ "Caught: " ++ show err
    Right val -> putStrLn $ "Got: " ++ show val

-- withAsync ด้วย cleanup
safeAsync :: IO ()
safeAsync = do
  result <- withAsync (do
    threadDelay 1000000
    return 42
    ) $ \a -> do
    x <- wait a
    return x
  print result

-- Async exceptions: จัดการ exceptions ที่มาจาก thread อื่น
handleAsync :: IO ()
handleAsync = do
  main_thread <- myThreadId
  
  -- Thread ที่ throw exception ไปยัง main
  forkIO $ do
    threadDelay 500000
    throwTo main_thread (userError "External exception")
  
  -- Handle ด้วย catch
  handle (\e -> putStrLn $ "Got async exception: " ++ show (e :: SomeException)) $ do
    threadDelay 2000000
    putStrLn "Done (won't reach here)"

-- mask: protect critical section
criticalSection :: IO ()
criticalSection = mask $ \restore -> do
  acquire_resource
  result <- restore action `onException` release_resource
  release_resource
  return result
  where
    acquire_resource = return ()
    release_resource = return ()
    action = return ()
```

---

## ขั้นตอนที่ 267: Parallel Computation

```haskell
-- parallel package: pure parallel computation

import Control.Parallel
import Control.Parallel.Strategies

-- par: hint to evaluate in parallel
parExample :: Int
parExample = let
  x = expensiveComputation 100
  y = expensiveComputation 200
  in x `par` (y `pseq` x + y)

expensiveComputation :: Int -> Int
expensiveComputation n = foldl' (+) 0 [1..n]

-- Strategies: high-level parallel programming
parListExample :: [Int] -> [Int]
parListExample xs = map (*2) xs `using` parList rseq

-- rpar: evaluate in parallel
-- rseq: evaluate sequentially
-- r0: don't evaluate (lazy)

-- parMap: parallel map
parDoubler :: [Int] -> [Int]
parDoubler = parMap rseq (*2)

-- Parallel fold
parSum :: [Int] -> Int
parSum xs = runEval $ do
  let (left, right) = splitAt (length xs `div` 2) xs
  leftSum  <- rpar (sum left)
  rightSum <- rpar (sum right)
  rseq leftSum
  rseq rightSum
  return (leftSum + rightSum)

-- Divide and conquer
parMergeSort :: Ord a => [a] -> [a]
parMergeSort [] = []
parMergeSort [x] = [x]
parMergeSort xs = runEval $ do
  let (left, right) = splitAt (length xs `div` 2) xs
  left'  <- rpar (parMergeSort left)
  right' <- rpar (parMergeSort right)
  rseq left'
  rseq right'
  return (merge left' right')
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
```

---

## ขั้นตอนที่ 268: Async Programming Patterns

```haskell
import Control.Concurrent.Async

-- Fan-out: สร้าง tasks หลาย tasks แล้วรอทั้งหมด
fanOut :: [IO a] -> IO [a]
fanOut actions = mapConcurrently id actions

-- Fan-in: รอ task แรกที่เสร็จ
fanIn :: [IO a] -> IO (a, [Async a])
fanIn actions = do
  asyncs <- mapM async actions
  (a, result) <- waitAny asyncs
  return (result, asyncs)

-- Pipeline: sequential stages แต่ parallel stages
type Stage a b = a -> IO b

pipeline :: [Stage a a] -> a -> IO a
pipeline stages input = foldM (\x stage -> stage x) input stages

-- ตัวอย่าง: Image processing pipeline
data Image = Image { pixels :: [Int] }

processImage :: Image -> IO Image
processImage img = do
  (r1, r2) <- concurrently
    (applyFilter "blur"     img)
    (applyFilter "sharpen"  img)
  combined <- combine r1 r2
  return combined
  where
    applyFilter name img = do
      threadDelay 100000
      return (img { pixels = map (+1) (pixels img) })
    combine i1 i2 = return (Image (zipWith (+) (pixels i1) (pixels i2)))

-- Retry logic
withRetry :: Int -> IO a -> IO (Either SomeException a)
withRetry 0 action = try action
withRetry n action = do
  result <- try action
  case result of
    Right val -> return (Right val)
    Left  err -> do
      putStrLn $ "Retrying after error: " ++ show (err :: SomeException)
      threadDelay 1000000
      withRetry (n-1) action
```

---

## ขั้นตอนที่ 269: Actor Model

```haskell
-- Actor model ใน Haskell ด้วย STM

data Actor msg = Actor
  { actorQueue :: TQueue msg
  , actorThread :: Async ()
  }

newActor :: (msg -> IO ()) -> IO (Actor msg)
newActor handler = do
  queue <- newTQueueIO
  thread <- async $ forever $ do
    msg <- atomically (readTQueue queue)
    handler msg
  return (Actor queue thread)

send :: Actor msg -> msg -> IO ()
send actor msg = atomically (writeTQueue (actorQueue actor) msg)

stopActor :: Actor msg -> IO ()
stopActor actor = cancel (actorThread actor)

-- ตัวอย่าง: Logger actor
data LogMsg = LogInfo String | LogWarn String | LogError String

loggerActor :: IO (Actor LogMsg)
loggerActor = newActor $ \msg -> case msg of
  LogInfo  m -> putStrLn $ "[INFO]  " ++ m
  LogWarn  m -> putStrLn $ "[WARN]  " ++ m
  LogError m -> putStrLn $ "[ERROR] " ++ m

-- ตัวอย่าง: Counter actor
data CounterMsg = Increment | Decrement | GetCount (MVar Int)

counterActor :: IO (Actor CounterMsg)
counterActor = do
  count <- newTVarIO (0 :: Int)
  newActor $ \msg -> case msg of
    Increment -> atomically (modifyTVar count (+1))
    Decrement -> atomically (modifyTVar count (subtract 1))
    GetCount reply -> do
      n <- readTVarIO count
      putMVar reply n

main :: IO ()
main = do
  logger  <- loggerActor
  counter <- counterActor
  
  send logger (LogInfo "Starting")
  send counter Increment
  send counter Increment
  send counter Increment
  
  reply <- newEmptyMVar
  send counter (GetCount reply)
  n <- takeMVar reply
  
  send logger (LogInfo $ "Counter: " ++ show n)
  
  threadDelay 100000  -- let actors process
  stopActor logger
  stopActor counter
```

---

## ขั้นตอนที่ 270: threadscope และ Profiling

```haskell
-- Profiling concurrent programs

-- Compile ด้วย eventlog:
-- ghc -eventlog -rtsopts program.hs

-- Run ด้วย eventlog:
-- ./program +RTS -l

-- เปิดด้วย threadscope:
-- threadscope program.eventlog

-- ตัวอย่าง: เพิ่ม labels สำหรับ threads
import Control.Concurrent
import GHC.Conc (labelThread)

namedThread :: String -> IO () -> IO ThreadId
namedThread name action = do
  tid <- forkIO action
  labelThread tid name
  return tid

-- ตัวอย่าง: ดู performance
parallelProgram :: IO ()
parallelProgram = do
  a1 <- async $ do
    labelThread =<< myThreadId $ "worker-1"
    -- work...
    return (sum [1..1000000 :: Int])
  a2 <- async $ do
    labelThread =<< myThreadId $ "worker-2"
    -- work...
    return (sum [1000001..2000000 :: Int])
  
  r1 <- wait a1
  r2 <- wait a2
  print (r1 + r2)
  where
    labelThread tid label = labelThread tid label
```

---

## ขั้นตอนที่ 271: Bracket Pattern

```haskell
-- Bracket: ensures cleanup even with exceptions

import Control.Exception (bracket, finally, onException)

-- bracket: acquire -> use -> release
withDatabase :: String -> (Connection -> IO a) -> IO a
withDatabase connStr = bracket
  (connectDB connStr)   -- acquire
  disconnectDB          -- release (always runs)

type Connection = String  -- simplified

connectDB :: String -> IO Connection
connectDB connStr = do
  putStrLn $ "Connecting to " ++ connStr
  return connStr

disconnectDB :: Connection -> IO ()
disconnectDB conn = putStrLn $ "Disconnecting from " ++ conn

-- ใช้
queryDB :: String -> IO [String]
queryDB connStr = withDatabase connStr $ \conn -> do
  putStrLn "Executing query..."
  return ["result1", "result2"]

-- ResourceT: bracket ใน transformer stack
import Control.Monad.Trans.Resource

withResources :: IO ()
withResources = runResourceT $ do
  (_, conn1) <- allocate (connectDB "db1") disconnectDB
  (_, conn2) <- allocate (connectDB "db2") disconnectDB
  liftIO $ putStrLn "Using both connections"
  -- cleanup happens automatically when block exits

-- ตัวอย่าง: nested resources
processFile :: FilePath -> IO ()
processFile path = do
  bracket
    (do putStrLn "Opening file"; openFile path ReadMode)
    (\h -> putStrLn "Closing file" >> hClose h)
    (\handle -> do
      content <- hGetContents handle
      putStrLn $ "File has " ++ show (length (lines content)) ++ " lines")
```

---

## ขั้นตอนที่ 272: Worker Pool Pattern

```haskell
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

-- Worker pool: fixed number of workers processing jobs

data WorkerPool a b = WorkerPool
  { wpJobQueue    :: TQueue a
  , wpResultQueue :: TQueue (Either SomeException b)
  , wpWorkers     :: [Async ()]
  }

newWorkerPool :: Int -> (a -> IO b) -> IO (WorkerPool a b)
newWorkerPool n worker = do
  jobs    <- newTQueueIO
  results <- newTQueueIO
  
  workers <- replicateM n $ async $ forever $ do
    job <- atomically (readTQueue jobs)
    result <- try (worker job)
    atomically (writeTQueue results result)
  
  return (WorkerPool jobs results workers)

submitWork :: WorkerPool a b -> a -> IO ()
submitWork pool job = atomically (writeTQueue (wpJobQueue pool) job)

collectResult :: WorkerPool a b -> IO (Either SomeException b)
collectResult pool = atomically (readTQueue (wpResultQueue pool))

shutdownPool :: WorkerPool a b -> IO ()
shutdownPool pool = mapM_ cancel (wpWorkers pool)

-- ใช้ worker pool
main :: IO ()
main = do
  pool <- newWorkerPool 4 processJob
  
  -- Submit 20 jobs
  forM_ [1..20] $ \i -> submitWork pool (Job i)
  
  -- Collect 20 results
  results <- replicateM 20 (collectResult pool)
  
  putStrLn $ "Success: " ++ show (length [r | Right r <- results])
  putStrLn $ "Failed:  " ++ show (length [r | Left  r <- results])
  
  shutdownPool pool

data Job = Job Int
processJob :: Job -> IO String
processJob (Job n) = do
  threadDelay (100000 * (n `mod` 5))
  return $ "Result of job " ++ show n
```

---

## ขั้นตอนที่ 273: Circuit Breaker Pattern

```haskell
-- Circuit Breaker: ป้องกันไม่ให้เรียก service ที่ fail ซ้ำๆ

data CircuitState = Closed | Open | HalfOpen deriving (Eq, Show)

data CircuitBreaker = CircuitBreaker
  { cbState      :: TVar CircuitState
  , cbFailures   :: TVar Int
  , cbThreshold  :: Int
  , cbLastFailed :: TVar (Maybe UTCTime)
  , cbTimeout    :: NominalDiffTime
  }

newCircuitBreaker :: Int -> NominalDiffTime -> IO CircuitBreaker
newCircuitBreaker threshold timeout = do
  state   <- newTVarIO Closed
  failures <- newTVarIO 0
  lastFailed <- newTVarIO Nothing
  return CircuitBreaker
    { cbState      = state
    , cbFailures   = failures
    , cbThreshold  = threshold
    , cbLastFailed = lastFailed
    , cbTimeout    = timeout
    }

callWithBreaker :: CircuitBreaker -> IO a -> IO (Either String a)
callWithBreaker cb action = do
  state <- readTVarIO (cbState cb)
  case state of
    Open -> checkTimeout cb action
    _    -> do
      result <- try action
      case result of
        Right val -> do
          atomically $ do
            writeTVar (cbState cb) Closed
            writeTVar (cbFailures cb) 0
          return (Right val)
        Left err -> do
          handleFailure cb
          return (Left (show (err :: SomeException)))

handleFailure :: CircuitBreaker -> IO ()
handleFailure cb = do
  now <- getCurrentTime
  atomically $ do
    failures <- readTVar (cbFailures cb)
    let newFailures = failures + 1
    writeTVar (cbFailures cb) newFailures
    writeTVar (cbLastFailed cb) (Just now)
    when (newFailures >= cbThreshold cb) $
      writeTVar (cbState cb) Open

checkTimeout :: CircuitBreaker -> IO a -> IO (Either String a)
checkTimeout cb action = do
  now <- getCurrentTime
  mLastFailed <- readTVarIO (cbLastFailed cb)
  case mLastFailed of
    Nothing -> return (Left "Circuit open")
    Just lastFailed ->
      if now `diffUTCTime` lastFailed > cbTimeout cb
        then do
          atomically (writeTVar (cbState cb) HalfOpen)
          callWithBreaker cb action
        else return (Left "Circuit open")
```

---

## ขั้นตอนที่ 274: Supervisor Pattern

```haskell
-- Supervisor: restart failed processes

data SupervisionStrategy
  = OneForOne  -- restart only failed child
  | AllForOne  -- restart all children when one fails
  | RestForOne -- restart failed child and those started after it

data Supervisor = Supervisor
  { children :: TVar [ChildSpec]
  , thread   :: Async ()
  }

data ChildSpec = ChildSpec
  { childName    :: String
  , childAction  :: IO ()
  , childMaxRestarts :: Int
  , childCurrent :: TVar (Maybe (Async (), Int))  -- (thread, restart count)
  }

-- Simplified supervisor
supervisedWorker :: String -> IO () -> IO (Async ())
supervisedWorker name action = async loop
  where
    loop = do
      result <- try action
      case result of
        Left err -> do
          putStrLn $ "Worker " ++ name ++ " failed: " ++ show (err :: SomeException)
          putStrLn $ "Restarting " ++ name
          threadDelay 1000000
          loop
        Right _ -> putStrLn $ "Worker " ++ name ++ " finished"

-- ตัวอย่าง usage
main :: IO ()
main = do
  w1 <- supervisedWorker "fetcher" $ do
    forM_ [1..] $ \i -> do
      putStrLn $ "Fetching page " ++ show i
      threadDelay 100000
  
  w2 <- supervisedWorker "processor" $ do
    forM_ [1..] $ \i -> do
      putStrLn $ "Processing item " ++ show i
      threadDelay 200000
  
  waitBoth w1 w2
  putStrLn "All workers done"
```

---

## ขั้นตอนที่ 275: Data Race Prevention

```haskell
-- Preventing data races ใน Haskell

-- 1. Pure functions: no shared state -> no races
pureFunction :: Int -> Int
pureFunction x = x * 2  -- always safe

-- 2. STM: transactions prevent races
safeUpdate :: TVar Int -> IO ()
safeUpdate var = atomically (modifyTVar var (+1))

-- 3. IORef ใน single thread
singleThreaded :: IORef Int -> IO ()
singleThreaded ref = modifyIORef' ref (+1)

-- ปัญหา: IORef ใน multiple threads
-- read-modify-write เป็น non-atomic
unsafeUpdate :: IORef Int -> IO ()
unsafeUpdate ref = do
  n <- readIORef ref
  writeIORef ref (n + 1)  -- race condition!

-- แก้ไข: ใช้ atomicModifyIORef
safeIORefUpdate :: IORef Int -> IO ()
safeIORefUpdate ref = atomicModifyIORef' ref (\n -> (n + 1, ()))

-- 4. Chan สำหรับ communication
data Message = Increment | Decrement | Get (MVar Int)

safeCounter :: IO (Chan Message)
safeCounter = do
  chan <- newChan
  var <- newTVarIO (0 :: Int)
  forkIO $ forever $ do
    msg <- readChan chan
    case msg of
      Increment -> atomically (modifyTVar var (+1))
      Decrement -> atomically (modifyTVar var (subtract 1))
      Get reply -> readTVarIO var >>= putMVar reply
  return chan
```

---

## ขั้นตอนที่ 276: Distributed Computing พื้นฐาน

```haskell
-- cloud-haskell: distributed computing ใน Haskell

import Control.Distributed.Process

-- Process: แบบ actor model แต่ distributed

-- spawn: สร้าง process บน node ใดก็ได้
-- send: ส่ง message ระหว่าง processes

-- ตัวอย่าง: simple distributed ping-pong
ping :: ProcessId -> Int -> Process ()
ping partner 0 = send partner (0 :: Int)
ping partner n = do
  send partner n
  n' <- expect :: Process Int
  ping partner (n' - 1)

pong :: Process ()
pong = do
  n <- expect :: Process Int
  if n == 0
    then return ()
    else do
      sender <- getSelfPid
      send sender (n - 1)
      pong

-- ตัวอย่าง: MapReduce
mapReduce :: [a] -> (a -> b) -> ([b] -> c) -> Process c
mapReduce xs mapper reducer = do
  results <- mapM (spawnLocal . return . mapper) xs
  values <- mapM (\pid -> expect :: Process b) results
  return (reducer values)

-- ข้อควรระวัง: cloud-haskell ซับซ้อนกว่านี้มาก
-- ตัวอย่างนี้เป็นแค่ concept illustration
```

---

## ขั้นตอนที่ 277: Fiber-based Concurrency

```haskell
-- ki: structured concurrency ใน Haskell

import Ki

-- Scope: lifetime สำหรับ group ของ threads
scopedConcurrency :: IO ()
scopedConcurrency = Ki.scoped $ \scope -> do
  Ki.fork scope $ do
    threadDelay 100000
    putStrLn "Thread 1 done"
  Ki.fork scope $ do
    threadDelay 200000
    putStrLn "Thread 2 done"
  -- Scope ends here: waits for all threads to finish

-- ถ้า thread fail: scope cancels others
failingScope :: IO ()
failingScope = Ki.scoped $ \scope -> do
  Ki.fork scope $ do
    threadDelay 100000
    throwIO (userError "Failed!")
  Ki.fork scope $ do
    threadDelay 500000
    putStrLn "Won't print"
  -- Exception propagated to parent

-- forkTry: catch exceptions from forked thread
safeFork :: Ki.Scope -> IO a -> IO (Async (Either SomeException a))
safeFork scope action = Ki.fork scope (try action)
```

---

## ขั้นตอนที่ 278: Scheduler และ Event Loop

```haskell
-- Event loop ใน Haskell

import Control.Concurrent
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Simple event loop
type EventHandler = IO ()
type EventId = Int

data EventLoop = EventLoop
  { handlers :: IORef (Map EventId EventHandler)
  , nextId   :: IORef EventId
  }

newEventLoop :: IO EventLoop
newEventLoop = EventLoop <$> newIORef Map.empty <*> newIORef 0

on :: EventLoop -> EventHandler -> IO EventId
on loop handler = do
  eid <- atomicModifyIORef' (nextId loop) (\n -> (n+1, n))
  modifyIORef (handlers loop) (Map.insert eid handler)
  return eid

off :: EventLoop -> EventId -> IO ()
off loop eid = modifyIORef (handlers loop) (Map.delete eid)

emit :: EventLoop -> EventId -> IO ()
emit loop eid = do
  hs <- readIORef (handlers loop)
  case Map.lookup eid hs of
    Just h  -> h
    Nothing -> return ()

-- ตัวอย่าง: timer
schedule :: Int -> IO () -> IO ()
schedule delay action = void $ forkIO $ do
  threadDelay delay
  action

scheduleRepeat :: Int -> IO Bool -> IO ()
scheduleRepeat interval action = void $ forkIO loop
  where
    loop = do
      continue <- action
      when continue $ do
        threadDelay interval
        loop
```

---

## ขั้นตอนที่ 279: Streaming ด้วย Conduit

```haskell
import Conduit

-- Conduit: streaming data processing
-- Source -> Conduit -> Sink

-- Source: produces data
numbers :: Source IO Int
numbers = yieldMany [1..100]

-- Conduit: transforms data
doubler :: Conduit Int IO Int
doubler = mapC (*2)

-- Sink: consumes data
sumSink :: Sink Int IO Int
sumSink = foldlC (+) 0

-- Connect them
result :: IO Int
result = runConduit $ numbers .| doubler .| sumSink

-- File processing
processFile :: FilePath -> FilePath -> IO ()
processFile input output = runConduitRes $
  sourceFile input
  .| decodeUtf8C
  .| linesUnboundedC
  .| mapC processLine
  .| unlineC
  .| encodeUtf8C
  .| sinkFile output
  where
    processLine line = T.toUpper line

-- Chunked processing
chunkedProcessing :: [Int] -> IO [[Int]]
chunkedProcessing xs = runConduit $
  yieldMany xs
  .| chunksOfC 10
  .| sinkList
```

---

## ขั้นตอนที่ 280: โปรเจกต์: Concurrent Web Crawler

```haskell
module WebCrawler where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Client
import Data.Text (Text)
import qualified Data.Text as T

data CrawlerState = CrawlerState
  { visited   :: TVar (Set String)
  , toVisit   :: TQueue String
  , results   :: TVar (Map String Int)
  , workers   :: TVar Int
  }

newCrawler :: IO CrawlerState
newCrawler = do
  visited   <- newTVarIO Set.empty
  toVisit   <- newTQueueIO
  results   <- newTVarIO Map.empty
  workers   <- newTVarIO 0
  return (CrawlerState visited toVisit results workers)

addUrl :: CrawlerState -> String -> STM ()
addUrl crawler url = do
  vis <- readTVar (visited crawler)
  unless (Set.member url vis) $ do
    modifyTVar (visited crawler) (Set.insert url)
    writeTQueue (toVisit crawler) url

crawlPage :: Manager -> CrawlerState -> String -> IO ()
crawlPage manager crawler url = do
  -- Simulate fetching page
  putStrLn $ "Crawling: " ++ url
  threadDelay 100000
  
  -- Mock: find links on page
  let links = [ url ++ "/page1"
              , url ++ "/page2"
              , url ++ "/page3"
              ]
  
  -- Record result
  atomically $ modifyTVar (results crawler) (Map.insert url 200)
  
  -- Add new links (max depth 2)
  let depth = length (filter (=='/') url) - 2  -- simplified
  when (depth < 2) $
    atomically $ mapM_ (addUrl crawler) links

workerThread :: Manager -> CrawlerState -> IO ()
workerThread manager crawler = do
  atomically $ modifyTVar (workers crawler) (+1)
  loop
  atomically $ modifyTVar (workers crawler) (subtract 1)
  where
    loop = do
      mUrl <- atomically $ do
        queue <- tryReadTQueue (toVisit crawler)
        return queue
      case mUrl of
        Nothing  -> return ()
        Just url -> do
          crawlPage manager crawler url
          loop

crawl :: String -> Int -> IO (Map String Int)
crawl seedUrl numWorkers = do
  manager <- newManager defaultManagerSettings
  crawler <- newCrawler
  
  atomically $ addUrl crawler seedUrl
  
  -- Start workers
  workers <- replicateM numWorkers $ async (workerThread manager crawler)
  
  -- Wait for completion
  mapM_ wait workers
  
  readTVarIO (results crawler)

main :: IO ()
main = do
  results <- crawl "http://example.com" 4
  putStrLn $ "Crawled " ++ show (Map.size results) ++ " pages"
  mapM_ (\(url, code) -> putStrLn $ "  " ++ url ++ " -> " ++ show code) 
        (Map.toList results)
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 15 เราจะเรียนเรื่อง **Servant Framework เบื้องต้น**:
- Servant type-safe API
- API definition ด้วย Types
- Handler implementation

---

*[← Part 13](part-13.md) | [Part 15 →](part-15.md)*
