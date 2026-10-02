# Part 25: Performance Engineering ขั้นสูง
## ขั้นตอนที่ 481-500

---

## ขั้นตอนที่ 481: Profiling ด้วย Criterion

```haskell
-- Criterion benchmarking

import Criterion.Main
import Criterion.Types

-- Basic benchmarks
benchmarks :: Benchmark
benchmarks = bgroup "Text processing"
  [ bench "T.pack"     $ nf T.pack testString
  , bench "T.unpack"   $ nf T.unpack testText
  , bench "T.length"   $ nf T.length testText
  , bench "T.toLower"  $ nf T.toLower testText
  , bench "T.words"    $ nf T.words testText
  , bench "T.replace"  $ nf (T.replace "foo" "bar") testText
  ]
  where
    testString = replicate 10000 'a'
    testText   = T.replicate 10000 "hello world "

-- Memory benchmarks ด้วย weigh
import Weigh

weightBenchmarks :: IO ()
weightBenchmarks = mainWith $ do
  func "list 1000"     length ([1..1000] :: [Int])
  func "seq  1000"     Seq.length (Seq.fromList [1..1000])
  func "vector 1000"   V.length (V.fromList ([1..1000] :: [Int]))
  func "map 1000"      Map.size (Map.fromList (zip [1..1000] [1..1000]))
  func "hashmap 1000"  HashMap.size (HashMap.fromList (zip [1..1000] [1..1000]))

-- Benchmark with configuration
main :: IO ()
main = defaultMainWith benchConfig
  [ benchmarks
  ]
  where
    benchConfig = defaultConfig
      { timeLimit = 5.0     -- 5 seconds per benchmark
      , resamples = 1000    -- bootstrap resamples
      , verbosity = Normal
      , csvFile   = Just "results.csv"
      , jsonFile  = Just "results.json"
      }
```

---

## ขั้นตอนที่ 482: Memory Optimization

```haskell
-- Memory optimization techniques

-- 1. Strict fields ลด thunk buildup
data Config = Config
  { configPort :: !Int        -- strict
  , configHost :: !Text       -- strict
  , configDebug :: !Bool      -- strict
  }

-- 2. Unboxed types สำหรับ numeric data
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV

sumVector :: UV.Vector Double -> Double
sumVector = UV.foldl' (+) 0.0  -- unboxed: no GC pressure

-- 3. Text vs String: ใช้ Text เสมอ
processText :: Text -> Text
processText = T.toLower . T.strip  -- efficient

processString :: String -> String  -- ❌ less efficient
processString = map toLower . dropWhile (== ' ')

-- 4. ByteString สำหรับ binary data
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- Lazy ByteString สำหรับ streaming
readLargeFile :: FilePath -> IO BSL.ByteString
readLargeFile = BSL.readFile  -- reads lazily

-- 5. STRef แทน IORef สำหรับ pure computation
import Control.Monad.ST
import Data.STRef

computeInST :: [Int] -> Int
computeInST xs = runST $ do
  total <- newSTRef 0
  forM_ xs $ \x -> modifySTRef' total (+x)
  readSTRef total

-- 6. GHC Memory Layout
-- newtype: zero overhead
newtype Name = Name Text  -- same layout as Text

-- data vs newtype
data Wrapper a = Wrapper a  -- heap allocation
newtype Newtype a = Newtype a  -- NO heap allocation
```

---

## ขั้นตอนที่ 483: CPU Optimization

```haskell
-- CPU optimization

-- 1. SIMD-like operations ด้วย vector
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV

-- BLAS operations (highly optimized)
dotProduct :: SV.Vector Double -> SV.Vector Double -> Double
dotProduct v1 v2 = SV.sum (SV.zipWith (*) v1 v2)

-- 2. Parallel computation
import Control.Parallel.Strategies

parMap' :: (a -> b) -> [a] -> [b]
parMap' f xs = map f xs `using` parList rdeepseq

-- Parallel with chunking
chunkedParMap :: Int -> (a -> b) -> [a] -> [b]
chunkedParMap chunkSize f xs =
  concat (map (map f) chunks) `using` parList rdeepseq
  where chunks = chunksOf chunkSize xs

-- 3. Avoid allocation in hot paths
-- ❌ Allocates per-iteration
sumBad :: [Int] -> Int
sumBad = foldl (+) 0  -- lazy foldl: builds thunks

-- ✓ Strict, no thunks
sumGood :: [Int] -> Int
sumGood = foldl' (+) 0  -- strict foldl'

-- 4. Avoid redundant traversals
-- ❌ Two traversals
processBad :: [Int] -> ([Int], [Int])
processBad xs = (filter even xs, filter odd xs)

-- ✓ Single traversal
processGood :: [Int] -> ([Int], [Int])
processGood xs = partition even xs

-- 5. Memoization
import Data.Map.Strict (Map)
import Control.Monad.State.Strict

fibonacci :: Int -> Integer
fibonacci n = evalState (go n) Map.empty
  where
    go 0 = return 0
    go 1 = return 1
    go k = do
      cache <- get
      case Map.lookup k cache of
        Just v  -> return v
        Nothing -> do
          a <- go (k-1)
          b <- go (k-2)
          let v = a + b
          modify (Map.insert k v)
          return v
```

---

## ขั้นตอนที่ 484: Database Query Optimization

```haskell
-- Database optimization ใน Haskell

-- 1. N+1 query problem
-- ❌ N+1 queries
badLoadPostsWithAuthors :: DB [(Post, User)]
badLoadPostsWithAuthors = do
  posts <- selectList [] [Asc PostId]
  forM posts $ \(Entity pid post) -> do
    author <- get404 (postAuthorId post)  -- 1 query per post!
    return (post, author)

-- ✓ Single JOIN query
goodLoadPostsWithAuthors :: DB [(Entity Post, Entity User)]
goodLoadPostsWithAuthors = E.select $ E.from $ \(post `E.InnerJoin` user) -> do
  E.on (post ^. PostAuthorId E.==. user ^. UserId)
  E.orderBy [E.asc (post ^. PostId)]
  return (post, user)

-- 2. SELECT only needed columns
-- ❌ SELECT *
selectAllColumns :: DB [Entity User]
selectAllColumns = selectList [] []

-- ✓ SELECT specific columns
selectNeededColumns :: DB [(Single Text, Single Text)]
selectNeededColumns = E.select $ E.from $ \u ->
  return (u ^. UserName, u ^. UserEmail)

-- 3. Use indexes
-- Add index in schema:
-- User sql=users
--   email Text
--   Index email_idx email  -- <-- add this

-- 4. Connection pooling
createOptimizedPool :: IO ConnectionPool
createOptimizedPool = runStderrLoggingT $ createPostgresqlPool
  "host=localhost dbname=myapp pool_size=20 connect_timeout=10"
  20  -- max connections

-- 5. Batch operations
-- ❌ N inserts
insertOneByone :: [NewPost] -> DB ()
insertOneByone posts = mapM_ insert posts

-- ✓ Batch insert
insertBatch :: [NewPost] -> DB ()
insertBatch posts = insertMany_ posts

-- 6. Prepared statements
-- Persistent/Esqueleto uses prepared statements automatically

-- 7. EXPLAIN ANALYZE
analyzeQuery :: Text -> DB [(Single Text)]
analyzeQuery sql = rawSql ("EXPLAIN ANALYZE " <> sql) []
```

---

## ขั้นตอนที่ 485: Lazy vs Strict Evaluation

```haskell
-- Understanding laziness for performance

-- Lazy: default in Haskell
-- - Thunks: unevaluated computations
-- - Can cause memory issues (space leaks)

-- Space leak example
sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0
-- foldl (+) 0 [1,2,3] ->
-- foldl (+) (0+1) [2,3] ->
-- foldl (+) ((0+1)+2) [3] ->
-- foldl (+) (((0+1)+2)+3) [] ->
-- evaluates ((0+1)+2)+3 -- entire list in memory!

-- Fix: use strict foldl'
sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0  -- forced evaluation at each step

-- Lazy for efficiency (when we need it)
infiniteList :: [Int]
infiniteList = [1..]  -- no problem! lazy evaluation

firstTen :: [Int]
firstTen = take 10 infiniteList  -- only evaluates 10 elements

-- Short-circuit evaluation
anyOdd :: [Int] -> Bool
anyOdd = any odd  -- stops at first odd number

-- Data.List.Lazy vs Data.List.Strict
import qualified Data.List as L    -- lazy
import qualified Data.List.Strict as LS  -- strict

-- Choose based on use case
processData :: [Int] -> Int
processData = LS.foldl' (+) 0  -- strict for aggregation

generateData :: Int -> [Int]
generateData n = L.unfoldr (\i -> if i > n then Nothing else Just (i, i+1)) 0  -- lazy generation

-- Force evaluation
import Control.DeepSeq

-- deepseq: force all nested thunks
forceAll :: NFData a => a -> a
forceAll x = x `deepseq` x

-- seq: force to WHNF (Weak Head Normal Form)
forceHead :: a -> b -> b
forceHead x y = x `seq` y

-- BangPatterns: strict binding
{-# LANGUAGE BangPatterns #-}
strictLoop :: [Int] -> Int
strictLoop xs = go 0 xs
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs
```

---

## ขั้นตอนที่ 486: GHC Optimization Flags

```haskell
-- GHC optimization flags

-- cabal.project / package.yaml:
-- ghc-options:
--   - -O2              # optimization level 2
--   - -fllvm           # LLVM backend (faster code)
--   - -funfolding-use-threshold=40
--   - -fspecialise-aggressively
--   - -fexpose-all-unfoldings

-- Pragmas per-file:
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -fno-full-laziness #-}  -- prevent floating bindings out

-- Profiling flags (for profiling builds only):
-- -prof -fprof-auto -rtsopts

-- LLVM backend optimization
-- Install: llvm-12
-- Enable: -fllvm
-- Benefits: better SIMD, better code gen for numeric

-- Profile-Guided Optimization (PGO)
-- 1. Build with -fprofile-generate
-- 2. Run on representative input
-- 3. Build again with -fprofile-use

-- Optimization checklist:
-- 1. Enable -O2
-- 2. Use strict data types
-- 3. Use unboxed types for numbers
-- 4. Avoid String, use Text/ByteString
-- 5. Use vector instead of list for numeric arrays
-- 6. Use HashMap instead of Map for string keys
-- 7. Use sequence instead of list for queue operations
-- 8. Measure before optimizing!
```

---

## ขั้นตอนที่ 487: Stream Processing Optimization

```haskell
-- Optimized stream processing

import Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Combinators as CC

-- Efficient batch processing
batchProcess :: Monad m => Int -> ([a] -> m [b]) -> ConduitT a b m ()
batchProcess batchSize f = loop []
  where
    loop batch = do
      mx <- await
      case mx of
        Nothing -> unless (null batch) $ do
          results <- lift (f (reverse batch))
          mapM_ yield results
        Just x  -> do
          let newBatch = x : batch
          if length newBatch >= batchSize
            then do
              results <- lift (f (reverse newBatch))
              mapM_ yield results
              loop []
            else loop newBatch

-- Parallel processing in pipeline
parallelMap :: (NFData b, MonadUnliftIO m) => Int -> (a -> m b) -> ConduitT a b m ()
parallelMap n f = do
  xs <- CL.take n
  if null xs
    then return ()
    else do
      ys <- lift $ mapConcurrently f xs
      mapM_ yield ys
      parallelMap n f

-- Efficient CSV processing
import Data.Csv.Streaming

processLargeCsv :: FilePath -> FilePath -> IO ()
processLargeCsv inFile outFile = do
  inputBs <- BS.readFile inFile
  let records = decode HasHeader inputBs :: Records (Text, Int, Double)
  
  withFile outFile WriteMode $ \h -> do
    hPutStr h "processed_name,age,score\n"
    mapM_ (processRecord h) records
  where
    processRecord h (Left err)  = putStrLn $ "Error: " ++ err
    processRecord h (Right row) = do
      let processed = transformRow row
      hPutStr h (encodeRow processed)

-- Memory-efficient JSON processing
import Data.Aeson.Streaming

processLargeJson :: FilePath -> IO ()
processLargeJson path = runConduitRes $
  sourceFile path
  .| parseJsonEvents
  .| processEvents
  .| sinkNull
```

---

## ขั้นตอนที่ 488: Concurrency Performance

```haskell
-- High-performance concurrency

import Control.Concurrent.Chan.Unagi

-- Unagi channels: faster than regular Chan
createHighPerfQueue :: IO (InChan a, OutChan a)
createHighPerfQueue = newChan

-- Worker pool with work stealing
data WorkPool = WorkPool
  { wpWorkers :: [ThreadId]
  , wpQueues  :: Vector (InChan (), OutChan ())
  , wpTask    :: IORef (Maybe (IO ()))
  }

-- Lock-free data structures
import Data.IORef.Unboxed

-- Atomic counter without STM
data Counter = Counter (IORefU Int)

newCounter :: IO Counter
newCounter = Counter <$> newIORefU 0

increment :: Counter -> IO Int
increment (Counter ref) = atomicAddIORefU ref 1

-- Thread-local storage
import Control.Concurrent
import qualified Data.Map.Strict as Map

newtype ThreadLocal a = ThreadLocal (IORef (Map ThreadId a))

newThreadLocal :: IO (ThreadLocal a)
newThreadLocal = ThreadLocal <$> newIORef Map.empty

getThreadLocal :: ThreadLocal a -> IO (Maybe a)
getThreadLocal (ThreadLocal ref) = do
  tid <- myThreadId
  fmap (Map.lookup tid) (readIORef ref)

setThreadLocal :: ThreadLocal a -> a -> IO ()
setThreadLocal (ThreadLocal ref) val = do
  tid <- myThreadId
  modifyIORef' ref (Map.insert tid val)

-- Connection pool with health check
data Pool a = Pool
  { poolConnections :: TQueue a
  , poolSize        :: Int
  , poolCheck       :: a -> IO Bool  -- health check
  , poolCreate      :: IO a
  , poolDestroy     :: a -> IO ()
  }

withPoolConn :: Pool a -> (a -> IO b) -> IO b
withPoolConn pool action = do
  conn <- getHealthyConn pool
  finally (action conn) (returnConn pool conn)
  where
    getHealthyConn pool = do
      conn <- atomically (readTQueue (poolConnections pool))
      ok   <- poolCheck pool conn
      if ok
        then return conn
        else do
          poolDestroy pool conn
          newConn <- poolCreate pool
          return newConn
    
    returnConn pool conn = atomically $
      writeTQueue (poolConnections pool) conn
```

---

## ขั้นตอนที่ 489: Network Performance

```haskell
-- High-performance HTTP client

import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Connection pooling ใน HTTP client
createOptimizedManager :: IO Manager
createOptimizedManager = newManager tlsManagerSettings
  { managerConnCount           = 100  -- max connections
  , managerIdleConnectionCount = 50   -- idle connections to keep
  , managerResponseTimeout     = responseTimeoutMicro 30000000  -- 30s
  , managerRawConnection       = Nothing
  }

-- Concurrent HTTP requests
fetchConcurrently :: Manager -> [String] -> IO [Either HttpException Response]
fetchConcurrently manager urls = do
  forConcurrently urls $ \url -> do
    E.try $ httpLbs (parseRequest_ url) manager

-- Keep-alive connections
keepAliveRequest :: Manager -> Request -> IO Response
keepAliveRequest manager req = do
  let reqWithKeepalive = req
        { requestHeaders = ("Connection", "keep-alive") : requestHeaders req }
  httpLbs reqWithKeepalive manager

-- HTTP/2 multiplexing (with http2)
import Network.HTTP2.Client

http2Request :: String -> IO ByteString
http2Request url = do
  let req = requestFromUrl url
  withHTTP2Client "api.example.com" 443 $ \conn -> do
    resp <- sendRequest conn req
    responseBody resp

-- Server-sent events client
consumeSse :: String -> (Text -> IO ()) -> IO ()
consumeSse url handler = do
  manager <- createOptimizedManager
  req     <- parseRequest url
  let req' = req { requestHeaders = ("Accept", "text/event-stream") : requestHeaders req }
  withResponse req' manager $ \resp -> do
    let body = responseBody resp
    runConduit $ bodyReader body .| sseParser .| mapM_C handler
```

---

## ขั้นตอนที่ 490: WASM (WebAssembly)

```haskell
-- Compile Haskell to WebAssembly ด้วย GHC WASM backend

-- GHC 9.6+ supports WASM target
-- Setup: ghcup install wasm32-wasi-ghc

-- example.hs
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

-- Export function to JavaScript
foreign export ccall "add" add :: CInt -> CInt -> CInt

add :: CInt -> CInt -> CInt
add x y = x + y

-- Export more complex types using GHCJS-style FFI
foreign export ccall "processData" processData :: Ptr CChar -> CInt -> IO (Ptr CChar)

processData :: Ptr CChar -> CInt -> IO (Ptr CChar)
processData input len = do
  bs <- BS.packCStringLen (input, fromIntegral len)
  let result = BS.map (+1) bs  -- process
  BS.useAsCStringLen result $ \(ptr, _) -> return ptr

-- Build: cabal build --ghc-options="-no-hs-main -optl --no-entry"

-- JavaScript interop
-- const wasmModule = await WebAssembly.instantiate(wasmBuffer, imports);
-- const result = wasmModule.instance.exports.add(3, 4);
-- console.log(result);  // 7
```

---

## ขั้นตอนที่ 491: High-Performance JSON

```haskell
-- Fast JSON processing

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key

-- เร็วที่สุด: สร้าง ToJSON instance ด้วย Template Haskell
data User = User
  { userId   :: Int
  , userName :: Text
  , userAge  :: Int
  } deriving (Generic)

instance ToJSON User where
  -- Default Generic-derived instance (fast)
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop 4 }  -- remove "user" prefix

-- Faster: manual instance (avoid overhead)
instance ToJSON UserFast where
  toJSON u = object
    [ "id"   .= userId u
    , "name" .= userName u
    , "age"  .= userAge u
    ]
  
  -- Even faster: toEncoding
  toEncoding u = pairs
    $ "id"   .= userId u
    <> "name" .= userName u
    <> "age"  .= userAge u

-- json-stream: streaming JSON parser
import Data.Aeson.Stream

parseStream :: BSL.ByteString -> [User]
parseStream bs = parseJsonArray parseUser bs

-- Fast bulk encoding
encodeUsers :: [User] -> BSL.ByteString
encodeUsers = encode  -- Array encoding is efficient

-- Partial parsing (ignore unknown fields)
instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .:  "id"
      <*> v .:  "name"
      <*> v .:? "age" .!= 0  -- optional with default
```

---

## ขั้นตอนที่ 492: Caching Strategy

```haskell
-- Multi-level caching strategy

-- Cache hierarchy:
-- L1: In-process (IORef/TVar) - fastest, limited size
-- L2: Local Redis - fast, larger
-- L3: Shared Redis Cluster - slower, distributed
-- L4: Database - slowest, source of truth

data CacheLevel = L1 | L2 | L3 | L4

data CacheConfig = CacheConfig
  { l1MaxEntries :: Int
  , l1Ttl        :: Int  -- seconds
  , l2Ttl        :: Int
  , l3Ttl        :: Int
  }

class Cache m where
  get :: Text -> m (Maybe (ByteString, UTCTime))
  set :: Text -> ByteString -> Int -> m ()
  del :: Text -> m ()

-- Write-through cache
writeThrough :: (Cache m1, Cache m2) => m1 -> m2 -> Text -> ByteString -> Int -> IO ()
writeThrough cache1 cache2 key val ttl = do
  set cache1 key val ttl
  set cache2 key val ttl

-- Read-through cache
readThrough :: (Cache m, Monad m) => m -> Text -> IO ByteString -> m ByteString
readThrough cache key fallback = do
  mVal <- get cache key
  case mVal of
    Just (v, _) -> return v
    Nothing     -> do
      v <- liftIO fallback
      set cache key v 300  -- default 5min TTL
      return v

-- Cache aside pattern (most common)
cacheAside
  :: (Cache m, Monad m)
  => m               -- cache
  -> Text            -- key
  -> IO a            -- fetch from source
  -> (a -> ByteString)  -- serialize
  -> (ByteString -> Maybe a)  -- deserialize
  -> m a
cacheAside cache key fetch serialize deserialize = do
  mCached <- get cache key
  case mCached >>= deserialize . fst of
    Just v  -> return v
    Nothing -> do
      v <- liftIO fetch
      set cache key (serialize v) 300
      return v
```

---

## ขั้นตอนที่ 493: Bulk Operations

```haskell
-- Bulk database operations

-- Bulk insert
bulkInsert :: [NewUser] -> DB [UserId]
bulkInsert users = do
  -- PostgreSQL COPY for maximum speed
  conn <- ask
  let rows = map userToTuple users
  liftIO $ executeMany conn
    "INSERT INTO users (name, email, age) VALUES (?,?,?) RETURNING id"
    rows

-- Bulk update
bulkUpdate :: [(UserId, UserUpdate)] -> DB ()
bulkUpdate updates = do
  let updateBatch = map (\(uid, upd) -> 
        (updateField upd, unUserId uid)) updates
  executeMany conn
    "UPDATE users SET name = ?, age = ? WHERE id = ?"
    updateBatch

-- COPY for maximum insert speed
bulkCopy :: Connection -> [User] -> IO ()
bulkCopy conn users = do
  let header = "COPY users (name, email, age) FROM STDIN WITH CSV"
  let rows   = BS.intercalate "\n" (map toCsvRow users)
  
  execute_ conn header
  copyData conn rows
  putCopyEnd conn

-- Batch delete
bulkDelete :: [UserId] -> DB ()
bulkDelete uids = do
  let ids = map unUserId uids
  executeQuery
    "DELETE FROM users WHERE id = ANY(?)"
    (Only (PGArray ids))

-- Paginated bulk processing
processBulk :: Int -> (Int -> Int -> DB [a]) -> (a -> IO ()) -> IO ()
processBulk pageSize fetch process = go 0
  where
    go offset = do
      rows <- runDB (fetch offset pageSize)
      unless (null rows) $ do
        mapM_ process rows
        go (offset + pageSize)
```

---

## ขั้นตอนที่ 494: Hot Path Optimization

```haskell
-- Optimize hot paths (code that runs millions of times)

-- 1. Avoid allocation in hot loops
-- ❌ Allocates per iteration
badHotPath :: [Int] -> [Int]
badHotPath = map (\x -> x * 2 + 1)  -- creates new list

-- ✓ In-place with vector
goodHotPath :: U.Vector Int -> U.Vector Int
goodHotPath = U.map (\x -> x * 2 + 1)  -- efficient

-- 2. Use ByteString operations for I/O
-- ❌
slowRead :: FilePath -> IO String
slowRead = readFile  -- String is [Char], very slow

-- ✓
fastRead :: FilePath -> IO Text
fastRead = T.readFile  -- Text is efficient

-- 3. Short-circuit expensive computations
data CacheResult = CacheHit ByteString | CacheMiss

lookupWithShortCircuit :: [Cache] -> Text -> IO CacheResult
lookupWithShortCircuit [] _ = return CacheMiss
lookupWithShortCircuit (c:cs) key = do
  mVal <- cacheGet c key
  case mVal of
    Just v  -> return (CacheHit v)  -- short-circuit!
    Nothing -> lookupWithShortCircuit cs key

-- 4. Compile-time computation with TH
import Language.Haskell.TH

-- Pre-compute regular expressions at compile time
$(deriveRegex "emailRegex" "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")

-- 5. Strict data for hot-path data structures
data HotData = HotData
  { hdCounter  :: !Int         -- strict, unboxed
  , hdTimestamp :: !Int64      -- strict, unboxed
  , hdKey       :: !ByteString -- strict
  }

-- 6. Worker-wrapper transformation
-- GHC does this automatically with -O2, but can help manually
sumWorker :: Int -> [Int] -> Int
sumWorker !acc []     = acc
sumWorker !acc (x:xs) = sumWorker (acc + x) xs

sumWrapper :: [Int] -> Int
sumWrapper = sumWorker 0
```

---

## ขั้นตอนที่ 495: Load Balancer Implementation

```haskell
-- Simple load balancer ใน Haskell

data Backend = Backend
  { bHost    :: Text
  , bPort    :: Int
  , bWeight  :: Int
  , bActive  :: TVar Bool
  , bConns   :: TVar Int
  }

data LoadBalancer = LoadBalancer
  { lbBackends :: [Backend]
  , lbStrategy :: LoadBalancingStrategy
  , lbCounter  :: TVar Int
  }

data LoadBalancingStrategy
  = RoundRobinStrategy
  | LeastConnectionsStrategy
  | WeightedRoundRobinStrategy

-- Select backend
selectBackend :: LoadBalancer -> IO (Maybe Backend)
selectBackend lb = do
  active <- filterM (\b -> readTVarIO (bActive b)) (lbBackends lb)
  case active of
    [] -> return Nothing
    _  -> Just <$> case lbStrategy lb of
      RoundRobinStrategy -> do
        idx <- atomically $ do
          n <- readTVar (lbCounter lb)
          writeTVar (lbCounter lb) ((n + 1) `mod` length active)
          return n
        return (active !! (idx `mod` length active))
      
      LeastConnectionsStrategy -> do
        conns <- mapM (\b -> (b,) <$> readTVarIO (bConns b)) active
        return (fst (minimumBy (compare `on` snd) conns))
      
      WeightedRoundRobinStrategy -> do
        -- Weighted selection
        let total  = sum (map bWeight active)
        r <- randomRIO (0, total - 1)
        return (weightedSelect r active)

-- Proxy request to backend
proxyRequest :: LoadBalancer -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
proxyRequest lb req respond = do
  mBackend <- selectBackend lb
  case mBackend of
    Nothing -> respond (responseLBS status503 [] "No backends available")
    Just b  -> do
      atomically $ modifyTVar (bConns b) (+1)
      let url = "http://" <> bHost b <> ":" <> T.pack (show (bPort b)) <> rawPathInfo req
      result <- E.try $ do
        upstreamReq <- parseRequest (T.unpack url)
        manager     <- newManager defaultManagerSettings
        httpLbs upstreamReq { requestBody = requestBody req
                            , requestHeaders = requestHeaders req } manager
      atomically $ modifyTVar (bConns b) (subtract 1)
      case result of
        Left (e :: SomeException) -> do
          atomically $ writeTVar (bActive b) False
          respond (responseLBS status502 [] "Bad Gateway")
        Right upstreamResp ->
          respond (responseLBS
            (responseStatus upstreamResp)
            (Map.toList (responseHeaders upstreamResp))
            (responseBody upstreamResp))
```

---

## ขั้นตอนที่ 496: Time Series Data

```haskell
-- Time series data processing

import Data.Time.Series

data DataPoint = DataPoint
  { dpTimestamp :: UTCTime
  , dpValue     :: Double
  , dpTags      :: Map Text Text
  }

-- Moving average
movingAverage :: Int -> [DataPoint] -> [DataPoint]
movingAverage window points = zipWith toPoint
  windows
  (drop (window-1) points)
  where
    windows = zipWith (\xs p -> (xs, p))
      (map (take window) (tails points))
      (drop (window-1) points)
    toPoint (window, latest) _orig =
      latest { dpValue = avg (map dpValue window) }
    avg xs = sum xs / fromIntegral (length xs)

-- Aggregation
groupByHour :: [DataPoint] -> Map UTCTime [DataPoint]
groupByHour = Map.fromListWith (++) . map (\dp ->
  (truncateToHour (dpTimestamp dp), [dp]))
  where
    truncateToHour t = 
      let (UTCTime day time) = t
          secs = floor (realToFrac time / 3600) * 3600
      in UTCTime day (secondsToDiffTime secs)

-- Store time series in PostgreSQL with TimescaleDB extension
createTimeseriesTable :: DB ()
createTimeseriesTable = do
  rawExecute "CREATE TABLE IF NOT EXISTS metrics (\
    \time TIMESTAMPTZ NOT NULL,\
    \name TEXT NOT NULL,\
    \value DOUBLE PRECISION,\
    \tags JSONB\
    \)" []
  rawExecute "SELECT create_hypertable('metrics', 'time')" []

insertMetric :: DataPoint -> DB ()
insertMetric dp = rawExecute
  "INSERT INTO metrics (time, name, value, tags) VALUES (?,?,?,?)"
  [ PersistUTCTime (dpTimestamp dp)
  , PersistText "metric"
  , PersistDouble (dpValue dp)
  , PersistByteString (encode (dpTags dp))
  ]

-- Query with time window
queryMetrics :: UTCTime -> UTCTime -> Text -> DB [DataPoint]
queryMetrics start end name = do
  rows <- rawSql
    "SELECT time, value, tags FROM metrics WHERE time >= ? AND time < ? AND name = ? ORDER BY time"
    [ PersistUTCTime start
    , PersistUTCTime end
    , PersistText name
    ]
  return (map rowToPoint rows)
```

---

## ขั้นตอนที่ 497: Graph Processing

```haskell
-- Graph processing ใน Haskell

import Data.Graph
import Data.Array

-- Build graph from edges
buildGraph :: [(Int, Int)] -> Graph
buildGraph edges =
  let maxNode = maximum (concatMap (\(a,b) -> [a,b]) edges)
  in buildG (0, maxNode) edges

-- BFS
bfs :: Graph -> Vertex -> [Vertex]
bfs graph start = go [start] (S.singleton start)
  where
    go []     _    = []
    go (v:vs) seen =
      let neighbors = filter (`S.notMember` seen) (graph ! v)
          newSeen   = foldl' (flip S.insert) seen neighbors
      in v : go (vs ++ neighbors) newSeen

-- DFS
dfs :: Graph -> Vertex -> [Vertex]
dfs graph = map vertex . reachable graph

-- Shortest path (BFS)
shortestPath :: Graph -> Vertex -> Vertex -> Maybe [Vertex]
shortestPath graph start end = go [(start, [start])] S.empty
  where
    go []             _    = Nothing
    go ((v, path):vs) seen
      | v == end   = Just (reverse path)
      | v `S.member` seen = go vs seen
      | otherwise  = 
          let neighbors = graph ! v
              newPaths  = map (\n -> (n, n:path)) neighbors
              newSeen   = S.insert v seen
          in go (vs ++ newPaths) newSeen

-- Social graph
data SocialGraph = SocialGraph
  { sgFollowing :: Map UserId [UserId]
  , sgFollowers :: Map UserId [UserId]
  }

getRecommendations :: SocialGraph -> UserId -> [UserId]
getRecommendations sg uid =
  let myFollowing    = fromMaybe [] (Map.lookup uid (sgFollowing sg))
      friendsFollowing = concatMap (\f -> fromMaybe [] (Map.lookup f (sgFollowing sg))) myFollowing
      notFollowing   = filter (`notElem` myFollowing) friendsFollowing
      notSelf        = filter (/= uid) notFollowing
  in take 10 . sortBy (comparing frequency) . nub $ notSelf
  where
    frequency u = length (filter (== u) friendsFollowing)
```

---

## ขั้นตอนที่ 498: Machine Learning Pipeline

```haskell
-- Simple ML pipeline ใน Haskell

import Numeric.LinearAlgebra

-- Feature engineering
type Features = Matrix Double
type Labels   = Vector Double

-- Normalize features
normalize :: Features -> Features
normalize matrix =
  let means  = row (cmap mean (toColumns matrix))
      stds   = row (cmap std  (toColumns matrix))
  in (matrix - means) / stds

-- Train/Test split
splitData :: Double -> Features -> Labels -> (Features, Labels, Features, Labels)
splitData ratio features labels =
  let n = rows features
      trainN = round (fromIntegral n * ratio)
      trainX = subMatrix (0,0) (trainN, cols features) features
      testX  = subMatrix (trainN, 0) (n-trainN, cols features) features
      trainY = subVector 0 trainN labels
      testY  = subVector trainN (n-trainN) labels
  in (trainX, trainY, testX, testY)

-- Linear regression
linearRegression :: Features -> Labels -> Vector Double
linearRegression x y =
  let xt = tr x
      w  = (xt <> x) <\> (xt #> y)
  in w

-- Prediction
predict :: Vector Double -> Features -> Labels
predict weights features = features #> weights

-- Evaluate
rmse :: Labels -> Labels -> Double
rmse predicted actual =
  sqrt (mean (cmap (^2) (predicted - actual)))

-- Example pipeline
trainModel :: FilePath -> IO (Vector Double)
trainModel csvPath = do
  (features, labels) <- loadData csvPath
  let normalized = normalize features
      (trainX, trainY, testX, testY) = splitData 0.8 normalized labels
      weights = linearRegression trainX trainY
      predictions = predict weights testX
      error = rmse predictions testY
  putStrLn $ "RMSE: " ++ show error
  return weights
```

---

## ขั้นตอนที่ 499: Production Monitoring Dashboard

```haskell
-- Real-time monitoring ใน Yesod

data Metrics = Metrics
  { mRequests     :: Counter
  , mErrors       :: Counter
  , mLatencyHist  :: Histogram
  , mActiveConns  :: Gauge
  , mQueueSize    :: Gauge
  , mDbPoolUsed   :: Gauge
  , mCacheHitRate :: Gauge
  }

-- Dashboard handler
getDashboardR :: Handler Html
getDashboardR = do
  requireAdmin
  
  metrics <- appMetrics <$> getYesod
  
  -- Get current values
  reqCount   <- liftIO $ getCounterValue (mRequests metrics)
  errCount   <- liftIO $ getCounterValue (mErrors metrics)
  activeConns <- liftIO $ getGaugeValue (mActiveConns metrics)
  queueSize   <- liftIO $ getGaugeValue (mQueueSize metrics)
  
  defaultLayout $ do
    setTitle "System Dashboard"
    addScript (StaticR js_chart_min_js)
    [whamlet|
      <h1 .text-3xl .font-bold .mb-6>System Dashboard
      
      <div .grid .grid-cols-4 .gap-4 .mb-8>
        <div .stat-card>
          <h3>Total Requests
          <p .stat-value>#{reqCount}
        <div .stat-card>
          <h3>Error Rate
          <p .stat-value>#{errorRate reqCount errCount}%
        <div .stat-card>
          <h3>Active Connections
          <p .stat-value>#{activeConns}
        <div .stat-card>
          <h3>Queue Size
          <p .stat-value>#{queueSize}
      
      <div .grid .grid-cols-2 .gap-6>
        <div .chart-container>
          <h2>Request Rate (Last 24h)
          <canvas id=requestChart>
        <div .chart-container>
          <h2>Response Time Distribution
          <canvas id=latencyChart>
      
      <script>
        // Fetch metrics and draw charts
        setInterval(function() {
          fetch('@{ApiMetricsR}')
            .then(r => r.json())
            .then(data => updateCharts(data));
        }, 5000);
    |]
  where
    errorRate total errors
      | total == 0 = 0
      | otherwise  = round (fromIntegral errors / fromIntegral total * 100 :: Double) :: Int

-- Real-time metrics API
getApiMetricsR :: Handler Value
getApiMetricsR = do
  metrics <- appMetrics <$> getYesod
  reqHistory <- getMetricsHistory
  return $ object
    [ "requests"   .= reqHistory
    , "errors"     .= ([] :: [Int])
    , "latency_p50" .= (50 :: Int)
    , "latency_p99" .= (100 :: Int)
    ]
```

---

## ขั้นตอนที่ 500: โปรเจกต์สรุป: World-Class Haskell Application

```haskell
-- Summary: สิ่งที่ได้เรียนรู้และ best practices

-- Architecture principles สำหรับ production Haskell:

-- 1. Type safety everywhere
newtype SafeId a = SafeId Int64  -- prevent ID mixups
newtype EmailAddress = EmailAddress Text  -- validated at creation

-- 2. Effect tracking
type App = ReaderT AppEnv (ExceptT AppError IO)

-- 3. Explicit error handling
data AppError
  = NotFound Resource
  | Unauthorized User Resource
  | ValidationError [FieldError]
  | DatabaseError SomeException
  | ExternalServiceError ServiceName SomeException
  deriving Show

-- 4. Resource safety
withDatabase :: (ConnectionPool -> IO a) -> App a
withDatabase f = do
  pool <- asks envPool
  liftIO (f pool)

-- 5. Structured concurrency
withWorkerPool :: Int -> (WorkerPool -> IO a) -> IO a
withWorkerPool n action = bracket (createPool n) destroyPool action

-- 6. Observability
data AppTrace = AppTrace
  { atTraceId  :: TraceId
  , atSpans    :: [Span]
  , atMetrics  :: Map Text Double
  , atLogs     :: [LogEntry]
  }

-- 7. Configuration management
data Config = Config
  { cfgDatabase    :: DatabaseConfig
  , cfgCache       :: CacheConfig
  , cfgAuth        :: AuthConfig
  , cfgObservability :: ObservabilityConfig
  } deriving (Generic, FromJSON)

-- 8. Clean shutdown
main :: IO ()
main = do
  config <- loadConfig
  resources <- acquireResources config
  
  let shutdown = releaseResources resources
  
  installHandler sigTERM (Catch shutdown) Nothing
  installHandler sigINT  (Catch shutdown) Nothing
  
  race_
    (runServer config resources)
    (waitForShutdown)

-- Summary of what we've covered:
-- ✓ Haskell basics (types, functions, typeclasses)
-- ✓ Data structures (Map, Set, Vector, Sequence)
-- ✓ Error handling (Maybe, Either, ExceptT)
-- ✓ IO Monad and concurrency
-- ✓ Monad transformers
-- ✓ Lazy/strict evaluation
-- ✓ Advanced types (GADTs, DataKinds, TypeFamilies)
-- ✓ Lenses and Optics
-- ✓ Servant framework (REST APIs)
-- ✓ Database (Persistent, Esqueleto, hasql)
-- ✓ Yesod framework (web applications)
-- ✓ Testing (HSpec, QuickCheck)
-- ✓ Deployment (Docker, CI/CD)
-- ✓ Free Monads and Effect Systems
-- ✓ Category Theory patterns
-- ✓ Clean Architecture and DDD
-- ✓ Distributed systems
-- ✓ Security engineering
-- ✓ Performance optimization
-- ✓ Production monitoring
```

---

## ยินดีด้วย! จบหลักสูตร 500 ขั้นตอน

คุณได้เรียนรู้ Haskell จากระดับพื้นฐานจนถึงระดับมืออาชีพและระดับโลก:

1. **Part 1-7** (Steps 1-140): Haskell Core - Types, Functions, Data Structures
2. **Part 8-13** (Steps 141-260): Advanced Haskell - Error Handling, Monads, Lenses
3. **Part 14-17** (Steps 261-340): Servant Framework + Database
4. **Part 18-20** (Steps 341-400): Yesod Framework + Testing + Deployment
5. **Part 21-22** (Steps 401-440): Advanced Patterns + Architecture
6. **Part 23-24** (Steps 441-480): Distributed Systems + Security
7. **Part 25** (Steps 481-500): Performance Engineering + Production

---

*[← Part 24](part-24.md)*
