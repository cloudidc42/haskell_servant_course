# Part 44: Performance Engineering
## ขั้นตอนที่ 861-880

---

## ขั้นตอนที่ 861: GHC Profiling

```haskell
-- GHC profiling และ performance analysis

-- ใช้ cabal run with profiling:
-- cabal run --enable-profiling -- +RTS -p -s -RTS

-- Cost center annotations
{-# SCC "hotFunction" #-}
hotFunction :: [Int] -> Int
hotFunction xs = foldl' (+) 0 xs

-- Space profiling
-- cabal run -- +RTS -hc -RTS (heap by cost center)
-- cabal run -- +RTS -hd -RTS (heap by closure description)

-- Retainer profiling
-- cabal run -- +RTS -hr -RTS

-- สร้าง flamegraph
-- stack exec -- +RTS -prof -fprof-auto -fprof-cafs -RTS
-- ghc-prof-flamegraph program.prof > flamegraph.html

-- Core inspection (intermediate representation)
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
{-# OPTIONS_GHC -dsuppress-all -ddump-simpl-stats #-}

-- Check for sharing vs. duplication in core
expensive :: Int -> Int -> Int
expensive x y = let common = x * x + y * y  -- should be shared
                in common + common

-- Strictness annotations for performance
data StrictPair a b = StrictPair !a !b

-- Unboxed types
{-# UNPACK #-} 
data PointU = PointU {-# UNPACK #-} !Double {-# UNPACK #-} !Double
```

---

## ขั้นตอนที่ 862: Memory Layout Optimization

```haskell
-- Memory layout optimization

-- Sum type layout (tagged union)
-- Normal: 2-4 words overhead per value
data Shape
  = Circle   !Double
  | Rectangle !Double !Double
  | Triangle  !Double !Double !Double

-- Avoid boxing with newtype
newtype Radius = Radius Double  -- same representation as Double at runtime

-- Strict record fields
data Point = Point
  { x :: !Double  -- strict, may be unboxed
  , y :: !Double
  } deriving (Show)

-- Unboxed vectors for numeric work
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-- Boxed (general): each element is a pointer to heap
boxedVector :: V.Vector Int
boxedVector = V.fromList [1..1000]

-- Unboxed (specialized): elements stored directly
unboxedVector :: VU.Vector Double
unboxedVector = VU.fromList [1.0..1000.0]

-- Storable vector (C-compatible layout)
import qualified Data.Vector.Storable as VS
storableVector :: VS.Vector Float
storableVector = VS.fromList [1.0..1000.0]

-- ByteString (unboxed Word8)
import qualified Data.ByteString as BS
rawBytes :: BS.ByteString
rawBytes = BS.pack [1..255]

-- Measure actual memory usage
{-# NOINLINE measureHeap #-}
measureHeap :: a -> IO ()
measureHeap x = do
  performGC
  before <- getGCStatistics
  evaluate x
  performGC
  after <- getGCStatistics
  let used = currentBytesUsed after - currentBytesUsed before
  putStrLn ("Heap used: " ++ show used ++ " bytes")
```

---

## ขั้นตอนที่ 863: Fusion and Stream Optimization

```haskell
-- Stream fusion optimization

import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as MS

-- Vector fusion: map/filter/fold fuses into single pass
efficientPipeline :: V.Vector Int -> Int
efficientPipeline = V.foldl' (+) 0 . V.filter even . V.map (*2)
-- GHC fuses this into: foldl' (\acc x -> if even (x*2) then acc + x*2 else acc) 0 v

-- Manual stream processing for guaranteed fusion
processStream :: [Int] -> Int
processStream = foldl' go 0
  where
    go acc x
      | even x    = acc + x * 2
      | otherwise = acc

-- Conduit fusion (streaming)
import Conduit

streamProcess :: ConduitT () Void IO Int
streamProcess = sourceList [1..1000000]
  .| mapC (*2)
  .| filterC even
  .| foldlC (+) 0

-- Text/ByteString builder (avoid quadratic concatenation)
import Data.Text.Lazy.Builder as TB

buildText :: [Text] -> Text
buildText parts = TL.toStrict . TB.toLazyText $
  mconcat [TB.fromText t <> TB.fromText ", " | t <- parts]

-- ByteString builder
import Data.ByteString.Builder as BB

buildBytes :: [Int] -> BS.ByteString
buildBytes ints = BL.toStrict . BB.toLazyByteString $
  mconcat [BB.int64BE (fromIntegral i) | i <- ints]
```

---

## ขั้นตอนที่ 864: Cache Optimization

```haskell
-- Cache-friendly data layout

-- Array of Structures (AoS) - cache unfriendly for single field access
data ParticleAoS = ParticleAoS
  { posX  :: Double
  , posY  :: Double
  , velX  :: Double
  , velY  :: Double
  , mass  :: Double
  }

-- Structure of Arrays (SoA) - cache friendly
data ParticlesSoA = ParticlesSoA
  { posXs :: VU.Vector Double
  , posYs :: VU.Vector Double
  , velXs :: VU.Vector Double
  , velYs :: VU.Vector Double
  , masses :: VU.Vector Double
  }

-- Update all X positions (SoA is much faster)
updatePositions :: ParticlesSoA -> Double -> ParticlesSoA
updatePositions p dt = p
  { posXs = VU.zipWith (\px vx -> px + vx * dt) (posXs p) (velXs p)
  , posYs = VU.zipWith (\py vy -> py + vy * dt) (posYs p) (velYs p)
  }

-- Tiling for cache efficiency
tiledMatMul :: VU.Vector Double -> VU.Vector Double -> Int -> VU.Vector Double
tiledMatMul a b n = VU.create $ do
  c <- VUM.new (n * n)
  let tileSize = 64
  forM_ [0, tileSize..n-1] $ \ii ->
    forM_ [0, tileSize..n-1] $ \jj ->
      forM_ [0, tileSize..n-1] $ \kk -> do
        let iEnd = min n (ii + tileSize)
        let jEnd = min n (jj + tileSize)
        let kEnd = min n (kk + tileSize)
        forM_ [ii..iEnd-1] $ \i ->
          forM_ [jj..jEnd-1] $ \j -> do
            s <- newIORef 0
            forM_ [kk..kEnd-1] $ \k ->
              modifyIORef' s (+ a VU.! (i*n+k) * b VU.! (k*n+j))
            v <- readIORef s
            current <- VUM.read c (i*n+j)
            VUM.write c (i*n+j) (current + v)
  return c
```

---

## ขั้นตอนที่ 865: Avoiding Common Performance Pitfalls

```haskell
-- Common performance pitfalls and fixes

-- Pitfall 1: Lazy evaluation causing space leaks
-- BAD: accumulates thunks
badSum :: [Int] -> Int
badSum = foldl (+) 0  -- lazy, O(n) space

-- GOOD: strict accumulation
goodSum :: [Int] -> Int
goodSum = foldl' (+) 0  -- strict, O(1) space

-- Pitfall 2: String processing with String ([Char])
-- BAD: O(n) for length, O(n) for concat
badConcat :: [String] -> String
badConcat = concat  -- quadratic in total length

-- GOOD: use Text or Builder
goodConcat :: [T.Text] -> T.Text
goodConcat = T.concat  -- O(total length), one allocation

-- Pitfall 3: Map.insertWith (++) for bag of words
-- BAD: creates long list of lists
badWordCount :: [String] -> Map String Int
badWordCount = foldl' (\m w -> Map.insertWith (+) w 1 m) Map.empty  -- actually fine

-- But BAD pattern:
badBuild :: [(String, [Int])] -> Map String [Int]
badBuild = foldl' (\m (k, vs) -> Map.insertWith (++) k vs m) Map.empty
-- GOOD: use Map.fromListWith or accumulate differently

-- Pitfall 4: Recomputing expensive operations
-- BAD: recomputes on each iteration
badSearch :: [Text] -> Text -> Bool
badSearch xs target = any (== target) (map T.toLower xs)

-- GOOD: compute once
goodSearch :: [Text] -> Text -> Bool
goodSearch xs target =
  let lowered = map T.toLower xs
      targetL = T.toLower target
  in any (== targetL) lowered

-- Pitfall 5: fromJust on hot path
badLookup :: Map Int Int -> [Int] -> [Int]
badLookup m = map (\k -> fromJust (Map.lookup k m))  -- partial!

-- GOOD: handle Nothing explicitly  
goodLookup :: Map Int Int -> [Int] -> [Int]
goodLookup m = mapMaybe (\k -> Map.lookup k m)
```

---

## ขั้นตอนที่ 866: Parallel Performance

```haskell
-- Parallel performance patterns

import Control.Parallel.Strategies
import Control.Concurrent.Async

-- Parallel map with chunking (better granularity)
parMapChunked :: NFData b => Int -> (a -> b) -> [a] -> [b]
parMapChunked chunkSize f xs =
  let chunks = chunksOf chunkSize xs
      results = chunks `using` parList (evalList rdeepseq) `on` map (map f)
  in concat results

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Work distribution with async
distributeWork :: Int -> [a] -> (a -> IO b) -> IO [b]
distributeWork numWorkers items action = do
  let batches = chunksOf (ceiling (fromIntegral (length items) / fromIntegral numWorkers)) items
  results <- mapConcurrently (mapM action) batches
  return (concat results)

-- STM-based work queue for producers/consumers
data WorkQueue a b = WorkQueue
  { wqInput   :: TBQueue a
  , wqOutput  :: TBQueue b
  , wqWorkers :: Int
  }

runWorkerPool :: WorkQueue a b -> (a -> IO b) -> IO ()
runWorkerPool wq action = do
  asyncs <- replicateM (wqWorkers wq) $ async $ forever $ do
    item   <- atomically (readTBQueue (wqInput wq))
    result <- action item
    atomically (writeTBQueue (wqOutput wq) result)
  
  mapM_ wait asyncs

-- Evaluate to WHNF vs NF
-- whnf: evaluate only outermost constructor
-- nf: evaluate fully (Deep Normal Form)
benchmarkEval :: NFData a => a -> IO ()
benchmarkEval x = do
  evaluate (rnf x)  -- force complete evaluation
```

---

## ขั้นตอนที่ 867: SIMD and Low-Level Optimizations

```haskell
-- Low-level performance with primitive operations

import GHC.Prim
import GHC.Exts

-- Bit manipulation
popcount :: Word -> Int
popcount = popCnt#  -- hardware instruction via GHC primop

leadingZeros :: Word -> Int
leadingZeros = countLeadingZeros  -- clz instruction

-- Bit tricks
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo n = bit (popcount (n - 1))

-- Unboxed operations
sumIntArray :: UArray Int Int -> Int
sumIntArray arr = runST $ do
  let (lo, hi) = bounds arr
  s <- newSTRef (0 :: Int)
  forM_ [lo..hi] $ \i -> do
    let v = arr ! i
    modifySTRef' s (+v)
  readSTRef s

-- FFI for performance-critical code
foreign import ccall unsafe "fast_memcpy"
  c_fast_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

fastMemcpy :: ByteString -> ByteString
fastMemcpy bs = unsafePerformIO $ do
  let len = BS.length bs
  result <- BS.create len $ \dst ->
    BS.useAsCStringLen bs $ \(src, _) ->
      c_fast_memcpy dst (castPtr src) (fromIntegral len)
  return result
```

---

## ขั้นตอนที่ 868: Lazy vs Strict Tradeoffs

```haskell
-- Understanding lazy vs strict tradeoffs

-- Lazy: enables infinite structures
naturals :: [Int]
naturals = [0..]

primes' :: [Int]
primes' = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Lazy: enables short-circuit evaluation
anyLazy :: (a -> Bool) -> [a] -> Bool
anyLazy = any  -- stops at first True

-- But lazy accumulation causes space leaks
lazyFold :: [Int] -> Int
lazyFold = foldl (+) 0  -- O(n) space!

-- Strict: better for accumulators
data StrictList a = Nil | Cons !a !(StrictList a)

-- Mixed: lazy spine, strict elements
data BoundedBuffer a = Buffer
  { bbList :: ![a]  -- strict elements
  , bbSize :: !Int  -- strict size
  , bbMax  :: Int   -- can be lazy (config)
  }

-- When to use bangs (!):
-- 1. Accumulator parameters
-- 2. Fields that are always evaluated
-- 3. Data structures used for performance

-- Lazy IO pattern (avoid unless necessary)
readLinesLazy :: FilePath -> IO [Text]
readLinesLazy path = T.lines <$> TIO.readFile path  -- reads entire file

-- Better: streaming
readLinesStream :: FilePath -> ConduitT () Text IO ()
readLinesStream path = sourceFile path .| linesUnboundedC
```

---

## ขั้นตอนที่ 869: Benchmarking with Criterion

```haskell
-- Systematic benchmarking

import Criterion.Main
import Criterion.Types

-- Basic benchmarks
basicBench :: IO ()
basicBench = defaultMain
  [ bench "list sum"   (nf (sum :: [Int] -> Int) [1..1000])
  , bench "vector sum" (nf (VU.foldl' (+) 0) (VU.fromList [1..1000] :: VU.Vector Int))
  ]

-- Comparison groups
comparisonBench :: IO ()
comparisonBench = defaultMainWith config
  [ bgroup "string concat"
      [ bench "++ (quadratic)" (nf concatBad ["hello " | _ <- [1..100]])
      , bench "T.concat"       (nf T.concat  (T.replicate 100 "hello "))
      , bench "builder"        (nf buildConcat ["hello " | _ <- [1..100]])
      ]
  , bgroup "map"
      [ bench "Data.Map"        (nf (Map.fromList :: [(Int,Int)] -> Map.Map Int Int) pairs)
      , bench "Data.HashMap"    (nf (HashMap.fromList :: [(Int,Int)] -> HashMap.HashMap Int Int) pairs)
      , bench "Data.IntMap"     (nf (IntMap.fromList :: [(Int,Int)] -> IntMap.IntMap Int) pairs)
      ]
  ]
  where
    config  = defaultConfig { resamples = 100, verbosity = Quiet }
    pairs   = [(i, i*2) | i <- [1..10000]]

-- Whnf vs nf
whnfBench :: IO ()
whnfBench = defaultMain
  [ bench "whnf (only head)"   (whnf head [1..1000000 :: Int])
  , bench "nf (full list)"     (nf id    [1..1000000 :: Int])
  ]
```

---

## ขั้นตอนที่ 870: Algorithmic Complexity Improvements

```haskell
-- Replace O(n^2) algorithms with faster ones

-- Naive: O(n^2) duplicate check
hasDuplicatesNaive :: Eq a => [a] -> Bool
hasDuplicatesNaive [] = False
hasDuplicatesNaive (x:xs) = x `elem` xs || hasDuplicatesNaive xs

-- Better: O(n log n)
hasDuplicatesSorted :: Ord a => [a] -> Bool
hasDuplicatesSorted xs = any (uncurry (==)) (zip sorted (tail sorted))
  where sorted = sort xs

-- Best for hashable: O(n) average
hasDuplicatesHash :: (Eq a, Hashable a) => [a] -> Bool
hasDuplicatesHash xs = Set.size (Set.fromList xs) < length xs

-- Naive string search: O(n*m)
naiveSearch :: String -> String -> [Int]
naiveSearch text pattern =
  [ i | i <- [0..length text - length pattern]
  , take (length pattern) (drop i text) == pattern
  ]

-- KMP: O(n+m)
kmpSearch' :: Eq a => [a] -> [a] -> [Int]
kmpSearch' = kmpSearch  -- use precomputed failure function

-- Naive group anagram: O(n * m * log m)
groupAnagramsNaive :: [String] -> [[String]]
groupAnagramsNaive = groupBy (on (==) sort) . sortBy (compare `on` sort)

-- Better: O(n * m)
groupAnagramsHash :: [String] -> [[String]]
groupAnagramsHash words =
  Map.elems $ Map.fromListWith (++)
  [(sort w, [w]) | w <- words]
```

---

## ขั้นตอนที่ 871: IO Performance

```haskell
-- High-performance IO

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- Buffered IO settings
configureHandle :: Handle -> IO ()
configureHandle h = do
  hSetBuffering h (BlockBuffering (Just 65536))  -- 64KB buffer
  hSetEncoding h utf8

-- Bulk operations (avoid per-element IO)
-- BAD: one syscall per element
badWrite :: Handle -> [Int] -> IO ()
badWrite h = mapM_ (\n -> hPutStr h (show n ++ "\n"))

-- GOOD: build then write once
goodWrite :: Handle -> [Int] -> IO ()
goodWrite h ints = BS.hPut h (BS.concat (map (\n -> BS.pack (encodeUtf8 (T.pack (show n ++ "\n")))) ints))

-- Even better: ByteString builder
bestWrite :: Handle -> [Int] -> IO ()
bestWrite h ints = do
  let builder = foldMap (\n -> BB.intDec n <> BB.char8 '\n') ints
  BL.hPut h (BB.toLazyByteString builder)

-- Async IO for concurrent operations  
import Control.Concurrent.Async

parallelReads :: [FilePath] -> IO [BS.ByteString]
parallelReads paths = mapConcurrently BS.readFile paths

-- Use of mmap for large files
import System.Posix.MMap

readLargeFile :: FilePath -> IO BS.ByteString
readLargeFile = mmapFileByteString  -- no copying, OS manages pages
```

---

## ขั้นตอนที่ 872: Network Performance

```haskell
-- Network performance optimization

import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Connection pooling
createPool :: IO Manager
createPool = newManager tlsManagerSettings
  { managerConnCount       = 100
  , managerIdleConnectionCount = 200
  , managerResponseTimeout = responseTimeoutMicro (30 * 1000000)
  }

-- HTTP/2 multiplexing (http2 library)
-- Multiple requests on single TCP connection
http2Client :: IO ()
http2Client = do
  ctx <- allocSimpleContext
  let conf = defaultClientConfig
  withHTTP2 conf ctx "example.com" 443 $ \http2 -> do
    -- Multiple concurrent requests on one connection
    asyncs <- mapM (\path -> async (sendRequest http2 (get path))) paths
    responses <- mapM wait asyncs
    mapM_ processResponse responses

-- Request batching
batchRequests :: Manager -> [Request] -> IO [Response BS.ByteString]
batchRequests mgr reqs = do
  -- Fire all requests concurrently
  asyncs <- mapM (\req -> async (httpLbs req mgr)) reqs
  mapM wait asyncs

-- Keep-alive and pipelining
pipelineRequests :: Manager -> [Request] -> IO [Response BS.ByteString]
pipelineRequests mgr reqs =
  -- HTTP client automatically reuses connections
  mapConcurrently (\req -> httpLbs req mgr) reqs

-- TCP tuning
setSocketOptions :: Socket -> IO ()
setSocketOptions sock = do
  setSocketOption sock NoDelay 1     -- disable Nagle
  setSocketOption sock ReuseAddr 1   -- reuse address
  setSocketOption sock KeepAlive 1   -- enable keepalive
```

---

## ขั้นตอนที่ 873: Database Performance

```haskell
-- Database performance optimization

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow

-- Connection pool with resource-pool
import Data.Pool

createDbPool :: ConnectInfo -> IO (Pool Connection)
createDbPool info = createPool
  (connect info)   -- create
  close            -- destroy
  1                -- stripes
  30               -- keep idle for 30s
  10               -- max 10 connections

-- Prepared statements for repeated queries
data PreparedStmts = PreparedStmts
  { psGetUser    :: Statement
  , psInsertUser :: Statement
  , psUpdateUser :: Statement
  }

prepareStatements :: Connection -> IO PreparedStmts
prepareStatements conn = PreparedStmts
  <$> prepare conn "SELECT * FROM users WHERE id = $1"
  <*> prepare conn "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id"
  <*> prepare conn "UPDATE users SET name = $1 WHERE id = $2"

-- Bulk insert (COPY for max performance)
bulkInsert :: Connection -> [User] -> IO Int64
bulkInsert conn users = do
  let rows = map (\u -> (userName u, userEmail u)) users
  copyFrom conn "COPY users (name, email) FROM STDIN" rows

-- Batch queries to avoid N+1
getUsersWithOrders :: Connection -> [UserId] -> IO [(User, [Order])]
getUsersWithOrders conn userIds = do
  users  <- query conn "SELECT * FROM users WHERE id = ANY(?)" (Only (In userIds))
  orders <- query conn "SELECT * FROM orders WHERE user_id = ANY(?)" (Only (In userIds))
  let orderMap = Map.fromListWith (++) [(ordUserId o, [o]) | o <- orders]
  return [(u, fromMaybe [] (Map.lookup (userId u) orderMap)) | u <- users]
```

---

## ขั้นตอนที่ 874: Memory Management

```haskell
-- Explicit memory management with Foreign

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

-- Manual allocation (use with care!)
withManualBuffer :: Int -> (Ptr Word8 -> IO a) -> IO a
withManualBuffer size action = do
  ptr <- mallocBytes size
  result <- action ptr
  free ptr
  return result

-- Storable for C-compatible data
data CPoint = CPoint
  { cpX :: CDouble
  , cpY :: CDouble
  } deriving (Show)

instance Storable CPoint where
  sizeOf    _ = 2 * sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr    = CPoint
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr (sizeOf (undefined :: CDouble))
  poke ptr (CPoint x y) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr (sizeOf (undefined :: CDouble)) y

-- Pinned memory for FFI (won't be moved by GC)
import Data.Vector.Storable.Mutable as VSM

allocPinnedBuffer :: Int -> IO (VSM.IOVector Double)
allocPinnedBuffer n = VSM.new n  -- automatically pinned for storable vectors

-- Finalizers for resource cleanup
import System.Mem.Weak

addWeakFinalizer :: a -> IO () -> IO ()
addWeakFinalizer obj cleanup = do
  weak <- mkWeak obj obj (Just cleanup)
  return ()
```

---

## ขั้นตอนที่ 875: Profiling-Driven Optimization

```haskell
-- Profiling-driven optimization workflow

-- Step 1: Profile to find hotspots
-- +RTS -p produces program.prof

-- Step 2: Look for these patterns in profile:
-- - High "ticks" or "time" percentage
-- - High "alloc" = frequent allocation
-- - Unexpected entries = called more than expected

-- Step 3: Common fixes

-- Fix 1: Strict fields to reduce allocation
data Config = Config
  { _port    :: !Int
  , _host    :: !Text
  , _timeout :: !Int
  }

-- Fix 2: Memoize expensive computations
import qualified Data.Map.Strict as Map
import Data.IORef

memoize :: (Ord k) => (k -> IO v) -> IO (k -> IO v)
memoize f = do
  cacheRef <- newIORef Map.empty
  return $ \k -> do
    cache <- readIORef cacheRef
    case Map.lookup k cache of
      Just v  -> return v
      Nothing -> do
        v <- f k
        modifyIORef' cacheRef (Map.insert k v)
        return v

-- Fix 3: Replace repeated list operations with array access
-- BAD: O(n) per access
accessNth :: [a] -> Int -> a
accessNth xs i = xs !! i  -- O(n)

-- GOOD: O(1) per access
buildArray :: [a] -> Array Int a
buildArray xs = listArray (0, length xs - 1) xs

-- Fix 4: Avoid unnecessary conversions
-- BAD: Text -> String -> Text (O(n) each)
roundTrip :: Text -> Text
roundTrip = T.pack . T.unpack  -- wasteful!

-- GOOD: stay in Text
processText :: Text -> Text
processText = T.map toUpper  -- stays in Text
```

---

## ขั้นตอนที่ 876: STM Performance

```haskell
-- STM performance optimization

import Control.Concurrent.STM

-- Minimize transaction size
-- BAD: entire function in STM
bigTransaction :: TVar State -> IO Result
bigTransaction stateVar = atomically $ do
  state <- readTVar stateVar
  let intermediate = expensiveComputation state  -- slow in STM!
  writeTVar stateVar (updateState state intermediate)
  return (extractResult state)

-- GOOD: compute outside STM
smallTransaction :: TVar State -> IO Result
smallTransaction stateVar = do
  state <- readTVarIO stateVar
  let intermediate = expensiveComputation state  -- outside STM
  atomically $ do
    currentState <- readTVar stateVar
    when (stateVersion currentState == stateVersion state) $  -- optimistic check
      writeTVar stateVar (updateState currentState intermediate)
  return (extractResult state)

-- Use TQueue for producer-consumer (no contention)
data TaskQueue = TaskQueue (TQueue Task)

enqueueTask :: TaskQueue -> Task -> IO ()
enqueueTask (TaskQueue q) task = atomically (writeTQueue q task)

dequeueTask :: TaskQueue -> IO Task
dequeueTask (TaskQueue q) = atomically (readTQueue q)

-- TMVar for simple mutex
simpleMutex :: IO (TMVar ())
simpleMutex = newTMVarIO ()

withMutex :: TMVar () -> IO a -> IO a
withMutex m action = bracket
  (atomically (takeTMVar m))
  (\_ -> atomically (putTMVar m ()))
  (\_ -> action)
```

---

## ขั้นตอนที่ 877: GC Tuning

```haskell
-- GHC GC tuning

-- RTS options for GC tuning:
-- +RTS -A64m          -- nursery size: 64MB (reduce GC pressure for long-running)
-- +RTS -H256m         -- minimum heap size
-- +RTS -c             -- compact GC (reduces fragmentation)
-- +RTS -N4            -- use 4 OS threads
-- +RTS -qn2           -- 2 parallel GC threads
-- +RTS -I0            -- disable idle GC

-- In code: set RTS options
import GHC.RTS.Flags (setRTSFlags)

configureGC :: IO ()
configureGC = do
  -- These would need to be set before runtime starts
  -- Usually set via +RTS or GHCRTS env var
  putStrLn "GC config: set via +RTS flags"

-- Force GC at specific points
import System.Mem (performGC, performMajorGC)

periodicGC :: IO ()
periodicGC = forever $ do
  performMajorGC
  threadDelay (60 * 1000000)  -- every minute

-- Compact regions (avoid GC traversal)
import GHC.Compact

-- Store long-lived data in compact region
compactLongLived :: [a] -> IO (Compact [a])
compactLongLived = compact  -- moves data to compact region (no GC pointer traversal)

getCompact :: Compact a -> a
getCompact = getCompact  -- re-export

-- Monitor GC stats
printGCStats :: IO ()
printGCStats = do
  stats <- getRTSStats
  putStrLn $ "Major GCs: " ++ show (major_gcs stats)
  putStrLn $ "Total bytes allocated: " ++ show (allocated_bytes stats)
  putStrLn $ "Max live bytes: " ++ show (max_live_bytes stats)
```

---

## ขั้นตอนที่ 878: Serialization Performance

```haskell
-- High-performance serialization

-- cereal: fast binary serialization
import qualified Data.Serialize as S

data MyData = MyData Int Double Text deriving (Show)

instance S.Serialize MyData where
  put (MyData i d t) = do
    S.put i
    S.put d
    S.put (T.encodeUtf8 t)
  get = MyData <$> S.get <*> S.get <*> (T.decodeUtf8 <$> S.get)

-- Benchmark: cereal vs aeson vs binary
benchSerialization :: MyData -> IO ()
benchSerialization data' = defaultMain
  [ bench "cereal encode"  (nf S.encode data')
  , bench "aeson encode"   (nf (BL.toStrict . encode) data')
  , bench "binary encode"  (nf (BL.toStrict . Binary.encode) data')
  ]

-- MessagePack (compact binary format)
import Data.MessagePack

msgpackEncode :: ToObject a => a -> ByteString
msgpackEncode = pack . toObject

-- Protocol Buffers (with proto3-wire)
encodeProto :: [Int32] -> ByteString
encodeProto ints = runPut $ do
  forM_ ints $ \v -> do
    putVarInt (fieldTag 1 VarInt)  -- field 1, varint type
    putVarInt (fromIntegral v)

-- CBOR (Concise Binary Object Representation)
import Codec.CBOR.Encoding
import Codec.CBOR.Write

encodeCbor :: Map Text Int -> ByteString
encodeCbor m = toStrictByteString $ do
  encodeMapLen (fromIntegral (Map.size m))
  forM_ (Map.toList m) $ \(k, v) -> do
    encodeString k
    encodeInt v
```

---

## ขั้นตอนที่ 879: Hot Path Optimization

```haskell
-- Optimizing hot paths in web servers

-- Avoid recomputing values on every request
data RequestContext = RequestContext
  { rcConfig   :: !Config         -- precomputed
  , rcParsed   :: !ParsedRequest  -- parse once
  , rcUser     :: !(Maybe User)   -- memoized auth
  }

-- INLINE critical functions
{-# INLINE lookupHeader #-}
lookupHeader :: RequestHeaders -> HeaderName -> Maybe BS.ByteString
lookupHeader headers name = lookup name headers

-- Avoid polymorphism on hot path (enable specialization)
{-# SPECIALIZE routeRequest :: Application -> Request -> IO Response #-}
routeRequest :: (MonadIO m) => Application -> Request -> m Response
routeRequest app req = liftIO (app req)

-- Avoid type class dispatch overhead with concrete types
-- BAD: polymorphic (vtable dispatch)
processRequest :: MonadIO m => Request -> m Response
processRequest = undefined

-- GOOD: concrete type
processRequestIO :: Request -> IO Response
processRequestIO = undefined

-- Avoid fromIntegral on hot path
{-# INLINE fastLength #-}
fastLength :: [a] -> Int
fastLength = foldl' (\n _ -> n + 1) 0

-- Unboxed tuples to avoid allocation
computeMinMax :: [Double] -> (# Double, Double #)
computeMinMax [] = (# 0.0, 0.0 #)
computeMinMax (x:xs) = foldl' (\(# mn, mx #) v -> (# min mn v, max mx v #)) (# x, x #) xs
```

---

## ขั้นตอนที่ 880: โปรเจกต์: High-Performance API Server

```haskell
-- High-performance API server optimization

module HiPerfServer where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as VU
import Data.IORef
import Control.Concurrent.STM

-- Pre-allocated response cache
data ResponseCache = ResponseCache
  { rcCache   :: IORef (HashMap.HashMap Text CachedResponse)
  , rcMaxSize :: Int
  , rcMetrics :: CacheMetrics
  }

data CachedResponse = CachedResponse
  { crBody    :: !BS.ByteString
  , crHeaders :: ![(BS.ByteString, BS.ByteString)]
  , crExpiry  :: !UTCTime
  }

-- Optimized routing table (hash map vs trie)
data Router = Router
  { rExact  :: HashMap.HashMap Text Handler
  , rParams :: Trie Handler
  , rFallback :: Handler
  }

-- Pre-allocated thread pool
data ThreadPool = ThreadPool
  { tpQueue   :: TBQueue Request
  , tpWorkers :: [Async ()]
  , tpSize    :: Int
  }

createThreadPool :: Int -> (Request -> IO Response) -> IO ThreadPool
createThreadPool n handler = do
  queue   <- newTBQueueIO 1000
  workers <- replicateM n $ async $ forever $ do
    req      <- atomically (readTBQueue queue)
    response <- handler req
    sendResponse response
  return (ThreadPool queue workers n)

-- Zero-copy response for static files
serveStaticFile :: FilePath -> IO Response
serveStaticFile path = do
  (fd, size) <- openAndStat path
  return Response
    { rsStatus  = 200
    , rsHeaders = [("Content-Length", BS.pack (show size))]
    , rsBody    = SendFile fd 0 size  -- kernel sendfile, no userspace copy
    }

-- Main optimized server
main :: IO ()
main = do
  -- Configure for performance
  setNumCapabilities =<< getNumProcessors
  
  -- Pre-warm caches
  warmCache
  
  -- Start optimized server
  runSettings (setPort 8080 . setNumThreads 16 $ defaultSettings) app
  where
    app req respond = do
      response <- handleRequest req
      respond response
    
    handleRequest req = do
      case routeRequest router req of
        Nothing      -> return notFound
        Just handler -> handler req
```

---

*[← Part 43](part-43.md) | [Part 45 →](part-45.md)*
