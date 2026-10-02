# Part 29: Haskell Internals & GHC Deep Dive
## ขั้นตอนที่ 561-580

---

## ขั้นตอนที่ 561: GHC Compilation Pipeline

```
GHC Compilation Pipeline:
  Source (.hs)
    ↓
  Parser → Haskell AST
    ↓
  Renamer → resolves names
    ↓
  Type Checker → adds types
    ↓
  Desugarer → Core IR
    ↓
  Core Optimization passes
    ↓
  STG (Spineless Tagless G-machine)
    ↓
  Code generation (LLVM / NCG)
    ↓
  Native code / LLVM IR
    ↓
  Executable
```

```haskell
-- Core IR: ภาษาที่ GHC ใช้ internally
-- ดู Core ด้วย -ddump-simpl

{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}

addOne :: Int -> Int
addOne x = x + 1
-- Core: addOne = \ x -> +# x 1#
-- (using unboxed Int# operations)

-- Viewing Core output
-- ghc -O2 -ddump-simpl -dsuppress-uniques -dsuppress-all Foo.hs

-- Understanding Core:
-- let!   = strict let (always evaluated)
-- case!  = strict case
-- box/unbox = coercions between Int and Int#
-- sat    = saturated (fully applied)
```

---

## ขั้นตอนที่ 562: Runtime System (RTS)

```haskell
-- GHC Runtime System

-- RTS options (pass with +RTS ... -RTS)
-- -Hn    heap size n MB
-- -A64m  allocation area size 64MB
-- -N4    use 4 OS threads
-- -p     enable profiling
-- -s     show GC stats
-- -T     collect runtime stats

-- Enable RTS options in program
main :: IO ()
main = do
  -- RTS stats
  stats <- getRTSStats
  putStrLn $ "GCs: " ++ show (gcs stats)
  putStrLn $ "Max live: " ++ show (max_live_bytes stats)
  putStrLn $ "Mutator CPU: " ++ show (mutator_cpu_ns stats)

-- Stack overflow
-- GHC uses segmented stacks (or single large stack with +RTS -K)
-- Default stack size: 8MB
-- Increase: +RTS -K100M -RTS

-- Controlling GC
-- Incremental GC (low latency):
-- +RTS -I0.05 -RTS  (GC interval 50ms)

-- Stats to file
-- +RTS -S stats.txt -RTS

-- Thread-local storage
import Control.Concurrent (myThreadId)

-- Each Haskell thread has its own:
-- - Heap nursery (default 512KB)
-- - Stack
-- - Thread ID
-- - Thread-local storage via TVars
```

---

## ขั้นตอนที่ 563: Memory Layout

```haskell
-- GHC Memory Layout

-- Heap objects:
-- - Header word (tag + forwarding pointer)
-- - Payload (pointers and non-pointers)

-- Unboxed values (stored directly, no header)
-- Int#, Double#, Char#, etc.

-- Boxed values (on heap with header)
-- Int = I# Int#  -- one extra indirection

-- Int in memory:
-- [ header | Int# value ]  (2 words)

-- Tuple (a, b):
-- [ header | ptr to a | ptr to b ]  (3 words)

-- List [1,2,3]:
-- [ : | ptr to 1 | ptr to [2,3] ]
-- [ : | ptr to 2 | ptr to [3] ]
-- [ : | ptr to 3 | ptr to [] ]
-- [ [] ]
-- Very expensive for simple integers!

-- Array (boxed, [a]):
-- [ header | length | ptrs... ]
-- Each element is a pointer to a boxed value

-- Unboxed array (UArray, Vector):
-- [ header | length | values... ]  (values stored directly)
-- Much more cache-friendly!

-- ByteString (pinned memory):
-- [ ForeignPtr | length | offset ]
-- Actual bytes in pinned memory (not moved by GC)

-- Measuring heap usage
import System.Mem
import GHC.DataSize

heapUsage :: IO ()
heapUsage = do
  performGC
  allocBytes <- currentAllocatedBytes
  liveBytes  <- getAllocationCounter
  putStrLn $ "Allocated: " ++ show allocBytes
```

---

## ขั้นตอนที่ 564: STG Machine

```haskell
-- Spineless Tagless G-machine (STG)
-- The abstract machine GHC targets

-- STG concepts:
-- - Thunks: suspended computations (unevaluated closures)
-- - Black holes: currently being evaluated (to detect loops)
-- - Update frames: stored on stack to update thunk after eval

-- Evaluation:
-- 1. Enter closure
-- 2. If thunk: push update frame, evaluate body
-- 3. If value (WHNF): return

-- WHNF (Weak Head Normal Form):
-- A value whose outermost constructor is known
-- Just 5    -- WHNF (Just known, 5 not forced)
-- [1,2,3]   -- WHNF (: known, 1 not forced)
-- 42        -- WHNF
-- \x -> x   -- WHNF (lambda)

-- seq forces to WHNF
-- deepseq forces to NF (Normal Form)

-- STG optimizations:
-- - Worker/wrapper: separate strict worker from lazy wrapper
-- - Let-no-escape: convert non-recursive lets to jumps
-- - Strictness analysis: avoid thunk allocation

-- See STG output:
-- ghc -O2 -ddump-stg -dsuppress-all Foo.hs

-- Avoid excessive thunk buildup:
badSum :: [Int] -> Int
badSum []     = 0
badSum (x:xs) = x + badSum xs  -- builds call stack

goodSum :: [Int] -> Int
goodSum = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs  -- tail recursive + strict
```

---

## ขั้นตอนที่ 565: GHC Core Language

```haskell
-- Understanding GHC Core

-- Core is System F with extensions
-- let (strict or lazy)
-- case (forces evaluation)
-- type abstractions

-- Example: map in Core
-- map f [] = []
-- map f (x:xs) = f x : map f xs
-- 
-- Core:
-- map = \@ a @ b (f :: a -> b) (xs :: [a]) ->
--   case xs of
--     []     -> []
--     (y:ys) -> f y : map @ a @ b f ys

-- Coercions in Core
-- Safe coerce (representational equality):
-- coerce :: Coercible a b => a -> b
-- In Core: becomes no-op cast

-- Type equality proofs
-- Refl :: a ~ a
-- Used in GADTs and type family applications

-- Inlining
{-# INLINE f #-}
f :: Int -> Int
f x = x * 2

-- After inlining:
-- g x = x * 2 + 1  (f inlined into g)

-- NOINLINE
{-# NOINLINE expensiveSetup #-}
expensiveSetup :: Config
expensiveSetup = ...  -- only computed once

-- SPECIALIZE
{-# SPECIALIZE sort :: [Int] -> [Int] #-}
sort :: Ord a => [a] -> [a]
sort = ...  -- specialized version for Int
```

---

## ขั้นตอนที่ 566: Profiling ด้วย ThreadScope

```haskell
-- ThreadScope: visualize parallel execution

-- Build with profiling:
-- cabal build --enable-profiling
-- ghc -eventlog -rtsopts -O2 Main.hs

-- Run with eventlog:
-- ./main +RTS -l -N4 -RTS

-- Open in ThreadScope:
-- threadscope main.eventlog

-- What to look for:
-- - Thread activity (green = running, orange = blocking)
-- - GC pauses (gray bars)
-- - Spark creation/conversion (parallel sparks)
-- - Load imbalance (one thread busy, others idle)

-- Inject user events for annotation
import Control.Concurrent.EventLog

importantSection :: IO ()
importantSection = do
  traceEventIO "BEGIN important"
  doSomethingImportant
  traceEventIO "END important"

-- Parallel spark analysis
import Control.Parallel.Strategies

-- Good: sparks all converted to work
goodParallel :: [Int] -> [Int]
goodParallel = parMap rdeepseq heavyComputation

-- Bad: sparks fizzle (GC'd before evaluated)
badParallel :: [Int] -> [Int]
badParallel xs = map lightweight xs `using` parList rseq
-- lightweight work: spark overhead > work

-- Monitor spark stats:
-- +RTS -s -RTS will show:
-- SPARKS: created converted fizzled GC'd
```

---

## ขั้นตอนที่ 567: Memory Profiling

```haskell
-- Heap profiling

-- Build:
-- ghc -prof -fprof-auto -rtsopts -O2 Main.hs

-- Run with heap profiling:
-- ./main +RTS -h -RTS      # heap by closure type
-- ./main +RTS -hc -RTS     # heap by cost center
-- ./main +RTS -hd -RTS     # heap by closure description
-- ./main +RTS -hm -RTS     # heap by module

-- Generate HP file, convert to graph:
-- hp2ps -c main.hp | ps2pdf - > heap.pdf
-- hp2pretty main.hp > heap.html

-- Typical space leaks:
-- 1. Lazy foldl
badLeak :: [Int] -> Int
badLeak = foldl (+) 0  -- accumulates thunks!

-- 2. Accumulating lazy structures
badAccum :: [Text] -> Text
badAccum = foldl (<>) ""  -- O(n^2) + space leak

-- Fix:
goodAccum :: [Text] -> Text
goodAccum = T.concat  -- efficient

-- 3. Data.Map.Lazy vs Data.Map.Strict
-- Use Data.Map.Strict to avoid lazy values in map

-- 4. Keep-alive references
data Cache = Cache
  { cacheMap :: Map Key Value
  , -- Don't keep reference to entire history here!
  }

-- Identify leaks with retainer profiling:
-- ./main +RTS -hr -RTS
-- Shows what is keeping values alive
```

---

## ขั้นตอนที่ 568: Unsafe Operations

```haskell
-- Unsafe operations (use carefully!)

import GHC.Exts
import Unsafe.Coerce

-- unsafeCoerce: bypass type checker
-- Useful for: heterogeneous collections, reflection tricks
-- Dangerous: can cause segfaults if types are wrong
evilCoerce :: a -> b
evilCoerce = unsafeCoerce

-- Safe use of unsafeCoerce:
-- Force newtype coercion (same representation)
coerceNewtype :: Identity a -> a
coerceNewtype (Identity x) = x  -- same as unsafeCoerce but safe

-- unsafePerformIO: run IO without monad
-- Useful for: top-level mutable state, runtime constants
import System.IO.Unsafe

globalState :: IORef Int
globalState = unsafePerformIO (newIORef 0)
{-# NOINLINE globalState #-}  -- prevent inlining (critical!)

-- Rules for unsafePerformIO:
-- 1. Result must be idempotent (same value if run multiple times)
-- 2. Must not observe external state that changes
-- 3. Must use NOINLINE pragma

-- reallyUnsafePtrEquality: check if two values are same object
import GHC.Prim (reallyUnsafePtrEq#)

sameObject :: a -> a -> Bool
sameObject x y = case reallyUnsafePtrEq# x y of
  0# -> False
  _  -> True

-- inlinePerformIO: like unsafePerformIO but can be inlined
-- Use for performance-critical code where IO is benign
```

---

## ขั้นตอนที่ 569: GHC Extensions สำหรับ Performance

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

-- MagicHash: unboxed primitives
import GHC.Exts

-- Unboxed Int operations (no allocation!)
addUnboxed :: Int# -> Int# -> Int#
addUnboxed x# y# = x# +# y#

-- Unboxed tuples (no heap allocation!)
splitAt# :: Int# -> ByteArray# -> (# ByteArray#, ByteArray# #)
splitAt# n# arr# = (# sliceBA arr# 0# n#, sliceBA arr# n# (sizeofByteArray# arr# -# n#) #)

-- Use unboxed in Haskell code
fastAbs :: Int -> Int
fastAbs (I# n#) = I# (absInt# n#)
  where absInt# x# = if isTrue# (x# <# 0#) then negateInt# x# else x#

-- Word# operations
popcount :: Word -> Int
popcount (W# w#) = I# (popCnt# w#)

-- ByteArray operations (zero-copy)
readByteArray :: ByteArray -> Int -> Word8
readByteArray (ByteArray ba#) (I# i#) =
  W8# (indexWord8Array# ba# i#)

-- Pinned arrays (don't move in GC)
newPinnedArray :: Int -> IO (MutableByteArray RealWorld)
newPinnedArray (I# n#) = IO $ \s ->
  case newPinnedByteArray# n# s of
    (# s', arr# #) -> (# s', MutableByteArray arr# #)
```

---

## ขั้นตอนที่ 570: LLVM Backend Optimization

```haskell
-- LLVM backend ใน GHC

-- Enable LLVM backend:
-- cabal.project: ghc-options: -fllvm
-- or: ghc -fllvm

-- Benefits:
-- - Better auto-vectorization (SIMD)
-- - Better optimization for numeric code
-- - Better dead code elimination

-- SIMD operations via vector extension
import Data.Vector.SIMD

-- 128-bit SIMD (SSE2)
addFloat4 :: FloatX4 -> FloatX4 -> FloatX4
addFloat4 = plusFloatX4

-- Process 4 floats at once
dotProduct4 :: [Float] -> [Float] -> Float
dotProduct4 xs ys = sum (zipWith (*) xs ys)
-- With LLVM: auto-vectorized to SIMD!

-- Manual SIMD via primops
import GHC.Exts

addDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#
addDoubleX2# = plusDoubleX2#

-- Numeric code tips for LLVM:
-- 1. Use -ffast-math for aggressive float optimization
--    (changes IEEE semantics)
-- 2. Use strict arrays (Vector, UArray)
-- 3. Avoid branching in tight loops
-- 4. Use fusion (stream fusion in vector package)

-- Example: LLVM-optimized inner product
innerProduct :: U.Vector Double -> U.Vector Double -> Double
innerProduct v1 v2 = U.sum (U.zipWith (*) v1 v2)
-- LLVM will auto-vectorize this with SSE/AVX!
```

---

## ขั้นตอนที่ 571: GHC Performance Regression Testing

```haskell
-- Track performance regressions

import Criterion.Main
import Test.Tasty.Bench

-- Tasty-bench: integrate with test suite
benchmarks :: [Benchmark]
benchmarks =
  [ bench "sort 1000 ints" $ nf sort (reverse [1..1000 :: Int])
  , bench "Text concat 100" $ nf (T.concat) (replicate 100 "hello")
  , bench "Map lookup 1000" $ nf (lookupAll keys) testMap
  ]

-- Memory benchmarks
memBenches :: [Benchmark]
memBenches =
  [ bench "list 10k" $ nfAppIO (evaluate . length) [1..10000 :: Int]
  , bench "vector 10k" $ nfAppIO (evaluate . U.length) (U.fromList [1..10000 :: Int])
  ]

-- Compare against baseline
main :: IO ()
main = defaultMain benchmarks

-- CI integration: fail if performance regresses by >10%
-- tasty-bench provides --timeout and --baseline flags

-- Baseline file format:
-- bench1: 1.23ms
-- bench2: 456μs

-- Run: stack bench --benchmark-arguments "--baseline baseline.csv --fail-if-slower 10"
```

---

## ขั้นตอนที่ 572: Haskell Foreign Function Interface (FFI)

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}

-- FFI: call C functions from Haskell

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

-- Declare C function
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

foreign import ccall "string.h strlen"
  c_strlen :: CString -> IO CSize

-- Safe vs unsafe calls
foreign import ccall safe "slow_function"
  slowFunction :: CInt -> IO CInt
  -- safe: releases Haskell RTS lock (allows other threads to run)
  -- use for blocking/slow C calls

foreign import ccall unsafe "fast_function"
  fastFunction :: CInt -> CInt
  -- unsafe: faster, but Haskell thread is blocked
  -- use for non-blocking fast C calls

-- Pass Haskell data to C
withHaskellArray :: [CInt] -> (Ptr CInt -> CSize -> IO a) -> IO a
withHaskellArray xs action =
  withArrayLen xs $ \len ptr ->
    action ptr (fromIntegral len)

-- Export Haskell function to C
foreign export ccall "haskell_sort"
  haskellSort :: Ptr CInt -> CInt -> IO ()

haskellSort :: Ptr CInt -> CInt -> IO ()
haskellSort ptr len = do
  xs <- peekArray (fromIntegral len) ptr
  let sorted = sort (xs :: [CInt])
  pokeArray ptr sorted

-- Inline C with inline-c package
import qualified Language.C.Inline as C

C.include "<string.h>"
C.include "<stdlib.h>"

-- Embed C code directly
mallocAndFill :: IO (Ptr CInt)
mallocAndFill = [C.exp| int* {
  int* arr = malloc(10 * sizeof(int));
  for (int i = 0; i < 10; i++) arr[i] = i;
  arr
} |]
```

---

## ขั้นตอนที่ 573: Haskell to JavaScript (GHCJS)

```haskell
-- GHCJS: compile Haskell to JavaScript

-- Install: ghcup install ghcjs

-- Build:
-- ghcjs Main.hs
-- Produces: Main.jsexe/all.js

-- JavaScript interop
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

-- DOM manipulation
import Web.DOM.Document
import Web.DOM.Element

updateUI :: IO ()
updateUI = do
  doc <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  
  el <- createElementUnchecked doc "div"
  setInnerHTML el "Hello from Haskell!"
  appendChild_ body el

-- FFI with JavaScript
foreign import javascript unsafe "console.log($1)"
  jsConsoleLog :: JSString -> IO ()

foreign import javascript "async" "$r = fetch($1).then(r => r.json())"
  jsFetch :: JSString -> IO JSVal

-- Miso: Elm-like framework compiled to JS with GHCJS
import Miso
import Miso.String (MisoString)

data Model = Model
  { count :: Int }

data Action = Increment | Decrement | Reset

updateModel :: Action -> Model -> Effect Action Model
updateModel Increment m = noEff m { count = count m + 1 }
updateModel Decrement m = noEff m { count = count m - 1 }
updateModel Reset m     = noEff m { count = 0 }

viewModel :: Model -> View Action
viewModel m = div_ []
  [ button_ [onClick Decrement] [text "-"]
  , text (ms (count m))
  , button_ [onClick Increment] [text "+"]
  , button_ [onClick Reset]     [text "Reset"]
  ]
```

---

## ขั้นตอนที่ 574: STM Advanced Patterns

```haskell
-- Advanced STM patterns

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

-- Bounded queue
data BoundedQueue a = BoundedQueue (TBQueue a) Int

newBoundedQueue :: Int -> IO (BoundedQueue a)
newBoundedQueue n = BoundedQueue <$> newTBQueueIO (fromIntegral n) <*> return n

tryEnqueue :: BoundedQueue a -> a -> STM Bool
tryEnqueue (BoundedQueue q _) x = do
  full <- isFullTBQueue q
  if full
    then return False
    else writeTBQueue q x >> return True

-- Transactional mutex
data TMutex = TMutex (TVar Bool)

newTMutex :: STM TMutex
newTMutex = TMutex <$> newTVar False

acquireTMutex :: TMutex -> STM ()
acquireTMutex (TMutex locked) = do
  isLocked <- readTVar locked
  if isLocked
    then retry  -- wait until unlocked
    else writeTVar locked True

releaseTMutex :: TMutex -> STM ()
releaseTMutex (TMutex locked) = writeTVar locked False

withTMutex :: TMutex -> IO a -> IO a
withTMutex m action = do
  atomically (acquireTMutex m)
  finally action (atomically (releaseTMutex m))

-- STM-based semaphore
data TSemaphore = TSemaphore (TVar Int)

newTSemaphore :: Int -> STM TSemaphore
newTSemaphore n = TSemaphore <$> newTVar n

acquireN :: TSemaphore -> Int -> STM ()
acquireN (TSemaphore count) n = do
  c <- readTVar count
  if c >= n
    then writeTVar count (c - n)
    else retry

releaseN :: TSemaphore -> Int -> STM ()
releaseN (TSemaphore count) n = modifyTVar' count (+n)

-- Multi-resource lock (prevent deadlocks via ordering)
acquireMultiple :: [TMutex] -> IO a -> IO a
acquireMultiple mutexes action = do
  let sorted = sortBy compareAddr mutexes  -- lock in consistent order!
  acquireAll sorted
  finally action (releaseAll sorted)
  where
    acquireAll = mapM_ (atomically . acquireTMutex)
    releaseAll = mapM_ (atomically . releaseTMutex) . reverse
```

---

## ขั้นตอนที่ 575: Async Exception Safety

```haskell
-- Async exceptions ใน Haskell

import Control.Exception
import Control.Monad

-- Async exceptions: delivered to any thread
-- Common: ThreadKilled, UserInterrupt, AsyncException

-- Safe resource acquisition
withResource :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
withResource acquire release action = 
  mask $ \restore -> do
    resource <- acquire
    result <- restore (action resource) `onException` release resource
    release resource
    return result
-- mask: prevent async exceptions during acquire/release
-- onException: release on exception

-- bracket is implemented similarly
-- bracket acquire release action

-- MVar-based critical section
withMVarUnmasked :: MVar a -> (a -> IO b) -> IO b
withMVarUnmasked mvar action = 
  mask $ \restore -> do
    val    <- takeMVar mvar
    result <- restore (action val) `onException` putMVar mvar val
    putMVar mvar val
    return result

-- Async exception hierarchy
data MyAsyncException = TimeoutException | CancelException
  deriving (Show)
instance Exception MyAsyncException

-- Throw to specific thread
timeoutThread :: ThreadId -> IO ()
timeoutThread tid = throwTo tid TimeoutException

-- Handle in receiver thread
handleAsyncExceptions :: IO () -> IO ()
handleAsyncExceptions action = action `catches`
  [ Handler (\TimeoutException -> putStrLn "Timed out")
  , Handler (\CancelException  -> putStrLn "Cancelled")
  ]

-- uninterruptibleMask: no async exceptions at all
criticalSection :: IO a -> IO a
criticalSection = uninterruptibleMask_
```

---

## ขั้นตอนที่ 576: GHC Plugins Development

```haskell
-- Developing a GHC plugin

import GHC.Plugins
import GHC.Core.Opt.Monad

-- Plugin registration
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installOurPlugin
  , pluginRecompile  = purePlugin
  }

-- Install plugin in optimization pipeline
installOurPlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installOurPlugin opts todos = do
  putMsgS "Installing our plugin"
  return $ ourPass : todos
  where
    ourPass = CoreDoPasses [CoreDoPluginPass "OurPlugin" transformPass]

-- Transform Core IR
transformPass :: ModGuts -> CoreM ModGuts
transformPass guts = do
  let binds  = mg_binds guts
  binds' <- mapM transformBind binds
  return guts { mg_binds = binds' }

transformBind :: CoreBind -> CoreM CoreBind
transformBind (NonRec b e) = NonRec b <$> transformExpr e
transformBind (Rec pairs)  = Rec <$> mapM (\(b,e) -> (b,) <$> transformExpr e) pairs

-- Transform expressions
transformExpr :: CoreExpr -> CoreM CoreExpr
transformExpr expr = case expr of
  App f arg -> App <$> transformExpr f <*> transformExpr arg
  Lam b body -> Lam b <$> transformExpr body
  Let bind body -> Let <$> transformBind bind <*> transformExpr body
  -- Insert debug logging before function calls
  _ -> return expr

-- Typechecker plugin
tcPlugin :: TcPlugin
tcPlugin = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = solveTc
  , tcPluginStop  = \_ -> return ()
  }

solveTc :: () -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveTc _ _ _ wanted = do
  -- Solve wanted constraints
  return (TcPluginOk [] [])
```

---

## ขั้นตอนที่ 577: Runtime Performance Analysis

```haskell
-- Runtime performance analysis

-- 1. Measure allocation
import GHC.Stats

measureAllocation :: IO a -> IO (a, Integer)
measureAllocation action = do
  before <- allocated_bytes <$> getRTSStats
  result <- action
  after  <- allocated_bytes <$> getRTSStats
  return (result, after - before)

-- 2. Measure time with high resolution
import System.Clock

measureTime :: IO a -> IO (a, TimeSpec)
measureTime action = do
  start  <- getTime Monotonic
  result <- action
  end    <- getTime Monotonic
  return (result, diffTimeSpec end start)

-- 3. CPU cycle counting
import Data.Time.Clock.TAI

cycleBenchmark :: IO a -> IO (a, Integer)
cycleBenchmark action = do
  start  <- getCPUTime
  result <- action
  end    <- getCPUTime
  return (result, (end - start) `div` 1000)  -- picoseconds -> nanoseconds

-- 4. Core perf counters (Linux)
-- perf stat ./program
-- Reports: instructions, cache-misses, branch-misses

-- 5. GC pressure analysis
gcPressureTest :: IO ()
gcPressureTest = do
  statsBefore <- getRTSStats
  runWorkload
  statsAfter  <- getRTSStats
  let gcCount = major_gcs statsAfter - major_gcs statsBefore
  let gcTime  = gc_cpu_ns statsAfter - gc_cpu_ns statsBefore
  putStrLn $ "Major GCs: " ++ show gcCount
  putStrLn $ "GC CPU: " ++ show (gcTime `div` 1000000) ++ "ms"

-- 6. Heap snapshot comparison
-- Take snapshots at intervals
-- Compare to identify growth
```

---

## ขั้นตอนที่ 578: Demand Analysis

```haskell
-- Demand analysis: GHC's strictness inference

-- GHC infers strictness automatically with -O2
-- You can inspect with -ddump-stranal

-- Strictness annotations for manual hints
{-# LANGUAGE Strict #-}  -- make all bindings strict in this module!

-- Or selective strict
processData :: Int -> [Int] -> Int
processData !n !xs = sum xs + n  -- ! forces evaluation

-- Demand signatures in core:
-- L  = Lazy (not demanded)
-- S  = Strict (demanded to WHNF)
-- S! = Hyperstrict (demanded to NF)
-- A  = Absent (not used)
-- 1* = used at most once

-- Worker/Wrapper transformation
-- GHC automatically splits:
-- sum :: [Int] -> Int
-- into:
-- sum_wrapper :: [Int] -> Int  (handles laziness)
-- sum_worker :: Int# -> [Int] -> Int#  (strict, fast)

-- You can help GHC by using accumulator pattern
sumAccum :: [Int] -> Int
sumAccum = go 0
  where
    go !acc []     = acc         -- strict acc -> GHC can unbox it!
    go !acc (x:xs) = go (acc+x) xs

-- CPR (Constructed Product Results)
-- GHC can return unboxed tuples from strict functions
pairSum :: [Int] -> (Int, Int)
pairSum xs = (sum xs, length xs)
-- With CPR: GHC returns (Int#, Int#) internally!
```

---

## ขั้นตอนที่ 579: Haskell Binary Format

```haskell
-- Binary serialization

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.Builder

-- Define Binary instance
data Packet = Packet
  { pktVersion :: Word8
  , pktType    :: Word8
  , pktLength  :: Word32
  , pktPayload :: ByteString
  }

instance Binary Packet where
  put pkt = do
    putWord8 (pktVersion pkt)
    putWord8 (pktType pkt)
    putWord32be (pktLength pkt)
    putByteString (pktPayload pkt)
  
  get = do
    version <- getWord8
    typ     <- getWord8
    len     <- getWord32be
    payload <- getByteString (fromIntegral len)
    return (Packet version typ len payload)

-- Efficient builder pattern
buildPacket :: Packet -> BSL.ByteString
buildPacket pkt = toLazyByteString $
     singleton (pktVersion pkt)
  <> singleton (pktType pkt)
  <> word32BE (pktLength pkt)
  <> byteString (pktPayload pkt)

-- Parse with GetState
parseMultiplePackets :: ByteString -> [Packet]
parseMultiplePackets bs = go (BSL.fromStrict bs)
  where
    go remaining
      | BSL.null remaining = []
      | otherwise =
          case runGetOrFail get remaining of
            Left _             -> []
            Right (rest, _, p) -> p : go rest

-- Incremental parsing
feedParser :: Decoder Packet -> ByteString -> Decoder Packet
feedParser decoder input = pushChunk decoder input
```

---

## ขั้นตอนที่ 580: โปรเจกต์: Custom Compiler Pass

```haskell
-- Build a custom GHC optimization pass

module LoggingPlugin where

import GHC.Plugins

-- Plugin: add logging to all top-level function calls
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile  = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = return $
  CoreDoPasses [CoreDoPluginPass "AddLogging" addLogging] : todos

addLogging :: ModGuts -> CoreM ModGuts
addLogging guts = do
  binds' <- mapM (addLoggingToBind (mg_module guts)) (mg_binds guts)
  return guts { mg_binds = binds' }

addLoggingToBind :: Module -> CoreBind -> CoreM CoreBind
addLoggingToBind mod (NonRec b e) = do
  e' <- wrapWithLogging mod (varName b) e
  return (NonRec b e')
addLoggingToBind mod (Rec pairs) = do
  pairs' <- mapM (\(b,e) -> (b,) <$> wrapWithLogging mod (varName b) e) pairs
  return (Rec pairs')

-- Wrap expression with debug output
wrapWithLogging :: Module -> Name -> CoreExpr -> CoreM CoreExpr
wrapWithLogging mod name expr = do
  printId  <- lookupId printName
  strLit   <- mkStringExpr (showSDoc (ppr name))
  return $ mkCoreLet 
    (NonRec (mkWildValBinder unitTy) (App (Var printId) strLit))
    expr

-- Build and use:
-- Build: cabal build
-- Use: {-# OPTIONS_GHC -fplugin LoggingPlugin #-}
-- Every top-level call will print its name
```

---

*[← Part 28](part-28.md) | [Part 30 →](part-30.md)*
