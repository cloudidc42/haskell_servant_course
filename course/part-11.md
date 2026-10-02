# Part 11: Laziness, Strictness และ Performance
## ขั้นตอนที่ 201-220: การเข้าใจ Lazy Evaluation

---

## บทนำ

Haskell เป็น lazy language: ค่าจะไม่ถูกคำนวณจนกว่าจะต้องการจริงๆ สิ่งนี้ทำให้สามารถสร้าง infinite lists และ efficient algorithms ได้ แต่ต้องเข้าใจ performance implications

---

## ขั้นตอนที่ 201: Lazy Evaluation คืออะไร

```haskell
-- Lazy evaluation: ค่าถูกคำนวณ on demand เท่านั้น

-- Infinite list ทำงานได้เพราะ laziness
nats :: [Int]
nats = [0..]

-- เอาแค่ 10 ตัวแรก
firstTen :: [Int]
firstTen = take 10 nats  -- [0,1,2,3,4,5,6,7,8,9]

-- Sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- เพราะ laziness เราสามารถใช้ infinite list เป็น intermediate ได้
hundredthPrime :: Int
hundredthPrime = primes !! 99

-- การใช้งาน lazy lists
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- ทำงานได้เพราะ Haskell ไม่ evaluate ทั้ง list ทันที
first20Fibs :: [Integer]
first20Fibs = take 20 fibonacci

-- iterate: สร้าง infinite list จาก seed และ function
powers2 :: [Int]
powers2 = iterate (*2) 1

powersOf :: Int -> [Int]
powersOf n = iterate (*n) 1
```

---

## ขั้นตอนที่ 202: Thunks และ WHNF

```haskell
-- Thunk: delayed computation ที่ยังไม่ถูก evaluate

-- ทุกค่าใน Haskell เริ่มเป็น thunk
x :: Int
x = 2 + 3  -- thunk: computation (2 + 3)

-- เมื่อ x ถูกใช้จริง thunk จะถูก evaluate เป็น 5

-- WHNF (Weak Head Normal Form):
-- ค่าที่ถูก evaluate แค่ level แรก

-- Data.List.foldl vs foldl'
-- foldl สะสม thunks:
badSum :: [Int] -> Int
badSum = foldl (+) 0
-- foldl (+) 0 [1,2,3] =
--   foldl (+) (0+1) [2,3] =
--   foldl (+) ((0+1)+2) [3] =
--   foldl (+) (((0+1)+2)+3) [] =
--   (((0+1)+2)+3)  <- ค่อย evaluate
-- ปัญหา: สะสม thunks จำนวนมาก -> stack overflow

-- foldl' ใช้ strict evaluation:
goodSum :: [Int] -> Int
goodSum = foldl' (+) 0
-- evaluate ทุก step -> ใช้ O(1) stack

-- ดูความแตกต่าง
import Data.List (foldl')

-- ตัวอย่าง: นับ elements ใน list ขนาดใหญ่
countBad :: [a] -> Int
countBad = foldl (\acc _ -> acc + 1) 0  -- อาจ stack overflow

countGood :: [a] -> Int
countGood = foldl' (\acc _ -> acc + 1) 0  -- OK
```

---

## ขั้นตอนที่ 203: seq และ $!

```haskell
-- seq: force evaluation to WHNF
-- seq :: a -> b -> b
-- seq x y: evaluate x to WHNF, then return y

-- ใช้ seq เพื่อป้องกัน thunk buildup
strictFoldl :: (b -> a -> b) -> b -> [a] -> b
strictFoldl _ acc [] = acc
strictFoldl f acc (x:xs) =
  let acc' = f acc x
  in seq acc' (strictFoldl f acc' xs)  -- force acc' ก่อนดำเนินต่อ

-- $! operator: strict application
-- f $! x = x `seq` f x
strictResult :: Int
strictResult = (+1) $! 5  -- force 5 before applying (+1)

-- BangPatterns extension: ! in patterns
{-# LANGUAGE BangPatterns #-}
sumStrict :: [Int] -> Int
sumStrict = go 0
  where
    go !acc []     = acc           -- ! forces acc before pattern match
    go !acc (x:xs) = go (acc+x) xs

-- ตัวอย่าง: Mean ที่มี strict accumulator
mean :: [Double] -> Double
mean xs = total / fromIntegral n
  where
    (total, n) = foldl' step (0, 0) xs
    step (!t, !c) x = (t + x, c + 1)
```

---

## ขั้นตอนที่ 204: deepseq

```haskell
-- seq evaluates to WHNF เท่านั้น (head constructor เท่านั้น)
-- deepseq: fully evaluate nested structures

import Control.DeepSeq

-- NFData: type class สำหรับ full evaluation
class NFData a where
  rnf :: a -> ()  -- reduce to normal form

-- Derived instances
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Generic, NFData)

-- deepseq: force full evaluation
forceTree :: Tree Int -> Tree Int
forceTree t = t `deepseq` t

-- evaluate: safe full evaluation in IO
evaluateCompletely :: NFData a => IO a -> IO a
evaluateCompletely action = do
  result <- action
  evaluate (force result)

-- force: like deepseq but returns the value
strictCompute :: [Int] -> [Int]
strictCompute = force . map (*2)

-- ตัวอย่าง: เมื่อไหร่ใช้ deepseq
-- 1. ก่อน fork thread เพื่อ avoid sharing thunks
-- 2. ใน benchmarks
-- 3. ป้องกัน memory leak ใน long-running applications

sendResult :: NFData a => MVar a -> a -> IO ()
sendResult mvar x = do
  result <- evaluate (force x)  -- force before putting in MVar
  putMVar mvar result
```

---

## ขั้นตอนที่ 205: Strict Data Structures

```haskell
-- Strict fields ใน data types

-- Lazy (default)
data LazyPair = LazyPair Int Int

-- Strict (ด้วย !)
data StrictPair = StrictPair !Int !Int

-- Strict ทุก field
{-# LANGUAGE StrictData #-}  -- หรือใช้ extension
data Config = Config
  { host :: !String
  , port :: !Int
  , debug :: !Bool
  }

-- เมื่อสร้าง StrictPair: Int values ถูก evaluate ทันที
mkStrict :: Int -> Int -> StrictPair
mkStrict x y = StrictPair x y  -- x, y ถูก evaluate ก่อน store

-- Map: Strict vs Lazy
import qualified Data.Map.Strict as MapS  -- strict values
import qualified Data.Map.Lazy   as MapL  -- lazy values (default)

-- ใช้ Strict Map เมื่อ update บ่อย
-- Lazy Map เมื่ออาจไม่ access ทุก value

-- Unboxed types: เก็บค่าดิบโดยไม่ผ่าน pointer
import qualified Data.Vector.Unboxed as UV

-- Unboxed vector: store Int values directly (no boxing)
sumVector :: UV.Vector Int -> Int
sumVector = UV.foldl' (+) 0
```

---

## ขั้นตอนที่ 206: Memory Profiling

```haskell
-- Profiling: หา memory leaks และ bottlenecks

-- Compile ด้วย profiling:
-- ghc -prof -fprof-auto -rtsopts program.hs

-- Run ด้วย heap profiling:
-- ./program +RTS -hc -p

-- ตัวอย่าง: Memory leak ที่พบบ่อย

-- ปัญหา: Lazy accumulation
badProgram :: IO ()
badProgram = do
  let xs = [1..1000000 :: Int]
  let total = sum xs  -- thunks สะสมก่อน evaluate
  print total

-- แก้ไข: Strict
goodProgram :: IO ()
goodProgram = do
  let xs = [1..1000000 :: Int]
  let total = foldl' (+) 0 xs  -- evaluate ทันที
  print total

-- Space leak ใน Reader Monad
-- ปัญหา: Lazy state accumulation
badReader :: Int -> Int
badReader n = execState loop 0
  where
    loop = replicateM_ n (modify (+1))  -- O(n) thunks

-- แก้ไข: Strict State
goodReader :: Int -> Int
goodReader n = execState loop 0
  where
    loop = replicateM_ n (modify' (+1))  -- modify' ใช้ strict update
```

---

## ขั้นตอนที่ 207: GHC Optimization

```haskell
-- GHC Optimization levels:
-- ghc -O0: no optimization
-- ghc -O:  standard optimization
-- ghc -O2: aggressive optimization

-- Pragmas ที่มีประโยชน์

-- INLINE: บอก GHC ให้ inline function
{-# INLINE myFunction #-}
myFunction :: Int -> Int
myFunction x = x * 2 + 1

-- NOINLINE: ป้องกัน inlining
{-# NOINLINE expensiveFunction #-}
expensiveFunction :: Int -> Int
expensiveFunction x = foldl' (+) 0 [1..x]

-- SPECIALIZE: สร้าง specialized version สำหรับ concrete type
{-# SPECIALIZE sumList :: [Int] -> Int #-}
sumList :: Num a => [a] -> a
sumList = foldl' (+) 0

-- RULES: rewrite rules
{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f . g) xs #-}

-- Fusion: ป้องกัน intermediate lists
-- GHC มี list fusion rules
-- [1..n] อาจ fuse กับ map/filter/fold

-- ตัวอย่าง: Fusion
badFused :: [Int] -> Int
badFused = sum . map (*2) . filter even  -- สร้าง intermediate lists

-- GHC มักจะ fuse แต่ใช้ Vector เพื่อ guarantee
import qualified Data.Vector as V

goodFused :: V.Vector Int -> Int
goodFused v = V.foldl' (+) 0 (V.map (*2) (V.filter even v))
-- V.filter, V.map, V.foldl' fuse เป็น single pass
```

---

## ขั้นตอนที่ 208: Benchmarking ด้วย Criterion

```haskell
-- Criterion: benchmarking library

import Criterion.Main
import Criterion.Types

-- ตัวอย่าง: benchmark sort algorithms
main :: IO ()
main = defaultMain
  [ bgroup "sort"
    [ bench "insertion" $ nf insertionSort [100,99..1]
    , bench "quick"     $ nf quickSort     [100,99..1]
    , bench "merge"     $ nf mergeSort     [100,99..1]
    ]
  , bgroup "sum"
    [ bench "lazy"      $ nf (foldl  (+) 0) [1..10000 :: Int]
    , bench "strict"    $ nf (foldl' (+) 0) [1..10000 :: Int]
    ]
  ]

-- nf: force full evaluation before measuring
-- whnf: only WHNF

-- ตัวอย่าง: เปรียบเทียบ String vs Text
import qualified Data.Text as T

benchStrings :: IO ()
benchStrings = defaultMain
  [ bgroup "concat"
    [ bench "String" $ nf (concat . replicate 1000) "hello"
    , bench "Text"   $ nf (mconcat . replicate 1000) (T.pack "hello")
    ]
  ]
```

---

## ขั้นตอนที่ 209: Strictness Analyzer

```haskell
-- GHC strictness analysis: GHC วิเคราะห์ว่า argument ไหน "always needed"

-- ตัวอย่าง: GHC รู้ว่า fst always needs first element
strictFst :: (a, b) -> a
strictFst (x, _) = x  -- GHC จะ make x strict

-- ตัวอย่าง: demand analysis
data Result = Ok Int | Err String

processResult :: Result -> String
processResult (Ok n)  = "Got: " ++ show n
processResult (Err e) = "Error: " ++ e

-- GHC รู้ว่า n และ e always needed -> ทำ strict

-- Strict pragma บน type
data Strict = Strict !Int !String

-- Unpack pragma: เก็บ field ใน parent ตรงๆ (no pointer)
data Point = Point { x :: {-# UNPACK #-} !Double
                   , y :: {-# UNPACK #-} !Double }

-- ประหยัด memory: ไม่สร้าง pointer ไปที่ Double heap objects
```

---

## ขั้นตอนที่ 210: Profiling Tools

```haskell
-- เครื่องมือสำหรับ profiling Haskell

-- 1. ghc -prof: time และ space profiling
-- 2. hp2ps / hp2pretty: แปลง heap profile เป็น graph
-- 3. eventlog + threadscope: concurrent profiling
-- 4. weigh: measure allocations

import Weigh

-- ตัวอย่าง: วัด allocation
main :: IO ()
main = mainWith $ do
  func "list sum" (sum :: [Int] -> Int) [1..1000]
  func "strict sum" (foldl' (+) 0 :: [Int] -> Int) [1..1000]

-- ผลลัพธ์แสดง bytes allocated

-- GHC statistics
-- +RTS -s: แสดง GC statistics
-- +RTS -l: เขียน eventlog

-- ตัวอย่าง output:
-- 100,456 bytes allocated in the heap
-- 2,345 bytes copied during GC
-- 1,024 bytes maximum residency (1 sample(s))
```

---

## ขั้นตอนที่ 211: Lazy vs Strict Trade-offs

```haskell
-- เมื่อไหร่ใช้ lazy/strict

-- ใช้ LAZY เมื่อ:
-- 1. Potentially infinite data
-- 2. Short-circuit evaluation
-- 3. Building value once, accessing seldom

-- ตัวอย่าง: lazy short-circuit
safeAnd :: [Bool] -> Bool
safeAnd = foldr (&&) True  -- stops at first False

-- ตัวอย่าง: lazy tree traversal (only traverse what's needed)
findFirst :: (a -> Bool) -> Tree a -> Maybe a
findFirst p Leaf = Nothing
findFirst p (Node x l r)
  | p x    = Just x
  | otherwise = case findFirst p l of
      Just v  -> Just v
      Nothing -> findFirst p r

-- ใช้ STRICT เมื่อ:
-- 1. Numeric accumulation
-- 2. Counter/state that updates frequently
-- 3. Data that will be fully consumed

-- ตัวอย่าง: strict stats
data Stats = Stats
  { count :: !Int
  , total :: !Double
  , minVal :: !Double
  , maxVal :: !Double
  }

updateStats :: Stats -> Double -> Stats
updateStats (Stats c t mn mx) x = Stats
  { count  = c + 1
  , total  = t + x
  , minVal = min mn x
  , maxVal = max mx x
  }

computeStats :: [Double] -> Stats
computeStats = foldl' updateStats (Stats 0 0 maxBound minBound)
```

---

## ขั้นตอนที่ 212: การใช้ Text และ ByteString อย่างถูกต้อง

```haskell
-- String: [Char] - lazy linked list
-- ใช้สำหรับ small strings เท่านั้น

-- Text: packed array ของ Unicode characters
-- ใช้สำหรับ human-readable text

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- ByteString: packed array ของ bytes
-- ใช้สำหรับ binary data, network protocol, file I/O

-- การแปลง
textToBytes :: T.Text -> BS.ByteString
textToBytes = TE.encodeUtf8

bytesToText :: BS.ByteString -> Either String T.Text
bytesToText bs = case TE.decodeUtf8' bs of
  Left err -> Left (show err)
  Right t  -> Right t

stringToText :: String -> T.Text
stringToText = T.pack

textToString :: T.Text -> String
textToString = T.unpack

-- Lazy vs Strict versions
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

-- Lazy เหมาะสำหรับ streaming
-- Strict เหมาะสำหรับ in-memory processing

-- ตัวอย่าง: เปรียบเทียบ
-- String concatenation: O(n^2)
badConcat :: [String] -> String
badConcat = foldl (++) ""

-- Text concat: O(n)
goodConcat :: [T.Text] -> T.Text
goodConcat = T.concat

-- Builder pattern: สะสม chunks แล้ว build ครั้งเดียว
import Data.Text.Lazy.Builder as TB

buildText :: [T.Text] -> T.Text
buildText ts = TL.toStrict . TB.toLazyText $
  mconcat (map TB.fromText ts)
```

---

## ขั้นตอนที่ 213: STM และ Concurrent Performance

```haskell
-- STM performance: atomic blocks อาจ retry เมื่อ conflict

import Control.Concurrent.STM

-- High-contention counter: TVar
counter :: TVar Int -> IO ()
counter var = atomically (modifyTVar var (+1))

-- Low-contention: IORef เร็วกว่า TVar สำหรับ single-threaded
fastCounter :: IORef Int -> IO ()
fastCounter ref = modifyIORef' ref (+1)

-- ตัวอย่าง: ลด STM contention ด้วย striped counters
import Data.Array

type StripedCounter = Array Int (TVar Int)

newStriped :: Int -> IO StripedCounter
newStriped n = do
  vars <- mapM (\_ -> newTVarIO 0) [1..n]
  return (listArray (1, n) vars)

incrementStripe :: StripedCounter -> Int -> IO ()
incrementStripe arr threadId = do
  let var = arr ! ((threadId `mod` (snd (bounds arr))) + 1)
  atomically (modifyTVar var (+1))

totalCount :: StripedCounter -> IO Int
totalCount arr = atomically $ do
  vals <- mapM readTVar (elems arr)
  return (sum vals)
```

---

## ขั้นตอนที่ 214: GHC Runtime System

```haskell
-- GHC RTS: manages threads, GC, memory

-- Thread scheduling: cooperative preemption
-- GHC switches threads at safe points every ~10ms

-- Garbage Collection: generational, parallel, concurrent

-- ควบคุม GC ด้วย RTS options:
-- +RTS -Hn: set heap size (n bytes, k=KB, m=MB, g=GB)
-- +RTS -A1m: set allocation area to 1MB
-- +RTS -N4: use 4 OS threads (for parallel GC)

-- ตัวอย่าง: explicit GC
import System.Mem (performGC, performMajorGC)

cleanUp :: IO ()
cleanUp = do
  -- free large data structures first
  performMajorGC  -- force full GC

-- Weak references: ไม่ป้องกัน GC
import System.Mem.Weak

weakRef :: IO ()
weakRef = do
  ref <- newIORef "data"
  weak <- mkWeakIORef ref (putStrLn "Data collected")
  -- If nothing else holds ref, GC can collect it
  val <- deRefWeak weak
  case val of
    Just r  -> putStrLn "Still alive"
    Nothing -> putStrLn "Was collected"
```

---

## ขั้นตอนที่ 215: Parallel Programming

```haskell
-- parallel package: pure parallel computation

import Control.Parallel
import Control.Parallel.Strategies

-- par: suggest parallel evaluation
parallelAdd :: Int -> Int -> Int
parallelAdd x y = x `par` y `pseq` (x + y)
-- x evaluated in parallel with (y `pseq` (x + y))

-- using: evaluate with strategy
parFib :: Int -> Int
parFib n = runEval $ do
  left  <- rpar (fib (n-1))
  right <- rpar (fib (n-2))
  return (left + right)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

-- Strategies
-- rpar: evaluate in parallel
-- rseq: evaluate sequentially  
-- r0:   don't evaluate

-- parList: parallel list processing
parSum :: [Int] -> Int
parSum xs = sum (xs `using` parList rseq)

-- parMap: parallel map
parDouble :: [Int] -> [Int]
parDouble xs = map (*2) xs `using` parList rseq

-- chunk: divide work into chunks
parChunkSum :: Int -> [Int] -> Int
parChunkSum chunkSize xs = sum chunks `using` parList rseq
  where chunks = map sum (chunksOf chunkSize xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
```

---

## ขั้นตอนที่ 216: SIMD และ Unboxed Operations

```haskell
-- Unboxed: เก็บค่าดิบโดยไม่มี heap indirection
{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Exts

-- GHC primitive operations
addInt :: Int# -> Int# -> Int#
addInt x# y# = x# +# y#

-- Unboxed tuple
minMax :: [Int] -> (# Int, Int #)
minMax [] = (# maxBound, minBound #)
minMax (x:xs) = case minMax xs of
  (# mn, mx #) -> (# min x mn, max x mx #)

-- SIMD ด้วย vector package
import Data.Vector.SIMD

-- ปกติ GHC จะใช้ SIMD เองผ่าน vectorizer

-- Data.Primitive: low-level mutable arrays
import Data.Primitive

fastSum :: [Int] -> IO Int
fastSum xs = do
  let n = length xs
  arr <- newPrimArray n
  forM_ (zip [0..] xs) $ \(i, x) -> writePrimArray arr i x
  go arr n 0 0
  where
    go arr n idx acc
      | idx >= n  = return acc
      | otherwise = do
          x <- readPrimArray arr idx
          go arr n (idx+1) (acc+x)
```

---

## ขั้นตอนที่ 217: Compiler Warnings และ Hints

```haskell
-- GHC warnings ที่สำคัญ

-- -Wall: enable all warnings
-- -Wincomplete-patterns: ตรวจสอบ pattern matching ครบถ้วน
-- -Wunused-binds: ตรวจสอบ bindings ที่ไม่ใช้
-- -Wpartial-fields: ตรวจสอบ partial record fields

-- ตัวอย่าง: incomplete patterns
-- data Color = Red | Green | Blue

-- WARNING: incomplete pattern
-- showColor Red   = "red"
-- showColor Green = "green"
-- (Blue ไม่ match)

-- HLint: suggests improvements
-- hlint src/

-- ตัวอย่าง HLint suggestions:
-- map f (filter p xs) => [ y | x <- xs, p x, let y = f x ]
-- foldl f z xs => foldl' f z xs (ถ้า z มีขนาดใหญ่)
-- length xs == 0 => null xs

-- นำไปใช้:
betterNull :: [a] -> Bool
betterNull = null  -- ดีกว่า length xs == 0

betterConcat :: Functor f => f [a] -> [a]
betterConcat = concatMap id  -- = concat
```

---

## ขั้นตอนที่ 218: Lazy Data Structures

```haskell
-- Lazy data structures: สร้างเมื่อต้องการ

-- Lazy ByteString: linked list ของ ByteString chunks
import qualified Data.ByteString.Lazy as BL

-- ใช้สำหรับ streaming
streamFile :: FilePath -> IO ()
streamFile path = do
  content <- BL.readFile path  -- lazy: ไม่อ่านทั้งไฟล์ทันที
  BL.putStr content              -- process chunk by chunk

-- Lazy Text: เหมือนกัน
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

streamText :: FilePath -> IO ()
streamText path = do
  content <- TLIO.readFile path
  TLIO.putStr content

-- Codata: coinductive (lazy) data
-- ตรงข้ามกับ data ซึ่งเป็น inductive (finite)

-- Stream: codata list
data Stream a = Cons a (Stream a)

-- เป็น infinite เสมอ

headS :: Stream a -> a
headS (Cons x _) = x

tailS :: Stream a -> Stream a
tailS (Cons _ xs) = xs

takeS :: Int -> Stream a -> [a]
takeS 0 _         = []
takeS n (Cons x xs) = x : takeS (n-1) xs

-- Natural numbers as Stream
naturals :: Stream Int
naturals = go 0
  where go n = Cons n (go (n+1))
```

---

## ขั้นตอนที่ 219: Fusion Techniques

```haskell
-- Stream Fusion: ป้องกัน intermediate data structures

-- Build/Destroy fusion สำหรับ lists
-- GHC rewrite rules ที่ fuse map, filter, fold

-- ตัวอย่าง manual fusion:
-- badSum = sum . map f . filter p = O(3n) traversals, 2 intermediate lists
-- goodSum = foldl' (\acc x -> if p x then acc + f x else acc) 0 = O(n), 0 intermediates

-- Vector fusion (guaranteed)
import qualified Data.Vector as V

-- V.sum . V.map f . V.filter p => single pass
fusedVectorOp :: V.Vector Int -> Int
fusedVectorOp v = V.foldl' acc 0 v
  where acc s x = if even x then s + x * 2 else s

-- Conduit fusion
import Conduit

fusedConduit :: [Int] -> Int
fusedConduit xs = runConduitPure $
  yieldMany xs
  .| filterC even
  .| mapC (*2)
  .| foldlC (+) 0

-- Pipes fusion: similar
-- Streaming fusion: similar
```

---

## ขั้นตอนที่ 220: โปรเจกต์: Performance Benchmark Suite

```haskell
-- Benchmark Suite: เปรียบเทียบ implementations ต่างๆ

module BenchSuite where

import Criterion.Main
import qualified Data.Map.Strict as MapS
import qualified Data.Map.Lazy   as MapL
import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.List (foldl')

-- ======== Sort Benchmarks ========
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = filter (<x) xs
    larger  = filter (>=x) xs

-- ======== Sum Benchmarks ========
lazySum :: [Int] -> Int
lazySum = foldl (+) 0

strictSum :: [Int] -> Int
strictSum = foldl' (+) 0

vectorSum :: [Int] -> Int
vectorSum = V.foldl' (+) 0 . V.fromList

unboxedSum :: [Int] -> Int
unboxedSum = UV.foldl' (+) 0 . UV.fromList

-- ======== Map Benchmarks ========
buildStrictMap :: [(String, Int)] -> MapS.Map String Int
buildStrictMap = MapS.fromList

buildLazyMap :: [(String, Int)] -> MapL.Map String Int
buildLazyMap = MapL.fromList

buildHashMap :: [(String, Int)] -> HMap.HashMap String Int
buildHashMap = HMap.fromList

-- ======== Main ========
main :: IO ()
main = defaultMain
  [ bgroup "sum"
    [ bench "lazy"     $ nf lazySum     [1..10000]
    , bench "strict"   $ nf strictSum   [1..10000]
    , bench "vector"   $ nf vectorSum   [1..10000]
    , bench "unboxed"  $ nf unboxedSum  [1..10000]
    ]
  , bgroup "sort"
    [ bench "insertion" $ nf insertionSort [1000,999..1 :: Int]
    , bench "quick"     $ nf quickSort     [1000,999..1 :: Int]
    ]
  , bgroup "map build 1000"
    [ bench "strict"   $ nf buildStrictMap testKVs
    , bench "lazy"     $ nf buildLazyMap   testKVs
    , bench "hashmap"  $ nf buildHashMap   testKVs
    ]
  ]
  where
    testKVs :: [(String, Int)]
    testKVs = [(show i, i) | i <- [1..1000]]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 12 เราจะเรียนเรื่อง **Type System ขั้นสูง**:
- Type Classes ขั้นสูง
- Functional Dependencies
- Type Families
- Higher Kinded Types

---

*[← Part 10](part-10.md) | [Part 12 →](part-12.md)*
