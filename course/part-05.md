# Part 05: Lists, Tuples และ Data Structures
## ขั้นตอนที่ 81-100: Collections และ Data Structures ใน Haskell

---

## บทนำ

ใน Part นี้เราจะเจาะลึก Data Structures ที่สำคัญใน Haskell รวมถึง List operations ขั้นสูง, Map, Set, Sequence และ Array

---

## ขั้นตอนที่ 81: List Operations ขั้นสูง

```haskell
import Data.List
import Data.Ord (comparing, Down(..))

-- nub: ลบ duplicates (O(n^2))
ghci> nub [1,2,3,1,2,4,3,5]
[1,2,3,4,5]

-- nubBy: ลบ duplicates ด้วย custom equality
ghci> nubBy (\x y -> x `mod` 3 == y `mod` 3) [1,2,3,4,5,6]
[1,2,3]

-- sort: sort list (O(n log n))
ghci> sort [3,1,4,1,5,9,2,6,5,3,5]
[1,1,2,3,3,4,5,5,5,6,9]

-- sortBy: sort ด้วย custom comparator
ghci> sortBy (comparing length) ["banana", "apple", "fig", "cherry"]
["fig","apple","banana","cherry"]

ghci> sortBy (comparing (Down . length)) ["banana", "apple", "fig", "cherry"]
["banana","cherry","apple","fig"]

-- sortOn: sort ด้วย function (efficient)
ghci> sortOn length ["banana", "apple", "fig"]
["fig","apple","banana"]

-- group: จัดกลุ่ม consecutive equal elements
ghci> group [1,1,2,3,3,3,4,4]
[[1,1],[2],[3,3,3],[4,4]]

-- groupBy: จัดกลุ่ม consecutive elements ตาม predicate
ghci> groupBy (\x y -> even x == even y) [2,4,1,3,6,8,5]
[[2,4],[1,3],[6,8],[5]]

-- tails: all suffixes
ghci> tails [1,2,3]
[[1,2,3],[2,3],[3],[]]

-- inits: all prefixes
ghci> inits [1,2,3]
[[],[1],[1,2],[1,2,3]]

-- subsequences: all subsequences (2^n)
ghci> subsequences [1,2,3]
[[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

-- permutations: all permutations (n!)
ghci> permutations [1,2,3]
[[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
```

---

## ขั้นตอนที่ 82: Sorting Algorithms

```haskell
-- Quicksort ใน Haskell (classic, elegant แต่ไม่ efficient มากนัก)
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

ghci> quicksort [3,1,4,1,5,9,2,6,5,3,5]
[1,1,2,3,3,4,5,5,5,6,9]

-- Merge sort
mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort left) (mergesort right)
  where
    mid   = length xs `div` 2
    left  = take mid xs
    right = drop mid xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

-- Test all sorting functions
testSort :: [Int]
testSort = [5, 3, 8, 1, 9, 2, 4, 7, 6]

main :: IO ()
main = do
  print $ quicksort testSort
  print $ mergesort testSort
  print $ insertionSort testSort
```

---

## ขั้นตอนที่ 83: Data.Map.Strict

```haskell
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Map เป็น balanced BST (O(log n) operations)

-- สร้าง Map
emptyMap :: Map String Int
emptyMap = Map.empty

singletonMap :: Map String Int
singletonMap = Map.singleton "one" 1

fromListMap :: Map String Int
fromListMap = Map.fromList [("one", 1), ("two", 2), ("three", 3)]

-- Insert/Update
insert1 :: Map String Int
insert1 = Map.insert "four" 4 fromListMap

-- insertWith: merge ถ้า key มีอยู่แล้ว
insert2 :: Map String Int
insert2 = Map.insertWith (+) "one" 10 fromListMap
-- "one" -> 11

-- insertWithKey: เหมือน insertWith แต่ได้รับ key ด้วย
insert3 :: Map String Int
insert3 = Map.insertWithKey (\k new old -> length k + new + old) "one" 10 fromListMap

-- Lookup
ghci> Map.lookup "two" fromListMap
Just 2

ghci> Map.lookup "four" fromListMap
Nothing

ghci> Map.findWithDefault 0 "four" fromListMap
0

ghci> Map.member "two" fromListMap
True

ghci> Map.notMember "four" fromListMap
True

-- Delete
delete1 :: Map String Int
delete1 = Map.delete "one" fromListMap

-- Adjust/Update
adjust1 :: Map String Int
adjust1 = Map.adjust (*2) "two" fromListMap   -- "two" -> 4

update1 :: Map String Int
update1 = Map.update (\v -> if v > 1 then Just (v*2) else Nothing) "one" fromListMap

-- Map over values
doubled :: Map String Int
doubled = Map.map (*2) fromListMap

-- Map over keys and values
mapWithKey :: Map String Int
mapWithKey = Map.mapWithKey (\k v -> length k + v) fromListMap

-- Filter
evens :: Map String Int
evens = Map.filter even fromListMap

filterWithKey :: Map String Int
filterWithKey = Map.filterWithKey (\k v -> length k > 3 && v > 1) fromListMap

-- Fold
total :: Int
total = Map.foldl' (+) 0 fromListMap

-- toList, toAscList, toDescList
ghci> Map.toAscList fromListMap
[("one",1),("three",3),("two",2)]

ghci> Map.toDescList fromListMap
[("two",2),("three",3),("one",1)]

-- Union, Intersection, Difference
m1 :: Map String Int
m1 = Map.fromList [("a", 1), ("b", 2)]

m2 :: Map String Int
m2 = Map.fromList [("b", 3), ("c", 4)]

ghci> Map.union m1 m2
fromList [("a",1),("b",2),("c",4)]   -- left-biased

ghci> Map.unionWith (+) m1 m2
fromList [("a",1),("b",5),("c",4)]   -- merge with (+)

ghci> Map.intersection m1 m2
fromList [("b",2)]

ghci> Map.intersectionWith (+) m1 m2
fromList [("b",5)]

ghci> Map.difference m1 m2
fromList [("a",1)]
```

### ตัวอย่าง: Word Counter

```haskell
import qualified Data.Map.Strict as Map
import Data.Char (toLower, isAlpha)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- นับคำ
wordCount :: String -> Map.Map String Int
wordCount = foldl countWord Map.empty . words . clean
  where
    clean = map (\c -> if isAlpha c then toLower c else ' ')
    countWord acc word = Map.insertWith (+) word 1 acc

-- Top 10 words
top10 :: String -> [(String, Int)]
top10 text = take 10 . sortBy (comparing (Down . snd)) . Map.toList $ wordCount text

-- ตัวอย่าง
sampleText :: String
sampleText = "to be or not to be that is the question whether tis nobler in the mind to suffer"

ghci> top10 sampleText
[("to",3),("be",2),("the",2),("in",1),("is",1),("mind",1),("nobler",1),("not",1),("or",1),("question",1)]
```

---

## ขั้นตอนที่ 84: Data.Set

```haskell
import qualified Data.Set as Set
import Data.Set (Set)

-- Set เป็น ordered set ของ unique elements

-- สร้าง Set
emptySet :: Set Int
emptySet = Set.empty

singletonSet :: Set Int
singletonSet = Set.singleton 42

fromListSet :: Set Int
fromListSet = Set.fromList [3,1,4,1,5,9,2,6,5,3,5]
-- Set.fromList [1,2,3,4,5,6,9]  -- duplicates removed

-- Insert/Delete
insert1 :: Set Int
insert1 = Set.insert 10 fromListSet

delete1 :: Set Int
delete1 = Set.delete 3 fromListSet

-- Membership
ghci> Set.member 5 fromListSet
True

ghci> Set.notMember 10 fromListSet
True

-- Size
ghci> Set.size fromListSet
7

-- Set operations
s1 :: Set Int
s1 = Set.fromList [1,2,3,4,5]

s2 :: Set Int
s2 = Set.fromList [3,4,5,6,7]

ghci> Set.union s1 s2
fromList [1,2,3,4,5,6,7]

ghci> Set.intersection s1 s2
fromList [3,4,5]

ghci> Set.difference s1 s2
fromList [1,2]

ghci> Set.isSubsetOf (Set.fromList [1,2,3]) s1
True

-- Map over Set
ghci> Set.map (*2) fromListSet
fromList [2,4,6,8,10,12,18]

-- Filter
ghci> Set.filter even fromListSet
fromList [2,4,6]

-- Fold
ghci> Set.foldl' (+) 0 fromListSet
30

-- Convert to/from list
ghci> Set.toAscList fromListSet
[1,2,3,4,5,6,9]

-- findMin, findMax
ghci> Set.findMin fromListSet
1

ghci> Set.findMax fromListSet
9
```

### ตัวอย่าง: Graph Algorithms

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- BFS (Breadth-First Search)
type Graph = Map.Map Int [Int]

bfs :: Graph -> Int -> [Int]
bfs graph start = go (Set.singleton start) [start] [start]
  where
    go visited [] result = result
    go visited queue result =
      let neighbors = concatMap (\n -> Map.findWithDefault [] n graph) queue
          newNodes  = filter (`Set.notMember` visited) neighbors
          visited'  = foldl (flip Set.insert) visited newNodes
      in go visited' newNodes (result ++ newNodes)

-- DFS (Depth-First Search)
dfs :: Graph -> Int -> [Int]
dfs graph start = go Set.empty start
  where
    go visited node
      | Set.member node visited = []
      | otherwise               =
          node : concatMap (go visited') neighbors
      where
        visited'   = Set.insert node visited
        neighbors  = Map.findWithDefault [] node graph

exampleGraph :: Graph
exampleGraph = Map.fromList
  [ (1, [2, 3])
  , (2, [4, 5])
  , (3, [5])
  , (4, [])
  , (5, [6])
  , (6, [])
  ]

ghci> bfs exampleGraph 1
[1,2,3,4,5,6]

ghci> dfs exampleGraph 1
[1,2,4,5,6,3]
```

---

## ขั้นตอนที่ 85: Data.Sequence

```haskell
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (<|), ViewL(..), ViewR(..))

-- Sequence: finger tree สำหรับ O(1) operations ที่ endpoints
-- ดีกว่า List เมื่อต้องการ append ที่ทั้งสองด้าน

-- สร้าง Sequence
emptySeq :: Seq Int
emptySeq = Seq.empty

fromListSeq :: Seq Int
fromListSeq = Seq.fromList [1,2,3,4,5]

singletonSeq :: Seq Int
singletonSeq = Seq.singleton 42

-- Add elements
-- |> เพิ่ม element ท้าย (O(1))
appendSeq :: Seq Int
appendSeq = fromListSeq |> 6

-- <| เพิ่ม element หน้า (O(1))
prependSeq :: Seq Int
prependSeq = 0 <| fromListSeq

-- Indexing (O(log n))
ghci> Seq.index fromListSeq 2
3

-- Update (O(log n))
updated :: Seq Int
updated = Seq.update 2 99 fromListSeq
-- [1,2,99,4,5]

-- Length (O(1))
ghci> Seq.length fromListSeq
5

-- Split
(left, right) = Seq.splitAt 3 fromListSeq
-- left = [1,2,3], right = [4,5]

-- ViewL: ดู element แรก
ghci> case Seq.viewl fromListSeq of
        EmptyL  -> "empty"
        x :< xs -> "head: " ++ show x
"head: 1"

-- ViewR: ดู element สุดท้าย
ghci> case Seq.viewr fromListSeq of
        EmptyR  -> "empty"
        xs :> x -> "last: " ++ show x
"last: 5"

-- Map, filter
ghci> fmap (*2) fromListSeq
fromList [2,4,6,8,10]

-- Convert to/from list
ghci> Seq.toList fromListSeq
[1,2,3,4,5]

-- ใช้ Sequence สำหรับ Queue ที่มีประสิทธิภาพ
type Queue a = Seq a

enqueue :: a -> Queue a -> Queue a
enqueue = (|>)   -- append to back

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue q = case Seq.viewl q of
  EmptyL   -> Nothing
  x :< xs  -> Just (x, xs)
```

---

## ขั้นตอนที่ 86: Data.Array

```haskell
import Data.Array

-- Array: immutable array with O(1) random access

-- สร้าง Array จาก list (index range, list of (index, value))
myArray :: Array Int String
myArray = listArray (0, 4) ["zero", "one", "two", "three", "four"]

-- หรือใช้ array function
myArray2 :: Array Int Int
myArray2 = array (1, 5) [(i, i*i) | i <- [1..5]]

-- Access
ghci> myArray ! 2
"two"

ghci> myArray2 ! 3
9

-- Bounds
ghci> bounds myArray
(0,4)

ghci> indices myArray
[0,1,2,3,4]

ghci> elems myArray
["zero","one","two","three","four"]

-- Update (creates new array)
updated :: Array Int String
updated = myArray // [(2, "TWO"), (4, "FOUR")]

-- 2D Array
matrix :: Array (Int, Int) Int
matrix = listArray ((0,0), (2,2)) [1..9]

ghci> matrix ! (1,1)
5

ghci> [matrix ! (r,c) | r <- [0..2], c <- [0..2]]
[1,2,3,4,5,6,7,8,9]

-- IOArray สำหรับ mutable array
import Data.IORef
import Data.Array.IO

-- Mutable array
mutableExample :: IO ()
mutableExample = do
  arr <- newArray (0, 4) 0 :: IO (IOArray Int Int)
  writeArray arr 2 42
  v <- readArray arr 2
  print v   -- 42
```

---

## ขั้นตอนที่ 87: Data.IntMap และ Data.HashMap

```haskell
-- IntMap: optimized Map สำหรับ Int keys
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

myIntMap :: IntMap String
myIntMap = IntMap.fromList [(1, "one"), (2, "two"), (3, "three")]

ghci> IntMap.lookup 2 myIntMap
Just "two"

-- HashMap: hash table (O(1) average)
-- ต้องติดตั้ง 'unordered-containers' package
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

myHashMap :: HashMap String Int
myHashMap = HashMap.fromList [("one", 1), ("two", 2), ("three", 3)]

ghci> HashMap.lookup "two" myHashMap
Just 2

-- HashMap เร็วกว่า Map สำหรับ lookups
-- แต่ไม่มี ordering

-- เลือกอันไหนดี?
-- Map      : ต้องการ ordered iteration
-- HashMap  : ต้องการ lookups ที่เร็วที่สุด  
-- IntMap   : keys เป็น Int ทั้งหมด
```

---

## ขั้นตอนที่ 88: Data.Vector

```haskell
-- Vector: packed arrays (ดีกว่า list สำหรับ numeric work)
-- ติดตั้ง 'vector' package

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Boxed Vector
v :: V.Vector Int
v = V.fromList [1,2,3,4,5]

-- Unboxed Vector (เร็วกว่าสำหรับ primitive types)
uv :: UV.Vector Int
uv = UV.fromList [1,2,3,4,5]

-- Operations (คล้ายกับ list)
ghci> V.map (*2) v
[2,4,6,8,10]

ghci> V.filter even v
[2,4]

ghci> V.sum v
15

ghci> v V.! 2
3

ghci> V.length v
5

-- Generate vector
ghci> V.generate 5 (*2)
[0,2,4,6,8]

-- Slice
ghci> V.slice 1 3 v   -- (start, length)
[2,3,4]

-- Efficient operations
V.foldl' (+) 0 uv    -- very fast for numeric types
```

---

## ขั้นตอนที่ 89: List Algorithms ขั้นสูง

```haskell
-- Sliding window
windows :: Int -> [a] -> [[a]]
windows n xs
  | length window < n = []
  | otherwise         = window : windows n (tail xs)
  where window = take n xs

ghci> windows 3 [1..5]
[[1,2,3],[2,3,4],[3,4,5]]

-- Moving average
movingAverage :: Int -> [Double] -> [Double]
movingAverage n xs = map (\w -> sum w / fromIntegral n) (windows n xs)

ghci> movingAverage 3 [1,2,3,4,5,6,7]
[2.0,3.0,4.0,5.0,6.0]

-- Binary search
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch x xs = go 0 (length xs - 1)
  where
    arr = listArray (0, length xs - 1) xs
    go lo hi
      | lo > hi   = Nothing
      | arr ! mid == x = Just mid
      | arr ! mid < x  = go (mid + 1) hi
      | otherwise      = go lo (mid - 1)
      where mid = (lo + hi) `div` 2

-- Rotate list
rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

ghci> rotate 2 [1,2,3,4,5]
[3,4,5,1,2]

ghci> rotate (-1) [1,2,3,4,5]
[5,1,2,3,4]

-- Chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

ghci> chunksOf 3 [1..10]
[[1,2,3],[4,5,6],[7,8,9],[10]]

-- Flatten list of lists (already in Data.List as concat)
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- Transpose matrix
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)

ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
```

---

## ขั้นตอนที่ 90: Priority Queue

```haskell
-- Min-Heap Priority Queue
import qualified Data.Map.Strict as Map

-- Simple priority queue using Map
type PQueue a = Map.Map Int [a]

empty :: PQueue a
empty = Map.empty

insert :: Int -> a -> PQueue a -> PQueue a
insert priority item = Map.insertWith (++) priority [item]

peek :: PQueue a -> Maybe (Int, a)
peek pq = case Map.minViewWithKey pq of
  Nothing              -> Nothing
  Just ((k, vs), _)   -> Just (k, head vs)

pop :: PQueue a -> Maybe (a, PQueue a)
pop pq = case Map.minViewWithKey pq of
  Nothing -> Nothing
  Just ((k, [v]), rest) -> Just (v, rest)
  Just ((k, (v:vs)), rest) -> Just (v, Map.insert k vs rest)

-- Dijkstra's algorithm example
type Dist = Int
type Node = Int
type Graph = Map.Map Node [(Node, Dist)]

dijkstra :: Graph -> Node -> Map.Map Node Dist
dijkstra graph start = go (Map.singleton start 0) (insert 0 start empty)
  where
    go dists pq = case pop pq of
      Nothing -> dists
      Just (node, pq') ->
        let dist = Map.findWithDefault maxBound node dists
            neighbors = Map.findWithDefault [] node graph
            (dists', pq'') = foldl (relax dist) (dists, pq') neighbors
        in go dists' pq''

    relax dist (dists, pq) (neighbor, weight) =
      let newDist = dist + weight
          oldDist = Map.findWithDefault maxBound neighbor dists
      in if newDist < oldDist
           then (Map.insert neighbor newDist dists, insert newDist neighbor pq)
           else (dists, pq)
```

---

## ขั้นตอนที่ 91: Zipper Data Structure

```haskell
-- Zipper: functional data structure สำหรับ efficient navigation และ update

-- List Zipper
data ListZipper a = ListZipper [a] a [a]
  deriving (Show)

-- สร้าง zipper จาก list
fromList :: [a] -> Maybe (ListZipper a)
fromList []     = Nothing
fromList (x:xs) = Just (ListZipper [] x xs)

-- เดินไปข้างหน้า
forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper _  _ [])     = Nothing
forward (ListZipper ls x (r:rs)) = Just (ListZipper (x:ls) r rs)

-- เดินไปข้างหลัง
backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper []     _ _)  = Nothing
backward (ListZipper (l:ls) x rs) = Just (ListZipper ls l (x:rs))

-- ดูค่าปัจจุบัน
current :: ListZipper a -> a
current (ListZipper _ x _) = x

-- แก้ไขค่าปัจจุบัน
modify :: (a -> a) -> ListZipper a -> ListZipper a
modify f (ListZipper ls x rs) = ListZipper ls (f x) rs

-- แปลงกลับเป็น list
toList :: ListZipper a -> [a]
toList (ListZipper ls x rs) = reverse ls ++ [x] ++ rs

-- ตัวอย่าง
example :: Maybe [Int]
example = do
  z0 <- fromList [1,2,3,4,5]
  z1 <- forward z0
  z2 <- forward z1
  let z3 = modify (*10) z2
  z4 <- backward z3
  return (toList z4)

-- Just [1,2,30,4,5]
```

---

## ขั้นตอนที่ 92: การใช้ Data.Map สำหรับ Caching

```haskell
import qualified Data.Map.Strict as Map
import Control.Monad.State

-- Fibonacci with Map-based memoization
type Memo = Map.Map Int Integer
type FibM = State Memo Integer

fibM :: Int -> FibM
fibM 0 = return 0
fibM 1 = return 1
fibM n = do
  memo <- get
  case Map.lookup n memo of
    Just v  -> return v
    Nothing -> do
      v1 <- fibM (n - 1)
      v2 <- fibM (n - 2)
      let result = v1 + v2
      modify (Map.insert n result)
      return result

fib :: Int -> Integer
fib n = evalState (fibM n) Map.empty

ghci> fib 100
354224848179261915075

-- IORef based memoization
import Data.IORef

makeMemo :: IO (Int -> IO Integer)
makeMemo = do
  cacheRef <- newIORef (Map.fromList [(0, 0), (1, 1)])
  let compute n = do
        cache <- readIORef cacheRef
        case Map.lookup n cache of
          Just v  -> return v
          Nothing -> do
            v1 <- compute (n-1)
            v2 <- compute (n-2)
            let result = v1 + v2
            modifyIORef cacheRef (Map.insert n result)
            return result
  return compute
```

---

## ขั้นตอนที่ 93: Persistent Data Structures

```haskell
-- Haskell structures เป็น persistent โดย default
-- การ update จะสร้าง version ใหม่ ไม่ modify ของเก่า

m1 :: Map.Map String Int
m1 = Map.fromList [("a", 1), ("b", 2)]

m2 :: Map.Map String Int
m2 = Map.insert "c" 3 m1   -- m1 ยังคงเหมือนเดิม

ghci> m1
fromList [("a",1),("b",2)]

ghci> m2
fromList [("a",1),("b",2),("c",3)]

-- ประโยชน์: version history ฟรี
type Version = Int
type VersionedDB a = Map.Map Version (Map.Map String a)

initialDB :: VersionedDB Int
initialDB = Map.singleton 0 Map.empty

insertRecord :: Version -> String -> Int -> VersionedDB Int -> VersionedDB Int
insertRecord version key val db =
  let current = Map.findWithDefault Map.empty version db
      updated = Map.insert key val current
  in Map.insert (version + 1) updated db

rollback :: Version -> VersionedDB Int -> Maybe (Map.Map String Int)
rollback = Map.lookup
```

---

## ขั้นตอนที่ 94: Graph Data Structures

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Adjacency List Representation
type Vertex = Int
type Graph = Map.Map Vertex (Set.Set Vertex)

-- สร้าง empty graph
emptyGraph :: Graph
emptyGraph = Map.empty

-- เพิ่ม vertex
addVertex :: Vertex -> Graph -> Graph
addVertex v = Map.insertWith Set.union v Set.empty

-- เพิ่ม edge (directed)
addEdge :: Vertex -> Vertex -> Graph -> Graph
addEdge from to = Map.insertWith Set.union from (Set.singleton to)

-- เพิ่ม edge (undirected)
addUndirectedEdge :: Vertex -> Vertex -> Graph -> Graph
addUndirectedEdge u v = addEdge v u . addEdge u v

-- สร้าง graph จาก edges
fromEdges :: [(Vertex, Vertex)] -> Graph
fromEdges = foldl (\g (u,v) -> addEdge u v g) emptyGraph

-- Neighbors
neighbors :: Vertex -> Graph -> Set.Set Vertex
neighbors v g = Map.findWithDefault Set.empty v g

-- Degree
degree :: Vertex -> Graph -> Int
degree v g = Set.size (neighbors v g)

-- Topological sort (for DAGs)
topSort :: Graph -> [Vertex]
topSort graph = reverse $ go Set.empty (Map.keys graph) []
  where
    go visited [] result = result
    go visited (v:vs) result
      | Set.member v visited = go visited vs result
      | otherwise =
          let (visited', result') = dfs v visited result
          in go visited' vs result'

    dfs v visited result
      | Set.member v visited = (visited, result)
      | otherwise =
          let visited' = Set.insert v visited
              ns = Set.toList (neighbors v graph)
              (visited'', result') = foldl (\(vis, res) n -> dfs n vis res)
                                           (visited', result) ns
          in (visited'', v : result')

-- Example
exGraph :: Graph
exGraph = fromEdges [(1,2),(1,3),(2,4),(3,4),(4,5)]

ghci> topSort exGraph
[1,3,2,4,5]
```

---

## ขั้นตอนที่ 95: Trie Data Structure

```haskell
import qualified Data.Map.Strict as Map

-- Trie: prefix tree สำหรับ string operations

data Trie = Trie
  { isEnd    :: Bool
  , children :: Map.Map Char Trie
  } deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie False Map.empty

-- Insert word
insert :: String -> Trie -> Trie
insert [] (Trie _ cs)    = Trie True cs
insert (c:cs) (Trie e m) = Trie e (Map.alter insertChild c m)
  where
    insertChild Nothing  = Just (insert cs emptyTrie)
    insertChild (Just t) = Just (insert cs t)

-- Search exact word
search :: String -> Trie -> Bool
search [] (Trie e _)    = e
search (c:cs) (Trie _ m) = case Map.lookup c m of
  Nothing -> False
  Just t  -> search cs t

-- Search prefix
startsWith :: String -> Trie -> Bool
startsWith [] _ = True
startsWith (c:cs) (Trie _ m) = case Map.lookup c m of
  Nothing -> False
  Just t  -> startsWith cs t

-- Get all words with prefix
withPrefix :: String -> Trie -> [String]
withPrefix prefix trie = case findNode prefix trie of
  Nothing -> []
  Just t  -> map (prefix ++) (allWords t)
  where
    findNode [] t      = Just t
    findNode (c:cs) (Trie _ m) = Map.lookup c m >>= findNode cs

    allWords (Trie e m) =
      (if e then [""] else []) ++
      concatMap (\(c, t) -> map (c:) (allWords t)) (Map.toList m)

-- ตัวอย่าง
exampleTrie :: Trie
exampleTrie = foldl (flip insert) emptyTrie
  ["apple", "app", "application", "apply", "apt", "banana", "band"]

ghci> search "app" exampleTrie
True

ghci> search "ap" exampleTrie
False

ghci> startsWith "ap" exampleTrie
True

ghci> withPrefix "app" exampleTrie
["app","apple","application","apply"]
```

---

## ขั้นตอนที่ 96: Immutable Queue

```haskell
-- Persistent Queue ด้วย 2 stacks
-- O(1) amortized enqueue/dequeue

data Queue a = Queue [a] [a]
  deriving (Show)

-- Invariant: ถ้า front เป็น [] แล้ว back ก็ต้องเป็น []

emptyQueue :: Queue a
emptyQueue = Queue [] []

isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue [] _) = True
isEmptyQueue _            = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = normalize (Queue front (x:back))

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] _)    = Nothing
dequeue (Queue (x:xs) back) = Just (x, normalize (Queue xs back))

peekQueue :: Queue a -> Maybe a
peekQueue (Queue [] _)  = Nothing
peekQueue (Queue (x:_) _) = Just x

normalize :: Queue a -> Queue a
normalize (Queue [] back) = Queue (reverse back) []
normalize q               = q

-- ตัวอย่าง BFS ด้วย Queue
bfs :: (a -> [a]) -> a -> [a]
bfs expand start = go (enqueue start emptyQueue) []
  where
    go q visited = case dequeue q of
      Nothing -> reverse visited
      Just (node, q') ->
        let neighbors = expand node
            q'' = foldl (flip enqueue) q' neighbors
        in go q'' (node : visited)
```

---

## ขั้นตอนที่ 97: Data.MultiMap

```haskell
-- MultiMap: Map ที่ key หนึ่งมีหลาย values

import qualified Data.Map.Strict as Map

type MultiMap k v = Map.Map k [v]

emptyMultiMap :: MultiMap k v
emptyMultiMap = Map.empty

insertMulti :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insertMulti k v = Map.insertWith (++) k [v]

lookupMulti :: Ord k => k -> MultiMap k v -> [v]
lookupMulti k = Map.findWithDefault [] k

deleteMulti :: (Ord k, Eq v) => k -> v -> MultiMap k v -> MultiMap k v
deleteMulti k v m = case Map.lookup k m of
  Nothing -> m
  Just vs -> case filter (/= v) vs of
    []  -> Map.delete k m
    vs' -> Map.insert k vs' m

-- ตัวอย่าง: Index words
buildIndex :: [(String, Int)] -> MultiMap String Int
buildIndex = foldl (\m (word, lineNum) -> insertMulti word lineNum m) emptyMultiMap

searchWord :: String -> MultiMap String Int -> [Int]
searchWord = lookupMulti

-- Index ข้อความ
indexText :: String -> MultiMap String Int
indexText text = buildIndex $ concatMap indexLine (zip [1..] (lines text))
  where
    indexLine (n, line) = [(word, n) | word <- words line]

sampleDocument :: String
sampleDocument = unlines
  [ "haskell is a functional programming language"
  , "functional programming is powerful"
  , "haskell uses lazy evaluation"
  ]

ghci> searchWord "haskell" (indexText sampleDocument)
[1,3]

ghci> searchWord "functional" (indexText sampleDocument)
[1,2]
```

---

## ขั้นตอนที่ 98: Diff Algorithm

```haskell
import qualified Data.Map.Strict as Map
import Data.Array

-- Myers Diff Algorithm (simplified)
-- หา least edit distance ระหว่าง 2 sequences

data Edit a
  = Keep a
  | Insert a
  | Delete a
  deriving (Show, Eq)

-- Simple LCS-based diff
lcs :: Eq a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (x:xs) (y:ys)
  | x == y    = x : lcs xs ys
  | otherwise = longer (lcs xs (y:ys)) (lcs (x:xs) ys)
  where longer a b = if length a >= length b then a else b

-- Diff ด้วย LCS
diff :: Eq a => [a] -> [a] -> [Edit a]
diff old new = go old new (lcs old new)
  where
    go [] [] _        = []
    go xs [] _        = map Delete xs
    go [] ys _        = map Insert ys
    go (x:xs) (y:ys) []
      | x == y    = go xs ys []
      | otherwise = Delete x : Insert y : go xs ys []
    go (x:xs) (y:ys) (c:cs)
      | x == c && y == c = Keep x   : go xs ys cs
      | x == c           = Insert y : go (x:xs) ys (c:cs)
      | otherwise        = Delete x : go xs (y:ys) (c:cs)

ghci> diff "abcdef" "acdfg"
[Keep 'a',Delete 'b',Keep 'c',Keep 'd',Delete 'e',Keep 'f',Insert 'g']
```

---

## ขั้นตอนที่ 99: Bloom Filter

```haskell
import Data.Bits
import Data.Word
import Data.Char (ord)

-- Bloom Filter: probabilistic data structure
-- ตรวจสอบ membership แบบ fast แต่มี false positives

type BloomFilter = Word64

-- Hash functions
hash1 :: String -> Int -> Int
hash1 s size = (foldl (\h c -> h * 31 + ord c) 0 s) `mod` size

hash2 :: String -> Int -> Int
hash2 s size = (foldl (\h c -> h * 37 + ord c) 7 s) `mod` size

hash3 :: String -> Int -> Int
hash3 s size = (foldl (\h c -> h * 41 + ord c) 17 s) `mod` size

-- Insert element
bloomInsert :: String -> BloomFilter -> BloomFilter
bloomInsert s bf = bf .|. bit (hash1 s 64) .|. bit (hash2 s 64) .|. bit (hash3 s 64)

-- Check membership
bloomCheck :: String -> BloomFilter -> Bool
bloomCheck s bf =
  let bits = bit (hash1 s 64) .|. bit (hash2 s 64) .|. bit (hash3 s 64) :: Word64
  in (bf .&. bits) == bits

-- ตัวอย่าง
emptyBloom :: BloomFilter
emptyBloom = 0

example :: IO ()
example = do
  let bloom0 = emptyBloom
      bloom1 = bloomInsert "apple" bloom0
      bloom2 = bloomInsert "banana" bloom1
      bloom3 = bloomInsert "cherry" bloom2

  print $ bloomCheck "apple" bloom3    -- True
  print $ bloomCheck "banana" bloom3   -- True
  print $ bloomCheck "mango" bloom3    -- False (probably)
  print $ bloomCheck "cherry" bloom3   -- True
```

---

## ขั้นตอนที่ 100: สรุปและแบบฝึกหัด Part 05

### สิ่งที่เรียนรู้

1. ✅ List Operations ขั้นสูง (nub, sort, group, etc.)
2. ✅ Data.Map.Strict สำหรับ key-value storage
3. ✅ Data.Set สำหรับ unique elements
4. ✅ Data.Sequence สำหรับ efficient sequences
5. ✅ Data.Array สำหรับ indexed access
6. ✅ Data.Vector สำหรับ packed arrays
7. ✅ Graph Algorithms (BFS, DFS, Dijkstra)
8. ✅ Trie, Queue, Priority Queue
9. ✅ Persistent Data Structures
10. ✅ Practical algorithms

### Project: Text Analysis Tool

```haskell
module TextAnalysis where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy, nub)
import Data.Ord (comparing, Down(..))
import Data.Char (toLower, isAlpha)

-- Data types
data TextStats = TextStats
  { wordCount      :: Int
  , uniqueWords    :: Int
  , charCount      :: Int
  , sentenceCount  :: Int
  , topWords       :: [(String, Int)]
  , avgWordLength  :: Double
  } deriving (Show)

-- Clean and tokenize
tokenize :: String -> [String]
tokenize = words . map (\c -> if isAlpha c then toLower c else ' ')

-- Count words
countWords :: String -> Map.Map String Int
countWords = foldl count Map.empty . tokenize
  where count m w = Map.insertWith (+) w 1 m

-- Count sentences (simple: split by . ! ?)
countSentences :: String -> Int
countSentences = length . filter (`elem` ".!?")

-- Analyze text
analyzeText :: String -> TextStats
analyzeText text = TextStats
  { wordCount     = total
  , uniqueWords   = Map.size freq
  , charCount     = length (filter (not . (`elem` " \n\t")) text)
  , sentenceCount = countSentences text
  , topWords      = take 10 $ sortBy (comparing (Down . snd)) $ Map.toList freq
  , avgWordLength = if null ws then 0
                    else fromIntegral (sum (map length ws)) / fromIntegral (length ws)
  }
  where
    freq  = countWords text
    ws    = tokenize text
    total = sum (Map.elems freq)

-- Pretty print stats
printStats :: TextStats -> IO ()
printStats stats = do
  putStrLn "=== Text Analysis ==="
  putStrLn $ "Total words:    " ++ show (wordCount stats)
  putStrLn $ "Unique words:   " ++ show (uniqueWords stats)
  putStrLn $ "Characters:     " ++ show (charCount stats)
  putStrLn $ "Sentences:      " ++ show (sentenceCount stats)
  putStrLn $ "Avg word len:   " ++ show (avgWordLength stats)
  putStrLn "\nTop 10 words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ w ++ ": " ++ show c) (topWords stats)

-- Main
main :: IO ()
main = do
  let text = "Haskell is a purely functional programming language. " ++
             "It features strong static typing and type inference. " ++
             "Haskell supports lazy evaluation and higher-order functions. " ++
             "Pure functional programming in Haskell ensures correctness."
  printStats (analyzeText text)
```

Output:
```
=== Text Analysis ===
Total words:    35
Unique words:   25
Characters:     194
Sentences:      4
Avg word len:   7.314285714285714

Top 10 words:
  haskell: 3
  functional: 2
  programming: 2
  and: 2
  a: 1
  correctness: 1
  ensures: 1
  evaluation: 1
  features: 1
  functions: 1
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 06 เราจะเรียนเรื่อง **Higher-Order Functions** เชิงลึก:
- Function Composition patterns
- Monoid และ Foldable
- Traversable
- Functors

---

*[← Part 04](part-04.md) | [Part 06 →](part-06.md)*
