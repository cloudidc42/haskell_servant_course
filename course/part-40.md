# Part 40: Advanced Algorithms & Data Structures
## ขั้นตอนที่ 781-800

---

## ขั้นตอนที่ 781: Purely Functional Data Structures

```haskell
-- Persistent balanced BST (Red-Black Tree)

data Color = Red | Black deriving (Show, Eq)

data RBTree a
  = Leaf
  | Node Color (RBTree a) a (RBTree a)

-- Insert with rebalancing
insert :: Ord a => a -> RBTree a -> RBTree a
insert x tree = blacken (ins tree)
  where
    blacken (Node _ l v r) = Node Black l v r
    blacken Leaf           = Leaf
    
    ins Leaf = Node Red Leaf x Leaf
    ins n@(Node c l v r)
      | x < v    = balance (Node c (ins l) v r)
      | x > v    = balance (Node c l v (ins r))
      | otherwise = n

-- Rebalance cases
balance :: RBTree a -> RBTree a
balance (Node Black (Node Red (Node Red a x b) y c) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black (Node Red a x (Node Red b y c)) z d) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red (Node Red b y c) z d)) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance (Node Black a x (Node Red b y (Node Red c z d))) =
  Node Red (Node Black a x b) y (Node Black c z d)
balance n = n

-- Membership
member :: Ord a => a -> RBTree a -> Bool
member _ Leaf = False
member x (Node _ l v r)
  | x < v    = member x l
  | x > v    = member x r
  | otherwise = True

-- Convert to/from list
toList :: RBTree a -> [a]
toList Leaf          = []
toList (Node _ l v r) = toList l ++ [v] ++ toList r

fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert Leaf
```

---

## ขั้นตอนที่ 782: Finger Trees

```haskell
-- Finger Trees for efficient deques and sequences

class Measured v a | a -> v where
  measure :: a -> v

data FingerTree v a
  = Empty
  | Single a
  | Deep { ftMeasure :: v
         , prefix    :: Digit a
         , spine     :: FingerTree v (Node v a)
         , suffix    :: Digit a
         }

data Digit a = One a | Two a a | Three a a a | Four a a a a

data Node v a = Node2 v a a | Node3 v a a a

-- Amortized O(1) push/pop both ends
pushLeft :: (Measured v a, Monoid v) => a -> FingerTree v a -> FingerTree v a
pushLeft a Empty          = Single a
pushLeft a (Single b)     = deep (One a) Empty (One b)
pushLeft a (Deep _ (Four b c d e) spine suffix) =
  deep (Two a b) (pushLeft (node3 c d e) spine) suffix
pushLeft a (Deep _ prefix spine suffix) =
  deep (consDigit a prefix) spine suffix

pushRight :: (Measured v a, Monoid v) => FingerTree v a -> a -> FingerTree v a
pushRight Empty          a = Single a
pushRight (Single b)     a = deep (One b) Empty (One a)
pushRight (Deep _ prefix spine (Four b c d e)) a =
  deep prefix (pushRight spine (node3 b c d)) (Two e a)
pushRight (Deep _ prefix spine suffix) a =
  deep prefix spine (snocDigit suffix a)

-- O(log n) split at position
splitAt' :: (Measured v a, Monoid v) => (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
splitAt' p tree = case splitTree p mempty tree of
  (l, x, r) -> (l, pushLeft x r)
```

---

## ขั้นตอนที่ 783: Tries and Patricia Trees

```haskell
-- Trie for string operations

data Trie v = Trie
  { trieValue    :: Maybe v
  , trieChildren :: Map Char (Trie v)
  }

emptyTrie :: Trie v
emptyTrie = Trie Nothing Map.empty

-- Insert
trieInsert :: String -> v -> Trie v -> Trie v
trieInsert [] v trie = trie { trieValue = Just v }
trieInsert (c:cs) v trie =
  let child  = fromMaybe emptyTrie (Map.lookup c (trieChildren trie))
      child' = trieInsert cs v child
  in trie { trieChildren = Map.insert c child' (trieChildren trie) }

-- Lookup
trieLookup :: String -> Trie v -> Maybe v
trieLookup [] trie = trieValue trie
trieLookup (c:cs) trie = do
  child <- Map.lookup c (trieChildren trie)
  trieLookup cs child

-- Prefix search (autocomplete)
triePrefixSearch :: String -> Trie v -> [(String, v)]
triePrefixSearch prefix trie = case navigateTo prefix trie of
  Nothing     -> []
  Just subtrie -> collectAll prefix subtrie

navigateTo :: String -> Trie v -> Maybe (Trie v)
navigateTo []     t = Just t
navigateTo (c:cs) t = do
  child <- Map.lookup c (trieChildren t)
  navigateTo cs child

collectAll :: String -> Trie v -> [(String, v)]
collectAll prefix trie =
  let here     = [(prefix, v) | Just v <- [trieValue trie]]
      children = concatMap (\(c, subtrie) -> collectAll (prefix ++ [c]) subtrie)
                            (Map.toList (trieChildren trie))
  in here ++ children

-- Count completions
trieCount :: Trie v -> Int
trieCount trie =
  (if isJust (trieValue trie) then 1 else 0)
  + sum (map trieCount (Map.elems (trieChildren trie)))
```

---

## ขั้นตอนที่ 784: Priority Queues and Heaps

```haskell
-- Leftist heap (purely functional priority queue)

data Heap a
  = HEmpty
  | HNode { hRank  :: Int
           , hElem  :: a
           , hLeft  :: Heap a
           , hRight :: Heap a
           }

rank :: Heap a -> Int
rank HEmpty         = 0
rank (HNode r _ _ _) = r

-- Merge two heaps (O(log n))
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h HEmpty = h
merge HEmpty h = h
merge h1@(HNode _ x l1 r1) h2@(HNode _ y l2 r2)
  | x <= y    = makeNode x l1 (merge r1 h2)
  | otherwise = makeNode y l2 (merge h1 r2)

makeNode :: a -> Heap a -> Heap a -> Heap a
makeNode x l r
  | rank l >= rank r = HNode (rank r + 1) x l r
  | otherwise        = HNode (rank l + 1) x r l

-- Insert O(log n)
heapInsert :: Ord a => a -> Heap a -> Heap a
heapInsert x = merge (HNode 1 x HEmpty HEmpty)

-- Extract minimum O(log n)
heapMin :: Heap a -> Maybe a
heapMin HEmpty         = Nothing
heapMin (HNode _ x _ _) = Just x

heapDeleteMin :: Ord a => Heap a -> Maybe (a, Heap a)
heapDeleteMin HEmpty          = Nothing
heapDeleteMin (HNode _ x l r) = Just (x, merge l r)

-- Build heap from list O(n)
heapFromList :: Ord a => [a] -> Heap a
heapFromList = foldr heapInsert HEmpty

-- Heap sort
heapSort :: Ord a => [a] -> [a]
heapSort = unfoldr heapDeleteMin . heapFromList
```

---

## ขั้นตอนที่ 785: Graph Algorithms

```haskell
-- Graph algorithms

import Data.Graph (Graph, Vertex, graphFromEdges, topSort, reachable)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- Adjacency list graph
type AdjGraph = Map Vertex [Vertex]

-- BFS
bfs :: AdjGraph -> Vertex -> [Vertex]
bfs graph start = go (Seq.singleton start) (Set.singleton start) []
  where
    go queue visited result
      | Seq.null queue = reverse result
      | otherwise =
          let v    = Seq.index queue 0
              rest = Seq.drop 1 queue
              nbrs = filter (`Set.notMember` visited) (fromMaybe [] (Map.lookup v graph))
              queue'   = foldl (|>) rest nbrs
              visited' = foldl (flip Set.insert) visited nbrs
          in go queue' visited' (v : result)

-- DFS
dfs :: AdjGraph -> Vertex -> [Vertex]
dfs graph start = go [start] Set.empty []
  where
    go [] _ result = reverse result
    go (v:stack) visited result
      | Set.member v visited = go stack visited result
      | otherwise =
          let nbrs    = fromMaybe [] (Map.lookup v graph)
              visited' = Set.insert v visited
          in go (nbrs ++ stack) visited' (v : result)

-- Dijkstra's shortest path
dijkstra :: Map Vertex [(Vertex, Int)] -> Vertex -> Map Vertex Int
dijkstra graph source =
  let dist0 = Map.singleton source 0
      heap0  = heapFromList [(0, source)]
  in go heap0 dist0
  where
    go heap dist = case heapDeleteMin heap of
      Nothing -> dist
      Just ((d, v), heap') ->
        let nbrs = fromMaybe [] (Map.lookup v graph)
            (heap'', dist') = foldl (relax d) (heap', dist) nbrs
        in go heap'' dist'
    
    relax d (heap, dist) (u, w) =
      let newDist = d + w
          oldDist = fromMaybe maxBound (Map.lookup u dist)
      in if newDist < oldDist
         then (heapInsert (newDist, u) heap, Map.insert u newDist dist)
         else (heap, dist)
```

---

## ขั้นตอนที่ 786: Dynamic Programming

```haskell
-- Dynamic programming patterns

-- Memoization with Data.Map
memo :: Ord k => (k -> r) -> k -> r
memo f = let cache = Map.empty
         in \k -> case Map.lookup k cache of
              Just v  -> v
              Nothing -> f k

-- Memoized Fibonacci
fib :: Int -> Integer
fib = (map fibs [0..] !!)
  where fibs 0 = 0
        fibs 1 = 1
        fibs n = fib (n-1) + fib (n-2)

-- Longest common subsequence
lcs :: Eq a => [a] -> [a] -> [a]
lcs xs ys = dp (length xs) (length ys)
  where
    xArr = listArray (1, length xs) xs
    yArr = listArray (1, length ys) ys
    
    table = array ((0,0), (length xs, length ys))
      [ ((i,j), cell i j) | i <- [0..length xs], j <- [0..length ys] ]
    
    cell 0 _ = []
    cell _ 0 = []
    cell i j
      | xArr ! i == yArr ! j = table ! (i-1, j-1) ++ [xArr ! i]
      | length (table ! (i-1, j)) >= length (table ! (i, j-1)) = table ! (i-1, j)
      | otherwise = table ! (i, j-1)
    
    dp i j = table ! (i, j)

-- Knapsack problem
knapsack :: Int -> [(Int, Int)] -> Int
knapsack capacity items = dp capacity (length items)
  where
    itemArr = listArray (1, length items) items
    
    table = array ((0,0), (capacity, length items))
      [ ((w,i), cell w i) | w <- [0..capacity], i <- [0..length items] ]
    
    cell _ 0 = 0
    cell w i =
      let (wi, vi) = itemArr ! i
      in if wi > w
         then table ! (w, i-1)
         else max (table ! (w, i-1)) (vi + table ! (w - wi, i-1))
    
    dp w i = table ! (w, i)
```

---

## ขั้นตอนที่ 787: String Algorithms

```haskell
-- String matching algorithms

-- KMP failure function
kmpFailure :: Eq a => [a] -> Array Int Int
kmpFailure pattern =
  let n = length pattern
      patArr = listArray (0, n-1) pattern
      f = array (0, n-1) (zip [0..] (0 : go 1 0))
  in f
  where
    n      = length pattern
    patArr = listArray (0, n-1) pattern
    
    go i k
      | i == n    = []
      | patArr ! i == patArr ! k = (k+1) : go (i+1) (k+1)
      | k > 0     = go i (f ! (k-1))
      | otherwise = 0 : go (i+1) 0
    f = kmpFailure pattern

-- KMP search O(n+m)
kmpSearch :: Eq a => [a] -> [a] -> [Int]
kmpSearch text pattern = go 0 0 []
  where
    n      = length text
    m      = length pattern
    tArr   = listArray (0, n-1) text
    pArr   = listArray (0, m-1) pattern
    f      = kmpFailure pattern
    
    go i k results
      | i == n    = reverse results
      | tArr ! i == pArr ! k =
          if k+1 == m
            then go (i+1) (f ! (k)) ((i - m + 1) : results)
            else go (i+1) (k+1) results
      | k > 0     = go i (f ! (k-1)) results
      | otherwise = go (i+1) 0 results

-- Edit distance (Levenshtein)
editDistance :: Eq a => [a] -> [a] -> Int
editDistance s1 s2 = dp (length s1) (length s2)
  where
    a1 = listArray (1, length s1) s1
    a2 = listArray (1, length s2) s2
    
    table = array ((0,0), (length s1, length s2))
      [ ((i,j), cell i j) | i <- [0..length s1], j <- [0..length s2] ]
    
    cell 0 j = j
    cell i 0 = i
    cell i j
      | a1 ! i == a2 ! j = table ! (i-1, j-1)
      | otherwise = 1 + minimum
          [ table ! (i-1, j)    -- delete
          , table ! (i, j-1)    -- insert
          , table ! (i-1, j-1)  -- replace
          ]
    
    dp i j = table ! (i, j)
```

---

## ขั้นตอนที่ 788: Segment Trees and Fenwick Trees

```haskell
-- Segment Tree for range queries

data SegTree a = SegTree
  { stData  :: IOArray Int a
  , stN     :: Int
  , stOp    :: a -> a -> a
  , stIdent :: a
  }

buildSegTree :: (a -> a -> a) -> a -> [a] -> IO (SegTree a)
buildSegTree op identity xs = do
  let n    = length xs
  arr <- newArray (1, 4 * n) identity
  buildST arr 1 1 n (listArray (1, n) xs) op
  return (SegTree arr n op identity)

buildST :: IOArray Int a -> Int -> Int -> Int -> Array Int a -> (a -> a -> a) -> IO ()
buildST arr node l r xs op
  | l == r    = writeArray arr node (xs ! l)
  | otherwise = do
      let mid = (l + r) `div` 2
      buildST arr (2*node) l mid xs op
      buildST arr (2*node+1) (mid+1) r xs op
      v1 <- readArray arr (2*node)
      v2 <- readArray arr (2*node+1)
      writeArray arr node (op v1 v2)

-- Range query O(log n)
queryRange :: SegTree a -> Int -> Int -> IO a
queryRange st ql qr = queryST (stData st) 1 1 (stN st) ql qr (stOp st) (stIdent st)

queryST :: IOArray Int a -> Int -> Int -> Int -> Int -> Int -> (a -> a -> a) -> a -> IO a
queryST arr node l r ql qr op identity
  | ql > r || qr < l = return identity
  | ql <= l && r <= qr = readArray arr node
  | otherwise = do
      let mid = (l + r) `div` 2
      v1 <- queryST arr (2*node) l mid ql qr op identity
      v2 <- queryST arr (2*node+1) (mid+1) r ql qr op identity
      return (op v1 v2)

-- Fenwick Tree (BIT) - prefix sums
type FenwickTree = IOArray Int Int

fenwickUpdate :: FenwickTree -> Int -> Int -> IO ()
fenwickUpdate tree i delta = do
  n <- (snd . bounds) <$> getBounds tree
  go i
  where
    go i | i > n     = return ()
         | otherwise = do
             modifyArray tree i (+delta)
             go (i + (i .&. (-i)))

fenwickQuery :: FenwickTree -> Int -> IO Int
fenwickQuery tree i = go i 0
  where
    go 0 acc = return acc
    go i acc = do
      v <- readArray tree i
      go (i - (i .&. (-i))) (acc + v)
```

---

## ขั้นตอนที่ 789: Union-Find (Disjoint Sets)

```haskell
-- Union-Find with path compression and rank

import Data.IORef
import qualified Data.Map.Strict as Map

data UnionFind = UnionFind
  { ufParent :: IOArray Int Int
  , ufRank   :: IOArray Int Int
  , ufCount  :: IORef Int  -- number of components
  }

newUnionFind :: Int -> IO UnionFind
newUnionFind n = do
  parent <- newListArray (0, n-1) [0..n-1]
  rank   <- newArray (0, n-1) 0
  count  <- newIORef n
  return (UnionFind parent rank count)

-- Find with path compression
find :: UnionFind -> Int -> IO Int
find uf x = do
  px <- readArray (ufParent uf) x
  if px == x
    then return x
    else do
      root <- find uf px
      writeArray (ufParent uf) x root  -- path compression
      return root

-- Union by rank
union :: UnionFind -> Int -> Int -> IO Bool
union uf x y = do
  rx <- find uf x
  ry <- find uf y
  if rx == ry
    then return False
    else do
      rankX <- readArray (ufRank uf) rx
      rankY <- readArray (ufRank uf) ry
      case compare rankX rankY of
        LT -> writeArray (ufParent uf) rx ry
        GT -> writeArray (ufParent uf) ry rx
        EQ -> do
          writeArray (ufParent uf) ry rx
          modifyArray (ufRank uf) rx (+1)
      modifyIORef (ufCount uf) (subtract 1)
      return True

-- Kruskal's MST using Union-Find
kruskal :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
kruskal n edges = unsafePerformIO $ do
  uf <- newUnionFind n
  let sorted = sortBy (comparing (\(w,_,_) -> w)) edges
  filterM (\(_, u, v) -> union uf u v) sorted
```

---

## ขั้นตอนที่ 790: Sorting and Selection

```haskell
-- Advanced sorting algorithms

-- Merge sort (stable)
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  =
  let (l, r) = splitAt (length xs `div` 2) xs
  in mergeSorted (mergeSort l) (mergeSort r)

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x:xs) (y:ys)
  | x <= y    = x : mergeSorted xs (y:ys)
  | otherwise = y : mergeSorted (x:xs) ys

-- Quicksort (three-way partition for duplicates)
quickSort3 :: Ord a => [a] -> [a]
quickSort3 [] = []
quickSort3 (x:xs) =
  let lt = filter (< x) xs
      eq = filter (== x) xs
      gt = filter (> x) xs
  in quickSort3 lt ++ [x] ++ eq ++ quickSort3 gt

-- Counting sort (for integer keys)
countingSort :: Int -> Int -> [Int] -> [Int]
countingSort lo hi xs =
  let counts = accumArray (+) 0 (lo, hi) (map (\x -> (x, 1)) xs)
  in concatMap (\i -> replicate (counts ! i) i) [lo..hi]

-- Quickselect (nth element in O(n) average)
quickSelect :: Ord a => [a] -> Int -> a
quickSelect [x] _ = x
quickSelect (x:xs) k =
  let lt = filter (< x) xs
      eq = filter (== x) xs  -- includes x
      gt = filter (> x) xs
      nLt = length lt
      nEq = length eq + 1
  in if k < nLt
     then quickSelect lt k
     else if k < nLt + nEq
          then x
          else quickSelect gt (k - nLt - nEq)
```

---

## ขั้นตอนที่ 791: Computational Geometry

```haskell
-- Computational geometry algorithms

data Point = Point { px :: Double, py :: Double } deriving (Show, Eq)

-- Cross product of vectors OA and OB
cross :: Point -> Point -> Point -> Double
cross o a b = (px a - px o) * (py b - py o) - (py a - py o) * (px b - px o)

-- Convex hull (Graham scan) O(n log n)
convexHull :: [Point] -> [Point]
convexHull points
  | length points < 3 = points
  | otherwise =
      let sorted = sortBy comparePoints points
          lower  = buildHalf sorted
          upper  = buildHalf (reverse sorted)
      in nub (lower ++ upper)
  where
    comparePoints a b = compare (px a) (px b) <> compare (py a) (py b)
    
    buildHalf = foldl' addPoint []
    
    addPoint [] p = [p]
    addPoint [q] p = [q, p]
    addPoint hull@(r:q:rest) p
      | cross q r p <= 0 = addPoint (q:rest) p
      | otherwise        = p : hull

-- Line segment intersection
segmentsIntersect :: Point -> Point -> Point -> Point -> Bool
segmentsIntersect a b c d =
  let d1 = cross a b c
      d2 = cross a b d
      d3 = cross c d a
      d4 = cross c d b
  in ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) &&
     ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))

-- Polygon area (Shoelace formula)
polygonArea :: [Point] -> Double
polygonArea points = abs (0.5 * sum (zipWith (\a b -> px a * py b - px b * py a) points (tail points ++ [head points])))

-- Point in polygon (ray casting)
pointInPolygon :: Point -> [Point] -> Bool
pointInPolygon p polygon = odd (length (filter crosses edges))
  where
    edges   = zip polygon (tail polygon ++ [head polygon])
    crosses (a, b) =
      ((py a > py p) /= (py b > py p)) &&
      (px p < (px b - px a) * (py p - py a) / (py b - py a) + px a)
```

---

## ขั้นตอนที่ 792: Number Theory

```haskell
-- Number theory algorithms

-- GCD and extended GCD
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

extGcd :: Integer -> Integer -> (Integer, Integer, Integer)
extGcd 0 b = (b, 0, 1)
extGcd a b =
  let (g, x, y) = extGcd (b `mod` a) a
  in (g, y - (b `div` a) * x, x)

-- Modular inverse
modInverse :: Integer -> Integer -> Maybe Integer
modInverse a m =
  let (g, x, _) = extGcd (a `mod` m) m
  in if g == 1 then Just ((x `mod` m + m) `mod` m) else Nothing

-- Miller-Rabin primality test
isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (millerRabin n) witnesses
  where
    witnesses = [2, 3, 5, 7, 11, 13, 17, 19, 23]
    
    (d, s) = factorOut2 (n - 1)
    
    factorOut2 x
      | even x    = let (d', s') = factorOut2 (x `div` 2) in (d', s' + 1)
      | otherwise = (x, 0)
    
    millerRabin n a
      | x == 1 || x == n - 1 = True
      | otherwise = go (s - 1) x
      where
        x = modPow a d n
        
        go 0 _ = False
        go r x
          | y == n - 1 = True
          | y == 1     = False
          | otherwise  = go (r-1) y
          where y = modPow x 2 n

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 _ = 1
modPow base exp' mod'
  | even exp' = let half = modPow base (exp' `div` 2) mod'
                in (half * half) `rem` mod'
  | otherwise = (base * modPow base (exp' - 1) mod') `rem` mod'

-- Sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
```

---

## ขั้นตอนที่ 793: Combinatorics

```haskell
-- Combinatorics

-- Binomial coefficient with memoization
binom :: Int -> Int -> Integer
binom n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise = table !! n !! k
  where
    table = [[binom' n k | k <- [0..n]] | n <- [0..]]
    binom' n k = binom (n-1) (k-1) + binom (n-1) k

-- Permutations
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs =
  [ x : rest
  | x    <- xs
  , rest <- permutations' (delete x xs)
  ]

-- Combinations
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) =
  map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Partition number (number of ways to partition n)
partition :: Int -> Int
partition n = table !! n
  where
    table = map partitionN [0..]
    partitionN 0 = 1
    partitionN n = sum
      [ table !! (n - k*(3*k-1)`div`2) + table !! (n - k*(3*k+1)`div`2)
      | k <- [1..n]
      , n - k*(3*k-1)`div`2 >= 0
      ]

-- Catalan numbers
catalan :: Int -> Integer
catalan n = binom (2*n) n `div` fromIntegral (n + 1)
```

---

## ขั้นตอนที่ 794: Cache-Efficient Algorithms

```haskell
-- Cache-efficient algorithm patterns

-- Blocked matrix multiply
blockMatMul :: Int -> Matrix Double -> Matrix Double -> Matrix Double
blockMatMul blockSize a b =
  let n = rows a
  in runST $ do
    result <- newMatrix 0 n n
    forM_ [0, blockSize..n-1] $ \ii ->
      forM_ [0, blockSize..n-1] $ \jj ->
        forM_ [0, blockSize..n-1] $ \kk ->
          let iEnd = min n (ii + blockSize)
              jEnd = min n (jj + blockSize)
              kEnd = min n (kk + blockSize)
          in forM_ [ii..iEnd-1] $ \i ->
               forM_ [jj..jEnd-1] $ \j ->
                 forM_ [kk..kEnd-1] $ \k ->
                   modifyMatrix result i j (+ (a `at` (i, k)) * (b `at` (k, j)))
    freeze result

-- Streaming algorithms (count distinct with HyperLogLog approximation)
data HyperLogLog = HyperLogLog
  { hllRegisters :: IOArray Int Int
  , hllM         :: Int
  }

addHll :: HyperLogLog -> Int -> IO ()
addHll hll x = do
  let h     = hash x
  let reg   = h `shiftR` (64 - truncate (logBase 2 (fromIntegral (hllM hll))))
  let zeros = countLeadingZeros (h `shiftL` truncate (logBase 2 (fromIntegral (hllM hll))))
  old <- readArray (hllRegisters hll) reg
  when (zeros + 1 > old) (writeArray (hllRegisters hll) reg (zeros + 1))

estimateCardinality :: HyperLogLog -> IO Double
estimateCardinality hll = do
  regs <- getElems (hllRegisters hll)
  let m    = fromIntegral (hllM hll)
  let z    = 1 / sum (map (\r -> 2 ** fromIntegral (-r)) regs)
  let est  = 0.7213 / (1 + 1.079 / m) * m * m * z
  return est
```

---

## ขั้นตอนที่ 795: Parallel Algorithms

```haskell
-- Parallel algorithms with Control.Parallel.Strategies

import Control.Parallel.Strategies

-- Parallel map
parMap' :: (a -> b) -> [a] -> [b]
parMap' f xs = map f xs `using` parList rdeepseq

-- Parallel fold (parallel prefix sum / scan)
parFold :: (Monoid m) => (a -> m) -> [a] -> m
parFold f xs
  | length xs <= threshold = foldMap f xs
  | otherwise =
      let (l, r) = splitAt (length xs `div` 2) xs
          (lm, rm) = (parFold f l, parFold f r) `using` parTuple2 rdeepseq rdeepseq
      in lm <> rm
  where threshold = 1000

-- Parallel quicksort
parQuickSort :: (Ord a, NFData a) => [a] -> [a]
parQuickSort [] = []
parQuickSort (x:xs) =
  let lt = filter (< x) xs
      gt = filter (>= x) xs
      (sortedLt, sortedGt) = (parQuickSort lt, parQuickSort gt) `using`
                              parTuple2 rdeepseq rdeepseq
  in sortedLt ++ [x] ++ sortedGt

-- Parallel matrix-vector multiply
parMatVec :: Matrix Double -> Vector Double -> Vector Double
parMatVec mat vec =
  let rowResults = parMap' (\row -> row `dot` vec) (toRows mat)
  in fromList rowResults

-- Work stealing with STM
data WorkQueue a = WorkQueue
  { wqDeque :: TVar (Seq a)
  }

steal :: WorkQueue a -> IO (Maybe a)
steal wq = atomically $ do
  deque <- readTVar (wqDeque wq)
  case Seq.viewr deque of
    Seq.EmptyR   -> return Nothing
    rest Seq.:> x -> writeTVar (wqDeque wq) rest >> return (Just x)
```

---

## ขั้นตอนที่ 796: Probabilistic Data Structures

```haskell
-- Probabilistic data structures

-- Bloom filter
data BloomFilter a = BloomFilter
  { bfBitset  :: IOArray Int Bool
  , bfSize    :: Int
  , bfHashes  :: [a -> Int]
  }

newBloomFilter :: Int -> Int -> IO (BloomFilter a)
newBloomFilter size numHashes = do
  bits <- newArray (0, size - 1) False
  return (BloomFilter bits size (take numHashes [hashWithSeed i | i <- [0..]]))

bloomInsert :: BloomFilter a -> a -> IO ()
bloomInsert bf item = do
  let positions = map (\h -> h item `mod` bfSize bf) (bfHashes bf)
  mapM_ (\pos -> writeArray (bfBitset bf) pos True) positions

bloomMember :: BloomFilter a -> a -> IO Bool
bloomMember bf item = do
  let positions = map (\h -> h item `mod` bfSize bf) (bfHashes bf)
  bits <- mapM (readArray (bfBitset bf)) positions
  return (and bits)

-- Count-Min Sketch
data CountMinSketch = CountMinSketch
  { cmsTable  :: IOArray (Int, Int) Int
  , cmsDepth  :: Int
  , cmsWidth  :: Int
  , cmsHashes :: [Int -> Int]
  }

cmsIncrement :: CountMinSketch -> Int -> IO ()
cmsIncrement cms item = do
  let positions = zipWith (\d h -> (d, h item `mod` cmsWidth cms)) [0..] (cmsHashes cms)
  mapM_ (\pos -> modifyArray (cmsTable cms) pos (+1)) positions

cmsQuery :: CountMinSketch -> Int -> IO Int
cmsQuery cms item = do
  let positions = zipWith (\d h -> (d, h item `mod` cmsWidth cms)) [0..] (cmsHashes cms)
  values <- mapM (readArray (cmsTable cms)) positions
  return (minimum values)
```

---

## ขั้นตอนที่ 797: Algorithm Complexity Analysis

```haskell
-- Benchmarking and complexity analysis

import Criterion.Main

-- Benchmark different sort implementations
benchSorts :: [Int] -> IO ()
benchSorts xs = defaultMain
  [ bgroup "sorting"
      [ bench "mergeSort"     (nf mergeSort xs)
      , bench "quickSort"     (nf quickSort3 xs)
      , bench "heapSort"      (nf heapSort xs)
      , bench "list sort"     (nf sort xs)
      ]
  ]

-- Complexity class analysis
data Complexity
  = O1       -- O(1) constant
  | OLogN    -- O(log n)
  | ON       -- O(n)
  | ONLogN   -- O(n log n)
  | ON2      -- O(n^2)
  | ON3      -- O(n^3)
  | O2N      -- O(2^n) exponential
  deriving (Show, Eq, Ord)

-- Algorithm info
data AlgorithmInfo = AlgorithmInfo
  { aiName        :: Text
  , aiTimeAvg     :: Complexity
  , aiTimeWorst   :: Complexity
  , aiSpace       :: Complexity
  , aiStable      :: Bool
  , aiDescription :: Text
  }

sortAlgorithms :: [AlgorithmInfo]
sortAlgorithms =
  [ AlgorithmInfo "QuickSort"    ONLogN ON2   OLogN False "Divide and conquer, in-place"
  , AlgorithmInfo "MergeSort"    ONLogN ONLogN ON   True  "Divide and conquer, stable"
  , AlgorithmInfo "HeapSort"     ONLogN ONLogN O1   False "Selection, in-place"
  , AlgorithmInfo "InsertionSort" ON2   ON2   O1   True  "Best for small/nearly sorted"
  , AlgorithmInfo "CountingSort" ON     ON    ON   True  "Non-comparison, integers"
  , AlgorithmInfo "RadixSort"    ON     ON    ON   True  "Non-comparison, fixed-width keys"
  ]
```

---

## ขั้นตอนที่ 798: Space-Efficient Algorithms

```haskell
-- Space-efficient patterns

-- Tortoise and hare (cycle detection)
floydCycleDetect :: Eq a => (a -> a) -> a -> Maybe (Int, Int)
floydCycleDetect f start = do
  -- Find meeting point
  let pairs = iterate (\(t, h) -> (f t, f (f h))) (f start, f (f start))
  let (tortoise, hare) = head (filter (\(t, h) -> t == h) pairs)
  
  -- Find cycle start
  let pairs2 = iterate (\(a, b) -> (f a, f b)) (start, hare)
  let (start', _) = head (filter (\(a, b) -> a == b) pairs2)
  let mu = length (takeWhile (\(a, _) -> a /= start') (iterate (\(a, b) -> (f a, f b)) (start, hare)))
  
  -- Find cycle length
  let lambda = 1 + length (takeWhile (\x -> f x /= start') (iterate f (f start')))
  
  return (mu, lambda)

-- Iterative deepening DFS (IDDFS) - O(n) space
iddfs :: (a -> Bool) -> (a -> [a]) -> Int -> a -> Maybe a
iddfs isGoal expand maxDepth start =
  foldr (\depth found -> found <|> dls depth start) Nothing [0..maxDepth]
  where
    dls 0 node = if isGoal node then Just node else Nothing
    dls d node
      | isGoal node = Just node
      | otherwise   = foldr (\child found -> found <|> dls (d-1) child) Nothing (expand node)

-- Meet in the middle
meetInMiddle :: (Hashable a, Ord a) => [a] -> [a] -> (a -> a -> Bool) -> [(a, a)]
meetInMiddle left right compatible =
  let leftSet = Set.fromList left
  in [ (r, l) | r <- right, l <- Set.toList leftSet, compatible l r ]
```

---

## ขั้นตอนที่ 799: Online Algorithms

```haskell
-- Online/streaming algorithms

-- Moving average
data MovingAvg = MovingAvg
  { maBuffer :: Seq Double
  , maWindow :: Int
  , maSum    :: Double
  }

newMovingAvg :: Int -> MovingAvg
newMovingAvg window = MovingAvg Seq.empty window 0

addToMovingAvg :: MovingAvg -> Double -> (MovingAvg, Double)
addToMovingAvg ma x =
  let buf' = maBuffer ma |> x
      sum' = maSum ma + x
      (buf'', sum'') = if Seq.length buf' > maWindow ma
            then (Seq.drop 1 buf', sum' - Seq.index buf' 0)
            else (buf', sum')
      avg = sum'' / fromIntegral (Seq.length buf'')
  in (MovingAvg buf'' (maWindow ma) sum'', avg)

-- Online median (two heaps)
data OnlineMedian = OnlineMedian
  { omMaxHeap :: Heap Double   -- lower half
  , omMinHeap :: Heap Double   -- upper half (negated)
  }

addToOnlineMedian :: OnlineMedian -> Double -> (OnlineMedian, Double)
addToOnlineMedian om x =
  let -- Add to appropriate heap
      (maxH, minH) = case heapMin (omMaxHeap om) of
        Nothing -> (heapInsert x (omMaxHeap om), omMinHeap om)
        Just top -> if x <= top
          then (heapInsert x (omMaxHeap om), omMinHeap om)
          else (omMaxHeap om, heapInsert (-x) (omMinHeap om))
      
      -- Rebalance
      (maxH', minH') = rebalance maxH minH
      
      median = case (heapMin maxH', heapMin minH') of
        (Just a, Just b)
          | heapSize maxH' == heapSize minH' -> (a + (-b)) / 2
          | heapSize maxH' > heapSize minH'  -> a
          | otherwise                        -> -b
        (Just a, Nothing) -> a
        _                 -> 0  -- shouldn't happen

  in (OnlineMedian maxH' minH', median)
```

---

## ขั้นตอนที่ 800: โปรเจกต์: Algorithm Benchmark Suite

```haskell
-- Complete algorithm benchmark and correctness testing

module AlgorithmBenchmark where

import Criterion.Main
import Test.QuickCheck
import Data.List (sort, nub)

-- Properties for sorting algorithms
prop_sortOrdered :: [Int] -> Bool
prop_sortOrdered xs = isSorted (mergeSort xs)
  where isSorted [] = True
        isSorted [_] = True
        isSorted (a:b:rest) = a <= b && isSorted (b:rest)

prop_sortSameElements :: [Int] -> Bool
prop_sortSameElements xs = sort xs == mergeSort xs

prop_sortStable :: [(Int, Int)] -> Bool
prop_sortStable pairs =
  let sorted = sortBy (comparing fst) pairs
      merged = mergeSort pairs
  in all (\((k1, v1), (k2, v2)) -> k1 /= k2 || v1 == v2) (zip sorted merged)

-- Properties for graph algorithms
prop_bfsReachable :: Vertex -> AdjGraph -> Bool
prop_bfsReachable v g = all (\u -> isReachable g v u) (bfs g v)
  where isReachable g src dst = dst `elem` bfs g src

-- Comprehensive benchmark
main :: IO ()
main = do
  -- Test correctness first
  quickCheck prop_sortOrdered
  quickCheck prop_sortSameElements
  
  -- Generate test data
  let small  = [1..100]   :: [Int]
  let medium = [1..10000] :: [Int]
  let large  = [1..100000] :: [Int]
  
  -- Randomize
  smallRand  <- shuffleIO small
  mediumRand <- shuffleIO medium
  largeRand  <- shuffleIO large
  
  defaultMain
    [ bgroup "sort-100"
        [ bench "merge"   (nf mergeSort   smallRand)
        , bench "quick3"  (nf quickSort3  smallRand)
        , bench "heap"    (nf heapSort    smallRand)
        ]
    , bgroup "sort-10k"
        [ bench "merge"   (nf mergeSort   mediumRand)
        , bench "quick3"  (nf quickSort3  mediumRand)
        ]
    , bgroup "search-structures"
        [ bench "rbtree-insert-1000"  (nf (foldr insert Leaf) (take 1000 mediumRand))
        , bench "trie-insert-1000"    (nf (foldr (\w t -> trieInsert w w t) emptyTrie) (map show (take 1000 mediumRand)))
        ]
    ]
```

---

*[← Part 39](part-39.md) | [Part 41 →](part-41.md)*
