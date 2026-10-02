# Part 03: Functions และ Lambda
## ขั้นตอนที่ 41-60: การทำงานกับ Functions ใน Haskell

---

## บทนำ

ใน Haskell ฟังก์ชันเป็น "first-class citizens" หมายความว่าฟังก์ชันสามารถ:
- ส่งเป็น argument ให้ฟังก์ชันอื่น
- คืนเป็นผลลัพธ์จากฟังก์ชัน
- เก็บในตัวแปร
- สร้างใน runtime

ความสามารถเหล่านี้ทำให้ Haskell เขียนโค้ดได้กระชับและยืดหยุ่นมาก

---

## ขั้นตอนที่ 41: Function Application

```haskell
-- Function application คือการเรียกใช้ฟังก์ชัน
-- ใน Haskell ใช้ space ระหว่างชื่อฟังก์ชันและ argument

-- รูปแบบ: function argument1 argument2 ...
add :: Int -> Int -> Int
add x y = x + y

result1 :: Int
result1 = add 3 4   -- 7

-- Function application มี precedence สูงสุด
-- f x + g y  หมายถึง  (f x) + (g y)  ไม่ใช่  f (x + g) y

square :: Int -> Int
square x = x * x

-- ผิด: square 2 + 3 จะได้ (square 2) + 3 = 4 + 3 = 7
-- ถูก: square (2 + 3) = square 5 = 25

ghci> square 2 + 3
7

ghci> square (2 + 3)
25

-- $ operator: function application ที่มี precedence ต่ำสุด
-- f $ x + y  หมายถึง  f (x + y)
ghci> square $ 2 + 3
25

ghci> putStrLn $ "Hello, " ++ "World!"
Hello, World!

-- $ ช่วยลด parentheses
-- แทนที่
putStrLn (show (factorial (10)))
-- ใช้ $ แทน
putStrLn $ show $ factorial 10
```

### Function Application ใน Practice

```haskell
-- Chain ของ function calls
import Data.Char (toUpper)
import Data.List (sort, nub)

processString :: String -> String
processString s = unwords . sort . nub . words . map toUpper $ s

ghci> processString "hello world hello haskell world"
"HASKELL HELLO WORLD"

-- การอ่านจากขวาไปซ้าย:
-- 1. map toUpper s: แปลงเป็น uppercase
-- 2. words: แบ่งเป็นคำ
-- 3. nub: เอาคำซ้ำออก
-- 4. sort: เรียงลำดับ
-- 5. unwords: รวมกลับเป็น string
```

---

## ขั้นตอนที่ 42: Lambda Expressions

```haskell
-- Lambda: anonymous function (ฟังก์ชันไม่มีชื่อ)
-- รูปแบบ: \arg1 arg2 -> body

-- Lambda พื้นฐาน
addLambda :: Int -> Int -> Int
addLambda = \x y -> x + y

ghci> (\x -> x + 1) 5
6

ghci> (\x y -> x + y) 3 4
7

-- ใช้ lambda ใน map
ghci> map (\x -> x * 2) [1..5]
[2,4,6,8,10]

-- เทียบกับ named function
double :: Int -> Int
double x = x * 2

ghci> map double [1..5]
[2,4,6,8,10]

-- Lambda กับหลาย patterns (LambdaCase extension)
{-# LANGUAGE LambdaCase #-}

describeNumber :: Int -> String
describeNumber = \case
  0 -> "zero"
  1 -> "one"
  n | n < 0    -> "negative"
    | otherwise -> "positive"

-- Lambda ใน filter
ghci> filter (\x -> x `mod` 3 == 0) [1..20]
[3,6,9,12,15,18]

-- Lambda ใน foldr
ghci> foldr (\x acc -> x : acc) [] [1,2,3]
[1,2,3]
```

### Lambda vs Named Functions

```haskell
-- Lambda เหมาะสำหรับ:
-- 1. ใช้ครั้งเดียว (one-time use)
-- 2. ส่ง inline เป็น argument

-- Named function เหมาะสำหรับ:
-- 1. ใช้หลายครั้ง
-- 2. ต้องการ documentation
-- 3. มี logic ซับซ้อน

-- ตัวอย่าง: ทั้งสองเหมือนกัน
example1 :: [Int]
example1 = map (\x -> x * x) [1..5]

example2 :: [Int]
example2 = map square [1..5]
  where square x = x * x

-- หรือใช้ sections (ดีกว่าในกรณีนี้)
example3 :: [Int]
example3 = map (^2) [1..5]
```

---

## ขั้นตอนที่ 43: Currying และ Partial Application

```haskell
-- Currying: ฟังก์ชันที่รับ argument หลายตัวจริงๆ แล้วเป็น
-- ฟังก์ชันที่รับ argument ทีละตัวและคืน function

-- Int -> Int -> Int  หมายถึง  Int -> (Int -> Int)

add :: Int -> Int -> Int
add x y = x + y

-- add เป็น function ที่รับ Int แล้วคืน (Int -> Int)
addFive :: Int -> Int
addFive = add 5   -- Partial application: ส่ง argument บางส่วน

ghci> addFive 3
8

ghci> addFive 10
15

-- Partial application ในทางปฏิบัติ
multiplyBy :: Int -> Int -> Int
multiplyBy factor n = factor * n

double' :: Int -> Int
double' = multiplyBy 2

triple' :: Int -> Int
triple' = multiplyBy 3

quadruple' :: Int -> Int
quadruple' = multiplyBy 4

ghci> map double' [1..5]
[2,4,6,8,10]

ghci> map triple' [1..5]
[3,6,9,12,15]

-- Partial application กับ operators
addThree :: Int -> Int
addThree = (+3)   -- Section: (+ 3)

subtractFromTen :: Int -> Int
subtractFromTen = (10 -)   -- Left section

divideByTwo :: Double -> Double
divideByTwo = (/ 2)

ghci> map (+3) [1..5]
[4,5,6,7,8]

ghci> map (10-) [1..5]
[9,8,7,6,5]

ghci> map (/2) [2,4,6,8,10]
[1.0,2.0,3.0,4.0,5.0]
```

### flip Function

```haskell
-- flip: สลับลำดับ argument ของฟังก์ชัน
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- ตัวอย่าง
ghci> flip subtract 10 3    -- subtract 3 10 = 10 - 3 = 7
7

ghci> flip div 3 15         -- div 15 3 = 5
5

-- ใช้ flip กับ partial application
subtractFromBase :: Int -> Int -> Int
subtractFromBase = flip subtract

ghci> map (subtractFromBase 10) [1..5]
[9,8,7,6,5]
-- นั่นคือ map (\x -> 10 - x) [1..5]
```

---

## ขั้นตอนที่ 44: Function Composition

```haskell
-- (.) operator: function composition
-- (f . g) x = f (g x)
-- อ่านว่า "f composed with g"

-- รูปแบบ: (.) :: (b -> c) -> (a -> b) -> a -> c

double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

-- แบบ manual:
doubleIncrement :: Int -> Int
doubleIncrement x = increment (double x)

-- แบบ composition:
doubleIncrement' :: Int -> Int
doubleIncrement' = increment . double

ghci> doubleIncrement' 5
11   -- double 5 = 10, increment 10 = 11

-- Compose หลายฟังก์ชัน
import Data.Char (toUpper, isAlpha)

processText :: String -> String
processText = unwords . map (map toUpper) . words . filter (\c -> isAlpha c || c == ' ')

ghci> processText "hello, world! 123"
"HELLO WORLD"

-- ลำดับการอ่าน: จากขวาไปซ้าย
-- 1. filter: เอาเฉพาะตัวอักษรและ space
-- 2. words: แบ่งเป็นคำ
-- 3. map (map toUpper): uppercase ทุกคำ
-- 4. unwords: รวมกลับ

-- . vs $
-- f . g $ x  ==  (f . g) x  ==  f (g x)
-- f $ g x    ==  f (g x)

-- f . g คือ function ใหม่
-- f $ g x คือ application ทันที

addOne :: Int -> Int
addOne = (+1)

timesTwo :: Int -> Int
timesTwo = (*2)

-- สร้าง function pipeline
pipeline :: Int -> Int
pipeline = addOne . timesTwo . addOne

ghci> pipeline 5
13   -- (5+1)*2+1 = 13
```

### Point-Free Style

```haskell
-- Point-free: เขียน function โดยไม่ระบุ arguments
-- มักใช้กับ function composition

-- แบบ Pointful (ระบุ argument x)
sumSquares :: [Int] -> Int
sumSquares xs = sum (map (^2) xs)

-- แบบ Point-free
sumSquares' :: [Int] -> Int
sumSquares' = sum . map (^2)

-- ตัวอย่างอื่น
-- แบบ Pointful
filterEven :: [Int] -> [Int]
filterEven xs = filter even xs

-- แบบ Point-free
filterEven' :: [Int] -> [Int]
filterEven' = filter even

-- ข้อแนะนำ: ใช้ point-free เมื่ออ่านง่ายขึ้น
-- ไม่ต้องใช้ point-free ทุกที่เพื่อ "ความเก่ง"

-- นี่อาจอ่านยากเกินไป (overuse point-free)
countDistinctWords :: String -> Int
countDistinctWords = length . nub . words

-- แบบนี้อ่านง่ายกว่า
countDistinctWords' :: String -> Int
countDistinctWords' s = length (nub (words s))
```

---

## ขั้นตอนที่ 45: Higher-Order Functions

```haskell
-- Higher-Order Functions: ฟังก์ชันที่รับหรือคืน function

-- map: apply function กับทุก element ใน list
-- map :: (a -> b) -> [a] -> [b]

ghci> map (*2) [1..5]
[2,4,6,8,10]

ghci> map show [1..5]
["1","2","3","4","5"]

ghci> map (++ "!") ["hello", "world"]
["hello!","world!"]

-- filter: เลือก elements ที่ตรงกับ predicate
-- filter :: (a -> Bool) -> [a] -> [a]

ghci> filter even [1..10]
[2,4,6,8,10]

ghci> filter (> 5) [1..10]
[6,7,8,9,10]

ghci> filter (`elem` "aeiou") "hello world"
"eoo"

-- takeWhile: เอา elements ตราบใดที่ predicate เป็น True
ghci> takeWhile (< 5) [1..10]
[1,2,3,4]

ghci> takeWhile (/= ' ') "hello world"
"hello"

-- dropWhile: ทิ้ง elements ตราบใดที่ predicate เป็น True
ghci> dropWhile (< 5) [1..10]
[5,6,7,8,9,10]

-- span: แบ่งด้วย predicate (ตราบใดที่ True)
ghci> span (< 5) [1..10]
([1,2,3,4],[5,6,7,8,9,10])

-- break: แบ่งด้วย predicate (ตั้งแต่ True)
ghci> break (> 5) [1..10]
([1,2,3,4,5],[6,7,8,9,10])

-- partition: แบ่งเป็น 2 groups
import Data.List (partition)
ghci> partition even [1..10]
([2,4,6,8,10],[1,3,5,7,9])
```

---

## ขั้นตอนที่ 46: fold Functions

```haskell
-- foldr: fold from right
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [x1,x2,x3] = x1 `f` (x2 `f` (x3 `f` z))

ghci> foldr (+) 0 [1,2,3,4,5]
15

ghci> foldr (*) 1 [1,2,3,4,5]
120

-- foldr สร้าง list ใหม่
ghci> foldr (:) [] [1,2,3]
[1,2,3]   -- identity

-- implement functions ด้วย foldr
myLength :: [a] -> Int
myLength = foldr (\_ acc -> acc + 1) 0

mySum :: Num a => [a] -> a
mySum = foldr (+) 0

myProduct :: Num a => [a] -> a
myProduct = foldr (*) 1

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

-- หรือ efficient version
myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- foldl: fold from left (strict version: foldl')
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z [x1,x2,x3] = ((z `f` x1) `f` x2) `f` x3

import Data.List (foldl')

ghci> foldl' (+) 0 [1..5]
15

ghci> foldl' (\acc x -> acc ++ [x*2]) [] [1,2,3]
[2,4,6]

-- foldl1, foldr1: ใช้ element แรกเป็น accumulator
ghci> foldl1 max [3,1,4,1,5,9,2,6]
9

ghci> foldr1 (+) [1,2,3,4,5]
15

-- scanl, scanr: เหมือน fold แต่เก็บผลกลางทุกขั้น
ghci> scanl (+) 0 [1..5]
[0,1,3,6,10,15]

ghci> scanr (+) 0 [1..5]
[15,14,12,9,5,0]
```

### ความแตกต่างระหว่าง foldr และ foldl

```haskell
-- foldr ทำงานกับ infinite list ได้ (ถ้า f lazy enough)
ghci> foldr (\x acc -> if x == 0 then 0 else x * acc) 1 [1, 2, 0, undefined, 4]
0   -- หยุดที่ 0 เพราะ x == 0

-- foldl ทำงานกับ infinite list ไม่ได้ (loop ตลอดกาล)
-- foldl จะ accumulate ก่อนแล้วค่อย apply

-- Rule of thumb:
-- ใช้ foldl' สำหรับ strict left fold (เช่น sum, product)
-- ใช้ foldr สำหรับ building data structures หรือ short-circuit

-- ตัวอย่าง: หา first element ที่ตรงกับ predicate (ใช้ foldr)
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p = foldr (\x acc -> if p x then Just x else acc) Nothing

ghci> findFirst even [1,3,5,6,7]
Just 6
```

---

## ขั้นตอนที่ 47: zipWith และ zipWith3

```haskell
-- zipWith: รวม 2 lists ด้วยฟังก์ชัน
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

ghci> zipWith (+) [1,2,3] [10,20,30]
[11,22,33]

ghci> zipWith (*) [1,2,3] [4,5,6]
[4,10,18]

ghci> zipWith (\x y -> x ++ " " ++ y) ["hello", "good"] ["world", "bye"]
["hello world","good bye"]

-- zipWith3: รวม 3 lists
ghci> zipWith3 (\a b c -> a + b + c) [1,2,3] [4,5,6] [7,8,9]
[12,15,18]

-- ใช้ zipWith สร้าง dot product
dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum (zipWith (*) xs ys)

ghci> dotProduct [1,2,3] [4,5,6]
32   -- 1*4 + 2*5 + 3*6

-- Matrix operations
type Matrix = [[Int]]
transpose' :: Matrix -> Matrix
transpose' [] = []
transpose' ([] : _) = []
transpose' m = map head m : transpose' (map tail m)

matMul :: Matrix -> Matrix -> Matrix
matMul m1 m2 = [[dotProduct row col | col <- transpose' m2] | row <- m1]
```

---

## ขั้นตอนที่ 48: concatMap และ mapM_

```haskell
-- concatMap: map แล้ว concat
-- concatMap :: (a -> [b]) -> [a] -> [b]

ghci> concatMap (\x -> [x, x*2]) [1,2,3]
[1,2,2,4,3,6]

-- เหมือนกับ
ghci> concat (map (\x -> [x, x*2]) [1,2,3])
[1,2,2,4,3,6]

-- >>= สำหรับ list เหมือนกับ concatMap
ghci> [1,2,3] >>= \x -> [x, x*2]
[1,2,2,4,3,6]

-- mapM_: map กับ IO actions, ทิ้ง results
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

printAll :: [String] -> IO ()
printAll = mapM_ putStrLn

main :: IO ()
main = printAll ["Hello", "World", "Haskell"]
-- Output:
-- Hello
-- World
-- Haskell

-- mapM: map กับ IO actions, เก็บ results
readAll :: [String] -> IO [String]
readAll filenames = mapM readFile filenames

-- forM_: เหมือน mapM_ แต่ arguments กลับ (สะดวกกว่าในบางกรณี)
import Control.Monad (forM_)

main' :: IO ()
main' = forM_ [1..5] $ \i -> do
  putStrLn $ "Processing item " ++ show i
```

---

## ขั้นตอนที่ 49: any, all, และ Functions อื่นๆ

```haskell
-- any: ตรวจสอบว่ามี element ใดตรงกับ predicate
ghci> any even [1,3,5,6,7]
True

ghci> any (> 10) [1..5]
False

-- all: ตรวจสอบว่าทุก element ตรงกับ predicate
ghci> all even [2,4,6,8]
True

ghci> all (< 10) [1..5]
True

-- find: หา element แรกที่ตรง (คืน Maybe)
import Data.List (find)
ghci> find even [1,3,5,6,7]
Just 6

ghci> find even [1,3,5,7]
Nothing

-- findIndex: หา index แรกที่ตรง
import Data.List (findIndex, findIndices)
ghci> findIndex even [1,2,3,4]
Just 1   -- 0-indexed

ghci> findIndices even [1,2,3,4,5,6]
[1,3,5]

-- lookup: ค้นหาใน association list
assocList :: [(String, Int)]
assocList = [("one",1), ("two",2), ("three",3)]

ghci> lookup "two" assocList
Just 2

ghci> lookup "four" assocList
Nothing

-- groupBy: จัดกลุ่มด้วย predicate
import Data.List (groupBy)
ghci> groupBy (\x y -> even x == even y) [2,4,1,3,6,8,5,7]
[[2,4],[1,3],[6,8],[5,7]]

-- sortBy, maximumBy, minimumBy: sort/find ด้วย custom comparator
import Data.List (sortBy, maximumBy, minimumBy)
import Data.Ord (comparing, Down(..))

people :: [(String, Int)]
people = [("Alice",25),("Bob",30),("Charlie",20)]

ghci> sortBy (comparing snd) people
[("Charlie",20),("Alice",25),("Bob",30)]

ghci> sortBy (comparing (Down . snd)) people
[("Bob",30),("Alice",25),("Charlie",20)]

ghci> maximumBy (comparing snd) people
("Bob",30)
```

---

## ขั้นตอนที่ 50: Function Operators

```haskell
-- Backtick: ใช้ function เป็น infix operator
ghci> 5 `div` 2
2

ghci> "hello" `isPrefixOf` "hello world"
True

-- ฟังก์ชันของเราก็ใช้ backtick ได้
divides :: Int -> Int -> Bool
divides divisor n = n `mod` divisor == 0

ghci> 3 `divides` 12
True

ghci> 4 `divides` 13
False

-- ใช้ backtick ใน list comprehension
multiplesOf :: Int -> [Int]
multiplesOf n = filter (n `divides`) [1..100]

ghci> multiplesOf 7
[7,14,21,28,35,42,49,56,63,70,77,84,91,98]

-- Operator sections
ghci> (2 ^) 10      -- 2^10 = 1024
1024

ghci> (^ 2) 10      -- 10^2 = 100
100

ghci> (`elem` [1..10]) 5
True
```

### การสร้าง Operators

```haskell
-- สร้าง custom infix operator
(.+.) :: [a] -> [a] -> [a]
xs .+. ys = xs ++ ys

ghci> [1,2] .+. [3,4]
[1,2,3,4]

-- Operator precedence
infixl 6 .+.    -- left associative, precedence 6
infixr 5 +++    -- right associative, precedence 5
infix  4 `myEq` -- non-associative, precedence 4

(+++) :: String -> String -> String
x +++ y = x ++ " " ++ y

ghci> "hello" +++ "world"
"hello world"
```

---

## ขั้นตอนที่ 51: ฟังก์ชันที่คืน Functions

```haskell
-- Functions เป็น first-class values

-- ฟังก์ชันที่คืน function
multiplier :: Int -> (Int -> Int)
multiplier n = \x -> n * x

double'' :: Int -> Int
double'' = multiplier 2

triple'' :: Int -> Int
triple'' = multiplier 3

-- ฟังก์ชันที่ build function จาก list ของ functions
applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f:fs) x = applyAll fs (f x)

ghci> applyAll [(+1), (*2), (+3)] 5
-- 5 -> (+1) -> 6 -> (*2) -> 12 -> (+3) -> 15
15

-- compose list of functions
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

ghci> composeAll [(+1), (*2), (+3)] 5
-- (+3) 5 = 8, (*2) 8 = 16, (+1) 16 = 17
17
```

---

## ขั้นตอนที่ 52: on Function

```haskell
import Data.Function (on)

-- on: apply function สองครั้งแล้ว combine ด้วย binary function
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- (f `on` g) x y = f (g x) (g y)

-- ตัวอย่าง: compare by length
compareByLength :: String -> String -> Ordering
compareByLength = compare `on` length

ghci> compareByLength "hi" "hello"
LT

-- sort strings by length
import Data.List (sortBy)
ghci> sortBy (compare `on` length) ["banana", "apple", "cherry", "fig"]
["fig","apple","banana","cherry"]

-- group by first character
ghci> groupBy ((==) `on` head) (sortBy (compare `on` head) ["banana","apple","cherry","avocado"])
[["apple","avocado"],["banana"],["cherry"]]
```

---

## ขั้นตอนที่ 53: iterate, unfoldr, และ recursion schemes

```haskell
-- iterate: apply function repeatedly สร้าง infinite list
-- iterate :: (a -> a) -> a -> [a]

ghci> take 10 (iterate (*2) 1)
[1,2,4,8,16,32,64,128,256,512]

ghci> take 10 (iterate (++ "!") "Hey")
["Hey","Hey!","Hey!!","Hey!!!","Hey!!!!","Hey!!!!!","Hey!!!!!!","Hey!!!!!!!","Hey!!!!!!!!","Hey!!!!!!!!!"]

-- until: apply function จนกว่า predicate จะ True
-- until :: (a -> Bool) -> (a -> a) -> a -> a

ghci> until (> 100) (*2) 1
128

-- unfoldr: สร้าง list จาก seed value
import Data.List (unfoldr)
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

-- ตัวอย่าง: สร้าง list จาก countdown
countdown :: Int -> [Int]
countdown = unfoldr (\n -> if n < 0 then Nothing else Just (n, n-1))

ghci> countdown 5
[5,4,3,2,1,0]

-- แปลงเลขเป็น binary digits
toBinary :: Int -> [Int]
toBinary = unfoldr (\n -> if n == 0 then Nothing else Just (n `mod` 2, n `div` 2))

ghci> toBinary 13
[1,0,1,1]   -- 13 = 1101 in binary (LSB first)
```

---

## ขั้นตอนที่ 54: ตัวอย่างที่ซับซ้อน

```haskell
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Char (toLower, isAlpha)
import Data.Map (Map)
import qualified Data.Map as Map

-- ตัวอย่าง 1: Word Frequency Counter
wordFrequency :: String -> Map String Int
wordFrequency text =
  foldl' (\acc word -> Map.insertWith (+) word 1 acc)
         Map.empty
         cleanWords
  where
    cleanWords = words . map normalize . filter valid $ text
    normalize c = if isAlpha c then toLower c else ' '
    valid c = isAlpha c || c == ' '

-- หา top N words
topNWords :: Int -> String -> [(String, Int)]
topNWords n text =
  take n
  . sortBy (comparing (negate . snd))
  . Map.toList
  $ wordFrequency text

ghci> topNWords 5 "the quick brown fox jumps over the lazy dog the"
[("the",3),("brown",1),("dog",1),("fox",1),("jumps",1)]

-- ตัวอย่าง 2: Caesar Cipher
import Data.Char (ord, chr, isLetter, isUpper, isLower)

encrypt :: Int -> String -> String
encrypt shift = map encryptChar
  where
    encryptChar c
      | isUpper c = shift' 'A' c
      | isLower c = shift' 'a' c
      | otherwise = c
    shift' base c = chr $ (ord c - ord base + shift) `mod` 26 + ord base

decrypt :: Int -> String -> String
decrypt shift = encrypt (26 - shift)

ghci> encrypt 3 "Hello, World!"
"Khoor, Zruog!"

ghci> decrypt 3 "Khoor, Zruog!"
"Hello, World!"
```

---

## ขั้นตอนที่ 55: Function Memoization

```haskell
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

-- Memoization: เก็บ cache ของ results ที่คำนวณแล้ว

-- Simple memoization ด้วย Map
memoize :: Ord k => (k -> v) -> [k] -> Map k v
memoize f keys = Map.fromList [(k, f k) | k <- keys]

-- Fibonacci with memoization
fibMemo :: Int -> Integer
fibMemo n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

ghci> fibMemo 100
354224848179261915075

-- Memoization ด้วย IORef (สำหรับ IO)
type Cache k v = IORef (Map k v)

newCache :: IO (Cache k v)
newCache = newIORef Map.empty

memoIO :: Ord k => Cache k v -> (k -> IO v) -> k -> IO v
memoIO cache f key = do
  memo <- readIORef cache
  case Map.lookup key memo of
    Just v  -> return v
    Nothing -> do
      v <- f key
      modifyIORef cache (Map.insert key v)
      return v
```

---

## ขั้นตอนที่ 56: Practical Function Patterns

```haskell
-- maybe: unpack Maybe value
-- maybe :: b -> (a -> b) -> Maybe a -> b

ghci> maybe 0 (*2) (Just 5)
10

ghci> maybe 0 (*2) Nothing
0

-- Chaining Maybe operations
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- Manual chaining
result :: Maybe Int
result =
  case safeDivide 10 2 of
    Nothing -> Nothing
    Just x  ->
      case safeDivide x 0 of
        Nothing -> Nothing
        Just y  -> Just (y + 1)

-- ดีกว่าด้วย >>= (bind)
result' :: Maybe Int
result' = safeDivide 10 2 >>= safeDivide `flip` 0 >>= \y -> Just (y + 1)

-- หรือ do-notation
result'' :: Maybe Int
result'' = do
  x <- safeDivide 10 2
  y <- safeDivide x 0
  return (y + 1)

-- either: unpack Either value
-- either :: (a -> c) -> (b -> c) -> Either a b -> c

data ValidationError = EmptyName | InvalidAge Int

validateName :: String -> Either ValidationError String
validateName "" = Left EmptyName
validateName n  = Right n

validateAge :: Int -> Either ValidationError Int
validateAge age
  | age < 0 || age > 150 = Left (InvalidAge age)
  | otherwise             = Right age

validate :: String -> Int -> Either ValidationError (String, Int)
validate name age = do
  n <- validateName name
  a <- validateAge age
  return (n, a)

ghci> validate "Alice" 25
Right ("Alice",25)

ghci> validate "" 25
Left EmptyName

ghci> validate "Alice" (-5)
Left (InvalidAge (-5))
```

---

## ขั้นตอนที่ 57: Functions กับ Data Structures

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe)

-- Map operations
example :: IO ()
example = do
  -- สร้าง Map
  let m = Map.fromList [("one", 1), ("two", 2), ("three", 3)]

  -- Lookup
  print $ Map.lookup "two" m      -- Just 2
  print $ Map.lookup "four" m     -- Nothing
  print $ Map.findWithDefault 0 "four" m  -- 0

  -- Insert/Update
  let m2 = Map.insert "four" 4 m
  let m3 = Map.insertWith (+) "one" 10 m  -- "one" -> 11

  -- Map over values
  let doubled = Map.map (*2) m    -- {"one":2,"two":4,"three":6}

  -- Filter
  let evens = Map.filter even m   -- {"two":2}

  -- Fold over Map
  let total = Map.foldl' (+) 0 m  -- 6

-- mapMaybe: map แล้ว filter Nothing
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

ghci> mapMaybe safeHead [[1,2,3], [], [4,5], [], [6]]
[1,4,6]

-- Set operations
mySet :: Set.Set Int
mySet = Set.fromList [3,1,4,1,5,9,2,6]  -- {1,2,3,4,5,6,9}

ghci> Set.member 5 mySet
True

ghci> Set.union (Set.fromList [1,2,3]) (Set.fromList [2,3,4])
fromList [1,2,3,4]

ghci> Set.intersection (Set.fromList [1,2,3]) (Set.fromList [2,3,4])
fromList [2,3]
```

---

## ขั้นตอนที่ 58: Anonymous Functions (Advanced)

```haskell
-- LambdaCase extension
{-# LANGUAGE LambdaCase #-}

-- แทนที่
describeList :: [a] -> String
describeList xs = case xs of
  []  -> "empty"
  [_] -> "singleton"
  _   -> "multiple elements"

-- ใช้ LambdaCase
describeList' :: [a] -> String
describeList' = \case
  []  -> "empty"
  [_] -> "singleton"
  _   -> "multiple elements"

-- Multi-way if (MultiWayIf extension)
{-# LANGUAGE MultiWayIf #-}

classify :: Int -> String
classify n = if
  | n < 0     -> "negative"
  | n == 0    -> "zero"
  | n < 10    -> "small"
  | n < 100   -> "medium"
  | otherwise -> "large"

-- Tuple sections (TupleSections extension)
{-# LANGUAGE TupleSections #-}

ghci> map (1,) [1..5]
[(1,1),(1,2),(1,3),(1,4),(1,5)]

ghci> map (,True) ["yes","no","maybe"]
[("yes",True),("no",True),("maybe",True)]
```

---

## ขั้นตอนที่ 59: id, const, และ Combinators

```haskell
-- id: identity function
-- id :: a -> a
-- id x = x

ghci> id 42
42

ghci> map id [1,2,3]
[1,2,3]

-- const: constant function
-- const :: a -> b -> a
-- const x _ = x

ghci> const 5 "ignored"
5

ghci> map (const 0) [1,2,3,4,5]
[0,0,0,0,0]

-- ใช้ const กับ whenever
whenever :: Monad m => m Bool -> m () -> m ()
whenever condition action = do
  result <- condition
  if result then action else return ()

-- fix: fixed point combinator (Y combinator in Haskell)
import Data.Function (fix)

-- fix :: (a -> a) -> a
-- fix f = f (fix f)

-- factorial ด้วย fix
factFix :: Integer -> Integer
factFix = fix $ \rec n ->
  if n <= 0 then 1 else n * rec (n - 1)

ghci> factFix 10
3628800

-- on, (&), (<&>) utility functions
import Data.Function ((&))

-- (&) คือ function application แบบ reversed
ghci> 5 & (+3)
8

ghci> [1,2,3] & map (*2) & filter (>3) & sum
10

-- ใช้ (&) สำหรับ data pipelines
processData :: [Int] -> Int
processData xs = xs
  & filter even
  & map (*2)
  & sum
```

---

## ขั้นตอนที่ 60: สรุปและแบบฝึกหัด Part 03

### สิ่งที่เรียนรู้

1. ✅ Function Application และ $ operator
2. ✅ Lambda Expressions
3. ✅ Currying และ Partial Application
4. ✅ Function Composition (.)
5. ✅ Point-Free Style
6. ✅ Higher-Order Functions (map, filter, fold)
7. ✅ zipWith, concatMap
8. ✅ Operators และ Sections
9. ✅ ฟังก์ชันที่คืน Functions
10. ✅ Practical Function Patterns

### แบบฝึกหัด

```haskell
-- 1. implement map ด้วย foldr
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

-- 2. implement filter ด้วย foldr
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []

-- 3. เขียน compose ที่ compose ฟังก์ชันทั้ง list
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- 4. เขียน applyTwice
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

-- 5. เขียน until' เหมือน until แต่คืน list ของ intermediate values
iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
iterateUntil p f x = x : if p x then [] else iterateUntil p f (f x)

-- 6. implement zipWith ด้วย zip
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map (uncurry f) (zip xs ys)

-- 7. ตรวจสอบคำตอบ
ghci> myMap (*2) [1..5]
[2,4,6,8,10]

ghci> myFilter even [1..10]
[2,4,6,8,10]

ghci> compose [(+1),(*2),(+3)] 5
-- (+3) 5 = 8, (*2) 8 = 16, (+1) 16 = 17
17

ghci> applyTwice (+3) 10
16

ghci> iterateUntil (>10) (*2) 1
[1,2,4,8,16]

ghci> myZipWith (+) [1,2,3] [10,20,30]
[11,22,33]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 04 เราจะเรียนเรื่อง **Pattern Matching และ Guards** อย่างละเอียด:
- Pattern matching บน types ต่างๆ
- Guards
- Case Expressions
- As-Patterns
- Wildcards

---

*[← Part 02](part-02.md) | [Part 04 →](part-04.md)*
