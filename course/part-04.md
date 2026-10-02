# Part 04: Pattern Matching และ Guards
## ขั้นตอนที่ 61-80: การจับคู่รูปแบบและเงื่อนไข

---

## บทนำ

Pattern Matching เป็นหนึ่งในฟีเจอร์ที่ทรงพลังที่สุดของ Haskell มันช่วยให้เราสามารถ destructure data และจัดการแต่ละกรณีได้อย่างชัดเจน Guards ช่วยเพิ่ม conditions ให้กับ pattern matching

---

## ขั้นตอนที่ 61: Pattern Matching พื้นฐาน

```haskell
-- Pattern matching บน literals
describeNumber :: Int -> String
describeNumber 0 = "zero"
describeNumber 1 = "one"
describeNumber 2 = "two"
describeNumber n = "some other number: " ++ show n

-- ลำดับ patterns สำคัญมาก!
-- Haskell จะ match จากบนลงล่าง

ghci> describeNumber 0
"zero"
ghci> describeNumber 5
"some other number: 5"

-- Pattern matching บน Booleans
boolToStr :: Bool -> String
boolToStr True  = "true"
boolToStr False = "false"

-- Pattern matching บน Char
isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False   -- wildcard: match ทุกอย่าง

-- Wildcard (_): ใช้เมื่อไม่ต้องการค่า
first :: (a, b) -> a
first (x, _) = x   -- ไม่ต้องการ b

second :: (a, b) -> b
second (_, y) = y   -- ไม่ต้องการ a
```

---

## ขั้นตอนที่ 62: Pattern Matching บน Lists

```haskell
-- Pattern matching บน list structure
isEmpty :: [a] -> Bool
isEmpty []     = True    -- empty list pattern
isEmpty (_:_)  = False   -- non-empty (head : tail)

myHead :: [a] -> a
myHead []    = error "empty list"
myHead (x:_) = x    -- x คือ head, _ คือ tail

myTail :: [a] -> [a]
myTail []     = error "empty list"
myTail (_:xs) = xs  -- _ คือ head, xs คือ tail

-- Match หลาย elements
describeList :: [a] -> String
describeList []        = "empty"
describeList [_]       = "exactly one element"
describeList [_, _]    = "exactly two elements"
describeList (_:_:_:_) = "three or more elements"

ghci> describeList ([] :: [Int])
"empty"
ghci> describeList [1]
"exactly one element"
ghci> describeList [1,2]
"exactly two elements"
ghci> describeList [1,2,3,4]
"three or more elements"

-- Pattern matching ที่ซับซ้อน
sumPairs :: [(Int, Int)] -> Int
sumPairs []              = 0
sumPairs ((x, y) : rest) = x + y + sumPairs rest

ghci> sumPairs [(1,2),(3,4),(5,6)]
21

-- Pattern matching บน Strings
greet :: String -> String
greet []          = "Hello, stranger!"
greet ('A':rest)  = "Hello, A" ++ rest
greet name        = "Hello, " ++ name ++ "!"

ghci> greet "Alice"
"Hello, Alice!"
ghci> greet "Alice Smith"
"Hello, Alice Smith!"
```

---

## ขั้นตอนที่ 63: Pattern Matching บน Tuples

```haskell
-- Pattern matching บน tuples
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

-- Pattern matching บน triple
addTriple :: (Int, Int, Int) -> Int
addTriple (x, y, z) = x + y + z

-- Nested tuple pattern
processData :: ((Int, Int), String) -> String
processData ((x, y), name) = name ++ ": " ++ show (x + y)

-- ใช้ใน do-notation
lookupPair :: IO ()
lookupPair = do
  let pairs = [("one", 1), ("two", 2), ("three", 3)]
  case lookup "two" pairs of
    Nothing      -> putStrLn "Not found"
    Just (k, v)  -> putStrLn $ "Found: " ++ show v  -- ผิด! Just v หรือ Just value
    -- ถูกต้อง:

-- จริงๆ lookup คืน Maybe Int ไม่ใช่ Maybe (String, Int)
lookupPair' :: IO ()
lookupPair' = do
  let pairs = [("one", 1), ("two", 2), ("three", 3)]
  case lookup "two" pairs of
    Nothing  -> putStrLn "Not found"
    Just n   -> putStrLn $ "Found: " ++ show n
```

---

## ขั้นตอนที่ 64: Guards

```haskell
-- Guards: เงื่อนไขใน pattern matching
-- ใช้ | แล้วตามด้วย boolean expression

classify :: Int -> String
classify n
  | n < 0     = "negative"
  | n == 0    = "zero"
  | n < 10    = "small"
  | n < 100   = "medium"
  | n < 1000  = "large"
  | otherwise = "very large"

-- 'otherwise' เป็น True เสมอ (เทียบเท่ากับ True)

-- Guards พร้อม pattern matching
bmiCategory :: Double -> Double -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise  = "Obese"
  where bmi = weight / height ^ 2

ghci> bmiCategory 60 1.70
"Normal"

ghci> bmiCategory 100 1.70
"Obese"

-- Guards กับหลาย arguments
max' :: Ord a => a -> a -> a
max' x y
  | x >= y    = x
  | otherwise = y

-- Grade system
letterGrade :: Int -> Char
letterGrade score
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  | score >= 70 = 'C'
  | score >= 60 = 'D'
  | otherwise   = 'F'

ghci> letterGrade 85
'B'

ghci> map letterGrade [95, 82, 73, 65, 55]
"ABCDF"
```

### Guards กับ Where

```haskell
-- Guards และ where ทำงานร่วมกันได้ดี

cylinderVolume :: Double -> Double -> Double
cylinderVolume radius height
  | valid     = pi * r2 * height
  | otherwise = error "Invalid dimensions"
  where
    r2    = radius ^ 2
    valid = radius > 0 && height > 0

-- Multiple calculations ใน where
bloodPressure :: Int -> Int -> String
bloodPressure systolic diastolic
  | isLow     = "Low Blood Pressure"
  | isNormal  = "Normal Blood Pressure"
  | isHigh    = "High Blood Pressure"
  | isCrisis  = "Hypertensive Crisis"
  | otherwise = "Unknown"
  where
    isLow    = systolic < 90 || diastolic < 60
    isNormal = systolic < 120 && diastolic < 80
    isHigh   = systolic < 180 && diastolic < 120
    isCrisis = systolic >= 180 || diastolic >= 120
```

---

## ขั้นตอนที่ 65: Case Expressions

```haskell
-- case ... of: pattern matching เป็น expression
-- สามารถใช้ได้ทุกที่ที่ expression ใช้ได้

describeHead :: [a] -> String
describeHead xs = case xs of
  []    -> "empty list"
  [_]   -> "singleton"
  (x:_) -> "list starting with something"

-- case กับ guards
classifyList :: [Int] -> String
classifyList xs = case xs of
  []  -> "empty"
  [n] | n > 0    -> "singleton positive"
      | otherwise -> "singleton non-positive"
  _   -> "multiple elements"

-- case เป็น expression สามารถใช้ใน expressions
result :: String
result = "The list is " ++ case [1,2,3] of
  []    -> "empty"
  [_]   -> "singleton"
  _     -> "longer"

-- case ใน do-notation
handleInput :: String -> IO ()
handleInput input = do
  putStrLn $ case input of
    "hello" -> "Hello there!"
    "bye"   -> "Goodbye!"
    other   -> "You said: " ++ other

-- Nested case
processInput :: Maybe (Either String Int) -> String
processInput input = case input of
  Nothing        -> "No input"
  Just (Left s)  -> "String: " ++ s
  Just (Right n) -> "Number: " ++ show n

ghci> processInput Nothing
"No input"
ghci> processInput (Just (Left "hello"))
"String: hello"
ghci> processInput (Just (Right 42))
"Number: 42"
```

---

## ขั้นตอนที่ 66: As-Patterns (@)

```haskell
-- As-pattern: bind ทั้ง value และส่วนย่อยๆ ของมัน
-- รูปแบบ: name@pattern

-- ตัวอย่าง: ใช้ทั้ง head และ whole list
firstAndAll :: [a] -> (a, [a])
firstAndAll []         = error "empty list"
firstAndAll xs@(x:_)   = (x, xs)
-- xs คือ whole list, x คือ head

ghci> firstAndAll [1,2,3]
(1,[1,2,3])

-- ตัวอย่าง: ตรวจสอบ list แต่ preserve ทั้ง list
describeSorted :: Ord a => [a] -> String
describeSorted [] = "empty list"
describeSorted [_] = "singleton (trivially sorted)"
describeSorted all@(x:y:_)
  | x <= y    = "starts sorted: " ++ show (length all) ++ " elements"
  | otherwise = "NOT sorted: " ++ show (length all) ++ " elements"

-- As-pattern กับ tuples
swapIfNeeded :: Ord a => (a, a) -> (a, a)
swapIfNeeded p@(x, y)
  | x <= y    = p         -- คืน original pair
  | otherwise = (y, x)    -- swap

ghci> swapIfNeeded (3, 1)
(1,3)
ghci> swapIfNeeded (1, 3)
(1,3)

-- Duplicate detection
hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate []     = False
hasDuplicate (x:xs) = x `elem` xs || hasDuplicate xs
```

---

## ขั้นตอนที่ 67: Exhaustive Patterns

```haskell
-- Haskell จะ warn เมื่อ patterns ไม่ exhaustive
-- (ด้วย -Wall flag)

-- ไม่ exhaustive (ขาด case สำหรับ [])
-- unsafeHead :: [a] -> a
-- unsafeHead (x:_) = x
-- Warning: Pattern match(es) are non-exhaustive

-- ควรจัดการทุก case
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- หรือให้ error ที่ชัดเจน
strictHead :: String -> [a] -> a
strictHead context []    = error $ "empty list in " ++ context
strictHead _ (x:_)       = x

-- Overlapping patterns
-- Haskell จะใช้ pattern แรกที่ match

-- ตัวอย่าง: patterns overlapping
bad :: Int -> String
bad 1 = "one"
bad n = "something"    -- match ทุกอย่างรวมถึง 1
bad 2 = "two"          -- ไม่มีวันถูก match (unreachable)
-- GHC จะ warn ว่า "Pattern match is redundant"

-- ถูกต้อง: เรียงจาก specific ไป general
good :: Int -> String
good 1 = "one"
good 2 = "two"
good n = "something"
```

---

## ขั้นตอนที่ 68: Pattern Matching บน Custom Types

```haskell
-- ก่อนจะเรียน ADT อย่างละเอียดใน Part 08
-- ลองดู pattern matching บน simple custom types

data Color = Red | Green | Blue

colorToHex :: Color -> String
colorToHex Red   = "#FF0000"
colorToHex Green = "#00FF00"
colorToHex Blue  = "#0000FF"

-- ใช้ GHCi
ghci> colorToHex Red
"#FF0000"

-- Boolean สามารถ pattern match ได้เหมือน custom type
trueOrFalse :: Bool -> String
trueOrFalse True  = "true"
trueOrFalse False = "false"

-- Maybe ก็เป็น custom type
describe :: Maybe Int -> String
describe Nothing  = "no value"
describe (Just 0) = "zero"
describe (Just n)
  | n > 0     = "positive: " ++ show n
  | otherwise = "negative: " ++ show n

ghci> describe Nothing
"no value"
ghci> describe (Just 5)
"positive: 5"
ghci> describe (Just (-3))
"negative: -3"

-- Either
describeResult :: Either String Int -> String
describeResult (Left err)  = "Error: " ++ err
describeResult (Right val) = "Success: " ++ show val
```

---

## ขั้นตอนที่ 69: Nested Pattern Matching

```haskell
-- Pattern matching ซ้อนกัน

-- List of Maybe
sumJusts :: [Maybe Int] -> Int
sumJusts []             = 0
sumJusts (Nothing : xs) = sumJusts xs
sumJusts (Just n  : xs) = n + sumJusts xs

ghci> sumJusts [Just 1, Nothing, Just 3, Nothing, Just 5]
9

-- Nested tuples
processNested :: (Maybe Int, [String]) -> String
processNested (Nothing, [])  = "no number, no strings"
processNested (Nothing, ss)  = "no number, " ++ show (length ss) ++ " strings"
processNested (Just n, [])   = "number " ++ show n ++ ", no strings"
processNested (Just n, s:_)  = "number " ++ show n ++ ", first string: " ++ s

-- Tree ก่อน define Tree type
data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Pattern match บน Tree
treeSize :: Tree a -> Int
treeSize Leaf           = 0
treeSize (Node l _ r)   = 1 + treeSize l + treeSize r

treeSum :: Num a => Tree a -> a
treeSum Leaf           = 0
treeSum (Node l n r)   = treeSum l + n + treeSum r

-- Inorder traversal
inorder :: Tree a -> [a]
inorder Leaf           = []
inorder (Node l n r)   = inorder l ++ [n] ++ inorder r
```

---

## ขั้นตอนที่ 70: Pattern Matching กับ String

```haskell
-- String เป็น [Char] ดังนั้น pattern match ได้แบบ list

-- Parse HTTP method
parseMethod :: String -> Maybe String
parseMethod "GET"    = Just "Read"
parseMethod "POST"   = Just "Create"
parseMethod "PUT"    = Just "Update"
parseMethod "DELETE" = Just "Delete"
parseMethod _        = Nothing

-- แยก URL path
parsePath :: String -> [String]
parsePath ""         = []
parsePath ('/':rest) = go rest
  where
    go "" = []
    go s  = case break (== '/') s of
      (segment, "")   -> [segment]
      (segment, rest) -> segment : go (tail rest)
parsePath s = parsePath ("/" ++ s)

ghci> parsePath "/users/123/posts"
["users","123","posts"]

-- String patterns ที่ซับซ้อน
countWords :: String -> Int
countWords [] = 0
countWords (' ':rest) = countWords rest
countWords (c:rest)   = 1 + skipWord rest
  where
    skipWord [] = 0
    skipWord (' ':rest) = countWords rest
    skipWord (_:rest) = skipWord rest
```

---

## ขั้นตอนที่ 71: Guards กับ Pattern Matching ร่วมกัน

```haskell
-- Guards และ patterns ทำงานร่วมกัน

processScore :: (String, Int) -> String
processScore (name, score)
  | score >= 90  = name ++ " passed with distinction"
  | score >= 60  = name ++ " passed"
  | otherwise    = name ++ " failed"

ghci> processScore ("Alice", 95)
"Alice passed with distinction"

-- Patterns และ guards บน lists
describeIntegers :: [Int] -> String
describeIntegers []   = "empty"
describeIntegers [n]
  | n > 0     = "one positive: " ++ show n
  | n < 0     = "one negative: " ++ show n
  | otherwise = "just zero"
describeIntegers (x:y:_)
  | x < y     = "starts increasing: " ++ show x ++ " then " ++ show y
  | x > y     = "starts decreasing: " ++ show x ++ " then " ++ show y
  | otherwise = "starts equal: " ++ show x

-- Guards กับ where ใน case
analyzeList :: [Int] -> String
analyzeList xs = case xs of
  []  -> "empty"
  [_] -> "singleton"
  _   | avg > 100 -> "large average: " ++ show avg
      | avg > 50  -> "medium average: " ++ show avg
      | otherwise -> "small average: " ++ show avg
  where
    avg = sum xs `div` length xs
```

---

## ขั้นตอนที่ 72: Bang Patterns

```haskell
-- Bang Patterns: บังคับ strict evaluation
{-# LANGUAGE BangPatterns #-}

-- ปัญหา: foldl สะสม thunks
badSum :: [Int] -> Int
badSum = foldl (+) 0    -- lazy, memory leak!

-- แก้ด้วย bang pattern
strictSum :: [Int] -> Int
strictSum xs = go 0 xs
  where
    go !acc []     = acc          -- !acc: evaluate acc ทันที
    go !acc (x:xs) = go (acc+x) xs

-- หรือใช้ foldl'
import Data.List (foldl')
goodSum :: [Int] -> Int
goodSum = foldl' (+) 0

-- Bang pattern ใน function args
f :: Int -> Int -> Int
f !x !y = x + y    -- force evaluate x และ y ก่อน

-- Bang pattern กับ data types
data StrictPair a b = StrictPair !a !b

-- ทั้ง a และ b จะถูก evaluate เมื่อ constructor ถูกสร้าง
```

---

## ขั้นตอนที่ 73: View Patterns

```haskell
{-# LANGUAGE ViewPatterns #-}

-- View Patterns: apply function แล้ว pattern match บนผล
-- รูปแบบ: (function -> pattern)

import Data.Map (Map)
import qualified Data.Map as Map

-- ตัวอย่าง: pattern match หลัง lookup
handleRequest :: Map String String -> String -> String
handleRequest db ((`Map.lookup` db) -> Just value) = "Found: " ++ value
handleRequest _ key = "Not found: " ++ key

-- อีกตัวอย่าง
import Data.List (isPrefixOf, stripPrefix)

handleCommand :: String -> String
handleCommand (stripPrefix "hello " -> Just name) = "Hi, " ++ name ++ "!"
handleCommand (stripPrefix "bye " -> Just name)   = "Goodbye, " ++ name ++ "!"
handleCommand cmd                                  = "Unknown: " ++ cmd

ghci> handleCommand "hello Alice"
"Hi, Alice!"

ghci> handleCommand "bye Bob"
"Goodbye, Bob!"
```

---

## ขั้นตอนที่ 74: Pattern Synonyms

```haskell
{-# LANGUAGE PatternSynonyms #-}

-- Pattern Synonyms: สร้าง alias สำหรับ patterns

-- Unidirectional pattern synonym
pattern Zero :: Int
pattern Zero = 0

pattern One :: Int
pattern One = 1

describeInt :: Int -> String
describeInt Zero = "zero"
describeInt One  = "one"
describeInt n    = "other: " ++ show n

-- Bidirectional pattern synonym (ใช้ทั้ง constructor และ pattern)
pattern Pair :: a -> b -> (a, b)
pattern Pair x y = (x, y)

myPair :: (Int, String)
myPair = Pair 42 "hello"

getFirst :: (a, b) -> a
getFirst (Pair x _) = x

-- ใช้กับ Maybe
pattern Just' :: a -> Maybe a
pattern Just' x = Just x

pattern Nothing' :: Maybe a
pattern Nothing' = Nothing

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing'
safeDiv x y = Just' (x `div` y)

-- Pattern synonym ที่ซับซ้อน
pattern Head :: a -> [a] -> [a]
pattern Head x xs = x:xs
{-# COMPLETE Head, [] #-}  -- บอก GHC ว่า patterns exhaustive
```

---

## ขั้นตอนที่ 75: Irrefutable Patterns (Lazy Patterns)

```haskell
-- Irrefutable pattern (~): lazy pattern matching
-- ไม่ evaluate argument จนกว่าจะต้องการค่า

-- Normal pattern (strict)
f1 :: (Int, Int) -> Int
f1 (x, _) = x

-- Irrefutable pattern (lazy)
f2 :: (Int, Int) -> Int
f2 ~(x, _) = x   -- ไม่ evaluate tuple จนกว่าจะใช้ x

-- ตัวอย่างที่เห็นความแตกต่าง
-- f1 undefined จะ throw exception
-- f2 undefined ก็จะ throw เพราะต้องการ x

-- แต่ใน context lazy evaluation:
-- ถ้าไม่ต้องการค่า irrefutable pattern จะไม่ evaluate

lazy :: (Int, Int) -> Int
lazy ~(x, y) = 42   -- ไม่ใช้ x, y ดังนั้นไม่ evaluate

ghci> lazy undefined
42   -- ทำงานได้!

strict :: (Int, Int) -> Int
strict (x, y) = 42  -- evaluate tuple ก่อน แม้ว่าไม่ใช้

ghci> strict undefined
*** Exception: Prelude.undefined
```

---

## ขั้นตอนที่ 76: ตัวอย่าง Pattern Matching ในชีวิตจริง

```haskell
-- JSON-like data processing
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]

showJson :: JsonValue -> String
showJson JsonNull        = "null"
showJson (JsonBool b)    = if b then "true" else "false"
showJson (JsonNumber n)  = show n
showJson (JsonString s)  = "\"" ++ s ++ "\""
showJson (JsonArray vs)  = "[" ++ intercalate "," (map showJson vs) ++ "]"
showJson (JsonObject kvs) = "{" ++ intercalate "," (map showKv kvs) ++ "}"
  where showKv (k, v) = "\"" ++ k ++ "\":" ++ showJson v

import Data.List (intercalate)

sampleData :: JsonValue
sampleData = JsonObject
  [ ("name",    JsonString "Alice")
  , ("age",     JsonNumber 25)
  , ("active",  JsonBool True)
  , ("scores",  JsonArray [JsonNumber 95, JsonNumber 87, JsonNumber 92])
  , ("address", JsonNull)
  ]

ghci> putStrLn (showJson sampleData)
{"name":"Alice","age":25.0,"active":true,"scores":[95.0,87.0,92.0],"address":null}

-- Extract values
getName :: JsonValue -> Maybe String
getName (JsonObject kvs) = case lookup "name" kvs of
  Just (JsonString s) -> Just s
  _                   -> Nothing
getName _ = Nothing

getAge :: JsonValue -> Maybe Int
getAge (JsonObject kvs) = case lookup "age" kvs of
  Just (JsonNumber n) -> Just (round n)
  _                   -> Nothing
getAge _ = Nothing
```

---

## ขั้นตอนที่ 77: Recursive Pattern Matching

```haskell
-- Recursion ร่วมกับ pattern matching

-- Binary to decimal
fromBinary :: [Int] -> Int
fromBinary []     = 0
fromBinary (b:bs) = b * 2 ^ length bs + fromBinary bs

ghci> fromBinary [1,1,0,1]
13   -- 8+4+0+1 = 13

-- Run-length encoding
encode :: Eq a => [a] -> [(a, Int)]
encode []     = []
encode (x:xs) = (x, 1 + length same) : encode rest
  where (same, rest) = span (== x) xs

ghci> encode "aabbbccddddee"
[('a',2),('b',3),('c',2),('d',4),('e',2)]

-- Decode
decode :: [(a, Int)] -> [a]
decode []          = []
decode ((x, n):xs) = replicate n x ++ decode xs

ghci> decode [('a',2),('b',3)]
"aabbb"

-- flatten nested lists
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)   = [x]
flatten (List xs)  = concatMap flatten xs

ghci> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
```

---

## ขั้นตอนที่ 78: Pattern Matching กับ Records

```haskell
-- Record syntax ช่วยให้ pattern matching ชัดเจนขึ้น

data Person = Person
  { personName :: String
  , personAge  :: Int
  , personEmail :: String
  } deriving (Show)

-- Pattern match ด้วย record fields
greetPerson :: Person -> String
greetPerson Person { personName = name, personAge = age }
  = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

-- ใช้ RecordWildCards extension
{-# LANGUAGE RecordWildCards #-}

greetPerson' :: Person -> String
greetPerson' Person{..} = "Hello, " ++ personName ++ "! Age: " ++ show personAge

-- Update record
birthday :: Person -> Person
birthday p = p { personAge = personAge p + 1 }

alice :: Person
alice = Person { personName = "Alice", personAge = 25, personEmail = "alice@example.com" }

ghci> greetPerson alice
"Hello, Alice! You are 25 years old."

ghci> personName (birthday alice)
"Alice"

ghci> personAge (birthday alice)
26
```

---

## ขั้นตอนที่ 79: Pattern Matching Performance

```haskell
-- Pattern matching ใน Haskell มี performance ดี
-- GHC compiles patterns เป็น efficient decision trees

-- Efficient: GHC จะ compile เป็น jump table สำหรับ bounded integers
classifyChar :: Char -> String
classifyChar c
  | c >= 'a' && c <= 'z' = "lowercase"
  | c >= 'A' && c <= 'Z' = "uppercase"
  | c >= '0' && c <= '9' = "digit"
  | otherwise             = "other"

-- ควรระวัง: String patterns เป็น O(n) comparison
-- ถ้ามี patterns มาก ควรใช้ Map แทน
import qualified Data.Map.Strict as Map

methodMap :: Map.Map String String
methodMap = Map.fromList
  [ ("GET", "Read")
  , ("POST", "Create")
  , ("PUT", "Update")
  , ("DELETE", "Delete")
  ]

parseMethod :: String -> Maybe String
parseMethod = (`Map.lookup` methodMap)  -- O(log n) แทน O(n*m)

-- BangPatterns สำหรับ performance
{-# LANGUAGE BangPatterns #-}

-- Strict accumulator ทำให้ไม่สะสม thunks
sumStrict :: [Double] -> Double
sumStrict = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs
```

---

## ขั้นตอนที่ 80: สรุปและแบบฝึกหัด Part 04

### สิ่งที่เรียนรู้

1. ✅ Pattern Matching พื้นฐาน (literals, wildcards)
2. ✅ Pattern Matching บน Lists
3. ✅ Pattern Matching บน Tuples
4. ✅ Guards
5. ✅ Case Expressions
6. ✅ As-Patterns (@)
7. ✅ Exhaustive Patterns
8. ✅ Pattern Matching บน Custom Types
9. ✅ Irrefutable Patterns (~)
10. ✅ Pattern Synonyms

### แบบฝึกหัด

```haskell
-- 1. เขียน function ที่ compress list โดย merge consecutive duplicates
compress :: Eq a => [a] -> [a]
compress []       = []
compress [x]      = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = x : compress (y:xs)

ghci> compress "aabbbccddddee"
"abcde"

-- 2. Pattern matching บน Maybe chain
safeOp :: Int -> Int -> Maybe Int
safeOp x y
  | y == 0    = Nothing
  | y < 0     = Nothing
  | otherwise = Just (x `div` y)

-- 3. เขียน function elementAt ที่ safe
elementAt :: [a] -> Int -> Maybe a
elementAt []     _     = Nothing
elementAt (x:_)  0     = Just x
elementAt (_:xs) n
  | n < 0     = Nothing
  | otherwise = elementAt xs (n - 1)

ghci> elementAt [1,2,3,4,5] 2
Just 3
ghci> elementAt [1,2,3] 10
Nothing

-- 4. เขียน function myZip ที่จับ 2 lists เป็น list of pairs
myZip :: [a] -> [b] -> [(a, b)]
myZip []     _      = []
myZip _      []     = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 5. เขียน decode สำหรับ modified run-length encoding
data ListItem a = Multiple Int a | Single a

decodeModified :: [ListItem a] -> [a]
decodeModified []                  = []
decodeModified (Single x     : xs) = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

ghci> decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c']
"aaaabcc"
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 05 เราจะเรียนเรื่อง **Lists, Tuples, และ Data Structures** เชิงลึก:
- List operations ขั้นสูง
- Sorting และ Searching
- Data.Map, Data.Set
- Data.Sequence

---

*[← Part 03](part-03.md) | [Part 05 →](part-05.md)*
