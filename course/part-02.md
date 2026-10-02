# Part 02: Types พื้นฐานและ Expressions
## ขั้นตอนที่ 21-40: ระบบ Type ของ Haskell

---

## บทนำ

ระบบ Type ใน Haskell เป็นหัวใจสำคัญของภาษา ทุกอย่างใน Haskell มี Type และ compiler จะตรวจสอบ Type ตั้งแต่ compile time ทำให้โปรแกรมที่ compile ผ่านมีโอกาสน้อยมากที่จะ crash ด้วย type errors

---

## ขั้นตอนที่ 21: Basic Types

### Numeric Types

```haskell
-- Int: จำนวนเต็มขนาดจำกัด (ขึ้นกับ architecture: 32 หรือ 64 bit)
myInt :: Int
myInt = 42

minInt :: Int
minInt = minBound   -- -9223372036854775808 (บน 64-bit)

maxInt :: Int
maxInt = maxBound   -- 9223372036854775807 (บน 64-bit)

-- Integer: จำนวนเต็มขนาดไม่จำกัด (arbitrary precision)
bigNumber :: Integer
bigNumber = 2 ^ 100  -- 1267650600228229401496703205376

-- Double: จำนวนทศนิยมความแม่นยำสูง (64-bit floating point)
myDouble :: Double
myDouble = 3.14159

pi' :: Double
pi' = 3.141592653589793

-- Float: จำนวนทศนิยมความแม่นยำต่ำกว่า (32-bit floating point)
myFloat :: Float
myFloat = 3.14

-- ข้อแนะนำ: ใช้ Double เป็นค่า default สำหรับ floating point
-- ใช้ Integer สำหรับจำนวนเต็มที่อาจใหญ่มาก
-- ใช้ Int สำหรับจำนวนเต็มที่ทราบว่าไม่ใหญ่เกิน
```

### Boolean Type

```haskell
-- Bool: มีแค่สองค่า
isTrue :: Bool
isTrue = True

isFalse :: Bool
isFalse = False

-- Boolean operations
andResult :: Bool
andResult = True && False  -- False

orResult :: Bool
orResult = True || False   -- True

notResult :: Bool
notResult = not True       -- False
```

### Char และ String

```haskell
-- Char: ตัวอักษรเดียว (Unicode)
myChar :: Char
myChar = 'A'

thaiChar :: Char
thaiChar = 'ก'

-- String: ที่จริงคือ [Char] - list of characters
myString :: String
myString = "Hello, World!"

-- String เหมือน list of Char
-- "Hello" เหมือน ['H','e','l','l','o']

-- String operations
firstChar :: Char
firstChar = head "Hello"   -- 'H'

restChars :: String
restChars = tail "Hello"   -- "ello"

stringLength :: Int
stringLength = length "Hello"   -- 5

-- String concatenation
combined :: String
combined = "Hello" ++ ", " ++ "World!"
```

### Unit Type

```haskell
-- () หรือ Unit: type ที่มีแค่ค่าเดียวคือ ()
-- ใช้เมื่อไม่มีค่าที่ meaningful จะคืน
unitValue :: ()
unitValue = ()

-- ฟังก์ชันที่ไม่คืนค่า meaningful จะคืน IO ()
printSomething :: IO ()
printSomething = putStrLn "Something"
```

---

## ขั้นตอนที่ 22: Type Inference

Haskell สามารถ infer (อนุมาน) types ได้โดยไม่ต้องระบุเสมอไป

```haskell
-- เราสามารถระบุ type signature หรือไม่ก็ได้
-- แต่แนะนำให้ระบุเสมอสำหรับ top-level functions

-- GHC รู้ว่า 42 เป็น Num ดังนั้น x จะเป็น Num
x = 42

-- GHC รู้ว่า "hello" เป็น String
greeting = "hello"

-- GHC รู้ว่า True เป็น Bool
flag = True

-- Type inference ในนิพจน์ซับซ้อน
result = if True then 42 else 0
-- GHC รู้ว่า result ต้องเป็น Num เพราะมาจาก if-then-else ที่คืน Num

-- ใน GHCi ตรวจสอบ type ที่ infer ได้
ghci> :type 42
42 :: Num a => a

ghci> :type "hello"
"hello" :: String

ghci> :type True
True :: Bool
```

### Polymorphic Types

```haskell
-- ฟังก์ชันที่ทำงานกับหลาย types (polymorphic)
identity :: a -> a
identity x = x

-- 'a' คือ type variable - ทำงานกับ type ใดก็ได้
ghci> identity 42
42
ghci> identity "hello"
"hello"
ghci> identity True
True

-- length ทำงานกับ list ใดก็ได้
length :: [a] -> Int
-- 'a' คือ type variable

ghci> length [1, 2, 3]
3
ghci> length "hello"
5
ghci> length [True, False, True]
3
```

---

## ขั้นตอนที่ 23: Numeric Type Classes

Haskell มี type classes สำหรับ numeric operations

```haskell
-- Num: type class สำหรับ basic arithmetic
-- (+), (-), (*), abs, signum, fromInteger, negate

-- Int, Integer, Double, Float ล้วน implement Num

-- Integral: type class สำหรับ integer division
-- div, mod, quot, rem, divMod, quotRem

ghci> div 17 5     -- 3 (floor division)
3
ghci> mod 17 5     -- 2 (modulo)
2
ghci> quot 17 5    -- 3 (truncated division)
3
ghci> rem 17 5     -- 2 (remainder)
2

-- ความแตกต่างระหว่าง div/mod กับ quot/rem
ghci> div (-7) 2   -- -4 (rounds toward negative infinity)
-4
ghci> quot (-7) 2  -- -3 (rounds toward zero)
-3
ghci> mod (-7) 2   -- 1
1
ghci> rem (-7) 2   -- -1
-1

-- Fractional: type class สำหรับ fractional division
-- (/), recip, fromRational

ghci> 7 / 2        -- 3.5
3.5
ghci> recip 4.0    -- 0.25
0.25

-- Floating: type class สำหรับ floating point operations
-- pi, exp, log, sqrt, sin, cos, tan, etc.

ghci> sqrt 16.0    -- 4.0
4.0
ghci> log (exp 1)  -- 1.0
1.0
ghci> sin (pi/2)   -- 1.0
1.0
```

### การแปลง Types

```haskell
-- fromIntegral: แปลง Integral เป็น Num ทั่วไป
ghci> fromIntegral (5 :: Int) :: Double
5.0

-- round, floor, ceiling, truncate: แปลง Floating เป็น Integral
ghci> round 3.7    -- 4
4
ghci> floor 3.7    -- 3
3
ghci> ceiling 3.2  -- 4
4
ghci> truncate 3.9 -- 3
3

-- ตัวอย่างปัญหาที่ต้องแปลง type
-- การหาค่าเฉลี่ย
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

ghci> average [1, 2, 3, 4, 5]
3.0
```

---

## ขั้นตอนที่ 24: Let Expressions

```haskell
-- let ... in ... สำหรับ local definitions
circleArea :: Double -> Double
circleArea r =
  let pi'  = 3.14159265358979
      r2   = r * r
  in pi' * r2

-- let สามารถมีหลาย definitions
bmi :: Double -> Double -> String
bmi weight height =
  let bmiValue    = weight / (height ^ 2)
      underweight = bmiValue < 18.5
      overweight  = bmiValue > 25.0
  in if underweight
       then "Underweight"
       else if overweight
         then "Overweight"
         else "Normal"

-- let ใน do-notation (IO)
main :: IO ()
main = do
  let name    = "Alice"
      age     = 25
      message = name ++ " is " ++ show age ++ " years old"
  putStrLn message

-- Let expressions มี scope ที่จำกัด
example :: Int
example =
  let x = 5
      y = x + 3    -- y สามารถใช้ x ได้
  in x + y         -- ได้ 13

-- ใน GHCi
ghci> let x = 10 in x * 2
20

ghci> let { x = 5; y = 3 } in x + y
8
```

---

## ขั้นตอนที่ 25: Where Clauses

```haskell
-- where clause อยู่ท้ายฟังก์ชัน
-- ต่างจาก let ตรงที่ where อยู่หลัง expression หลัก

circleArea' :: Double -> Double
circleArea' r = pi' * r2
  where
    pi' = 3.14159265358979
    r2  = r * r

-- where clause สามารถ define ฟังก์ชันได้
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    f = head firstname
    l = head lastname

ghci> initials "John" "Doe"
"J. D."

-- where clause กับ guards
bmiCategory :: Double -> String
bmiCategory bmi
  | bmi <= thin   = "Underweight"
  | bmi <= normal = "Normal"
  | bmi <= fat    = "Overweight"
  | otherwise     = "Obese"
  where
    thin   = 18.5
    normal = 25.0
    fat    = 30.0

-- where สามารถมี nested functions
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where
    what [] = "empty."
    what [_] = "a singleton list."
    what _ = "a longer list."
```

### Let vs Where

```haskell
-- ความแตกต่างหลัก:
-- let: เป็น expression (มีค่า)
-- where: เป็น declaration (ไม่มีค่าของตัวเอง)

-- ใช้ let เมื่อ:
-- 1. ต้องการใช้ใน expression context
-- 2. ต้องการนิยามในช่วงกลางของ do block
-- 3. ต้องการ let bindings ใน list comprehensions

-- ใช้ where เมื่อ:
-- 1. ต้องการ helper functions หรือ values ที่ใช้หลายที่ในฟังก์ชัน
-- 2. ต้องการให้อ่านง่าย (main logic อยู่บน, helpers อยู่ล่าง)
```

---

## ขั้นตอนที่ 26: If-Then-Else

```haskell
-- if-then-else เป็น expression ใน Haskell (ต่างจาก statement ใน C/Java)
-- ทั้ง then และ else ต้องมี type เดียวกัน

-- รูปแบบพื้นฐาน
absolute :: Int -> Int
absolute n = if n < 0 then -n else n

-- if-then-else แบบ nested
classify :: Int -> String
classify n
  = if n < 0
      then "negative"
      else if n == 0
        then "zero"
        else "positive"

-- ใช้ guards แทนได้ (อ่านง่ายกว่า)
classify' :: Int -> String
classify' n
  | n < 0     = "negative"
  | n == 0    = "zero"
  | otherwise = "positive"

-- if ใน do-notation
checkAge :: Int -> IO ()
checkAge age = do
  let message = if age >= 18
                  then "You can vote!"
                  else "You are too young to vote."
  putStrLn message

-- if เป็น expression ดังนั้นใช้ในนิพจน์ได้
doubleIfPositive :: Int -> Int
doubleIfPositive n = n * (if n > 0 then 2 else 1)
```

---

## ขั้นตอนที่ 27: Tuples

```haskell
-- Tuple: ชุดของค่าหลายค่าที่มี types ต่างกันได้
-- มีขนาดคงที่ (fixed size)

-- 2-tuple (pair)
myPair :: (Int, String)
myPair = (42, "hello")

-- 3-tuple (triple)
myTriple :: (Int, String, Bool)
myTriple = (1, "one", True)

-- 4-tuple
myQuad :: (Int, Double, String, Bool)
myQuad = (1, 3.14, "pi", True)

-- ไม่แนะนำให้ใช้ tuple ขนาดใหญ่ ควรใช้ record แทน

-- การ access tuple
fstExample :: Int
fstExample = fst (1, 2)   -- 1

sndExample :: String
sndExample = snd (1, "hello")   -- "hello"

-- สำหรับ tuple ขนาดใหญ่กว่า ใช้ pattern matching
getThird :: (a, b, c) -> c
getThird (_, _, z) = z

-- Swap tuple
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

ghci> swap (1, "hello")
("hello", 1)
```

### Tuple Functions ที่มีประโยชน์

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- zip: รวม 2 lists เป็น list of pairs
ghci> zip [1,2,3] ["a","b","c"]
[(1,"a"),(2,"b"),(3,"c")]

-- zip หยุดที่ list สั้นกว่า
ghci> zip [1..] ["a","b","c"]
[(1,"a"),(2,"b"),(3,"c")]

-- unzip: แยก list of pairs เป็น 2 lists
ghci> unzip [(1,"a"),(2,"b"),(3,"c")]
([1,2,3],["a","b","c"])

-- zip3: รวม 3 lists
ghci> zip3 [1,2,3] ["a","b","c"] [True, False, True]
[(1,"a",True),(2,"b",False),(3,"c",True)]

-- zipWith: รวมด้วยฟังก์ชัน
ghci> zipWith (+) [1,2,3] [10,20,30]
[11,22,33]

ghci> zipWith (++) ["Hello, ", "Goodbye, "] ["Alice", "Bob"]
["Hello, Alice","Goodbye, Bob"]

-- สร้าง index list
indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

ghci> indexed ["a", "b", "c"]
[(0,"a"),(1,"b"),(2,"c")]
```

---

## ขั้นตอนที่ 28: Lists

```haskell
-- List: sequence ของค่าที่มี type เดียวกัน
-- ขนาดไม่จำกัด (dynamic size)

-- การสร้าง list
emptyList :: [Int]
emptyList = []

numberList :: [Int]
numberList = [1, 2, 3, 4, 5]

stringList :: [String]
stringList = ["apple", "banana", "cherry"]

-- Range
oneToTen :: [Int]
oneToTen = [1..10]

evens :: [Int]
evens = [2, 4..20]    -- [2,4,6,8,10,12,14,16,18,20]

odds :: [Int]
odds = [1, 3..20]     -- [1,3,5,7,9,11,13,15,17,19]

countdown :: [Int]
countdown = [10, 9..1]  -- [10,9,8,7,6,5,4,3,2,1]

-- String เป็น [Char]
helloChars :: [Char]
helloChars = ['H', 'e', 'l', 'l', 'o']
-- เหมือนกับ "Hello"
```

### List Operations

```haskell
-- head: เอา element แรก
ghci> head [1,2,3]
1

ghci> head "Hello"
'H'

-- tail: เอาทุกอย่างยกเว้น element แรก
ghci> tail [1,2,3]
[2,3]

ghci> tail "Hello"
"ello"

-- last: เอา element สุดท้าย (O(n))
ghci> last [1,2,3]
3

-- init: เอาทุกอย่างยกเว้น element สุดท้าย (O(n))
ghci> init [1,2,3]
[1,2]

-- length: ความยาวของ list (O(n))
ghci> length [1,2,3]
3

-- null: ตรวจสอบว่า list ว่างหรือไม่
ghci> null []
True
ghci> null [1,2,3]
False

-- reverse: กลับลำดับ list (O(n))
ghci> reverse [1,2,3]
[3,2,1]

ghci> reverse "Hello"
"olleH"

-- take: เอา n elements แรก
ghci> take 3 [1,2,3,4,5]
[1,2,3]

-- drop: ทิ้ง n elements แรก
ghci> drop 3 [1,2,3,4,5]
[4,5]

-- splitAt: แบ่ง list ที่ตำแหน่ง n
ghci> splitAt 3 [1,2,3,4,5]
([1,2,3],[4,5])

-- elem: ตรวจสอบว่ามี element อยู่หรือไม่
ghci> elem 3 [1,2,3,4,5]
True
ghci> 3 `elem` [1,2,3,4,5]
True

-- !! operator: เข้าถึง element ตาม index (O(n), 0-indexed)
ghci> [1,2,3,4,5] !! 2
3

-- maximum, minimum
ghci> maximum [3,1,4,1,5,9,2,6]
9
ghci> minimum [3,1,4,1,5,9,2,6]
1

-- sum, product
ghci> sum [1..10]
55
ghci> product [1..5]
120

-- concat: รวม list of lists
ghci> concat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]

-- concatMap: map แล้ว concat
ghci> concatMap (\x -> [x, x*2]) [1,2,3]
[1,2,2,4,3,6]

-- replicate: สร้าง list ซ้ำ n ครั้ง
ghci> replicate 3 "ha"
["ha","ha","ha"]

-- cycle: list วนซ้ำไม่สิ้นสุด (infinite)
ghci> take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]

-- repeat: ค่าเดิมซ้ำไม่สิ้นสุด (infinite)
ghci> take 5 (repeat 42)
[42,42,42,42,42]

-- iterate: apply ฟังก์ชันซ้ำๆ (infinite)
ghci> take 10 (iterate (*2) 1)
[1,2,4,8,16,32,64,128,256,512]
```

### Cons Operator (:)

```haskell
-- : (cons) เพิ่ม element ไว้หน้า list
ghci> 1 : [2,3,4]
[1,2,3,4]

ghci> 'H' : "ello"
"Hello"

-- [1,2,3] ก็คือ 1 : 2 : 3 : []
-- หรือ 1:(2:(3:[]))
```

---

## ขั้นตอนที่ 29: List Comprehensions

```haskell
-- List comprehension: สร้าง list จาก pattern
-- คล้ายกับ set notation ในคณิตศาสตร์

-- รูปแบบ: [expression | generator, ..., predicate, ...]

-- สร้าง list ของ squares
squares :: [Int]
squares = [x^2 | x <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]

-- ใช้ predicate (filter)
evenSquares :: [Int]
evenSquares = [x^2 | x <- [1..10], even x]
-- [4,16,36,64,100]

-- หลาย generators (Cartesian product)
pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1..3], y <- [1..3]]
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

-- ไม่เอา pairs ที่เหมือนกัน
uniquePairs :: [(Int, Int)]
uniquePairs = [(x, y) | x <- [1..5], y <- [x..5], x /= y]

-- triangles
rightTriangles :: [(Int, Int, Int)]
rightTriangles = [(a, b, c) |
  c <- [1..20],
  b <- [1..c],
  a <- [1..b],
  a^2 + b^2 == c^2]

ghci> rightTriangles
[(3,4,5),(6,8,10),(5,12,13),(8,15,17),(9,12,15),(12,16,20)]

-- แปลง String เป็น uppercase
import Data.Char (toUpper)
toUpperStr :: String -> String
toUpperStr s = [toUpper c | c <- s]

-- กรองพยัญชนะ
removeVowels :: String -> String
removeVowels s = [c | c <- s, c `notElem` "aeiouAEIOU"]

ghci> removeVowels "Hello World"
"Hll Wrld"

-- nested list comprehensions
flatMatrix :: [[Int]] -> [Int]
flatMatrix matrix = [x | row <- matrix, x <- row]

ghci> flatMatrix [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

---

## ขั้นตอนที่ 30: Type Signatures เชิงลึก

```haskell
-- Type signature อยู่บนบรรทัดก่อนฟังก์ชัน definition
-- รูปแบบ: functionName :: type

-- Simple type signature
myNumber :: Int
myNumber = 42

-- Function type signature (-> คือ function type constructor)
increment :: Int -> Int
increment n = n + 1

-- ฟังก์ชันหลาย arguments
add :: Int -> Int -> Int
add x y = x + y
-- เหมือนกับ add :: Int -> (Int -> Int)
-- Currying! ทุก function ใน Haskell รับ argument เดียว

-- Polymorphic type signature
myId :: a -> a
myId x = x

-- Type constraint
showValue :: Show a => a -> String
showValue x = show x
-- Show a => หมายถึง a ต้อง implement Show typeclass

-- หลาย constraints
sortAndShow :: (Ord a, Show a) => [a] -> String
sortAndShow xs = show (sort xs)
  where sort = foldr insertSorted []
        insertSorted x [] = [x]
        insertSorted x (y:ys)
          | x <= y    = x : y : ys
          | otherwise = y : insertSorted x ys
```

### Type Aliases

```haskell
-- type: สร้าง alias สำหรับ type ที่มีอยู่แล้ว
type Name = String
type Age = Int
type Coordinate = (Double, Double)

-- ใช้ type alias
greeting :: Name -> String
greeting name = "Hello, " ++ name

-- ใช้ใน record
type PersonInfo = (Name, Age)

alice :: PersonInfo
alice = ("Alice", 25)

-- String ก็คือ type alias ของ [Char]
-- String = [Char]
```

---

## ขั้นตอนที่ 31: Newtype

```haskell
-- newtype: สร้าง type ใหม่จาก type ที่มีอยู่ (wrapper)
-- แตกต่างจาก type alias ตรงที่เป็น type ใหม่จริงๆ

newtype Name = Name String
newtype Age = Age Int
newtype Email = Email String

-- ต้อง unwrap เพื่อใช้งาน
getName :: Name -> String
getName (Name n) = n

getAge :: Age -> Int
getAge (Age a) = a

-- ป้องกัน type confusion
welcome :: Name -> Email -> String
welcome (Name n) (Email e) = "Welcome " ++ n ++ " <" ++ e ++ ">"

-- ถ้าไม่มี newtype เราอาจ pass ผิดลำดับได้
-- welcome "alice@email.com" "Alice"  -- compile error!

-- ตัวอย่างการใช้
alice :: Name
alice = Name "Alice"

aliceEmail :: Email
aliceEmail = Email "alice@example.com"

-- welcome aliceEmail alice  -- Type Error!
result :: String
result = welcome alice aliceEmail  -- ถูกต้อง
```

---

## ขั้นตอนที่ 32: Show และ Read

```haskell
-- Show: แปลงค่าเป็น String
ghci> show 42
"42"

ghci> show 3.14
"3.14"

ghci> show True
"True"

ghci> show [1,2,3]
"[1,2,3]"

ghci> show (1, "hello", True)
"(1,\"hello\",True)"

-- Read: แปลง String เป็นค่า
ghci> read "42" :: Int
42

ghci> read "3.14" :: Double
3.14

ghci> read "[1,2,3]" :: [Int]
[1,2,3]

-- reads: parse อย่างปลอดภัย (คืน list of results)
ghci> reads "42 abc" :: [(Int, String)]
[(42," abc")]

-- ปัญหาของ read: ถ้า parse ไม่ได้จะ throw exception
ghci> read "abc" :: Int
*** Exception: Prelude.read: no parse

-- แนะนำให้ใช้ readMaybe แทน
import Text.Read (readMaybe)

safeRead :: Read a => String -> Maybe a
safeRead = readMaybe

ghci> readMaybe "42" :: Maybe Int
Just 42

ghci> readMaybe "abc" :: Maybe Int
Nothing
```

---

## ขั้นตอนที่ 33: Eq และ Ord

```haskell
-- Eq: type class สำหรับ equality comparison
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)  -- default implementation

-- Ord: type class สำหรับ ordering (ต้อง implement Eq ก่อน)
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a

data Ordering = LT | EQ | GT

-- ตัวอย่างการใช้
ghci> 5 == 5
True

ghci> "hello" == "hello"
True

ghci> "hello" /= "world"
True

ghci> compare 3 5
LT

ghci> compare 5 5
EQ

ghci> compare 7 5
GT

ghci> max 3 7
7

ghci> min 3 7
3

-- sort ต้องการ Ord
import Data.List (sort)
ghci> sort [3,1,4,1,5,9,2,6]
[1,1,2,3,4,5,6,9]

ghci> sort "haskell"
"aehklls"
```

---

## ขั้นตอนที่ 34: ทำความเข้าใจ Numeric Operations เชิงลึก

```haskell
-- Numeric conversions ที่ต้องระวัง
ghci> 5 / 2      -- Double division
2.5

ghci> 5 `div` 2  -- Integer division
2

ghci> 5.0 / 2.0  -- Explicit Double
2.5

-- ต้อง explicit conversion
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- ปัญหา: integer overflow ใน Int
ghci> (maxBound :: Int) + 1
-9223372036854775808  -- overflow!

-- ใช้ Integer แทนสำหรับค่าใหญ่
largeFactorial :: Integer -> Integer
largeFactorial 0 = 1
largeFactorial n = n * largeFactorial (n - 1)

ghci> largeFactorial 100
93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000

-- Scientific notation
ghci> 1.5e3    -- 1500.0
1500.0

ghci> 1.5e-3   -- 0.0015
1.5e-3

-- Hexadecimal และ Octal literals
ghci> 0xFF     -- 255
255

ghci> 0o77     -- 63
63
```

---

## ขั้นตอนที่ 35: String Operations เชิงลึก

```haskell
import Data.Char
import Data.List

-- Character operations
ghci> isAlpha 'a'     -- True
ghci> isDigit '5'     -- True
ghci> isSpace ' '     -- True
ghci> toUpper 'a'     -- 'A'
ghci> toLower 'A'     -- 'a'
ghci> ord 'A'         -- 65 (ASCII/Unicode code point)
ghci> chr 65          -- 'A'

-- String operations
ghci> words "hello world foo"
["hello","world","foo"]

ghci> unwords ["hello","world","foo"]
"hello world foo"

ghci> lines "line1\nline2\nline3"
["line1","line2","line3"]

ghci> unlines ["line1","line2","line3"]
"line1\nline2\nline3\n"

-- isPrefixOf, isSuffixOf, isInfixOf
ghci> isPrefixOf "He" "Hello"
True

ghci> isSuffixOf "lo" "Hello"
True

ghci> isInfixOf "ell" "Hello"
True

-- stripPrefix, stripSuffix
import Data.List (stripPrefix)
ghci> stripPrefix "foo" "foobar"
Just "bar"

ghci> stripPrefix "foo" "barfoo"
Nothing

-- intercalate: join list of strings with separator
ghci> intercalate ", " ["apple", "banana", "cherry"]
"apple, banana, cherry"

ghci> intercalate "\n" ["line1", "line2", "line3"]
"line1\nline2\nline3"

-- intersperse: แทรก element ระหว่าง elements
ghci> intersperse ',' "hello"
"h,e,l,l,o"

-- transpose: สลับ rows และ columns
ghci> transpose ["abc", "def", "ghi"]
["adg","beh","cfi"]
```

---

## ขั้นตอนที่ 36: Text vs String

```haskell
-- String (=[Char]) มีปัญหาด้าน performance สำหรับ text จริงๆ
-- ควรใช้ Text หรือ ByteString แทน

-- ติดตั้ง text package ใน .cabal:
-- build-depends: text ^>=2.0

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-# LANGUAGE OverloadedStrings #-}

-- สร้าง Text
myText :: T.Text
myText = "Hello, World!"  -- ต้องการ OverloadedStrings extension

-- String -> Text
fromString :: String -> T.Text
fromString = T.pack

-- Text -> String
toString :: T.Text -> String
toString = T.unpack

-- Text operations
example :: IO ()
example = do
  let t1 = T.pack "Hello"
      t2 = T.pack " World"
      combined = t1 <> t2  -- T.append
  TIO.putStrLn combined

  -- Text ยังมี operations คล้าย String
  print $ T.length (T.pack "Hello")      -- 5
  print $ T.toUpper (T.pack "hello")     -- "HELLO"
  print $ T.words (T.pack "hello world") -- ["hello","world"]
  print $ T.isPrefixOf "He" "Hello"      -- True

-- ByteString สำหรับ binary data หรือ UTF-8 encoded text
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

myBytes :: B.ByteString
myBytes = BC.pack "Hello"
```

---

## ขั้นตอนที่ 37: Number Formatting

```haskell
import Numeric (showHex, showOct, showFFloat, showEFloat)
import Data.List (intercalate)
import Text.Printf

-- Printf สำหรับ formatting
main :: IO ()
main = do
  -- Integer formatting
  printf "Decimal: %d\n" (42 :: Int)
  printf "Hex: %x\n" (255 :: Int)
  printf "Octal: %o\n" (8 :: Int)

  -- Float formatting
  printf "Fixed: %.2f\n" (3.14159 :: Double)
  printf "Scientific: %e\n" (12345.678 :: Double)
  printf "Generic: %g\n" (0.0001234 :: Double)

  -- String formatting
  printf "Padded: %10s\n" ("hello" :: String)
  printf "Left: %-10s|\n" ("hello" :: String)

-- Numeric module
ghci> showHex 255 ""
"ff"

ghci> showOct 8 ""
"10"

ghci> showFFloat (Just 2) 3.14159 ""
"3.14"

ghci> showEFloat (Just 3) 12345.678 ""
"1.235e4"
```

---

## ขั้นตอนที่ 38: ความเข้าใจเรื่อง Evaluation

```haskell
-- Haskell ใช้ lazy evaluation (call-by-need)
-- ค่าจะถูกคำนวณเมื่อจำเป็น

-- thunk: ค่าที่ยังไม่ได้คำนวณ
x :: Int
x = 1 + 2   -- x คือ thunk ที่เก็บ expression "1 + 2"
             -- จะคำนวณเมื่อ x ถูกใช้

-- Strict evaluation ด้วย seq และ $!
-- seq: บังคับให้ evaluate argument แรกก่อน
force :: a -> b -> b
force x y = x `seq` y

-- $! เป็น strict function application
strictApply :: (a -> b) -> a -> b
strictApply f x = f $! x

-- การ force evaluation
import Control.DeepSeq

-- deepseq: evaluate ลึกทั้ง data structure
result :: [Int]
result = [1,2,3] `deepseq` [4,5,6]

-- ปัญหา space leak กับ foldl
-- ไม่ดี: foldl สะสม thunks ทำให้ใช้ memory มาก
badSum :: [Int] -> Int
badSum = foldl (+) 0

-- ดีกว่า: foldl' evaluate ทุก step
import Data.List (foldl')
goodSum :: [Int] -> Int
goodSum = foldl' (+) 0
```

---

## ขั้นตอนที่ 39: Numeric Literals และ Overloading

```haskell
-- ใน Haskell numeric literals เป็น polymorphic
ghci> :type 42
42 :: Num a => a

-- 42 สามารถเป็น Int, Integer, Double, Float, etc.
fortyTwoInt :: Int
fortyTwoInt = 42

fortyTwoInteger :: Integer
fortyTwoInteger = 42

fortyTwoDouble :: Double
fortyTwoDouble = 42

-- Fractional literals ก็ polymorphic เช่นกัน
ghci> :type 3.14
3.14 :: Fractional a => a

-- fromInteger: แปลง Integer literal เป็น Num
-- fromRational: แปลง Rational literal เป็น Fractional

-- ตัวอย่างที่อาจ confuse
ghci> (2 :: Double) + (3 :: Int)
-- Error! ไม่สามารถบวก Double กับ Int โดยตรง

ghci> (2 :: Double) + fromIntegral (3 :: Int)
5.0   -- ถูกต้อง ต้อง convert ก่อน
```

---

## ขั้นตอนที่ 40: สรุปและแบบฝึกหัด Part 02

### สิ่งที่เรียนรู้

1. ✅ Basic Types: Int, Integer, Double, Float, Bool, Char, String
2. ✅ Type Inference
3. ✅ Let Expressions และ Where Clauses
4. ✅ If-Then-Else Expressions
5. ✅ Tuples
6. ✅ Lists และ List Operations
7. ✅ List Comprehensions
8. ✅ Type Signatures เชิงลึก
9. ✅ Show, Read, Eq, Ord
10. ✅ String vs Text

### แบบฝึกหัด

```haskell
-- 1. เขียนฟังก์ชัน pythagoras ที่คำนวณ hypotenuse
pythagoras :: Double -> Double -> Double
pythagoras a b = sqrt (a^2 + b^2)

-- 2. เขียนฟังก์ชัน clamp ที่จำกัดค่าในช่วง [lo, hi]
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- 3. เขียน List Comprehension สำหรับหา Pythagorean triples
-- (a, b, c) where a^2 + b^2 = c^2 และ a,b,c <= 20
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a, b, c) | c <- [1..20],
                                   b <- [1..c],
                                   a <- [1..b],
                                   a^2 + b^2 == c^2]

-- 4. เขียนฟังก์ชัน capitalizeWords ที่ capitalize ทุกคำใน string
import Data.Char (toUpper, toLower)
capitalizeWords :: String -> String
capitalizeWords = unwords . map capitalize . words
  where
    capitalize [] = []
    capitalize (c:cs) = toUpper c : map toLower cs

-- 5. เขียนฟังก์ชัน frequencies ที่นับจำนวนครั้งของแต่ละ element
import Data.Map (Map)
import qualified Data.Map as Map

frequencies :: Ord a => [a] -> Map a Int
frequencies = foldr (\x -> Map.insertWith (+) x 1) Map.empty
```

### คำตอบทดสอบ

```bash
ghci> pythagoras 3 4
5.0

ghci> clamp 0 10 (-5)
0
ghci> clamp 0 10 15
10
ghci> clamp 0 10 7
7

ghci> pythagoreanTriples
[(3,4,5),(6,8,10),(5,12,13),(8,15,17),(9,12,15),(12,16,20)]

ghci> capitalizeWords "hello world foo bar"
"Hello World Foo Bar"

ghci> frequencies "abracadabra"
fromList [('a',5),('b',2),('c',1),('d',1),('r',2)]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 03 เราจะเรียนเรื่อง **Functions และ Lambda** ใน Haskell:
- Function Application
- Lambda Expressions
- Higher-Order Functions พื้นฐาน
- Function Composition
- Partial Application และ Currying

---

*[← Part 01](part-01.md) | [Part 03 →](part-03.md)*
