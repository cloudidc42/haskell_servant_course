# Part 06: Higher-Order Functions และ Type Classes
## ขั้นตอนที่ 101-120: Type Classes และ Polymorphism

---

## บทนำ

Type Classes ใน Haskell เป็นกลไกสำหรับ polymorphism ที่ทรงพลัง คล้ายกับ interfaces ใน Java หรือ protocols ใน Swift แต่มีความสามารถมากกว่ามาก

---

## ขั้นตอนที่ 101: Type Classes พื้นฐาน

```haskell
-- Type Class คือ interface ที่กำหนด operations
-- Types ที่ implement type class เรียกว่า instances

-- ดู type classes ที่สำคัญ
-- Show: แปลงเป็น String
class Show a where
  show :: a -> String
  showList :: [a] -> ShowS

-- Eq: equality comparison
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)  -- default

-- Ord: ordering (requires Eq)
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool

-- Num: numeric operations
class Num a where
  (+), (-), (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

-- Enum: enumeration
class Enum a where
  toEnum :: Int -> a
  fromEnum :: a -> Int
  -- default methods: succ, pred, [x..], [x,y..], etc.

-- Bounded: has min and max
class Bounded a where
  minBound :: a
  maxBound :: a
```

---

## ขั้นตอนที่ 102: สร้าง Type Class ของตัวเอง

```haskell
-- สร้าง type class ชื่อ Describable
class Describable a where
  describe :: a -> String

-- สร้าง instance สำหรับ Int
instance Describable Int where
  describe n
    | n < 0     = "negative number: " ++ show n
    | n == 0    = "zero"
    | otherwise = "positive number: " ++ show n

-- สร้าง instance สำหรับ Bool
instance Describable Bool where
  describe True  = "the truth"
  describe False = "a lie"

-- สร้าง instance สำหรับ list
instance Describable a => Describable [a] where
  describe []  = "empty list"
  describe [x] = "list with one element: " ++ describe x
  describe xs  = "list with " ++ show (length xs) ++ " elements"

-- ใช้งาน
ghci> describe (42 :: Int)
"positive number: 42"

ghci> describe True
"the truth"

ghci> describe ([1,2,3] :: [Int])
"list with 3 elements"

-- Type class กับ default methods
class Container f where
  empty  :: f a
  insert :: a -> f a -> f a
  toList :: f a -> [a]
  
  -- Default method
  size :: f a -> Int
  size = length . toList
  
  isEmpty :: f a -> Bool
  isEmpty = null . toList
```

---

## ขั้นตอนที่ 103: Deriving

```haskell
-- Haskell สามารถ auto-derive implementations ของ type classes ทั่วไป

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord, Enum, Bounded)

ghci> show Red
"Red"

ghci> Red == Blue
False

ghci> Red < Blue
True   -- ลำดับตาม definition

ghci> [minBound..maxBound] :: [Color]
[Red,Green,Blue]

ghci> succ Red
Green

-- Record types กับ deriving
data Point = Point { x :: Double, y :: Double }
  deriving (Show, Eq)

ghci> show (Point 1.0 2.0)
"Point {x = 1.0, y = 2.0}"

ghci> Point 1.0 2.0 == Point 1.0 2.0
True

-- Ord ต้องการ Eq ก่อน
data Priority = Low | Medium | High
  deriving (Show, Eq, Ord, Enum, Bounded)

ghci> compare Low High
LT

ghci> [Low, High, Medium]
[Low,High,Medium]

ghci> sort [Low, High, Medium, Low]
[Low,Low,Medium,High]
  where sort = Data.List.sort
```

### DerivingStrategies

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Control.DeepSeq (NFData)

data Person = Person
  { name :: String
  , age  :: Int
  } deriving stock (Show, Eq, Generic)    -- stock deriving
    deriving anyclass (ToJSON, FromJSON, NFData)  -- generic deriving via GHC.Generics
```

---

## ขั้นตอนที่ 104: Functor

```haskell
-- Functor: type class สำหรับ mapping over a structure

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- หรือเทียบเท่า
-- fmap :: (a -> b) -> f a -> f b
-- <$>  :: (a -> b) -> f a -> f b  (infix version)

-- Functor laws:
-- 1. fmap id = id                    (identity)
-- 2. fmap (f . g) = fmap f . fmap g  (composition)

-- Instances ที่สำคัญ:

-- Maybe Functor
-- fmap f Nothing  = Nothing
-- fmap f (Just x) = Just (f x)

ghci> fmap (*2) (Just 5)
Just 10

ghci> fmap (*2) Nothing
Nothing

-- List Functor
-- fmap = map
ghci> fmap (*2) [1,2,3]
[2,4,6]

-- Either Functor (maps over Right)
ghci> fmap (*2) (Right 5 :: Either String Int)
Right 10

ghci> fmap (*2) (Left "error" :: Either String Int)
Left "error"

-- ((->) r) Functor
-- fmap f g = f . g
ghci> fmap (+1) (*2) $ 3    -- (+1) . (*2) $ 3 = 7
7

-- IO Functor
ghci> fmap length getLine
-- อ่าน string แล้วคืน length ของมัน

-- Tree Functor (custom type)
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- <$> เป็น infix version ของ fmap
ghci> (*2) <$> [1,2,3]
[2,4,6]

ghci> (*2) <$> Just 5
Just 10

-- <$ แทนที่ทุก element ด้วยค่าเดียวกัน
ghci> 'x' <$ [1,2,3]
"xxx"

ghci> True <$ Just 5
Just True
```

---

## ขั้นตอนที่ 105: Foldable

```haskell
-- Foldable: type class สำหรับ structures ที่ fold ได้

class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  -- มี default implementations สำหรับ methods อื่น

-- Instances:

-- List (default)
-- Tree
-- Maybe (fold over Just)
-- (,) (fold over second element)

-- Methods ที่ derive จาก foldr:
-- fold, foldMap, foldl, foldl', foldr', toList
-- null, length, elem, maximum, minimum, sum, product
-- and, or, any, all, concat, concatMap

-- Custom Foldable
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Foldable Tree where
  foldr _ z Leaf           = z
  foldr f z (Node l x r)  = foldr f (f x (foldr f z r)) l

-- ตอนนี้ Tree ใช้ functions ทั้งหมดของ Foldable ได้
myTree :: Tree Int
myTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

ghci> sum myTree
6

ghci> maximum myTree
3

ghci> elem 2 myTree
True

ghci> toList myTree
[1,2,3]

ghci> length myTree
3

-- Foldable กับ structures อื่น
ghci> sum (Just 5)
5

ghci> sum Nothing
0

ghci> product [1..5]
120

ghci> and [True, True, True]
True

ghci> or [False, False, True]
True
```

---

## ขั้นตอนที่ 106: Traversable

```haskell
-- Traversable: type class สำหรับ traversal ด้วย effects

class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  
  -- default implementations
  traverse f = sequenceA . fmap f
  sequenceA  = traverse id

-- ตัวอย่างการใช้งาน

-- traverse กับ Maybe
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

ghci> traverse (safeDiv 10) [1, 2, 5]
Just [10,5,2]

ghci> traverse (safeDiv 10) [1, 0, 5]
Nothing   -- หยุดทันทีที่เจอ Nothing

-- traverse กับ IO
readLines :: [FilePath] -> IO [String]
readLines = traverse readFile

-- sequenceA: "flip" structure of effects
ghci> sequenceA [Just 1, Just 2, Just 3]
Just [1,2,3]

ghci> sequenceA [Just 1, Nothing, Just 3]
Nothing

ghci> sequenceA [[1,2],[3,4]]
[[1,3],[1,4],[2,3],[2,4]]   -- Cartesian product!

-- mapM เป็น traverse สำหรับ Monad
ghci> mapM (safeDiv 10) [1, 2, 5]
Just [10,5,2]

-- sequence เป็น sequenceA สำหรับ Monad
ghci> sequence [Just 1, Just 2]
Just [1,2]

-- Custom Traversable
data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Node l x r) =
    Node <$> traverse f l <*> f x <*> traverse f r
```

---

## ขั้นตอนที่ 107: Monoid

```haskell
-- Monoid: type class สำหรับ structures ที่มี "empty" และ "combine"

class Semigroup a => Monoid a where
  mempty  :: a           -- identity element
  mappend :: a -> a -> a  -- combine (deprecated, use <> from Semigroup)
  mconcat :: [a] -> a    -- combine list (default: foldr mappend mempty)

-- Semigroup: requires only (<>)
class Semigroup a where
  (<>) :: a -> a -> a

-- Monoid laws:
-- mempty <> x = x
-- x <> mempty = x
-- (x <> y) <> z = x <> (y <> z)

-- Instances:

-- String/[a]: concatenation
ghci> "hello" <> " " <> "world"
"hello world"

ghci> mempty :: String
""

ghci> mconcat ["hello", " ", "world"]
"hello world"

-- Maybe: lifts Semigroup operation
ghci> Just "hello" <> Just " world"
Just "hello world"

ghci> Nothing <> Just "hello"
Just "hello"

ghci> Just "hello" <> Nothing
Just "hello"

-- Sum/Product newtypes
import Data.Monoid (Sum(..), Product(..))

ghci> mconcat (map Sum [1..5])
Sum {getSum = 15}

ghci> mconcat (map Product [1..5])
Product {getProduct = 120}

-- All/Any
import Data.Monoid (All(..), Any(..))

ghci> mconcat (map All [True, True, True])
All {getAll = True}

ghci> mconcat (map Any [False, False, True])
Any {getAny = True}

-- Custom Monoid
newtype Max a = Max { getMax :: a } deriving (Show)

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

ghci> getMax (mconcat (map Max [3,1,4,1,5,9,2,6]))
9
```

---

## ขั้นตอนที่ 108: Semigroup Examples

```haskell
-- ใช้ Semigroup สำหรับ combining data

-- Validated: accumulate errors
data Validated e a = Failure [e] | Success a

instance Semigroup (Validated e a) where
  Failure e1 <> Failure e2 = Failure (e1 ++ e2)
  Failure e  <> _          = Failure e
  _          <> Failure e  = Failure e
  Success a  <> _          = Success a

-- Configuration combination
data Config = Config
  { host    :: Maybe String
  , port    :: Maybe Int
  , debug   :: Maybe Bool
  } deriving (Show)

instance Semigroup Config where
  c1 <> c2 = Config
    { host  = host c1  <|> host c2
    , port  = port c1  <|> port c2
    , debug = debug c1 <|> debug c2
    }
    where (<|>) = \a b -> case a of { Nothing -> b; x -> x }

instance Monoid Config where
  mempty = Config Nothing Nothing Nothing

-- Default config
defaultConfig :: Config
defaultConfig = Config
  { host  = Just "localhost"
  , port  = Just 8080
  , debug = Just False
  }

-- User config (overrides default)
userConfig :: Config
userConfig = Config
  { host  = Nothing      -- use default
  , port  = Just 9090    -- override port
  , debug = Just True    -- enable debug
  }

finalConfig :: Config
finalConfig = userConfig <> defaultConfig

ghci> finalConfig
Config {host = Just "localhost", port = Just 9090, debug = Just True}
```

---

## ขั้นตอนที่ 109: Newtype Deriving

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Newtype Deriving: derive instances โดย reuse ของ underlying type

newtype Sum' a = Sum' { getSum' :: a }
  deriving (Show, Eq, Ord, Num)   -- reuse Num from a

ghci> Sum' 3 + Sum' 4
Sum' {getSum' = 7}

-- Useful pattern: newtype สำหรับ avoiding orphan instances
newtype Age = Age { unAge :: Int }
  deriving (Show, Eq, Ord, Num, Enum, Bounded)

age :: Age
age = 25

ghci> age + 1
Age {unAge = 26}

-- Newtype สำหรับ wrapping behaviors
newtype Reversed a = Reversed { getReversed :: a }

instance Ord a => Eq (Reversed a) where
  Reversed x == Reversed y = x == y

instance Ord a => Ord (Reversed a) where
  compare (Reversed x) (Reversed y) = compare y x   -- reversed!

import Data.List (sort)
ghci> map getReversed . sort . map Reversed $ [3,1,4,1,5,9,2,6]
[9,6,5,4,3,2,1,1]
```

---

## ขั้นตอนที่ 110: Num และ Numeric Classes

```haskell
-- สร้าง custom Num instance

data Vec2 = Vec2 Double Double deriving (Eq)

instance Show Vec2 where
  show (Vec2 x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Num Vec2 where
  (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
  (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
  (Vec2 x1 y1) * (Vec2 x2 y2) = Vec2 (x1*x2 - y1*y2) (x1*y2 + y1*x2)  -- complex multiplication
  negate (Vec2 x y)  = Vec2 (-x) (-y)
  abs (Vec2 x y)     = Vec2 (sqrt (x*x + y*y)) 0
  signum v@(Vec2 x y)
    | magnitude == 0 = Vec2 0 0
    | otherwise      = Vec2 (x / magnitude) (y / magnitude)
    where magnitude = let Vec2 m _ = abs v in m
  fromInteger n = Vec2 (fromInteger n) 0

ghci> Vec2 1 2 + Vec2 3 4
(4.0, 6.0)

ghci> Vec2 1 2 * Vec2 3 4    -- complex multiplication
(-5.0, 10.0)

-- Matrix type with Num
data Matrix2x2 = Matrix2x2 Double Double Double Double

instance Num Matrix2x2 where
  (Matrix2x2 a b c d) + (Matrix2x2 e f g h) = Matrix2x2 (a+e) (b+f) (c+g) (d+h)
  (Matrix2x2 a b c d) * (Matrix2x2 e f g h) = Matrix2x2
    (a*e+b*g) (a*f+b*h)
    (c*e+d*g) (c*f+d*h)
  negate (Matrix2x2 a b c d) = Matrix2x2 (-a) (-b) (-c) (-d)
  fromInteger n = let x = fromInteger n in Matrix2x2 x 0 0 x
  abs = undefined
  signum = undefined
```

---

## ขั้นตอนที่ 111: Read Type Class

```haskell
-- Read: opposite ของ Show

-- ง่ายๆ
ghci> read "42" :: Int
42

ghci> read "[1,2,3]" :: [Int]
[1,2,3]

-- ปัญหา: read throws exception ถ้า parse fails
-- ใช้ readMaybe แทน
import Text.Read (readMaybe, readEither)

ghci> readMaybe "42" :: Maybe Int
Just 42

ghci> readMaybe "abc" :: Maybe Int
Nothing

ghci> readEither "42" :: Either String Int
Right 42

ghci> readEither "abc" :: Either String Int
Left "Prelude.read: no parse"

-- Custom Read instance (ยุ่งยาก, แนะนำให้ใช้ parser library แทน)
data Color = Red | Green | Blue deriving (Show)

instance Read Color where
  readsPrec _ s = case s of
    ('R':'e':'d':rest)   -> [(Red, rest)]
    ('G':'r':'e':'e':'n':rest) -> [(Green, rest)]
    ('B':'l':'u':'e':rest)  -> [(Blue, rest)]
    _                    -> []

ghci> read "Red" :: Color
Red

-- ใช้ reads สำหรับ more control
ghci> reads "42abc" :: [(Int, String)]
[(42,"abc")]

ghci> reads "abc" :: [(Int, String)]
[]
```

---

## ขั้นตอนที่ 112: Enum Type Class

```haskell
-- Enum: types ที่มี sequential ordering

-- Built-in Enum instances
ghci> [1..5]
[1,2,3,4,5]

ghci> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"

ghci> succ 'a'
'b'

ghci> pred 'z'
'y'

ghci> toEnum 65 :: Char
'A'

ghci> fromEnum 'A'
65

-- Custom Enum
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq, Ord, Enum, Bounded)

ghci> [Monday..Sunday]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

ghci> succ Monday
Tuesday

ghci> [Monday, Wednesday..Sunday]
[Monday,Wednesday,Friday,Sunday]

ghci> toEnum 0 :: Day
Monday

ghci> fromEnum Friday
4

-- Weekdays
weekdays :: [Day]
weekdays = [Monday..Friday]

-- Next weekday
nextWeekday :: Day -> Day
nextWeekday Friday = Monday
nextWeekday d      = succ d
```

---

## ขั้นตอนที่ 113: Bounded Type Class

```haskell
-- Bounded: types ที่มี minimum และ maximum

ghci> minBound :: Int
-9223372036854775808

ghci> maxBound :: Int
9223372036854775807

ghci> minBound :: Char
'\NUL'

ghci> maxBound :: Char
'\1114111'

ghci> minBound :: Bool
False

ghci> maxBound :: Bool
True

-- Custom Bounded
data Priority = Low | Medium | High
  deriving (Show, Eq, Ord, Enum, Bounded)

ghci> minBound :: Priority
Low

ghci> maxBound :: Priority
High

ghci> [minBound..maxBound] :: [Priority]
[Low,Medium,High]

-- Useful: enumerate all values
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

ghci> allValues :: [Bool]
[False,True]

ghci> allValues :: [Priority]
[Low,Medium,High]
```

---

## ขั้นตอนที่ 114: Typeable และ Data

```haskell
import Data.Typeable
import Data.Data

-- Typeable: runtime type information
{-# LANGUAGE DeriveDataTypeable #-}

data Person = Person String Int deriving (Show, Data, Typeable)

-- typeOf ให้ runtime type
ghci> typeOf (42 :: Int)
Int

ghci> typeOf "hello"
[Char]

ghci> typeOf (Just True)
Maybe Bool

-- cast: safe casting
cast :: (Typeable a, Typeable b) => a -> Maybe b

ghci> cast (42 :: Int) :: Maybe Int
Just 42

ghci> cast (42 :: Int) :: Maybe String
Nothing

-- SomeException ใน exception handling
data SomeException = forall e. Exception e => SomeException e
```

---

## ขั้นตอนที่ 115: Coerce และ Newtype Operations

```haskell
import Data.Coerce

-- coerce: zero-cost conversion ระหว่าง newtype และ underlying type
newtype Name = Name String deriving (Show)
newtype Email = Email String deriving (Show)

-- coerce สำหรับ safe conversion
nameToString :: Name -> String
nameToString = coerce

-- ตัวอย่างที่ใช้บ่อย
import Data.List (sortBy)
import Data.Ord (comparing)

newtype Age = Age Int deriving (Show, Eq, Ord)

people :: [(Name, Age)]
people = [(Name "Bob", Age 30), (Name "Alice", Age 25), (Name "Charlie", Age 35)]

sortByAge :: [(Name, Age)] -> [(Name, Age)]
sortByAge = sortBy (comparing snd)

ghci> sortByAge people
[(Name "Alice",Age 25),(Name "Bob",Age 30),(Name "Charlie",Age 35)]
```

---

## ขั้นตอนที่ 116: Type Class Instances กับ Constraints

```haskell
-- Instances กับ multiple constraints

-- Show instance สำหรับ pair
instance (Show a, Show b) => Show (a, b) where
  show (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- Custom instance กับ constraints
class Container f where
  empty  :: f a
  insert :: a -> f a -> f a
  toList :: f a -> [a]

newtype Stack a = Stack [a] deriving (Show)

instance Container Stack where
  empty = Stack []
  insert x (Stack xs) = Stack (x:xs)
  toList (Stack xs) = xs

-- Functions ที่ใช้ type class constraint
printAll :: (Container f, Show a) => f a -> IO ()
printAll = mapM_ print . toList

-- Higher-kinded constraints
class (Functor f) => MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b
  myFmap = fmap   -- default using Functor

-- Constraint kinds
{-# LANGUAGE ConstraintKinds #-}
import GHC.Exts (Constraint)

type ShowOrd a = (Show a, Ord a)

printSorted :: ShowOrd a => [a] -> IO ()
printSorted = mapM_ print . sort
  where sort = Data.List.sort
```

---

## ขั้นตอนที่ 117: Instances ที่ซับซ้อน

```haskell
-- Instance ที่อาจมีปัญหา: Overlapping Instances

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

class Printable a where
  printIt :: a -> String

instance Printable Int where
  printIt n = "Int: " ++ show n

instance Printable String where
  printIt s = "String: " ++ s

-- FlexibleInstances ต้องการสำหรับ [Char]
instance {-# OVERLAPPING #-} Printable [Char] where
  printIt s = "Str: " ++ s

instance Show a => Printable [a] where
  printIt xs = "List: " ++ show xs

-- Incoherent Instances (ใช้ด้วยความระมัดระวัง)
-- {-# INCOHERENT #-} อนุญาตให้ GHC เลือก instance ใดก็ได้

-- Orphan Instances: instance สำหรับ type ที่ define นอก module ทั้งคู่
-- ควรหลีกเลี่ยงเพราะอาจเกิด conflicts
```

---

## ขั้นตอนที่ 118: Multiparameter Type Classes

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Multiparameter Type Class: type class กับหลาย type parameters

class Convert a b where
  convert :: a -> b

instance Convert Int Double where
  convert = fromIntegral

instance Convert String Int where
  convert = read

-- FunctionalDependencies: บอก GHC ว่า a กำหนด b (หรือ b กำหนด a)
class Container f e | f -> e where
  empty :: f
  insert :: e -> f -> f

-- Class ที่มี 2 parameters
class Elem container element | container -> element where
  member :: element -> container -> Bool
  toList :: container -> [element]

-- Instance สำหรับ List
instance Eq a => Elem [a] a where
  member = elem
  toList = id

-- Instance สำหรับ Set
instance Ord a => Elem (Set.Set a) a where
  member = Set.member
  toList = Set.toList
```

---

## ขั้นตอนที่ 119: Type Class Best Practices

```haskell
-- 1. Minimal complete definition
class Eq a where
  (==) :: a -> a -> Bool
  x /= y = not (x == y)   -- default
  {-# MINIMAL (==) #-}    -- GHC pragma ระบุ minimal set

-- 2. ใช้ default methods อย่างชาญฉลาด
class Collection c where
  empty    :: c a
  insert   :: a -> c a -> c a
  delete   :: Eq a => a -> c a -> c a
  member   :: Eq a => a -> c a -> Bool
  toList   :: c a -> [a]
  fromList :: [a] -> c a

  -- defaults
  member x c = x `elem` toList c
  fromList   = foldr insert empty

-- 3. Avoid making laws too restrictive
-- Functor law: fmap id = id (บางครั้ง relax ได้สำหรับ performance)

-- 4. Class hierarchy
class (Eq a) => Ord a    -- Ord requires Eq
class (Functor f, Foldable f) => Traversable f  -- require both

-- 5. ใช้ type class สำหรับ abstraction ไม่ใช่สำหรับ multiple dispatch ง่ายๆ
-- ถ้าต้องการแค่ subtyping ง่ายๆ ใช้ data types แทน

-- 6. Document laws
-- | Instances must satisfy:
-- prop> x <> mempty == x
-- prop> mempty <> x == x
-- prop> (x <> y) <> z == x <> (y <> z)
class (Semigroup a) => Monoid a where
  mempty  :: a
  mconcat :: [a] -> a
  mconcat = foldr (<>) mempty
```

---

## ขั้นตอนที่ 120: สรุปและแบบฝึกหัด Part 06

### สิ่งที่เรียนรู้

1. ✅ Type Classes พื้นฐาน (Show, Eq, Ord, Num)
2. ✅ สร้าง Custom Type Classes
3. ✅ Deriving Strategies
4. ✅ Functor, Foldable, Traversable
5. ✅ Monoid และ Semigroup
6. ✅ Enum, Bounded
7. ✅ Multiparameter Type Classes
8. ✅ Type Class Best Practices

### Project: Validation Library

```haskell
{-# LANGUAGE FlexibleInstances #-}

module Validation where

import Data.List (intercalate)

-- Validation type: accumulates errors
data Validation e a = Invalid [e] | Valid a
  deriving (Show, Eq)

instance Functor (Validation e) where
  fmap _ (Invalid es) = Invalid es
  fmap f (Valid x)    = Valid (f x)

instance Semigroup (Validation e a) where
  Invalid e1 <> Invalid e2 = Invalid (e1 ++ e2)
  Invalid e  <> _          = Invalid e
  _          <> Invalid e  = Invalid e
  Valid a    <> _          = Valid a

-- Applicative instance สำหรับ accumulate errors
instance Applicative (Validation e) where
  pure = Valid
  Invalid e1 <*> Invalid e2 = Invalid (e1 ++ e2)
  Invalid e  <*> _          = Invalid e
  _          <*> Invalid e  = Invalid e
  Valid f    <*> Valid x    = Valid (f x)

-- Validators
type Validator a b = a -> Validation String b

nonEmpty :: Validator String String
nonEmpty "" = Invalid ["must not be empty"]
nonEmpty s  = Valid s

minLength :: Int -> Validator String String
minLength n s
  | length s >= n = Valid s
  | otherwise     = Invalid ["must be at least " ++ show n ++ " characters"]

maxLength :: Int -> Validator String String
maxLength n s
  | length s <= n = Valid s
  | otherwise     = Invalid ["must be at most " ++ show n ++ " characters"]

isEmail :: Validator String String
isEmail s
  | '@' `elem` s = Valid s
  | otherwise    = Invalid ["must be a valid email address"]

isPositive :: Validator Int Int
isPositive n
  | n > 0     = Valid n
  | otherwise = Invalid ["must be positive"]

-- Combine validators
(>>>) :: Validator a b -> Validator b c -> Validator a c
v1 >>> v2 = \x -> case v1 x of
  Invalid e -> Invalid e
  Valid y   -> v2 y

-- Validate form data
data UserForm = UserForm String String Int deriving (Show)

validateUsername :: Validator String String
validateUsername = nonEmpty >>> minLength 3 >>> maxLength 20

validateEmail :: Validator String String
validateEmail = nonEmpty >>> isEmail

validateAge :: Validator Int Int
validateAge = isPositive

validateUser :: String -> String -> Int -> Validation String UserForm
validateUser username email age =
  UserForm
    <$> validateUsername username
    <*> validateEmail email
    <*> validateAge age

-- ทดสอบ
ghci> validateUser "alice" "alice@example.com" 25
Valid (UserForm "alice" "alice@example.com" 25)

ghci> validateUser "" "not-an-email" (-5)
Invalid ["must not be empty","must be a valid email address","must be positive"]

ghci> validateUser "al" "alice@example.com" 25
Invalid ["must be at least 3 characters"]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 07 เราจะเรียนเรื่อง **Type Classes ขั้นสูง** และ **GADTs**:
- Type Class Instances ที่ซับซ้อน
- GADTs (Generalized Algebraic Data Types)
- Type Families
- Associated Types

---

*[← Part 05](part-05.md) | [Part 07 →](part-07.md)*
