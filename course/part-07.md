# Part 07: Data Types และ Records
## ขั้นตอนที่ 121-140: Algebraic Data Types

---

## บทนำ

Algebraic Data Types (ADTs) เป็นหัวใจของการเขียนโปรแกรมใน Haskell ช่วยให้เราสร้าง data structures ที่ expressive และ type-safe

---

## ขั้นตอนที่ 121: Algebraic Data Types

```haskell
-- ADTs มี 2 รูปแบบหลัก:
-- 1. Sum Types (Either/Or): หนึ่งใน choices
-- 2. Product Types (Both/And): รวมหลายค่า

-- Sum Type: Color เป็น Red หรือ Green หรือ Blue
data Color = Red | Green | Blue
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Product Type: Point คือ (x, y)
data Point = Point Double Double
  deriving (Show, Eq)

-- Sum Type ที่มี data (tagged union)
data Shape
  = Circle Double               -- radius
  | Rectangle Double Double     -- width, height
  | Triangle Double Double Double  -- sides a, b, c
  deriving (Show, Eq)

area :: Shape -> Double
area (Circle r)         = pi * r * r
area (Rectangle w h)    = w * h
area (Triangle a b c)   = let s = (a + b + c) / 2  -- Heron's formula
                           in sqrt (s * (s-a) * (s-b) * (s-c))

perimeter :: Shape -> Double
perimeter (Circle r)         = 2 * pi * r
perimeter (Rectangle w h)    = 2 * (w + h)
perimeter (Triangle a b c)   = a + b + c

ghci> area (Circle 5)
78.53981633974483

ghci> area (Rectangle 4 6)
24.0

ghci> area (Triangle 3 4 5)
6.0
```

---

## ขั้นตอนที่ 122: Record Syntax

```haskell
-- Record syntax: ให้ field names

-- แบบ positional (ไม่แนะนำสำหรับ many fields)
data Person1 = Person1 String Int String

-- แบบ record (แนะนำ)
data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  , email     :: String
  } deriving (Show, Eq)

-- สร้าง record
alice :: Person
alice = Person
  { firstName = "Alice"
  , lastName  = "Smith"
  , age       = 25
  , email     = "alice@example.com"
  }

-- Access fields
ghci> firstName alice
"Alice"

ghci> age alice
25

-- Record update syntax
olderAlice :: Person
olderAlice = alice { age = 26 }

ghci> age olderAlice
26
ghci> firstName olderAlice
"Alice"   -- ไม่เปลี่ยน

-- Pattern matching กับ records
greet :: Person -> String
greet Person { firstName = fn, lastName = ln } =
  "Hello, " ++ fn ++ " " ++ ln ++ "!"

-- RecordWildCards extension
{-# LANGUAGE RecordWildCards #-}
greet' :: Person -> String
greet' Person{..} = "Hello, " ++ firstName ++ " " ++ lastName ++ "!"

ghci> greet alice
"Hello, Alice Smith!"
```

---

## ขั้นตอนที่ 123: Recursive Data Types

```haskell
-- Recursive ADT: type ที่อ้างถึงตัวเอง

-- Linked List (ทำเองเพื่อเข้าใจ)
data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- แปลงไปมา
fromList :: [a] -> List a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

-- Binary Tree
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Insert into BST
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r)
  | x < v     = Node (insert x l) v r
  | x > v     = Node l v (insert x r)
  | otherwise = Node l v r    -- already exists

-- Search in BST
search :: Ord a => a -> Tree a -> Bool
search _ Leaf = False
search x (Node l v r)
  | x == v    = True
  | x < v     = search x l
  | otherwise = search x r

-- Create BST from list
fromListBST :: Ord a => [a] -> Tree a
fromListBST = foldl (flip insert) Leaf

-- Inorder traversal (returns sorted list)
inorder :: Tree a -> [a]
inorder Leaf           = []
inorder (Node l v r)   = inorder l ++ [v] ++ inorder r

ghci> inorder (fromListBST [5, 3, 7, 1, 4, 6, 8])
[1,3,4,5,6,7,8]
```

---

## ขั้นตอนที่ 124: Rose Tree

```haskell
-- Rose Tree: tree ที่แต่ละ node มีหลาย children

data RoseTree a = RoseNode a [RoseTree a]
  deriving (Show, Eq)

-- File system representation
type FileName = String
type FileSize = Int

data FileSystem
  = File FileName FileSize
  | Directory FileName [FileSystem]
  deriving (Show)

-- Total size
totalSize :: FileSystem -> FileSize
totalSize (File _ size)    = size
totalSize (Directory _ xs) = sum (map totalSize xs)

-- Find all files
allFiles :: FileSystem -> [(FileName, FileSize)]
allFiles (File name size)    = [(name, size)]
allFiles (Directory _ items) = concatMap allFiles items

-- Pretty print tree
printTree :: FileSystem -> IO ()
printTree = go 0
  where
    go indent (File name size) =
      putStrLn $ replicate indent ' ' ++ name ++ " (" ++ show size ++ " bytes)"
    go indent (Directory name items) = do
      putStrLn $ replicate indent ' ' ++ name ++ "/"
      mapM_ (go (indent + 2)) items

-- ตัวอย่าง
exampleFS :: FileSystem
exampleFS = Directory "root"
  [ Directory "src"
    [ File "Main.hs" 1024
    , File "Utils.hs" 512
    , Directory "lib"
      [ File "Parser.hs" 2048
      , File "Types.hs" 768
      ]
    ]
  , Directory "test"
    [ File "Spec.hs" 384
    ]
  , File "README.md" 256
  ]

ghci> totalSize exampleFS
4992

ghci> printTree exampleFS
root/
  src/
    Main.hs (1024 bytes)
    Utils.hs (512 bytes)
    lib/
      Parser.hs (2048 bytes)
      Types.hs (768 bytes)
  test/
    Spec.hs (384 bytes)
  README.md (256 bytes)
```

---

## ขั้นตอนที่ 125: Phantom Types

```haskell
-- Phantom Types: type parameters ที่ไม่ปรากฏใน value
-- ใช้สำหรับ type-level tagging

{-# LANGUAGE DataKinds #-}

-- ตัวอย่าง: Safe vs Unsafe strings
data Safety = Safe | Unsafe   -- phantom type

newtype Html (s :: Safety) = Html { getHtml :: String }
  deriving (Show, Eq)

-- สร้าง unsafe HTML (จาก user input)
unsafeHtml :: String -> Html 'Unsafe
unsafeHtml = Html

-- Sanitize HTML (ทำให้ safe)
sanitize :: Html 'Unsafe -> Html 'Safe
sanitize (Html s) = Html (escapeHtml s)
  where
    escapeHtml = concatMap escape
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape '"' = "&quot;"
    escape c   = [c]

-- Render HTML (ต้อง safe เท่านั้น)
render :: Html 'Safe -> String
render (Html s) = s

-- Type safety: ไม่สามารถ render unsafe HTML
-- render (unsafeHtml "<script>") -- Type Error!

-- ถูกต้อง:
result :: String
result = render (sanitize (unsafeHtml "<script>alert('xss')</script>"))

ghci> result
"&lt;script&gt;alert('xss')&lt;/script&gt;"
```

---

## ขั้นตอนที่ 126: Existential Types

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- Existential Types: ซ่อน type parameter

-- ตัวอย่าง: Heterogeneous collection
data Showable = forall a. Show a => MkShowable a

showIt :: Showable -> String
showIt (MkShowable x) = show x

-- Collection ที่เก็บหลาย types ได้
mixedList :: [Showable]
mixedList = [MkShowable 42, MkShowable "hello", MkShowable True, MkShowable [1,2,3]]

ghci> map showIt mixedList
["42","\"hello\"","True","[1,2,3]"]

-- สะดวกกว่าด้วย GADT syntax
data Any where
  MkAny :: Show a => a -> Any

-- ตัวอย่าง: Plugin system
data Plugin = Plugin
  { pluginName :: String
  , pluginRun  :: String -> IO String
  }

makePlugin :: (String -> IO String) -> String -> Plugin
makePlugin f name = Plugin name f

-- Different plugins with different internal state
data EchoPlugin = EchoPlugin
data ReversePlugin = ReversePlugin

mkEcho :: Plugin
mkEcho = Plugin "echo" return

mkReverse :: Plugin
mkReverse = Plugin "reverse" (return . reverse)
```

---

## ขั้นตอนที่ 127: GADTs

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- GADT: Generalized Algebraic Data Types
-- แต่ละ constructor สามารถมี return type ที่แตกต่างกัน

-- ตัวอย่าง: Type-safe expressions
data Expr a where
  Lit    :: Int -> Expr Int
  Bool   :: Bool -> Expr Bool
  Add    :: Expr Int -> Expr Int -> Expr Int
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a
  Eq     :: Eq a => Expr a -> Expr a -> Expr Bool

-- Evaluate expression (type-safe!)
eval :: Expr a -> a
eval (Lit n)      = n
eval (Bool b)     = b
eval (Add e1 e2)  = eval e1 + eval e2
eval (If c t f)   = if eval c then eval t else eval f
eval (Eq e1 e2)   = eval e1 == eval e2

-- ตัวอย่างการใช้งาน
example1 :: Expr Int
example1 = Add (Lit 3) (Lit 4)

example2 :: Expr Bool
example2 = Eq (Add (Lit 1) (Lit 2)) (Lit 3)

example3 :: Expr Int
example3 = If (Bool True) (Lit 42) (Lit 0)

ghci> eval example1
7

ghci> eval example2
True

ghci> eval example3
42

-- GADT สำหรับ typed list
data HList (ts :: [*]) where
  HNil  :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

-- สร้าง heterogeneous list ที่ type-safe
myHList :: HList '[Int, String, Bool]
myHList = HCons 42 (HCons "hello" (HCons True HNil))
```

---

## ขั้นตอนที่ 128: Type Families

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Type Families: functions บน types

-- ตัวอย่าง: Container type family
type family Container (a :: *) :: * where
  Container Int    = [Int]
  Container String = Map String Int
  Container Bool   = Set Bool

-- Associated Type Families
class HasContainer a where
  type Container' a :: *
  toContainer :: a -> Container' a

instance HasContainer [Int] where
  type Container' [Int] = Map.Map Int Int
  toContainer xs = Map.fromList (zip [0..] xs)

-- ตัวอย่าง: Type-level addition
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import GHC.TypeLits

type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 0 m = m
  Add n m = 1 + Add (n - 1) m

-- Vector with length in type
data Vec (n :: Nat) a where
  VNil  :: Vec 0 a
  VCons :: a -> Vec n a -> Vec (n + 1) a

-- Type-safe head
vHead :: Vec (n + 1) a -> a
vHead (VCons x _) = x

-- Type-safe append
vAppend :: Vec n a -> Vec m a -> Vec (n + m) a
vAppend VNil         ys = ys
vAppend (VCons x xs) ys = VCons x (vAppend xs ys)

-- ตัวอย่าง
v1 :: Vec 3 Int
v1 = VCons 1 (VCons 2 (VCons 3 VNil))

v2 :: Vec 2 Int
v2 = VCons 4 (VCons 5 VNil)

v3 :: Vec 5 Int
v3 = vAppend v1 v2
```

---

## ขั้นตอนที่ 129: Data Types สำหรับ Domain Modeling

```haskell
-- ตัวอย่าง: E-commerce domain modeling

-- Basic types
newtype CustomerId = CustomerId Int deriving (Show, Eq, Ord)
newtype ProductId  = ProductId Int  deriving (Show, Eq, Ord)
newtype OrderId    = OrderId Int    deriving (Show, Eq, Ord)
newtype Price      = Price Double   deriving (Show, Eq, Ord, Num)
newtype Quantity   = Quantity Int   deriving (Show, Eq, Ord, Num)

-- Customer
data Customer = Customer
  { customerId   :: CustomerId
  , customerName :: String
  , customerEmail :: String
  } deriving (Show, Eq)

-- Product
data Product = Product
  { productId    :: ProductId
  , productName  :: String
  , productPrice :: Price
  , inStock      :: Bool
  } deriving (Show, Eq)

-- Order Status
data OrderStatus
  = Pending
  | Confirmed
  | Shipped { trackingNumber :: String }
  | Delivered
  | Cancelled { reason :: String }
  deriving (Show, Eq)

-- Order Item
data OrderItem = OrderItem
  { itemProduct  :: Product
  , itemQuantity :: Quantity
  } deriving (Show, Eq)

itemTotal :: OrderItem -> Price
itemTotal item = itemQuantity item `timesPrice` productPrice (itemProduct item)
  where
    timesPrice (Quantity q) (Price p) = Price (fromIntegral q * p)

-- Order
data Order = Order
  { orderId       :: OrderId
  , orderCustomer :: Customer
  , orderItems    :: [OrderItem]
  , orderStatus   :: OrderStatus
  } deriving (Show, Eq)

orderTotal :: Order -> Price
orderTotal order = foldl (\(Price acc) item -> Price (acc + getPrice (itemTotal item)))
                         (Price 0)
                         (orderItems order)
  where getPrice (Price p) = p

-- Workflows
confirmOrder :: Order -> Either String Order
confirmOrder order = case orderStatus order of
  Pending -> Right order { orderStatus = Confirmed }
  status  -> Left $ "Cannot confirm order in status: " ++ show status

shipOrder :: String -> Order -> Either String Order
shipOrder tracking order = case orderStatus order of
  Confirmed -> Right order { orderStatus = Shipped tracking }
  status    -> Left $ "Cannot ship order in status: " ++ show status
```

---

## ขั้นตอนที่ 130: Smart Constructors

```haskell
-- Smart Constructors: validate data เมื่อสร้าง

module Data.Email (Email, mkEmail, getEmail) where

-- hide constructor (ไม่ export Email(..))
newtype Email = Email String deriving (Eq)

instance Show Email where
  show (Email e) = e

-- Smart constructor
mkEmail :: String -> Either String Email
mkEmail s
  | null s            = Left "email cannot be empty"
  | '@' `notElem` s  = Left "email must contain @"
  | length s < 5     = Left "email too short"
  | otherwise         = Right (Email s)

getEmail :: Email -> String
getEmail (Email e) = e

-- ใช้งาน
example :: IO ()
example = do
  case mkEmail "alice@example.com" of
    Left err -> putStrLn $ "Error: " ++ err
    Right e  -> putStrLn $ "Valid: " ++ show e

  case mkEmail "not-an-email" of
    Left err -> putStrLn $ "Error: " ++ err
    Right e  -> putStrLn $ "Valid: " ++ show e
```

---

## ขั้นตอนที่ 131: Lenses (Preview)

```haskell
-- Lens: functional reference ไปยัง field ใน data structure
-- เรียนละเอียดใน Part 17

{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving (Show)

-- Auto-generate lenses
makeLenses ''Person

-- ใช้ lens
alice :: Person
alice = Person { _name = "Alice", _age = 25 }

-- Get
ghci> alice ^. name
"Alice"

-- Set
ghci> alice & name .~ "Bob"
Person {_name = "Bob", _age = 25}

-- Modify
ghci> alice & age %~ (+1)
Person {_name = "Alice", _age = 26}

-- Nested
data Company = Company
  { _companyName :: String
  , _ceo         :: Person
  } deriving (Show)

makeLenses ''Company

company :: Company
company = Company "Haskell Corp" alice

-- Access nested field
ghci> company ^. ceo . name
"Alice"

-- Modify nested field
ghci> company & ceo . age %~ (+1)
Company {_companyName = "Haskell Corp", _ceo = Person {_name = "Alice", _age = 26}}
```

---

## ขั้นตอนที่ 132: Data Types สำหรับ Config

```haskell
-- Pattern: config data types

data DatabaseConfig = DatabaseConfig
  { dbHost     :: String
  , dbPort     :: Int
  , dbName     :: String
  , dbUser     :: String
  , dbPassword :: String
  , dbPoolSize :: Int
  } deriving (Show)

data ServerConfig = ServerConfig
  { serverHost    :: String
  , serverPort    :: Int
  , serverTimeout :: Int  -- seconds
  } deriving (Show)

data LogLevel = Debug | Info | Warning | Error | Critical
  deriving (Show, Eq, Ord, Enum, Bounded)

data LogConfig = LogConfig
  { logLevel  :: LogLevel
  , logFile   :: Maybe FilePath
  , logFormat :: String
  } deriving (Show)

data AppConfig = AppConfig
  { database :: DatabaseConfig
  , server   :: ServerConfig
  , logging  :: LogConfig
  } deriving (Show)

-- Default configurations
defaultDB :: DatabaseConfig
defaultDB = DatabaseConfig
  { dbHost     = "localhost"
  , dbPort     = 5432
  , dbName     = "myapp"
  , dbUser     = "postgres"
  , dbPassword = ""
  , dbPoolSize = 10
  }

defaultServer :: ServerConfig
defaultServer = ServerConfig
  { serverHost    = "0.0.0.0"
  , serverPort    = 8080
  , serverTimeout = 30
  }

defaultLog :: LogConfig
defaultLog = LogConfig
  { logLevel  = Info
  , logFile   = Nothing
  , logFormat = "[%(level)s] %(message)s"
  }

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { database = defaultDB
  , server   = defaultServer
  , logging  = defaultLog
  }
```

---

## ขั้นตอนที่ 133: Data Type Anti-patterns

```haskell
-- Anti-patterns ที่ควรหลีกเลี่ยง

-- 1. Boolean Blindness
-- ไม่ดี: เดาไม่ออกว่า True/False หมายถึงอะไร
setUserStatus :: String -> Bool -> IO ()
setUserStatus userId active = undefined

-- ดีกว่า: ใช้ newtype หรือ enum
data UserStatus = Active | Inactive | Banned
  deriving (Show, Eq)

setUserStatus' :: String -> UserStatus -> IO ()
setUserStatus' userId status = undefined

-- 2. Stringly typed
-- ไม่ดี
processPayment :: String -> String -> String -> IO ()
processPayment orderId currency amount = undefined

-- ดีกว่า
newtype Amount = Amount Double
newtype Currency = Currency String

processPayment' :: OrderId -> Currency -> Amount -> IO ()
processPayment' orderId currency amount = undefined

-- 3. God Object
-- ไม่ดี: มี field มากเกินไปในที่เดียว
data GodUser = GodUser
  { userId :: Int, userName :: String, userEmail :: String
  , userPasswordHash :: String, userCreatedAt :: String
  , userLastLogin :: String, userIsAdmin :: Bool
  , userPreference :: String, userAvatarUrl :: String
  , userBillingAddress :: String, userShippingAddress :: String
  } deriving (Show)

-- ดีกว่า: แยก concerns
data UserAuth = UserAuth { authId :: Int, passwordHash :: String }
data UserProfile = UserProfile { profileName :: String, profileEmail :: String }
data UserPreferences = UserPreferences { theme :: String, language :: String }
```

---

## ขั้นตอนที่ 134: Deriving Via

```haskell
{-# LANGUAGE DerivingVia #-}

-- DerivingVia: derive instances โดย delegation

newtype Celsius = Celsius Double
newtype Fahrenheit = Fahrenheit Double

-- Celsius ใช้ Show ของ Double
newtype Celsius2 = Celsius2 Double
  deriving (Show, Eq, Ord) via Double

-- ตัวอย่างที่ใช้บ่อย
import Data.Aeson (ToJSON, FromJSON)

newtype UserId = UserId Int
  deriving (Show, Eq, Ord, ToJSON, FromJSON) via Int

-- Custom: derive Monoid via Sum
newtype Score = Score Int
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via Sum Int

-- ตอนนี้ Score เป็น Monoid โดยการบวก
score1 :: Score
score1 = Score 10

score2 :: Score
score2 = Score 20

ghci> score1 <> score2
Score 30

ghci> mempty :: Score
Score 0
```

---

## ขั้นตอนที่ 135: Data.Map กับ ADTs

```haskell
import qualified Data.Map.Strict as Map

-- ใช้ Map กับ ADTs สำหรับ dynamic dispatch

data Animal = Cat | Dog | Bird deriving (Show, Eq, Ord)

animalSounds :: Map.Map Animal String
animalSounds = Map.fromList
  [ (Cat, "Meow")
  , (Dog, "Woof")
  , (Bird, "Tweet")
  ]

makeSound :: Animal -> String
makeSound animal = case Map.lookup animal animalSounds of
  Just sound -> sound
  Nothing    -> "..."

-- Registry pattern
type Handler a b = a -> IO b

newtype Registry a b = Registry
  { unRegistry :: Map.Map String (Handler a b)
  }

emptyRegistry :: Registry a b
emptyRegistry = Registry Map.empty

register :: String -> Handler a b -> Registry a b -> Registry a b
register name handler (Registry m) = Registry (Map.insert name handler m)

dispatch :: Registry a b -> String -> a -> IO (Maybe b)
dispatch (Registry m) name input = case Map.lookup name m of
  Nothing      -> return Nothing
  Just handler -> fmap Just (handler input)
```

---

## ขั้นตอนที่ 136: Recursive Descent Parsing

```haskell
-- ตัวอย่าง: Simple expression parser ด้วย ADT

data Expr
  = Num Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show, Eq)

-- Pretty printer
prettyExpr :: Expr -> String
prettyExpr (Num n)     = show n
prettyExpr (Var s)     = s
prettyExpr (Add e1 e2) = "(" ++ prettyExpr e1 ++ " + " ++ prettyExpr e2 ++ ")"
prettyExpr (Sub e1 e2) = "(" ++ prettyExpr e1 ++ " - " ++ prettyExpr e2 ++ ")"
prettyExpr (Mul e1 e2) = "(" ++ prettyExpr e1 ++ " * " ++ prettyExpr e2 ++ ")"
prettyExpr (Div e1 e2) = "(" ++ prettyExpr e1 ++ " / " ++ prettyExpr e2 ++ ")"
prettyExpr (Neg e)     = "(-" ++ prettyExpr e ++ ")"

-- Evaluator
type Env = Map.Map String Double

eval :: Env -> Expr -> Either String Double
eval _   (Num n)     = Right n
eval env (Var s)     = case Map.lookup s env of
  Just v  -> Right v
  Nothing -> Left $ "Undefined variable: " ++ s
eval env (Add e1 e2) = (+) <$> eval env e1 <*> eval env e2
eval env (Sub e1 e2) = (-) <$> eval env e1 <*> eval env e2
eval env (Mul e1 e2) = (*) <$> eval env e1 <*> eval env e2
eval env (Div e1 e2) = do
  v1 <- eval env e1
  v2 <- eval env e2
  if v2 == 0 then Left "Division by zero" else Right (v1 / v2)
eval env (Neg e)     = negate <$> eval env e

-- ตัวอย่าง
expr :: Expr
expr = Add (Mul (Num 2) (Var "x")) (Num 3)

ghci> prettyExpr expr
"((2.0 * x) + 3.0)"

ghci> eval (Map.fromList [("x", 5)]) expr
Right 13.0

ghci> eval Map.empty expr
Left "Undefined variable: x"
```

---

## ขั้นตอนที่ 137: Variance และ Covariance

```haskell
-- Covariant: f a  -> f b  when a is subtype of b
-- Contravariant: g b -> g a when a is subtype of b

-- ใน Haskell บน type classes:
-- Functor = covariant in type parameter
-- Contravariant = contravariant in type parameter

import Data.Functor.Contravariant

-- Contravariant functor
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- ตัวอย่าง: Predicate
newtype Predicate a = Predicate { runPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

-- String predicate
isLongString :: Predicate String
isLongString = Predicate (\s -> length s > 5)

-- Convert to Person predicate
hasLongName :: Predicate Person
hasLongName = contramap personName isLongString

-- Profunctor: both co- and contravariant
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  rmap :: (b -> c) -> p a b -> p a c

-- Function is a Profunctor
instance Profunctor (->) where
  dimap f g h = g . h . f
  lmap f g = g . f
  rmap = (.)
```

---

## ขั้นตอนที่ 138: Data Types สำหรับ Parsing

```haskell
-- State machine ด้วย ADTs

data Token
  = TNum Int
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  | TEOF
  deriving (Show, Eq)

data ParseError
  = UnexpectedToken Token
  | UnexpectedEOF
  | InvalidNumber String
  deriving (Show, Eq)

-- Tokenize
tokenize :: String -> Either ParseError [Token]
tokenize [] = Right [TEOF]
tokenize (c:cs)
  | c == '+' = fmap (TPlus  :) (tokenize cs)
  | c == '-' = fmap (TMinus :) (tokenize cs)
  | c == '*' = fmap (TStar  :) (tokenize cs)
  | c == '/' = fmap (TSlash :) (tokenize cs)
  | c == '(' = fmap (TLParen:) (tokenize cs)
  | c == ')' = fmap (TRParen:) (tokenize cs)
  | c == ' ' = tokenize cs
  | isDigit c =
    let (nums, rest) = span isDigit (c:cs)
    in fmap (TNum (read nums) :) (tokenize rest)
  | otherwise = Left (InvalidNumber [c])
  where
    isDigit x = x >= '0' && x <= '9'

ghci> tokenize "1 + 2 * 3"
Right [TNum 1,TPlus,TNum 2,TStar,TNum 3,TEOF]
```

---

## ขั้นตอนที่ 139: Opaque Types และ Information Hiding

```haskell
-- ซ่อน implementation ด้วย module system

-- module หนึ่ง:
module Data.Queue
  ( Queue  -- export type แต่ไม่ export constructors
  , empty
  , enqueue
  , dequeue
  , isEmpty
  , size
  ) where

-- Implementation ซ่อนอยู่ใน module
data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

size :: Queue a -> Int
size (Queue f b) = length f + length b

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue f b) = normalize (Queue f (x:b))

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue (x:xs) b) = Just (x, normalize (Queue xs b))
dequeue (Queue [] b) = dequeue (Queue (reverse b) [])

normalize :: Queue a -> Queue a
normalize (Queue [] b@(_:_:_)) = Queue (reverse b) []
normalize q = q

-- ผู้ใช้ไม่รู้ว่า implementation คืออะไร
-- ใช้ได้แค่ functions ที่ export
```

---

## ขั้นตอนที่ 140: สรุปและ Project

### สิ่งที่เรียนรู้

1. ✅ Sum Types และ Product Types
2. ✅ Record Syntax
3. ✅ Recursive Data Types
4. ✅ Phantom Types
5. ✅ GADTs
6. ✅ Type Families
7. ✅ Smart Constructors
8. ✅ Domain Modeling

### Project: Library Management System

```haskell
module Library where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- Types
newtype ISBN = ISBN String deriving (Show, Eq, Ord)
newtype MemberId = MemberId Int deriving (Show, Eq, Ord)

data BookStatus = Available | CheckedOut MemberId | Reserved MemberId
  deriving (Show, Eq)

data Book = Book
  { isbn      :: ISBN
  , title     :: String
  , author    :: String
  , status    :: BookStatus
  } deriving (Show)

data Member = Member
  { memberId    :: MemberId
  , memberName  :: String
  , borrowedBooks :: [ISBN]
  } deriving (Show)

-- Library state
data Library = Library
  { books   :: Map.Map ISBN Book
  , members :: Map.Map MemberId Member
  } deriving (Show)

emptyLibrary :: Library
emptyLibrary = Library Map.empty Map.empty

-- Operations
addBook :: Book -> Library -> Library
addBook book lib = lib { books = Map.insert (isbn book) book (books lib) }

addMember :: Member -> Library -> Library
addMember member lib = lib { members = Map.insert (memberId member) member (members lib) }

checkOut :: ISBN -> MemberId -> Library -> Either String Library
checkOut isbnNo mid lib = do
  book <- maybe (Left "Book not found") Right (Map.lookup isbnNo (books lib))
  member <- maybe (Left "Member not found") Right (Map.lookup mid (members lib))
  case status book of
    Available -> Right $ lib
      { books   = Map.insert isbnNo (book { status = CheckedOut mid }) (books lib)
      , members = Map.insert mid (member { borrowedBooks = isbnNo : borrowedBooks member }) (members lib)
      }
    CheckedOut _ -> Left "Book already checked out"
    Reserved r   -> if r == mid
                      then Right $ lib { books = Map.insert isbnNo (book { status = CheckedOut mid }) (books lib) }
                      else Left "Book is reserved by another member"

returnBook :: ISBN -> Library -> Either String Library
returnBook isbnNo lib = do
  book <- maybe (Left "Book not found") Right (Map.lookup isbnNo (books lib))
  case status book of
    CheckedOut mid ->
      let updatedMember = fmap (\m -> m { borrowedBooks = filter (/= isbnNo) (borrowedBooks m) })
                               (Map.lookup mid (members lib))
          members' = maybe (members lib) (\m -> Map.insert mid m (members lib)) updatedMember
      in Right $ lib
           { books   = Map.insert isbnNo (book { status = Available }) (books lib)
           , members = members'
           }
    _ -> Left "Book is not checked out"

-- Search
searchByTitle :: String -> Library -> [Book]
searchByTitle query = filter (isInfixOf query . title) . Map.elems . books
  where isInfixOf sub str = any (sub `isPrefixOf`) (tails str)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs@(_:rest) = xs : tails rest

-- Stats
availableBooks :: Library -> [Book]
availableBooks = filter (\b -> status b == Available) . Map.elems . books

checkedOutBooks :: Library -> [(Book, MemberId)]
checkedOutBooks lib =
  [(book, mid) | book <- Map.elems (books lib)
               , let status' = status book
               , CheckedOut mid <- [status']]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 08 เราจะเรียนเรื่อง **Maybe, Either, และ Error Handling**:
- Maybe Monad
- Either Monad
- ExceptT
- Custom error types

---

*[← Part 06](part-06.md) | [Part 08 →](part-08.md)*
