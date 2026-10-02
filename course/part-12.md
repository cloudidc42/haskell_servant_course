# Part 12: Type System ขั้นสูง
## ขั้นตอนที่ 221-240: Type Classes, Families และ Higher Kinds

---

## บทนำ

Haskell มี type system ที่ทรงพลังมาก รองรับ type-level computation, dependent-like types, และ advanced polymorphism ที่ช่วยสร้าง code ที่ correct by construction

---

## ขั้นตอนที่ 221: Multi-Parameter Type Classes

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

-- Type class ที่มีหลาย type parameters
class Convertible a b where
  convert :: a -> b

instance Convertible Int Double where
  convert = fromIntegral

instance Convertible String [Char] where
  convert = id

-- ปัญหา: type inference ambiguous
-- convert (5 :: Int) :: ???  -- ไม่รู้ว่าจะ convert เป็นอะไร

-- แก้ไขด้วย FunctionalDependencies
{-# LANGUAGE FunctionalDependencies #-}

class Convertible' a b | a -> b where
  convert' :: a -> b

-- a -> b หมายความว่า: ถ้ารู้ a จะรู้ b ด้วย
-- ดังนั้น inference ทำงานได้

instance Convertible' Int Double where
  convert' = fromIntegral

-- ตัวอย่าง: Collection type class
class (Foldable c) => Collection c where
  empty   :: c a
  insert  :: a -> c a -> c a
  toList  :: c a -> [a]
  toList  = foldr (:) []

instance Collection [] where
  empty  = []
  insert = (:)

instance Collection Maybe where
  empty  = Nothing
  insert x _ = Just x  -- simplified
```

---

## ขั้นตอนที่ 222: Type Families

```haskell
{-# LANGUAGE TypeFamilies #-}

-- Type Family: function at type level
-- Type Families ช่วยให้ type ขึ้นกับ type parameter อื่น

-- Associated Type Family
class Container f where
  type Elem f :: *
  empty  :: f
  insert :: Elem f -> f -> f
  toList :: f -> [Elem f]

-- Instance สำหรับ list
instance Container [a] where
  type Elem [a] = a
  empty    = []
  insert   = (:)
  toList   = id

-- Instance สำหรับ Map
instance Ord k => Container (Map k v) where
  type Elem (Map k v) = (k, v)
  empty             = Map.empty
  insert (k, v) m   = Map.insert k v m
  toList            = Map.toList

-- Standalone Type Family
type family Flip (f :: * -> * -> *) a b where
  Flip f a b = f b a

type FlipEither e a = Flip Either e a
-- FlipEither e a = Either a e

-- Closed Type Family (exhaustive)
type family IsString a :: Bool where
  IsString String = 'True
  IsString _      = 'False

-- Type-level computation
type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add 'Z     n = n
  Add ('S m) n = 'S (Add m n)
```

---

## ขั้นตอนที่ 223: Data Kinds

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- DataKinds: promote data types to kinds
-- ทุก type กลายเป็น kind ด้วย

-- Nat: natural numbers at type level
data Nat = Z | S Nat

-- 'Z :: Nat  (type-level zero)
-- 'S 'Z :: Nat  (type-level one)

-- Kind: type of a type
-- * :: type of ordinary types (Int, Bool, etc.)
-- * -> * :: type constructor (Maybe, [], IO, etc.)
-- Nat :: type of type-level naturals

-- Sized Vector
import GHC.TypeLits  -- ใช้ built-in Nat

data Vec (n :: Nat) a where
  VNil  :: Vec 0 a
  VCons :: a -> Vec n a -> Vec (n+1) a

head' :: Vec (n+1) a -> a
head' (VCons x _) = x

tail' :: Vec (n+1) a -> Vec n a
tail' (VCons _ xs) = xs

-- head' VNil ไม่ compile! (type error)

-- append: type-safe
append' :: Vec m a -> Vec n a -> Vec (m+n) a
append' VNil         ys = ys
append' (VCons x xs) ys = VCons x (append' xs ys)

-- ตัวอย่าง usage
v1 :: Vec 3 Int
v1 = VCons 1 (VCons 2 (VCons 3 VNil))

v2 :: Vec 2 Int
v2 = VCons 4 (VCons 5 VNil)

v3 :: Vec 5 Int
v3 = append' v1 v2
```

---

## ขั้นตอนที่ 224: GADTs ขั้นสูง

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- GADT: Generalized Algebraic Data Types

-- Type-safe Expression Tree
data Expr a where
  Lit  :: Int -> Expr Int
  Bool :: Bool -> Expr Bool
  Add  :: Expr Int -> Expr Int -> Expr Int
  If   :: Expr Bool -> Expr a -> Expr a -> Expr a
  Eq   :: Eq a => Expr a -> Expr a -> Expr Bool

-- Type-safe evaluator
eval :: Expr a -> a
eval (Lit n)    = n
eval (Bool b)   = b
eval (Add x y)  = eval x + eval y
eval (If c t f) = if eval c then eval t else eval f
eval (Eq x y)   = eval x == eval y

-- ตัวอย่าง
expr :: Expr Int
expr = If (Bool True) (Lit 42) (Add (Lit 1) (Lit 2))

result :: Int
result = eval expr  -- 42

-- Type-safe heterogeneous list
data HList xs where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- ใส่ค่า
exampleHList :: HList '[Int, String, Bool]
exampleHList = HCons 42 (HCons "hello" (HCons True HNil))

-- ดึงค่า head
hHead :: HList (x ': xs) -> x
hHead (HCons x _) = x

-- ดึงค่า tail
hTail :: HList (x ': xs) -> HList xs
hTail (HCons _ xs) = xs
```

---

## ขั้นตอนที่ 225: Higher-Kinded Types

```haskell
-- Higher-Kinded Types: type parameters ที่มี kind * -> *

-- Functor: type class สำหรับ things ที่ map over
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- f มี kind * -> *

-- Bifunctor: map over สอง type parameters
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Bifunctor Either where
  bimap f _ (Left  x) = Left  (f x)
  bimap _ g (Right y) = Right (g y)

instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)

-- Profunctor: contravariant in first, covariant in second
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g h = g . h . f

-- ตัวอย่าง: lens-like types
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- Rank-N types จำเป็นสำหรับ lens
{-# LANGUAGE RankNTypes #-}

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (Identity . f) s)
```

---

## ขั้นตอนที่ 226: Type-Level Programming

```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Type-level lists
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- Type-level map
type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

-- Type-level filter
type family Filter (p :: a -> Bool) (xs :: [a]) :: [a] where
  Filter p '[]       = '[]
  Filter p (x ': xs) = If (p x) (x ': Filter p xs) (Filter p xs)

-- Type-level if
type family If (cond :: Bool) (thn :: k) (els :: k) :: k where
  If 'True  thn _   = thn
  If 'False _   els = els

-- ตัวอย่าง: Type-level computation
type NumberedList xs = Zip (Range (Length xs)) xs

type family Length (xs :: [k]) :: Nat where
  Length '[]       = 0
  Length (_ ': xs) = 1 + Length xs

-- ตัวอย่าง: เปรียบเทียบ types
type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[]       = 'False
  Elem x (x ': _)  = 'True
  Elem x (_ ': xs) = Elem x xs
```

---

## ขั้นตอนที่ 227: Phantom Types ขั้นสูง

```haskell
-- Phantom Types: type parameters ที่ไม่ปรากฎใน constructor

{-# LANGUAGE PhantomTypes #-}

-- ตัวอย่าง: Type-safe units
data Meter
data Second
data Kilogram

newtype Quantity unit = Quantity { getValue :: Double }
  deriving (Show, Eq, Ord)

-- Smart constructors
meters :: Double -> Quantity Meter
meters = Quantity

seconds :: Double -> Quantity Second
seconds = Quantity

kilograms :: Double -> Quantity Kilogram
kilograms = Quantity

-- Type-safe arithmetic
add :: Quantity u -> Quantity u -> Quantity u
add (Quantity x) (Quantity y) = Quantity (x + y)

-- ป้องกัน invalid operation
-- add (meters 5) (seconds 3)  -- type error!

-- Division ที่สร้าง compound unit
newtype Velocity = Velocity Double
  deriving (Show)

speed :: Quantity Meter -> Quantity Second -> Velocity
speed (Quantity d) (Quantity t) = Velocity (d / t)

-- ตัวอย่าง: Tagged phantom
data Tag

newtype Tagged (tag :: k) a = Tagged { unTagged :: a }

-- ตัวอย่าง: Permission checking
data User
data Admin

newtype Token (role :: *) = Token String

adminOnly :: Token Admin -> IO ()
adminOnly _ = putStrLn "Admin action"

-- userToken :: Token User
-- adminOnly userToken  -- type error!
```

---

## ขั้นตอนที่ 228: Existential Types ขั้นสูง

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Existential: ซ่อน type parameter

-- แบบ ExistentialQuantification
data AnyShow = forall a. Show a => AnyShow a

showAny :: AnyShow -> String
showAny (AnyShow x) = show x

-- แบบ GADT (ชัดเจนกว่า)
data AnyShow' where
  MkAnyShow :: Show a => a -> AnyShow'

-- Dynamic dispatch
data Plugin where
  Plugin :: { pluginName :: String
            , pluginRun  :: IO ()
            } -> Plugin

loadPlugin :: String -> IO () -> Plugin
loadPlugin name action = Plugin name action

runPlugin :: Plugin -> IO ()
runPlugin (Plugin _ run) = run

-- Heterogeneous containers
data AnyNum where
  AnyNum :: (Num a, Show a) => a -> AnyNum

showNum :: AnyNum -> String
showNum (AnyNum x) = show x

addAny :: AnyNum -> AnyNum -> AnyNum
addAny (AnyNum x) (AnyNum y) = AnyNum (... )  -- ต้องการ same type

-- SomeException ใน Haskell ใช้ existential
-- data SomeException = forall e. Exception e => SomeException e
```

---

## ขั้นตอนที่ 229: Coercions และ Newtypes

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- Coerce: safe zero-cost conversion ระหว่าง newtypes
import Data.Coerce

newtype Age = Age Int deriving (Show, Eq, Ord)
newtype Score = Score Int deriving (Show, Eq, Ord)

-- coerce: convert ระหว่าง types ที่มี same representation
ageToInt :: Age -> Int
ageToInt = coerce

intToAge :: Int -> Age
intToAge = coerce

-- Coercible: type class สำหรับ coercible types
class Coercible a b => ...

-- ตัวอย่าง: Efficient newtype wrapping
newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

-- ใช้ coerce เพื่อ avoid wrapping/unwrapping
sumList :: [Int] -> Int
sumList = getSum . mconcat . coerce
-- coerce [Int] -> [Sum Int] เป็น zero-cost!

-- DerivingVia: inherit instances ผ่าน isomorphic type
newtype MyList a = MyList [a]

-- Derive Functor ผ่าน []
deriving via [] instance Functor MyList

-- ตัวอย่าง: Validation via Either
newtype Validation e a = Validation (Either e a)

deriving via (Either e) instance Functor (Validation e)
```

---

## ขั้นตอนที่ 230: Constraint Kinds

```haskell
{-# LANGUAGE ConstraintKinds #-}

-- Constraint: kind ของ type class constraints

-- ConstraintKinds: ใช้ Constraint เป็น type

-- Type aliases สำหรับ constraint groups
type Printable a = (Show a, Eq a)
type Numeric a = (Num a, Ord a, Show a)

-- ฟังก์ชันที่ใช้ constraint alias
printIfPositive :: Numeric a => a -> IO ()
printIfPositive x
  | x > 0     = print x
  | otherwise = return ()

-- Higher-order constraint
type Constraint1 c a = c a

-- ตัวอย่าง: All constraint
type family AllC (c :: * -> Constraint) (xs :: [*]) :: Constraint where
  AllC c '[]       = ()
  AllC c (x ': xs) = (c x, AllC c xs)

-- ใช้ AllC
showAll :: AllC Show xs => HList xs -> [String]
showAll HNil         = []
showAll (HCons x xs) = show x : showAll xs

-- Dict: reify constraint as value
import Data.Constraint

data Dict c where
  Dict :: c => Dict c

-- เก็บ constraint ไว้ใน value
withDict :: Dict c -> (c => r) -> r
withDict Dict f = f

-- ตัวอย่าง
dictShow :: Dict (Show Int)
dictShow = Dict

printWithDict :: Dict (Show a) -> a -> IO ()
printWithDict Dict x = print x
```

---

## ขั้นตอนที่ 231: Linear Types (GHC 9.0+)

```haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Linear Types: ป้องกัน use-after-free และ resource leaks
-- Function ที่ใช้ argument ครั้งเดียวพอดี

import Prelude.Linear

-- Linear function arrow: a ⊸ b
-- argument ต้องถูกใช้ exactly once

-- ตัวอย่าง: safe file handling
withLinearFile :: FilePath -> (Handle ⊸ IO (Ur a)) ⊸ IO a
withLinearFile path action = ...

-- Ur a: unrestricted a (สามารถใช้กี่ครั้งก็ได้)

-- ตัวอย่าง: เหมาะสำหรับ Resource management
class Resource r where
  acquire :: IO r
  release :: r ⊸ IO ()
  use     :: r ⊸ (r ⊸ IO a) ⊸ IO a

-- ยังอยู่ใน development แต่เป็นทิศทางสำคัญของ Haskell
```

---

## ขั้นตอนที่ 232: TypeApplications

```haskell
{-# LANGUAGE TypeApplications #-}

-- TypeApplications: ระบุ type argument ชัดเจน

-- read แบบปกติ: ต้องมี type annotation
x :: Int
x = read "42"

-- ด้วย TypeApplications:
x' :: Int
x' = read @Int "42"

-- mempty
emptyList :: [Int]
emptyList = mempty @[Int]

-- minBound/maxBound
maxInt :: Int
maxInt = maxBound @Int

-- ตัวอย่าง: เมื่อ type inference ไม่พอ
parseJSON :: FromJSON a => String -> Maybe a
parseJSON = ...

-- ระบุ type ชัดเจน
userId :: Maybe Int
userId = parseJSON @Int "42"

-- Generic programming
import GHC.Generics
import Data.Proxy

printTypeRep :: forall a. Typeable a => String
printTypeRep = show (typeRep (Proxy @a))

printInt :: String
printInt = printTypeRep @Int  -- "Int"
```

---

## ขั้นตอนที่ 233: ScopedTypeVariables

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

-- ScopedTypeVariables: type variables ใน forall สามารถใช้ใน body

-- ปัญหาแบบปกติ:
-- sortWith :: Ord b => (a -> b) -> [a] -> [a]
-- sortWith f xs = ... -- ไม่สามารถ annotate intermediate ได้

-- ด้วย ScopedTypeVariables:
sortWith :: forall a b. Ord b => (a -> b) -> [a] -> [a]
sortWith f xs = 
  let scored :: [(b, a)]    -- b ถูกกำหนดโดย forall b
      scored = map (\x -> (f x, x)) xs
  in map snd (sortBy (comparing fst) scored)

-- ตัวอย่าง: helper function ใน where clause
process :: forall a. Show a => [a] -> String
process xs = unlines (map showItem xs)
  where
    showItem :: a -> String  -- a ถูกกำหนดโดย outer forall
    showItem x = "[" ++ show x ++ "]"

-- ตัวอย่าง: เมื่อต้องการ explicit type dalam lambda
mapWithType :: [Int] -> [String]
mapWithType = map (\(x :: Int) -> show x)
```

---

## ขั้นตอนที่ 234: TypeFamilies ขั้นสูง

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- Injective Type Families
{-# LANGUAGE TypeFamilyDependencies #-}

type family F a = r | r -> a where
  F Int    = Bool
  F Bool   = Int
  F String = Char

-- r -> a หมายความว่า: F เป็น injective (one-to-one)

-- Overlapping Type Family Instances
type family G a where
  G Int  = String
  G _    = ()  -- default case

-- Partial Type Functions
type family H (a :: Nat) :: Nat where
  H 0 = 1
  H n = n * H (n - 1)

-- ตัวอย่าง: Type-level Matrix dimensions
data Matrix (m :: Nat) (n :: Nat) a = Matrix [[a]]

matMul :: Matrix m n a -> Matrix n p a -> Matrix m p a
matMul (Matrix xs) (Matrix ys) = Matrix result
  where result = undefined -- implementation

-- Compile-time check: dimensions must match
-- matMul (Matrix [[1,2]] :: Matrix 1 2 Int) (Matrix [[1],[2]] :: Matrix 2 1 Int)
-- Result type: Matrix 1 1 Int
```

---

## ขั้นตอนที่ 235: Reflection และ Typeable

```haskell
import Data.Typeable
import Type.Reflection

-- Typeable: runtime type information

showType :: forall a. Typeable a => a -> String
showType _ = show (typeRep (Proxy @a))

-- TypeRep: runtime representation of a type
intRep :: TypeRep Int
intRep = typeRep

-- eqT: compare types at runtime
sameType :: (Typeable a, Typeable b) => Maybe (a :~: b)
sameType = eqT  -- Just Refl if same, Nothing otherwise

-- cast: safe cast at runtime
safeCast :: (Typeable a, Typeable b) => a -> Maybe b
safeCast = cast

-- ตัวอย่าง: dynamic dispatch
process :: Typeable a => a -> String
process x
  | Just n  <- cast x :: Maybe Int    = "Int: "    ++ show n
  | Just s  <- cast x :: Maybe String = "String: " ++ s
  | Just b  <- cast x :: Maybe Bool   = "Bool: "   ++ show b
  | otherwise = "Unknown type: " ++ show (typeOf x)

-- Dynamic: heterogeneous container
import Data.Dynamic

dyn :: Dynamic
dyn = toDyn (42 :: Int)

fromDyn' :: Typeable a => Dynamic -> Maybe a
fromDyn' = fromDynamic
```

---

## ขั้นตอนที่ 236: Generic Programming

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

import GHC.Generics

-- Generic: derive instances automatically

data Person = Person { name :: String, age :: Int }
  deriving (Generic, Show)

-- ตัวอย่าง: derive ToJSON ด้วย Generic
class ToJSON a where
  toJSON :: a -> String
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> String
  toJSON = gToJSON . from

-- GToJSON: process Generic Rep
class GToJSON f where
  gToJSON :: f a -> String

-- Generic traversal
class GFoldable f where
  gFold :: Monoid m => (a -> m) -> f a -> m

-- ตัวอย่าง: derive Eq automatically
class GEq f where
  gEq :: f a -> f a -> Bool

instance GEq U1 where
  gEq U1 U1 = True

instance Eq c => GEq (K1 i c) where
  gEq (K1 x) (K1 y) = x == y

instance (GEq f, GEq g) => GEq (f :*: g) where
  gEq (fx :*: gx) (fy :*: gy) = gEq fx fy && gEq gx gy

instance (GEq f, GEq g) => GEq (f :+: g) where
  gEq (L1 x) (L1 y) = gEq x y
  gEq (R1 x) (R1 y) = gEq x y
  gEq _      _      = False
```

---

## ขั้นตอนที่ 237: Kind Polymorphism

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- PolyKinds: type variables ที่มี polymorphic kind

-- Proxy ที่ polymorphic ใน kind
data Proxy (a :: k) = Proxy

-- Proxy สำหรับทุก kind:
p1 :: Proxy Int        -- Proxy (a :: *)
p2 :: Proxy Maybe      -- Proxy (a :: * -> *)
p3 :: Proxy 'True      -- Proxy (a :: Bool)

-- Type class ที่ polymorphic ใน kind
class Typeable (a :: k) where
  typeRep :: Proxy a -> TypeRep

-- Kind-indexed type family
type family KindOf (a :: k) :: k where
  KindOf (a :: *) = a  -- สำหรับ * kind
  KindOf (f :: * -> *) = f  -- สำหรับ * -> * kind

-- ตัวอย่าง: Functor ใน category theory sense
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- f มี kind * -> * โดย default
-- ด้วย PolyKinds สามารถมี f :: k -> k
```

---

## ขั้นตอนที่ 238: Role Annotations

```haskell
{-# LANGUAGE RoleAnnotations #-}

-- Roles: ควบคุม coercion safety

-- Roles ใน Haskell:
-- nominal: ต้องเป็น same type (ไม่ coerce ได้)
-- representational: coerce ได้ถ้า representation same
-- phantom: ไม่มี relationship กับ value

type role Map nominal representational
-- Map k v:
-- k ต้องเป็น same type (key ordering ขึ้นกับ type)
-- v สามารถ coerce ได้

-- ตัวอย่าง:
newtype Age = Age Int
newtype Score = Score Int

-- Map Age Int -> Map Score Int ไม่ได้! (nominal key)
-- Map String Age -> Map String Score ได้! (representational value)

-- ตัวอย่าง: Set
type role Set nominal
-- Set Age -> Set Score ไม่ได้ (nominal)

-- Custom role annotation
data MyMap k v = MyMap [(k, v)]
type role MyMap nominal representational

-- Phantom role
newtype Tagged t a = Tagged a
type role Tagged phantom representational
```

---

## ขั้นตอนที่ 239: Advanced Type Inference

```haskell
-- MonoLocalBinds: ป้องกัน generalizing local bindings

{-# LANGUAGE MonoLocalBinds #-}

-- ตัวอย่าง: let polymorphism
-- ปกติ:
f :: (a -> b) -> [a] -> [b]
f g xs = let mapped = map g xs  -- mapped :: [b]
         in mapped

-- MonoLocalBinds: local let ไม่ generalize (เพื่อ performance และ predictability)

-- Polymorphic recursion
data Tree a = Leaf | Node a (Tree [a])

-- depth :: Tree a -> Int requires polymorphic recursion
depth :: Tree a -> Int
depth Leaf       = 0
depth (Node _ t) = 1 + depth t  -- depth :: Tree [a] -> Int (different instance!)

-- ต้องมี explicit type signature
-- ไม่งั้น GHC จะ reject

-- TypeHoles: ? ใน expressions
myFunc :: Int -> Int
myFunc x = _ + x  -- ? is a hole, GHC tells you what type it expects

-- NamedWildCards
myFunc' :: Int -> _
myFunc' x = x + 1  -- GHC infers return type
```

---

## ขั้นตอนที่ 240: โปรเจกต์: Type-Safe API DSL

```haskell
-- Type-Safe API Definition

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module ApiDSL where

import GHC.TypeLits

-- HTTP Methods at type level
data GET
data POST
data PUT
data DELETE

-- Path segments
data (path :: Symbol) :/ rest
data Capture (name :: Symbol) ty
data QueryParam (name :: Symbol) ty
data ReqBody ty
data JSON
data PlainText

-- API type
type family ApiType method returns = result where
  ApiType GET    returns = IO returns
  ApiType POST   returns = IO returns
  ApiType PUT    returns = IO returns
  ApiType DELETE returns = IO ()

-- Route definition
data Route method path returns where
  Route :: Method method => Route method path returns

-- ตัวอย่าง API
type UserAPI =
         "users" :/ GET '[JSON] [User]
  :<|>  ("users" :/ Capture "id" Int :/ GET '[JSON] User)
  :<|>  ("users" :/ POST '[JSON] User '[JSON] User)

-- Handler types derived from API
type UserHandler =
         IO [User]                -- GET /users
  :<|>  (Int -> IO User)          -- GET /users/:id
  :<|>  (User -> IO User)         -- POST /users

-- Type-level API verification
type family HasRoute (api :: *) (method :: *) (path :: *) :: Bool where
  HasRoute (a :<|> b) method path = HasRoute a method path || HasRoute b method path
  HasRoute (path :/ method) method path = 'True
  HasRoute _ _ _ = 'False

-- Simple implementation
data User = User { userId :: Int, userName :: String }
  deriving (Show)

userHandlers :: UserHandler
userHandlers = getUsers :<|> getUser :<|> createUser
  where
    getUsers :: IO [User]
    getUsers = return [User 1 "Alice", User 2 "Bob"]
    
    getUser :: Int -> IO User
    getUser n = return (User n ("User " ++ show n))
    
    createUser :: User -> IO User
    createUser user = return user { userId = 99 }

-- Type-safe router
data (:<|>) a b = a :<|> b
infixr 8 :<|>
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 13 เราจะเรียนเรื่อง **Lenses และ Optics**:
- Lens definition
- Using lens library
- Prisms, Traversals
- Composing optics

---

*[← Part 11](part-11.md) | [Part 13 →](part-13.md)*
