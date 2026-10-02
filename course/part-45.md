# Part 45: Advanced Haskell Patterns & Meta-Programming
## ขั้นตอนที่ 881-900

---

## ขั้นตอนที่ 881: Template Haskell Deep Dive

```haskell
-- Template Haskell: code generation at compile time

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Generate accessors for record fields
makeAccessors :: Name -> Q [Dec]
makeAccessors typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
      mapM makeAccessor fields
    _ -> fail "makeAccessors: expected record type"

makeAccessor :: (Name, Bang, Type) -> Q Dec
makeAccessor (fieldName, _, fieldType) = do
  let getterName = mkName (nameBase fieldName ++ "Getter")
  argName <- newName "x"
  return $ FunD getterName
    [Clause [VarP argName] (NormalB (AppE (VarE fieldName) (VarE argName))) []]

-- Generate enum FromString instances
deriveEnumFromString :: Name -> Q [Dec]
deriveEnumFromString typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      let ctors = [nameBase n | NormalC n _ <- constructors]
      let pairs = listE [tupE [litE (stringL c), conE (mkName c)] | c <- ctors]
      let dict  = [| Map.fromList $pairs |]
      parseFunc <- [| \s -> Map.lookup s $dict |]
      return []  -- simplified: would return actual Dec
    _ -> fail "Expected enum type"

-- Splice constant at compile time
{-# NOINLINE appVersion #-}
appVersion :: Text
appVersion = $(do
  let version = "1.0.0"
  [| T.pack version |])

-- Type-safe printf
$(genPrintf "%d + %d = %d")
-- Generates: printf3 :: Int -> Int -> Int -> Text
```

---

## ขั้นตอนที่ 882: Generic Programming

```haskell
-- GHC.Generics for generic programming

{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

import GHC.Generics

-- Generic serialization
class Serialize' f where
  serialize' :: f a -> [Word8]

instance Serialize' V1 where
  serialize' _ = error "Void type"

instance Serialize' U1 where
  serialize' U1 = []

instance (Serialize' f, Serialize' g) => Serialize' (f :+: g) where
  serialize' (L1 x) = 0 : serialize' x
  serialize' (R1 x) = 1 : serialize' x

instance (Serialize' f, Serialize' g) => Serialize' (f :*: g) where
  serialize' (x :*: y) = serialize' x ++ serialize' y

instance Serialize a => Serialize' (K1 i a) where
  serialize' (K1 x) = serializeValue x

instance Serialize' f => Serialize' (M1 i c f) where
  serialize' (M1 x) = serialize' x

-- Main class with default implementation
class Serialize a where
  serializeValue :: a -> [Word8]
  default serializeValue :: (Generic a, Serialize' (Rep a)) => a -> [Word8]
  serializeValue = serialize' . from

-- Derive for any type with Generic
data Person = Person { personName :: Text, personAge :: Int }
  deriving (Generic)

instance Serialize Person  -- uses default generic implementation

-- Generic fold
class GFold f where
  gFold :: (a -> b -> b) -> b -> f a -> b

instance GFold V1 where
  gFold _ z _ = z

instance GFold U1 where
  gFold _ z _ = z

instance (GFold f, GFold g) => GFold (f :+: g) where
  gFold f z (L1 x) = gFold f z x
  gFold f z (R1 x) = gFold f z x

instance (GFold f, GFold g) => GFold (f :*: g) where
  gFold f z (x :*: y) = gFold f (gFold f z x) y

instance GFold (K1 i a) where
  gFold f z (K1 x) = f x z

instance GFold f => GFold (M1 i c f) where
  gFold f z (M1 x) = gFold f z x
```

---

## ขั้นตอนที่ 883: Data.SOP (Sum of Products)

```haskell
-- Data.SOP for generic programming over heterogeneous lists

{-# LANGUAGE DataKinds, TypeFamilies #-}

import Data.SOP

-- NP: N-ary Product (heterogeneous list)
exampleNP :: NP I '[Int, Text, Bool]
exampleNP = I 42 :* I "hello" :* I True :* Nil

-- NS: N-ary Sum (heterogeneous variant/sum)
exampleNS :: NS I '[Int, Text, Bool]
exampleNS = Z (I 42)  -- first variant

-- SOP: Sum of Products (ADT representation)
-- type SOP f xss = NS (NP f) xss

-- Generic map over NP
mapNP :: (forall a. f a -> g a) -> NP f xs -> NP g xs
mapNP _ Nil       = Nil
mapNP f (x :* xs) = f x :* mapNP f xs

-- Generic fold over NP
foldNP :: Monoid m => (forall a. f a -> m) -> NP f xs -> m
foldNP _ Nil       = mempty
foldNP f (x :* xs) = f x <> foldNP f xs

-- Using hsequence for applicative traversal
traverseNP :: Applicative g => NP (g :.: f) xs -> g (NP f xs)
traverseNP = hsequence

-- Convert record to NP
data MyRecord = MyRecord { field1 :: Int, field2 :: Text, field3 :: Bool }

toNP :: MyRecord -> NP I '[Int, Text, Bool]
toNP (MyRecord a b c) = I a :* I b :* I c :* Nil

fromNP :: NP I '[Int, Text, Bool] -> MyRecord
fromNP (I a :* I b :* I c :* Nil) = MyRecord a b c
```

---

## ขั้นตอนที่ 884: Type Classes as Evidence

```haskell
-- Type classes as implicit evidence passing

-- Dict: explicit evidence
data Dict p where
  Dict :: p => Dict p

-- Use evidence explicitly
withEvidence :: Dict p -> (p => r) -> r
withEvidence Dict r = r

-- Combine evidence
both :: Dict p -> Dict q -> Dict (p, q)
both Dict Dict = Dict

-- Evidence-based dispatch
class HasSerializer a where
  serializer :: Serializer a

data Serializer a = Serializer
  { serToBytes :: a -> ByteString
  , serFromBytes :: ByteString -> Either Text a
  }

-- Serialize any type with evidence
serializeWithEvidence :: Dict (HasSerializer a) -> a -> ByteString
serializeWithEvidence Dict x = serToBytes serializer x

-- Runtime evidence lookup
data SomeSerializer = forall a. HasSerializer a => SomeSerializer (Proxy a)

-- Type-class dictionary as first-class value
data SerializerDict a = SerializerDict
  { sdSerialize   :: a -> ByteString
  , sdDeserialize :: ByteString -> Either Text a
  }

toSerializerDict :: HasSerializer a => SerializerDict a
toSerializerDict = SerializerDict
  { sdSerialize   = serToBytes serializer
  , sdDeserialize = serFromBytes serializer
  }

-- Heterogeneous list of serializers
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

allSerializers :: HList [SerializerDict Int, SerializerDict Text, SerializerDict Bool]
allSerializers = HCons toSerializerDict (HCons toSerializerDict (HCons toSerializerDict HNil))
```

---

## ขั้นตอนที่ 885: Advanced Monoid Patterns

```haskell
-- Advanced monoid patterns

-- Writer monad as monoid morphism
newtype Writer' w a = Writer' { runWriter' :: (a, w) }

instance Monoid w => Monad (Writer' w) where
  return a = Writer' (a, mempty)
  Writer' (a, w) >>= f =
    let Writer' (b, w') = f a
    in Writer' (b, w <> w')

-- Free monoid (list) as universal
-- Every monoid morphism from [a] is uniquely determined by f :: a -> m
foldMonoid :: Monoid m => (a -> m) -> [a] -> m
foldMonoid = foldMap

-- Monoid homomorphisms
class (Monoid a, Monoid b) => MonoidHom a b where
  hom :: a -> b
  -- Laws: hom mempty = mempty
  --       hom (x <> y) = hom x <> hom y

-- Sum monoid
newtype Sum' a = Sum' { getSum' :: a } deriving (Show, Eq)
instance Num a => Semigroup (Sum' a) where (<>) (Sum' a) (Sum' b) = Sum' (a + b)
instance Num a => Monoid    (Sum' a) where mempty = Sum' 0

-- Product monoid
newtype Product' a = Product' { getProduct' :: a } deriving (Show, Eq)
instance Num a => Semigroup (Product' a) where (<>) (Product' a) (Product' b) = Product' (a * b)
instance Num a => Monoid    (Product' a) where mempty = Product' 1

-- First/Last
newtype First' a = First' { getFirst' :: Maybe a } deriving (Show, Eq)
instance Semigroup (First' a) where
  First' Nothing <> r = r
  l               <> _ = l
instance Monoid (First' a) where mempty = First' Nothing

-- Dual monoid
newtype Dual' a = Dual' { getDual' :: a } deriving (Show, Eq)
instance Semigroup a => Semigroup (Dual' a) where Dual' a <> Dual' b = Dual' (b <> a)
instance Monoid    a => Monoid    (Dual' a) where mempty = Dual' mempty
```

---

## ขั้นตอนที่ 886: Comonads in Practice

```haskell
-- Practical comonad patterns

-- Comonad class
class Functor w => Comonad w where
  extract   :: w a -> a
  duplicate :: w a -> w (w a)
  extend    :: (w a -> b) -> w a -> w b
  extend f  = fmap f . duplicate

-- Zipper as comonad
data Zipper a = Zipper [a] a [a]  -- left (reversed), focus, right

instance Functor Zipper where
  fmap f (Zipper ls x rs) = Zipper (map f ls) (f x) (map f rs)

instance Comonad Zipper where
  extract (Zipper _ x _) = x
  duplicate z = Zipper (tail (iterate left z)) z (tail (iterate right z))

left :: Zipper a -> Zipper a
left (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
left z = z

right :: Zipper a -> Zipper a
right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
right z = z

-- Conway's Game of Life with Zipper
type Grid a = Zipper (Zipper a)

getNeighbors :: Grid Bool -> [Bool]
getNeighbors grid =
  map (\(dx, dy) -> extract (applyDelta dx dy grid))
    [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

stepCell :: Grid Bool -> Bool
stepCell grid =
  let alive     = extract (extract grid)
      neighbors = length (filter id (getNeighbors grid))
  in case (alive, neighbors) of
       (True,  2) -> True
       (True,  3) -> True
       (False, 3) -> True
       _          -> False

stepLife :: Grid Bool -> Grid Bool
stepLife = extend (extend stepCell)
```

---

## ขั้นตอนที่ 887: Lenses Composition

```haskell
-- Advanced lens composition

import Control.Lens

-- Lens composition for nested access
data Company = Company
  { _companyName :: Text
  , _employees   :: [Employee]
  }

data Employee = Employee
  { _empName    :: Text
  , _empSalary  :: Double
  , _empAddress :: Address
  }

data Address = Address
  { _street :: Text
  , _city   :: Text
  , _zip    :: Text
  }

makeLenses ''Company
makeLenses ''Employee
makeLenses ''Address

-- Nested access
getFirstEmployeeCity :: Company -> Maybe Text
getFirstEmployeeCity co = co ^? employees . _head . empAddress . city

-- Modify all employee salaries
giveRaise :: Double -> Company -> Company
giveRaise pct = over (employees . each . empSalary) (* (1 + pct/100))

-- Prisms for sum types
data Shape
  = Circle'   Double
  | Rectangle' Double Double
  deriving (Show)

makePrisms ''Shape

-- Safe access with prism
getRadius :: Shape -> Maybe Double
getRadius s = s ^? _Circle'

-- Set only circles to radius 5
setAllCircles :: [Shape] -> [Shape]
setAllCircles = map (set (_Circle') 5)

-- Traversals for collections
data Tree a = TLeaf | TNode a (Tree a) (Tree a)

instance Traversable Tree where
  traverse _ TLeaf         = pure TLeaf
  traverse f (TNode v l r) = TNode <$> f v <*> traverse f l <*> traverse f r

-- Count nodes over 10
countBig :: Tree Int -> Int
countBig = lengthOf (folded . filtered (>10))
```

---

## ขั้นตอนที่ 888: Optics Beyond Lenses

```haskell
-- Optics: Prisms, Traversals, Isos, Folds

import Optics

-- Iso: bijection between types
textBytestring :: Iso' Text ByteString
textBytestring = iso T.encodeUtf8 T.decodeUtf8

-- Prism: partial injection/extraction
_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism Just $ \case
  Just x  -> Right x
  Nothing -> Left Nothing

-- Affine traversal: 0 or 1 targets
_NonEmpty :: AffineTraversal' [a] (NonEmpty a)
_NonEmpty = atraversal
  (\case { [] -> Left []; (x:xs) -> Right (x:|xs) })
  (\_ ne -> NE.toList ne)

-- ReversedLens / Grate
-- Grate: structure where all positions have the same index type
grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate f = undefined  -- simplified

-- Indexed traversals
-- Access elements with their index
itraverseList :: IndexedTraversal Int [a] [b] a b
itraverseList = itraversed

-- Sum elements at even indices
sumEven :: [Int] -> Int
sumEven = sumOf (itraversed . ifiltered (\i _ -> even i))

-- Fold with state
statefulFold :: [Int] -> Int
statefulFold = evalState (foldlOf' folded (\acc x -> do
  modify (+1)
  return (acc + x)) 0) 0
```

---

## ขั้นตอนที่ 889: Effect Systems with Polysemy

```haskell
-- Polysemy effect system

{-# LANGUAGE TemplateHaskell #-}

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Reader

-- Define effect
data Database m a where
  QueryDb  :: Text -> [Value] -> Database m [Row]
  ExecuteDb :: Text -> [Value] -> Database m Int64

makeSem ''Database

-- Define another effect
data Logger m a where
  LogInfo  :: Text -> Logger m ()
  LogError :: Text -> Logger m ()

makeSem ''Logger

-- Use effects
processUser :: Member Database r
            => Member Logger r
            => Member (Error AppError) r
            => UserId -> Sem r User
processUser uid = do
  logInfo ("Processing user: " <> tshow uid)
  rows <- queryDb "SELECT * FROM users WHERE id = ?" [toValue uid]
  case rows of
    []    -> throw (UserNotFound uid)
    (r:_) -> return (fromRow r)

-- Interpret effects
runDatabasePg :: Member (Embed IO) r => Pool Connection -> Sem (Database ': r) a -> Sem r a
runDatabasePg pool = interpret $ \case
  QueryDb sql params  -> embed (withPool pool (\conn -> query conn sql params))
  ExecuteDb sql params -> embed (withPool pool (\conn -> execute conn sql params))

runLoggerStdout :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
runLoggerStdout = interpret $ \case
  LogInfo  msg -> embed (putStrLn ("INFO: " ++ T.unpack msg))
  LogError msg -> embed (putStrLn ("ERROR: " ++ T.unpack msg))

-- Run the program
main' :: IO ()
main' = do
  pool <- createPool
  result <- runM
    . runDatabasePg pool
    . runLoggerStdout
    . runError @AppError
    $ processUser 42
  print result
```

---

## ขั้นตอนที่ 890: Free Monads Advanced

```haskell
-- Advanced free monad patterns

import Control.Monad.Free

-- DSL using Free
data FileSystemF a
  = ReadFile  FilePath (String -> a)
  | WriteFile FilePath String a
  | DeleteFile FilePath a
  | ListDir   FilePath ([FilePath] -> a)
  | FileExists FilePath (Bool -> a)
  deriving Functor

type FileSystem = Free FileSystemF

-- Smart constructors
readFile' :: FilePath -> FileSystem String
readFile' path = liftF (ReadFile path id)

writeFile' :: FilePath -> String -> FileSystem ()
writeFile' path content = liftF (WriteFile path content ())

-- DSL program
program :: FileSystem ()
program = do
  exists <- liftF (FileExists "config.txt" id)
  when exists $ do
    config <- readFile' "config.txt"
    writeFile' "backup.txt" config
  files <- liftF (ListDir "." id)
  mapM_ (\f -> writeFile' "/dev/null" f) files

-- IO interpreter
interpretIO :: FileSystem a -> IO a
interpretIO (Pure a) = return a
interpretIO (Free (ReadFile path k)) = do
  content <- Prelude.readFile path
  interpretIO (k content)
interpretIO (Free (WriteFile path content next)) = do
  Prelude.writeFile path content
  interpretIO next
interpretIO (Free (DeleteFile path next)) = do
  removeFile path
  interpretIO next

-- Pure test interpreter (in-memory filesystem)
type FakeFSState = Map FilePath String

interpretPure :: FileSystem a -> State FakeFSState a
interpretPure (Pure a) = return a
interpretPure (Free (ReadFile path k)) = do
  fs  <- get
  let content = fromMaybe "" (Map.lookup path fs)
  interpretPure (k content)
interpretPure (Free (WriteFile path content next)) = do
  modify (Map.insert path content)
  interpretPure next
```

---

## ขั้นตอนที่ 891: Recursion Schemes Advanced

```haskell
-- Advanced recursion schemes

import Data.Functor.Foldable

-- Base functor for expression trees
data ExprF a
  = NumF   Int
  | VarF   Text
  | AddF   a a
  | MulF   a a
  | IfF    a a a
  deriving Functor

type Expr' = Fix ExprF

-- Catamorphism: fold (bottom-up)
eval' :: Map Text Int -> Expr' -> Int
eval' env = cata $ \case
  NumF n     -> n
  VarF x     -> fromMaybe 0 (Map.lookup x env)
  AddF l r   -> l + r
  MulF l r   -> l * r
  IfF c t e  -> if c /= 0 then t else e

-- Anamorphism: unfold (top-down)
expand :: Int -> Expr'
expand n = ana (\m -> if m <= 0 then NumF 0 else AddF (m-1) (m-1)) n

-- Hylomorphism: unfold then fold (avoids intermediate structure)
-- histo: catamorphism with access to previous results
-- para: catamorphism with access to original subtrees

-- Attribute grammar (annotate tree)
annotate :: (ExprF (Int, Expr') -> Int) -> Expr' -> Fix (Compose ((,) Int) ExprF)
annotate alg = para $ \case
  expr -> Fix (Compose (alg (fmap snd expr), fmap snd expr))

-- Mutation (transform each node)
simplify :: Expr' -> Expr'
simplify = cata $ \case
  MulF (Fix (NumF 0)) _ -> Fix (NumF 0)  -- 0 * x = 0
  MulF _ (Fix (NumF 0)) -> Fix (NumF 0)
  MulF (Fix (NumF 1)) r -> r               -- 1 * x = x
  MulF l (Fix (NumF 1)) -> l
  AddF (Fix (NumF 0)) r -> r               -- 0 + x = x
  AddF l (Fix (NumF 0)) -> l
  other -> Fix other
```

---

## ขั้นตอนที่ 892: Tagless Final Encoding

```haskell
-- Tagless final: interpret-as-you-go

-- Algebra as type class
class Expr' repr where
  lit  :: Int  -> repr Int
  add  :: repr Int -> repr Int -> repr Int
  mul  :: repr Int -> repr Int -> repr Int
  var  :: Text -> repr Int
  let' :: Text -> repr Int -> repr Int -> repr Int

-- Program (works for any interpreter)
program :: Expr' repr => Map Text (repr Int) -> repr Int
program env = add (lit 3) (mul (var "x") (lit 4))

-- Evaluation interpreter
newtype Eval a = Eval { runEval :: Map Text Int -> a }

instance Expr' Eval where
  lit n   = Eval (\_ -> n)
  add l r = Eval (\env -> runEval l env + runEval r env)
  mul l r = Eval (\env -> runEval l env * runEval r env)
  var x   = Eval (\env -> fromMaybe 0 (Map.lookup x env))
  let' x e body = Eval (\env ->
    let v = runEval e env
    in runEval body (Map.insert x v env))

-- Pretty-print interpreter
newtype Pretty a = Pretty { prettyPrint :: Text }

instance Expr' Pretty where
  lit n   = Pretty (tshow n)
  add l r = Pretty ("(" <> prettyPrint l <> " + " <> prettyPrint r <> ")")
  mul l r = Pretty ("(" <> prettyPrint l <> " * " <> prettyPrint r <> ")")
  var x   = Pretty x
  let' x e body = Pretty ("let " <> x <> " = " <> prettyPrint e <> " in " <> prettyPrint body)

-- Type-safe compilation to bytecode
newtype Compile a = Compile { compile :: [Bytecode] }
```

---

## ขั้นตอนที่ 893: Monadic Reflection

```antml
-- Monadic reflection and reification

-- Reify monad to data (continuation-based)
class Monad m => MonadReify m where
  reifyStep :: m a -> (a -> m b) -> ReifiedStep m a b

data ReifiedStep m a b
  = Pure' a
  | Bind' (m a) (a -> m b)
  | Fail' String

-- Codensity monad (optimize left-biased binds)
newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  }

instance Functor (Codensity m) where
  fmap f (Codensity k) = Codensity (\kb -> k (kb . f))

instance Applicative (Codensity m) where
  pure a = Codensity (\k -> k a)
  Codensity kf <*> Codensity ka = Codensity (\kb -> kf (\f -> ka (\a -> kb (f a))))

instance Monad (Codensity m) where
  return = pure
  Codensity ka >>= f = Codensity (\kb -> ka (\a -> runCodensity (f a) kb))

-- Lower back to base monad
lower :: Monad m => Codensity m a -> m a
lower (Codensity k) = k return

-- Use Codensity to optimize free monad interpretation
type CodFree f = Codensity (Free f)

liftCod :: f a -> CodFree f a
liftCod fa = Codensity (\k -> Free (fmap k fa))

-- Church-encoded free monad (efficient)
newtype Church f a = Church
  { runChurch :: forall r. (a -> r) -> (f r -> r) -> r
  }
```

---

## ขั้นตอนที่ 894: Type Families Advanced

```haskell
-- Advanced type families

{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}

-- Injective type families (from GHC 8.0)
type family Id a = r | r -> a where
  Id Int  = Int
  Id Bool = Bool
  Id a    = a

-- Closed type families (overlap allowed)
type family IsEq a b where
  IsEq a a = 'True
  IsEq a b = 'False

-- Type family with constraints
type family ValidIndex (xs :: [*]) (n :: Nat) :: Constraint where
  ValidIndex '[]     _       = TypeError ('Text "Index out of bounds")
  ValidIndex (x:_)   'Zero   = ()
  ValidIndex (_:xs)  ('Succ n) = ValidIndex xs n

-- Type-level computation
type family Reverse xs where
  Reverse '[]       = '[]
  Reverse (x ': xs) = Append (Reverse xs) '[x]

type family Append xs ys where
  Append '[]       ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- Flatten type-level lists of lists
type family Concat xss where
  Concat '[]         = '[]
  Concat (xs ': xss) = Append xs (Concat xss)

-- Type-level if-then-else
type family If (b :: Bool) t f where
  If 'True  t _ = t
  If 'False _ f = f

-- HList indexed access
type family Index (xs :: [*]) (n :: Nat) where
  Index (x ': _)  'Zero     = x
  Index (_ ': xs) ('Succ n) = Index xs n

class NthElem (xs :: [*]) (n :: Nat) where
  nthElem :: proxy n -> HList xs -> Index xs n

instance NthElem (x ': xs) 'Zero where
  nthElem _ (HCons x _) = x

instance NthElem xs n => NthElem (y ': xs) ('Succ n) where
  nthElem _ (HCons _ xs) = nthElem (Proxy @n) xs
```

---

## ขั้นตอนที่ 895: Kind System

```haskell
-- Haskell's kind system

-- Base kinds
-- * (Type): kind of ordinary types (Int, Bool, etc.)
-- * -> *: kind of type constructors (Maybe, [], IO)
-- * -> * -> *: kind of binary type constructors (Either, Map)

-- DataKinds: promote values to types
{-# LANGUAGE DataKinds #-}

data Nat = Zero | Succ Nat  -- type
-- 'Zero :: Nat  -- kind (promoted)
-- 'Succ :: Nat -> Nat  -- kind (promoted)

-- Constraint kind
type MyConstraints a = (Show a, Eq a, Ord a)

-- Show all constrained values
showAll :: MyConstraints a => [a] -> String
showAll = show . sort . nub

-- ~ (equality constraint)
useEquality :: a ~ Int => a -> String
useEquality n = show (n + 1 :: Int)

-- Polykinded type classes
class Container (f :: k -> *) where
  empty  :: f a
  insert :: a -> f a -> f a

-- PolyKinds
{-# LANGUAGE PolyKinds #-}
class SingKind k where
  type Demote k :: *
  fromSing :: Sing (a :: k) -> Demote k

-- Kind aliases
type Effect = (* -> *) -> * -> *
type Handler eff = forall r. Member eff r => Sem (eff ': r) ~> Sem r
```

---

## ขั้นตอนที่ 896: Deriving Strategies

```haskell
-- Deriving strategies

{-# LANGUAGE DerivingStrategies, DerivingVia, DeriveAnyClass #-}

-- stock: built-in deriving
data Color = Red | Green | Blue
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

-- newtype: unwrap newtype and use underlying instance
newtype Wrapped a = Wrap a
  deriving newtype (Show, Eq, Ord, Num, Semigroup, Monoid)

-- anyclass: use class's default methods (often via Generic)
data Thing = Thing { thingName :: Text, thingValue :: Int }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, Hashable)

-- via: derive via another type's instance
newtype Salary = Salary Double
  deriving (Semigroup, Monoid) via Sum Double  -- use Sum's instance

newtype Name = Name Text
  deriving (IsString) via Text  -- use Text's IsString

-- Deriving Read/Show consistently
data Expr'' = Num'' Int | Add'' Expr'' Expr''
  deriving (Show, Read, Eq)

-- Roundtrip property
prop_showRead :: Expr'' -> Bool
prop_showRead e = read (show e) == e

-- DerivingVia for Functor/Foldable
newtype MyList a = MyList [a]
  deriving (Functor, Foldable, Traversable) via []

-- Deriving JSON with customization
data Config' = Config'
  { configPort :: Int
  , configHost :: Text
  } deriving stock Generic

instance FromJSON Config' where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop (length "config")
    }
```

---

## ขั้นตอนที่ 897: Phantom Types Advanced

```haskell
-- Advanced phantom type patterns

-- Phantom types for units
newtype Meters = Meters Double
newtype Kilograms = Kilograms Double
newtype Seconds = Seconds Double

-- Dimensional quantities
newtype Quantity (unit :: *) = Quantity Double

type Distance   = Quantity Meters
type Mass       = Quantity Kilograms
type Duration   = Quantity Seconds
type Velocity   = Quantity (Meters, Seconds)  -- m/s

-- Type-safe arithmetic
divQuantity :: Quantity a -> Quantity b -> Quantity (a, b)
divQuantity (Quantity a) (Quantity b) = Quantity (a / b)

mulQuantity :: Quantity a -> Quantity b -> Quantity (a, b)
mulQuantity (Quantity a) (Quantity b) = Quantity (a * b)

velocity :: Distance -> Duration -> Velocity
velocity d t = divQuantity d t

-- Cannot add meters to kilograms (type error)
-- addBad :: Distance -> Mass -> ???  -- would be type error

-- Capability tokens
data ReadOnly
data ReadWrite

newtype File (access :: *) = File FilePath

openReadOnly :: FilePath -> IO (File ReadOnly)
openReadOnly = return . File

openReadWrite :: FilePath -> IO (File ReadWrite)
openReadWrite = return . File

readFile'' :: File a -> IO Text
readFile'' (File path) = TIO.readFile path

writeFile'' :: File ReadWrite -> Text -> IO ()
writeFile'' (File path) content = TIO.writeFile path content

-- Cannot write to read-only file
-- badWrite :: File ReadOnly -> IO ()
-- badWrite f = writeFile'' f "..."  -- TYPE ERROR
```

---

## ขั้นตอนที่ 898: Continuation Passing Style

```haskell
-- CPS transformations and continuations

-- CPS form
factCps :: Integer -> (Integer -> a) -> a
factCps 0 k = k 1
factCps n k = factCps (n-1) (\r -> k (n * r))

-- Call with current continuation
callcc :: MonadCont m => ((a -> m b) -> m a) -> m a
callcc f = ContT (\k -> runContT (f (\a -> ContT (\_ -> k a))) k)

-- Early exit pattern
findFirst :: MonadCont m => (a -> Bool) -> [a] -> m (Maybe a)
findFirst pred xs = callcc $ \exit -> do
  mapM_ (\x -> when (pred x) (exit (Just x))) xs
  return Nothing

-- Delimited continuations
data Prompt = forall a. Prompt (TVar (Ans a))
type Ans a = a -> IO ()

-- shift/reset simulation
newtype CC r a = CC { runCC :: (a -> r) -> r }

shift :: ((a -> CC r b) -> CC r r) -> CC r a
shift f = CC $ \k -> runCC (f (\a -> CC $ \k' -> k' (k a))) id

reset :: CC a a -> a
reset m = runCC m id

-- Async continuation
newtype Async' a = Async' { runAsync :: (a -> IO ()) -> IO () }

instance Functor Async' where
  fmap f (Async' k) = Async' (\cb -> k (cb . f))

instance Applicative Async' where
  pure a = Async' (\cb -> cb a)
  Async' kf <*> Async' ka = Async' (\cb ->
    kf (\f -> ka (\a -> cb (f a))))
```

---

## ขั้นตอนที่ 899: Profunctor Optics

```haskell
-- Profunctor-based optics

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (c -> d) -> p c d -> p a d
  rmap f = dimap id f

-- Function is a profunctor
instance Profunctor (->) where
  dimap f g h = g . h . f

-- Cartesian profunctor (for lenses)
class Profunctor p => Cartesian p where
  first'  :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

-- Cocartesian profunctor (for prisms)
class Profunctor p => Cocartesian p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

-- Lens as Cartesian transformer
type Lens s t a b = forall p. Cartesian p => p a b -> p s t

lens' :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens' get set = dimap (\s -> (get s, s)) (\(b, s) -> set s b) . first'

-- Prism as Cocartesian transformer
type Prism s t a b = forall p. (Cartesian p, Cocartesian p) => p a b -> p s t

prism' :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism' build match = dimap match (either id build) . right'

-- Traversal (Wander)
class (Cartesian p, Cocartesian p) => Wander p where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
```

---

## ขั้นตอนที่ 900: โปรเจกต์: Meta-Programming Framework

```haskell
-- Meta-programming framework: generate code from specifications

{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module MetaFramework where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Specification DSL
data ApiSpec = ApiSpec
  { apiName    :: Text
  , apiVersion :: Text
  , apiEndpoints :: [EndpointSpec]
  }

data EndpointSpec = EndpointSpec
  { epPath    :: Text
  , epMethod  :: Method
  , epRequest :: TypeSpec
  , epResponse :: TypeSpec
  }

-- Generate servant API type from spec
generateApi :: ApiSpec -> Q [Dec]
generateApi spec = do
  let apiTypeName = mkName (T.unpack (apiName spec) ++ "API")
  let endpointTypes = map generateEndpoint (apiEndpoints spec)
  let apiType = foldl1 (\a b -> InfixT a (mkName ":<|>") b) endpointTypes
  return [TySynD apiTypeName [] apiType]

generateEndpoint :: EndpointSpec -> Type
generateEndpoint ep =
  foldr (\t acc -> AppT (AppT (ConT (mkName ":>")) t) acc)
    (AppT (AppT (ConT (mkName "Post")) (PromotedT (mkName "'[JSON]")))
          (typeFromSpec (epResponse ep)))
    [ LitT (StrTyLit (T.unpack (epPath ep)))
    , AppT (AppT (ConT (mkName "ReqBody")) (PromotedT (mkName "'[JSON]")))
           (typeFromSpec (epRequest ep))
    ]

-- Generate validation code
generateValidators :: [FieldSpec] -> Q [Dec]
generateValidators fields = do
  validateFunc <- [| mapM_ (\(name, validate, value) ->
    unless (validate value) (throwError (name ++ " is invalid"))) |]
  return []  -- simplified

-- QuasiQuoter for API DSL
apiDsl :: QuasiQuoter
apiDsl = QuasiQuoter
  { quoteExp  = parseAndGenerateApi
  , quotePat  = error "Not supported in patterns"
  , quoteType = error "Not supported in types"
  , quoteDec  = parseAndGenerateApiDecs
  }

-- Usage:
-- [apiDsl|
-- api UserAPI v1:
--   GET /users -> [User]
--   POST /users -> User
--   GET /users/:id -> User
-- |]
```

---

*[← Part 44](part-44.md) | [Part 46 →](part-46.md)*
