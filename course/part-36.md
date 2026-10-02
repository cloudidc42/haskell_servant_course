# Part 36: Advanced Type-Level Programming
## ขั้นตอนที่ 701-720

---

## ขั้นตอนที่ 701: Dependent Types with Singletons

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, GADTs, ScopedTypeVariables #-}

-- Dependent types simulation ด้วย singletons library
import Data.Singletons
import Data.Singletons.TH

-- Type-level natural numbers
$(singletons [d|
  data Nat = Zero | Succ Nat deriving (Eq, Ord)
  
  type family Plus (a :: Nat) (b :: Nat) :: Nat where
    Plus Zero     b = b
    Plus (Succ a) b = Succ (Plus a b)
  |])

-- Vectors with length in type
data Vec :: Nat -> * -> * where
  Nil  :: Vec 'Zero a
  (:>) :: a -> Vec n a -> Vec ('Succ n) a

infixr 5 :>

-- Safe head — compiles only when vector non-empty
head' :: Vec ('Succ n) a -> a
head' (x :> _) = x

-- Safe zip — only zips vectors of equal length
zip' :: Vec n a -> Vec n b -> Vec n (a, b)
zip' Nil       Nil       = Nil
zip' (x :> xs) (y :> ys) = (x, y) :> zip' xs ys

-- Matrix multiplication is type-safe
type Matrix m n a = Vec m (Vec n a)

-- Transpose: Matrix m n -> Matrix n m
transpose' :: SingI n => Matrix m n a -> Matrix n m a
transpose' Nil = replicate' Nil
transpose' (row :> rows) = zipWith' (:>) row (transpose' rows)

-- Safe index into vector
data Fin :: Nat -> * where
  FZero :: Fin ('Succ n)
  FSucc :: Fin n -> Fin ('Succ n)

index :: Vec n a -> Fin n -> a
index (x :> _)  FZero     = x
index (_ :> xs) (FSucc i) = index xs i
```

---

## ขั้นตอนที่ 702: Type-Level State Machines

```haskell
-- Type-safe state machine ที่ transitions ถูกต้องตอน compile time

{-# LANGUAGE TypeFamilies, DataKinds, GADTs #-}

-- States
data TrafficLight = Red | Yellow | Green

-- Type-level transition
type family NextLight (l :: TrafficLight) :: TrafficLight where
  NextLight 'Red    = 'Green
  NextLight 'Green  = 'Yellow
  NextLight 'Yellow = 'Red

-- Typed light
newtype Light (l :: TrafficLight) = Light ()

-- Transition function: only valid transitions compile
advance :: Light l -> Light (NextLight l)
advance (Light ()) = Light ()

-- TCP Connection state machine
data TcpState = Closed | Listen | SynSent | SynReceived | Established | CloseWait

data TcpConn :: TcpState -> * where
  ClosedConn      :: TcpConn 'Closed
  ListeningConn   :: TcpConn 'Listen
  ConnectedConn   :: Socket -> TcpConn 'Established
  CloseWaitConn   :: TcpConn 'CloseWait

-- Only valid operations on each state
listen :: TcpConn 'Closed -> IO (TcpConn 'Listen)
listen _ = ListeningConn <$ bindSocket

connect :: TcpConn 'Closed -> HostName -> PortNumber -> IO (TcpConn 'Established)
connect _ host port = ConnectedConn <$> tcpConnect host port

send :: TcpConn 'Established -> ByteString -> IO Int
send (ConnectedConn s) = socketSend s

close :: TcpConn 'Established -> IO (TcpConn 'CloseWait)
close (ConnectedConn s) = CloseWaitConn <$ gracefulClose s
```

---

## ขั้นตอนที่ 703: Higher-Kinded Type Classes

```haskell
-- Higher-kinded type classes ขั้นสูง

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies #-}

-- Bifunctor
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  first  :: (a -> c) -> f a b -> f c b
  first  f = bimap f id
  second :: (b -> d) -> f a b -> f a d
  second   = bimap id

-- Profunctor (contravariant in a, covariant in b)
class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
  lmap  :: (a' -> a) -> p a b -> p a' b
  lmap  f = dimap f id
  rmap  :: (b -> b') -> p a b -> p a b'
  rmap    = dimap id

instance Profunctor (->) where
  dimap f g h = g . h . f

-- Optics using profunctors
type Lens s t a b = forall p. Strong p => p a b -> p s t

type Prism s t a b = forall p. Choice p => p a b -> p s t

class Profunctor p => Strong p where
  first'  :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

-- Traversal using applicative
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
```

---

## ขั้นตอนที่ 704: Free Algebras

```haskell
-- Free monoids, free monads, and free algebras

-- Free monoid = List
-- Free monad = recursive structure

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
  pure              = Pure
  Pure f <*> ma     = fmap f ma
  Free ff <*> ma    = Free (fmap (<*> ma) ff)

instance Functor f => Monad (Free f) where
  return  = Pure
  Pure a  >>= f = f a
  Free fa >>= f = Free (fmap (>>= f) fa)

-- Lift into Free
liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- Interpret Free monad
foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free fa) = f fa >>= foldFree f

-- Example: DSL for key-value store
data KvF next
  = Get Text (Maybe Text -> next)
  | Set Text Text next
  | Delete Text next
  deriving Functor

type Kv = Free KvF

get :: Text -> Kv (Maybe Text)
get k = liftF (Get k id)

set :: Text -> Text -> Kv ()
set k v = liftF (Set k v ())

delete :: Text -> Kv ()
delete k = liftF (Delete k ())

-- Run against real Redis
runKvRedis :: Redis.Connection -> Kv a -> IO a
runKvRedis conn = foldFree (interpret conn)
  where
    interpret c (Get k next) = do
      v <- Redis.runRedis c (Redis.get (encodeUtf8 k))
      return (next (fmap decodeUtf8 (join (either (const Nothing) id v))))
    interpret c (Set k v next) = do
      Redis.runRedis c (Redis.set (encodeUtf8 k) (encodeUtf8 v))
      return next
    interpret c (Delete k next) = do
      Redis.runRedis c (Redis.del [encodeUtf8 k])
      return next
```

---

## ขั้นตอนที่ 705: Effect Systems with Polysemy

```haskell
-- Polysemy effects ขั้นสูง

{-# LANGUAGE TemplateHaskell #-}

import Polysemy
import Polysemy.State
import Polysemy.Error
import Polysemy.Reader

-- Define effects
data Cache k v m a where
  CacheGet :: k -> Cache k v m (Maybe v)
  CachePut :: k -> v -> Cache k v m ()
  CacheDel :: k -> Cache k v m ()

makeSem ''Cache

-- Business logic using multiple effects
getUserWithCache
  :: Members '[Cache UserId User, Database, Logging] r
  => UserId -> Sem r (Maybe User)
getUserWithCache uid = do
  mCached <- cacheGet uid
  case mCached of
    Just user -> do
      logDebug ("Cache hit for user " <> show uid)
      return (Just user)
    Nothing -> do
      logDebug ("Cache miss for user " <> show uid)
      mUser <- dbGetUser uid
      forM_ mUser $ \user -> cachePut uid user
      return mUser

-- Interpreters
runCacheRedis
  :: Member (Embed IO) r
  => Redis.Connection -> Sem (Cache UserId User ': r) a -> Sem r a
runCacheRedis conn = interpret $ \case
  CacheGet k -> embed $ getFromRedis conn k
  CachePut k v -> embed $ putToRedis conn k v
  CacheDel k -> embed $ delFromRedis conn k

runCacheInMemory
  :: Member (State (Map k v)) r
  => Sem (Cache k v ': r) a -> Sem r a
runCacheInMemory = interpret $ \case
  CacheGet k -> gets (Map.lookup k)
  CachePut k v -> modify (Map.insert k v)
  CacheDel k -> modify (Map.delete k)

-- Compose effects
runApp :: Sem '[Cache UserId User, Database, Logging, Embed IO] a -> IO a
runApp =
    runM
  . runLoggingIO
  . runDatabasePostgres pool
  . runCacheRedis redisConn
```

---

## ขั้นตอนที่ 706: Recursion Schemes

```haskell
-- Recursion schemes: catamorphisms, anamorphisms, hylomorphisms

{-# LANGUAGE DeriveFunctor #-}

-- Base functor for a tree
data TreeF a r
  = LeafF
  | NodeF a r r
  deriving Functor

-- Fix point
newtype Fix f = Fix { unFix :: f (Fix f) }

type Tree a = Fix (TreeF a)

-- Smart constructors
leaf :: Tree a
leaf = Fix LeafF

node :: a -> Tree a -> Tree a -> Tree a
node x l r = Fix (NodeF x l r)

-- Catamorphism (fold/destroy)
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Count nodes
countNodes :: Tree a -> Int
countNodes = cata $ \case
  LeafF       -> 0
  NodeF _ l r -> 1 + l + r

-- Sum values
sumTree :: Num a => Tree a -> a
sumTree = cata $ \case
  LeafF       -> 0
  NodeF x l r -> x + l + r

-- Anamorphism (unfold/build)
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- Build balanced BST from sorted list
buildBST :: Ord a => [a] -> Tree a
buildBST = ana coalg
  where
    coalg []     = LeafF
    coalg xs =
      let n    = length xs `div` 2
          mid  = xs !! n
          left = take n xs
          right= drop (n+1) xs
      in NodeF mid left right

-- Hylomorphism (unfold then fold)
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = cata alg . ana coalg

-- Merge sort via hylomorphism
mergeSort :: Ord a => [a] -> [a]
mergeSort = hylo merge split
  where
    split [] = LeafF
    split xs = NodeF (head xs) (take half xs) (drop half xs)
      where half = length xs `div` 2
    merge LeafF       = []
    merge (NodeF _ l r) = mergeLists l r
```

---

## ขั้นตอนที่ 707: Category Theory in Haskell

```haskell
-- Category theory abstractions

-- Category
class Category cat where
  identity :: cat a a
  (>>>) :: cat a b -> cat b c -> cat a c

-- Functor between categories
class (Category c, Category d) => CFunctor f c d where
  cfmap :: c a b -> d (f a) (f b)

-- Natural transformation
type f :~> g = forall a. f a -> g a

safeToMaybe :: [] :~> Maybe
safeToMaybe []    = Nothing
safeToMaybe (x:_) = Just x

-- Adjunction
class (Functor f, Functor g) => Adjunction f g where
  unit   :: a -> g (f a)
  counit :: f (g a) -> a
  leftAdj  :: (f a -> b) -> a -> g b
  rightAdj :: (a -> g b) -> f a -> b

-- State monad from adjunction: (- × S) ⊣ (S → -)
instance Adjunction ((,) s) ((->) s) where
  unit a = \s -> (s, a)
  counit (s, f) = f s
  leftAdj f a s = let (s', b) = f (s, a) in undefined
  rightAdj g (s, a) = g a s

-- Monad from adjunction
monadFromAdj :: Adjunction f g => g (f a) -> g (f a)
monadFromAdj = fmap (fmap id)

-- Comonad from adjunction
comonadFromAdj :: Adjunction f g => f (g a) -> a
comonadFromAdj = counit
```

---

## ขั้นตอนที่ 708: Type-Safe Heterogeneous Collections

```haskell
-- HList and type-indexed collections

{-# LANGUAGE TypeOperators, DataKinds, TypeFamilies #-}

-- Heterogeneous list
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: h -> HList t -> HList (h ': t)

infixr 5 `HCons`

-- Type-level length
type family Length (xs :: [*]) :: Nat where
  Length '[]       = 'Zero
  Length (_ ': xs) = 'Succ (Length xs)

-- Get element by type-level index
class Get (n :: Nat) (xs :: [*]) where
  type GetType n xs :: *
  hGet :: HList xs -> GetType n xs

instance Get 'Zero (x ': xs) where
  type GetType 'Zero (x ': xs) = x
  hGet (HCons x _) = x

instance Get n xs => Get ('Succ n) (y ': xs) where
  type GetType ('Succ n) (y ': xs) = GetType n xs
  hGet (HCons _ xs) = hGet @n xs

-- Record with named fields
data Field (name :: Symbol) a = Field a

type (:=) = Field

-- Named record
data Rec :: [(Symbol, *)] -> * where
  RNil  :: Rec '[]
  RCons :: Field s a -> Rec fields -> Rec ('(s, a) ': fields)

-- Type class for field access
class HasField (s :: Symbol) (fields :: [(Symbol, *)]) a | s fields -> a where
  rGet :: Rec fields -> a

-- Example
type Person = Rec
  [ '("name",  Text)
  , '("age",   Int)
  , '("email", Text)
  ]

mkPerson :: Text -> Int -> Text -> Person
mkPerson name age email =
  RCons (Field name) $ RCons (Field age) $ RCons (Field email) RNil
```

---

## ขั้นตอนที่ 709: Indexed Monads

```haskell
-- Indexed monads for tracking state at type level

{-# LANGUAGE RankNTypes, PolyKinds #-}

-- IxMonad: monad indexed by pre/post states
class IxMonad m where
  ireturn :: a -> m i i a
  ibind   :: m i j a -> (a -> m j k b) -> m i k b

(>>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>>=) = ibind

-- Typed state machine using IxMonad
newtype IxState i o a = IxState { runIxState :: i -> (a, o) }

instance IxMonad IxState where
  ireturn a = IxState (\s -> (a, s))
  ibind m f = IxState $ \s ->
    let (a, s') = runIxState m s
    in runIxState (f a) s'

-- Example: file handle with type-safe open/close
data FileState = FileClosed | FileOpen

newtype FileOp i o a = FileOp { runFileOp :: IO a }

openFile :: FileOp 'FileClosed 'FileOpen Handle
openFile = FileOp (System.IO.openFile "test.txt" ReadMode)

closeFile :: Handle -> FileOp 'FileOpen 'FileClosed ()
closeFile h = FileOp (System.IO.hClose h)

readFileLine :: Handle -> FileOp 'FileOpen 'FileOpen Text
readFileLine h = FileOp (T.hGetLine h)

-- Cannot close a closed file (type error):
-- bad :: FileOp 'FileClosed 'FileClosed ()
-- bad = closeFile undefined  -- won't compile
```

---

## ขั้นตอนที่ 710: Type-Level Computations

```haskell
-- Compute at type level with type families

{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- Type-level list operations
type family Map (f :: k -> j) (xs :: [k]) :: [j] where
  Map _ '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Filter (p :: k -> Bool) (xs :: [k]) :: [k] where
  Filter _ '[]       = '[]
  Filter p (x ': xs) = If (p x) (x ': Filter p xs) (Filter p xs)

type family If (b :: Bool) (t :: k) (e :: k) :: k where
  If 'True  t _ = t
  If 'False _ e = e

-- Type-level sorting (insertion sort)
type family Insert (x :: Nat) (xs :: [Nat]) :: [Nat] where
  Insert x '[]       = '[x]
  Insert x (y ': ys) = If (x <=? y) (x ': y ': ys) (y ': Insert x ys)

type family Sort (xs :: [Nat]) :: [Nat] where
  Sort '[]       = '[]
  Sort (x ': xs) = Insert x (Sort xs)

-- Type-level map with constraints
type family MapConstraint (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  MapConstraint _ '[]       = ()
  MapConstraint c (x ': xs) = (c x, MapConstraint c xs)

-- All types in list must satisfy constraint
class MapConstraint Show xs => ShowAll xs where
  showAll :: HList xs -> [String]

instance ShowAll '[] where
  showAll HNil = []

instance (Show x, ShowAll xs) => ShowAll (x ': xs) where
  showAll (HCons x xs) = show x : showAll xs
```

---

## ขั้นตอนที่ 711: Generic Programming Advanced

```haskell
-- Advanced GHC.Generics usage

{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

import GHC.Generics

-- Deep equality via generics
class GDeepEq f where
  gdeepEq :: f a -> f a -> Bool

instance GDeepEq V1 where
  gdeepEq _ _ = True

instance GDeepEq U1 where
  gdeepEq U1 U1 = True

instance Eq c => GDeepEq (K1 i c) where
  gdeepEq (K1 x) (K1 y) = x == y

instance (GDeepEq f, GDeepEq g) => GDeepEq (f :*: g) where
  gdeepEq (f1 :*: g1) (f2 :*: g2) = gdeepEq f1 f2 && gdeepEq g1 g2

instance (GDeepEq f, GDeepEq g) => GDeepEq (f :+: g) where
  gdeepEq (L1 f1) (L1 f2) = gdeepEq f1 f2
  gdeepEq (R1 g1) (R1 g2) = gdeepEq g1 g2
  gdeepEq _       _        = False

instance GDeepEq f => GDeepEq (M1 i t f) where
  gdeepEq (M1 f) (M1 g) = gdeepEq f g

-- Generic serialization
class GSerialize f where
  gserialize :: f a -> [Word8]

-- Generic CSV derivation
class GenericCsv a where
  csvHeaders :: Proxy a -> [Text]
  toCsvRow   :: a -> [Text]
  
  default csvHeaders :: (Generic a, GCsvHeaders (Rep a)) => Proxy a -> [Text]
  csvHeaders _ = gcsvHeaders (Proxy :: Proxy (Rep a))
  
  default toCsvRow :: (Generic a, GCsvRow (Rep a)) => a -> [Text]
  toCsvRow = gcsvRow . from
```

---

## ขั้นตอนที่ 712: Type-Safe Builder Pattern

```haskell
-- Builder pattern ที่ type-safe ด้วย phantom types

{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds #-}

-- Track which fields have been set
data RequestBuilder (fields :: [Symbol]) = RequestBuilder
  { rbUrl     :: Maybe Text
  , rbMethod  :: Maybe Method
  , rbBody    :: Maybe ByteString
  , rbHeaders :: Map Text Text
  }

-- Initially empty
emptyBuilder :: RequestBuilder '[]
emptyBuilder = RequestBuilder Nothing Nothing Nothing Map.empty

-- Add fields (tracked in type)
type family SetField (f :: Symbol) (fs :: [Symbol]) :: [Symbol] where
  SetField f fs = f ': fs

withUrl :: Text -> RequestBuilder fields -> RequestBuilder (SetField "url" fields)
withUrl url b = coerce b { rbUrl = Just url }

withMethod :: Method -> RequestBuilder fields -> RequestBuilder (SetField "method" fields)
withMethod m b = coerce b { rbMethod = Just m }

withBody :: ByteString -> RequestBuilder fields -> RequestBuilder (SetField "body" fields)
withBody body b = coerce b { rbBody = Just body }

-- Build only works when required fields are present
type family HasAll (required :: [Symbol]) (present :: [Symbol]) :: Constraint where
  HasAll '[]       _       = ()
  HasAll (r ': rs) present = (Elem r present ~ 'True, HasAll rs present)

build
  :: HasAll '["url", "method"] fields
  => RequestBuilder fields -> Request
build b = Request
  { reqUrl    = fromJust (rbUrl b)
  , reqMethod = fromJust (rbMethod b)
  , reqBody   = rbBody b
  , reqHeaders = rbHeaders b
  }

-- Won't compile without url and method:
-- bad = build emptyBuilder
```

---

## ขั้นตอนที่ 713: Tagless Final Encoding

```haskell
-- Tagless final: multiple interpretations of same program

-- Type class defines operations
class Expr repr where
  lit :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  neg :: repr Int -> repr Int

class Expr repr => BoolExpr repr where
  bool :: Bool -> repr Bool
  not' :: repr Bool -> repr Bool
  and' :: repr Bool -> repr Bool -> repr Bool
  eq   :: Eq a => repr a -> repr a -> repr Bool
  if'  :: repr Bool -> repr a -> repr a -> repr a

-- Evaluator
newtype Eval a = Eval { runEval :: a }

instance Expr Eval where
  lit n     = Eval n
  add x y   = Eval (runEval x + runEval y)
  mul x y   = Eval (runEval x * runEval y)
  neg x     = Eval (negate (runEval x))

instance BoolExpr Eval where
  bool b    = Eval b
  not' x    = Eval (not (runEval x))
  and' x y  = Eval (runEval x && runEval y)
  eq x y    = Eval (runEval x == runEval y)
  if' c t e = Eval (if runEval c then runEval t else runEval e)

-- Pretty printer
newtype Pretty a = Pretty { runPretty :: Text }

instance Expr Pretty where
  lit n     = Pretty (T.pack (show n))
  add x y   = Pretty ("(" <> runPretty x <> " + " <> runPretty y <> ")")
  mul x y   = Pretty ("(" <> runPretty x <> " * " <> runPretty y <> ")")
  neg x     = Pretty ("(-" <> runPretty x <> ")")

-- Same expression, different interpretations
program :: (Expr repr, BoolExpr repr) => repr Int
program = if' (eq (lit 2) (lit 2)) (add (lit 1) (lit 2)) (lit 0)

eval   = runEval   (program :: Eval Int)   -- 3
pretty = runPretty (program :: Pretty Int) -- "(if (2 == 2) (1 + 2) 0)"
```

---

## ขั้นตอนที่ 714: Existential Types

```haskell
-- Existential types สำหรับ heterogeneous collections

{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

-- Type hiding
data ShowBox = forall a. Show a => ShowBox a

printBox :: ShowBox -> IO ()
printBox (ShowBox x) = print x

-- Heterogeneous list of showable things
showBoxes :: [ShowBox]
showBoxes = [ShowBox (1 :: Int), ShowBox "hello", ShowBox True]

-- Existential with multiple constraints
data SomeException' = forall e. (Exception e, Show e) => SomeException' e

-- Continuation-style existentials (Church encoding)
type Exists f = forall r. (forall a. f a -> r) -> r

pack :: f a -> Exists f
pack fa = \k -> k fa

unpack :: Exists f -> (forall a. f a -> r) -> r
unpack e k = e k

-- Dynamic dispatch via existentials
data Animal = forall a. Animal { aName :: Text, aSpeak :: a -> Text, aData :: a }

makeAnimal :: Text -> (a -> Text) -> a -> Animal
makeAnimal = Animal

speak :: Animal -> Text
speak (Animal _ s d) = s d

animals :: [Animal]
animals =
  [ makeAnimal "Dog"  (\() -> "Woof!") ()
  , makeAnimal "Cat"  (\() -> "Meow!") ()
  , makeAnimal "Duck" (\() -> "Quack!") ()
  ]
```

---

## ขั้นตอนที่ 715: Comonads

```haskell
-- Comonads: dual of monads

class Functor w => Comonad w where
  extract   :: w a -> a       -- dual of return
  duplicate :: w a -> w (w a) -- dual of join
  extend    :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- Streams (infinite lists)
data Stream a = a :< Stream a
infixr 5 :<

instance Functor Stream where
  fmap f (x :< xs) = f x :< fmap f xs

instance Comonad Stream where
  extract (x :< _) = x
  duplicate s@(_ :< xs) = s :< duplicate xs

-- Moving average using comonad
movingAvg :: Int -> Stream Double -> Stream Double
movingAvg n = extend (avg . take n . streamToList)
  where
    streamToList (x :< xs) = x : streamToList xs
    avg xs = sum xs / fromIntegral (length xs)

-- Cellular automata using comonad
data Zipper a = Zipper [a] a [a]  -- left, focus, right

instance Functor Zipper where
  fmap f (Zipper l c r) = Zipper (fmap f l) (f c) (fmap f r)

instance Comonad Zipper where
  extract (Zipper _ c _) = c
  duplicate z@(Zipper l _ r) = Zipper (map go (tail (iterate shiftLeft  z)))
                                      z
                                      (map go (tail (iterate shiftRight z)))
    where go = id

-- Conway's Game of Life rule using comonad
rule :: Zipper (Zipper Bool) -> Bool
rule z =
  let alive     = extract (extract z)
      neighbors = count True (surrounding z)
  in if alive then neighbors `elem` [2, 3] else neighbors == 3
```

---

## ขั้นตอนที่ 716: Monad Transformers Deep Dive

```haskell
-- Monad transformer stacks ขั้นสูง

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- ReaderT + WriterT + StateT + ExceptT + IO
newtype AppM e s w r a = AppM
  { unAppM :: ReaderT r (StateT s (WriterT w (ExceptT e IO))) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadReader r, MonadState s, MonadWriter w, MonadError e
    , MonadIO
    )

runAppM :: AppM e s w r a -> r -> s -> IO (Either e ((a, s), w))
runAppM m env initState =
  runExceptT (runWriterT (runStateT (runReaderT (unAppM m) env) initState))

-- Type class MTL style
class Monad m => MonadApp m where
  getConfig   :: m AppConfig
  getState    :: m AppState
  putState    :: AppState -> m ()
  logMsg      :: Text -> m ()
  throwAppErr :: AppError -> m a

-- Multiple implementations
instance MonadApp (AppM AppError AppState [Text] AppConfig) where
  getConfig   = ask
  getState    = get
  putState    = put
  logMsg msg  = tell [msg]
  throwAppErr = throwError

instance MonadApp (ReaderT AppConfig IO) where
  getConfig   = ask
  getState    = liftIO (readIORef globalState)
  putState s  = liftIO (writeIORef globalState s)
  logMsg msg  = liftIO (putStrLn (T.unpack msg))
  throwAppErr = liftIO . throwIO

-- Run business logic with any monad
processRequest :: MonadApp m => Request -> m Response
processRequest req = do
  cfg   <- getConfig
  state <- getState
  logMsg ("Processing: " <> requestId req)
  -- ... business logic works with any m
  return (buildResponse state cfg req)
```

---

## ขั้นตอนที่ 717: Advanced Pattern Matching

```haskell
-- Pattern matching ขั้นสูงด้วย GADTs และ view patterns

{-# LANGUAGE ViewPatterns, PatternSynonyms, LambdaCase #-}

-- View patterns
data Person = Person { personName :: Text, personAge :: Int }

isAdult :: Person -> Bool
isAdult (personAge -> age) = age >= 18

nameLength :: Person -> Int
nameLength (T.length . personName -> n) = n

-- Pattern synonyms
pattern Email :: Text -> Text -> Text
pattern Email user domain <- (T.breakOn "@" -> (user, T.drop 1 -> domain))

-- Named patterns
pattern Adult :: Person
pattern Adult <- Person { personAge = (>= 18) -> True }

pattern Child :: Person
pattern Child <- Person { personAge = (< 18) -> True }

classify :: Person -> Text
classify Adult = "Adult"
classify Child = "Child"

-- GADT patterns
data Expr' a where
  Lit'  :: Int -> Expr' Int
  Bool' :: Bool -> Expr' Bool
  Add'  :: Expr' Int -> Expr' Int -> Expr' Int
  If'   :: Expr' Bool -> Expr' a -> Expr' a -> Expr' a

eval' :: Expr' a -> a
eval' = \case
  Lit'  n     -> n
  Bool' b     -> b
  Add'  e1 e2 -> eval' e1 + eval' e2
  If' c t e   -> if eval' c then eval' t else eval' e
```

---

## ขั้นตอนที่ 718: Arrows

```haskell
-- Arrows: generalization of functions

import Control.Arrow

-- Arrow laws:
-- arr id = id
-- arr (f >>> g) = arr f >>> arr g
-- first (arr f) = arr (first f)

-- Function arrow
addArrow :: Arrow a => a Int Int
addArrow = arr (+1)

-- Kleisli arrow (monadic functions)
type Kleisli m a b = a -> m b

safeDiv :: Kleisli Maybe Int (Int, Int)
safeDiv n = \d -> if d == 0 then Nothing else Just (n `div` d, n `mod` d)

-- Automaton arrow (stream processing)
newtype Auto a b = Auto { step :: a -> (b, Auto a b) }

instance Category Auto where
  id = Auto (\a -> (a, id))
  g . f = Auto $ \a ->
    let (b, f') = step f a
        (c, g') = step g b
    in (c, g' . f')

instance Arrow Auto where
  arr f = Auto (\a -> (f a, arr f))
  first (Auto step) = Auto $ \(a, d) ->
    let (b, aut') = step a
    in ((b, d), first aut')

-- Moving sum automaton
movingSum :: Int -> Auto Int Int
movingSum initial = Auto (go [initial])
  where
    go buf x =
      let buf' = x : take 4 buf
      in (sum buf', Auto (go buf'))
```

---

## ขั้นตอนที่ 719: Type Inference and Unification

```haskell
-- Simple type inference engine

-- Types
data Ty
  = TInt
  | TBool
  | TFun Ty Ty
  | TVar TyVar
  deriving (Eq, Show)

newtype TyVar = TyVar Int deriving (Eq, Ord, Show)

-- Substitution
type Subst = Map TyVar Ty

applySubst :: Subst -> Ty -> Ty
applySubst s (TVar v)   = fromMaybe (TVar v) (Map.lookup v s)
applySubst s (TFun a b) = TFun (applySubst s a) (applySubst s b)
applySubst _ ty         = ty

-- Unification
unify :: Ty -> Ty -> Either Text Subst
unify TInt TInt   = Right Map.empty
unify TBool TBool = Right Map.empty
unify (TVar v) ty = bind v ty
unify ty (TVar v) = bind v ty
unify (TFun a1 b1) (TFun a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
  return (Map.union s1 s2)
unify t1 t2 = Left ("Cannot unify " <> T.pack (show t1) <> " with " <> T.pack (show t2))

bind :: TyVar -> Ty -> Either Text Subst
bind v ty
  | ty == TVar v     = Right Map.empty
  | v `occursIn` ty  = Left "Occurs check failed"
  | otherwise        = Right (Map.singleton v ty)

occursIn :: TyVar -> Ty -> Bool
occursIn v (TVar v')  = v == v'
occursIn v (TFun a b) = v `occursIn` a || v `occursIn` b
occursIn _ _          = False
```

---

## ขั้นตอนที่ 720: โปรเจกต์: Type-Safe DSL Compiler

```haskell
-- Type-safe DSL with compilation phases

-- Source language
data Expr
  = ELit Int
  | EVar Name
  | ELam Name Expr
  | EApp Expr Expr
  | ELet Name Expr Expr
  | EIf  Expr Expr Expr

-- Typed core
data CoreExpr :: Ty -> * where
  CLit :: Int -> CoreExpr TInt
  CVar :: Name -> Proxy t -> CoreExpr t
  CLam :: Name -> CoreExpr b -> CoreExpr (TFun a b)
  CApp :: CoreExpr (TFun a b) -> CoreExpr a -> CoreExpr b
  CIf  :: CoreExpr TBool -> CoreExpr t -> CoreExpr t -> CoreExpr t

-- Type-checking produces typed core
typeCheck :: Expr -> TyEnv -> Either TypeError (SomeTy CoreExpr)
typeCheck (ELit n) _   = Right (Some TInt (CLit n))
typeCheck (EVar x) env =
  case Map.lookup x env of
    Nothing -> Left (UnboundVar x)
    Just ty -> Right (Some ty (CVar x (Proxy :: Proxy ty)))
typeCheck (EIf c t e) env = do
  (Some TBool c') <- typeCheck c env
  (Some tt t')    <- typeCheck t env
  (Some te e')    <- typeCheck e env
  case testEquality tt te of
    Nothing   -> Left (TypeMismatch tt te)
    Just Refl -> Right (Some tt (CIf c' t' e'))

-- Optimize typed core
optimizeCore :: CoreExpr t -> CoreExpr t
optimizeCore (CIf (CLit 1) t _) = optimizeCore t
optimizeCore (CIf (CLit 0) _ e) = optimizeCore e
optimizeCore (CApp (CLam x body) arg) = substitute x arg body
optimizeCore e = e

-- Compile to machine code
compileToClosure :: CoreExpr t -> IO (IO t)
compileToClosure (CLit n) = return (return n)
compileToClosure (CApp f a) = do
  compiledF <- compileToClosure f
  compiledA <- compileToClosure a
  return (compiledF <*> compiledA)
```

---

*[← Part 35](part-35.md) | [Part 37 →](part-37.md)*
