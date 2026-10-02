# Part 26: Type-Level Programming ขั้นสูง
## ขั้นตอนที่ 501-520

---

## ขั้นตอนที่ 501: DataKinds และ Type-Level Values

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- DataKinds: ใช้ค่าเป็น type
data Nat = Zero | Succ Nat  -- promoted to kind Nat

-- Natural number ที่ level type
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- Type-indexed Vector (length is part of type!)
data Vec (n :: Nat) (a :: Type) where
  VNil  :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a

-- Type-safe head (ไม่สามารถ head empty vec ได้)
vHead :: Vec ('Succ n) a -> a
vHead (VCons x _) = x  -- always safe!

-- Type-safe tail
vTail :: Vec ('Succ n) a -> Vec n a
vTail (VCons _ xs) = xs

-- Append with type-level addition
type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add 'Zero     n = n
  Add ('Succ m) n = 'Succ (Add m n)

vAppend :: Vec m a -> Vec n a -> Vec (Add m n) a
vAppend VNil         ys = ys
vAppend (VCons x xs) ys = VCons x (vAppend xs ys)

-- Convert to list
vToList :: Vec n a -> [a]
vToList VNil         = []
vToList (VCons x xs) = x : vToList xs

-- Reify type-level Nat to value
class KnownNat' (n :: Nat) where
  natVal' :: SNat n

instance KnownNat' 'Zero where
  natVal' = SZero

instance KnownNat' n => KnownNat' ('Succ n) where
  natVal' = SSucc natVal'

-- Length from type
vLength :: KnownNat' n => Vec n a -> Int
vLength _ = snatToInt (natVal' :: SNat n)
  where
    snatToInt :: SNat n -> Int
    snatToInt SZero     = 0
    snatToInt (SSucc n) = 1 + snatToInt n
```

---

## ขั้นตอนที่ 502: Type Families ขั้นสูง

```haskell
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances  #-}

-- Open type families
type family Elem (a :: Type) (xs :: [Type]) :: Bool where
  Elem a '[]       = 'False
  Elem a (a ': xs) = 'True
  Elem a (b ': xs) = Elem a xs

-- Injective type families
type family Id (a :: Type) = r | r -> a where
  Id Int  = Int
  Id Bool = Bool
  Id Char = Char

-- Associated type families ใน type class
class Container f where
  type Element f :: Type
  empty   :: f
  insert  :: Element f -> f -> f
  toList  :: f -> [Element f]

instance Container [a] where
  type Element [a] = a
  empty  = []
  insert = (:)
  toList = id

instance Container (Set a) where
  type Element (Set a) = a
  empty  = S.empty
  insert = S.insert
  toList = S.toList

-- Closed type family for type-level computation
type family If (b :: Bool) (t :: Type) (f :: Type) :: Type where
  If 'True  t f = t
  If 'False t f = f

-- Type level list operations
type family Length (xs :: [k]) :: Nat where
  Length '[]      = 'Zero
  Length (_ ': xs) = 'Succ (Length xs)

type family Head (xs :: [k]) :: k where
  Head (x ': _) = x

type family Tail (xs :: [k]) :: [k] where
  Tail (_ ': xs) = xs

type family Reverse (xs :: [k]) :: [k] where
  Reverse '[]      = '[]
  Reverse (x ': xs) = Append (Reverse xs) '[x]

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[]      ys = ys
  Append (x ': xs) ys = x ': Append xs ys
```

---

## ขั้นตอนที่ 503: Heterogeneous Lists (HList)

```haskell
-- HList: list ที่เก็บค่าต่าง type ได้

data HList (ts :: [Type]) where
  HNil  :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- Infixr operator
(<:>) :: t -> HList ts -> HList (t ': ts)
(<:>) = HCons
infixr 5 <:>

-- Example
exampleList :: HList '[Int, Text, Bool, Double]
exampleList = 42 <:> "hello" <:> True <:> 3.14 <:> HNil

-- Type-safe indexing
type family Index (n :: Nat) (ts :: [Type]) :: Type where
  Index 'Zero     (t ': _) = t
  Index ('Succ n) (_ ': ts) = Index n ts

class HIndex (n :: Nat) (ts :: [Type]) where
  hIndex :: SNat n -> HList ts -> Index n ts

instance HIndex 'Zero (t ': ts) where
  hIndex SZero (HCons x _) = x

instance HIndex n ts => HIndex ('Succ n) (t ': ts) where
  hIndex (SSucc n) (HCons _ xs) = hIndex n xs

-- Usage
getFirst :: HList (t ': ts) -> t
getFirst (HCons x _) = x

getSecond :: HList (t1 ': t2 ': ts) -> t2
getSecond (HCons _ (HCons y _)) = y

-- Mapping over HList
class HMap f ts where
  hMap :: f -> HList ts -> HList (MapResult f ts)

type family MapResult f ts :: [Type]

-- Folding over HList
class HFoldr f b ts where
  hFoldr :: f -> b -> HList ts -> b

instance HFoldr f b '[] where
  hFoldr _ acc HNil = acc

instance (Apply f t b, HFoldr f b ts) => HFoldr f b (t ': ts) where
  hFoldr f acc (HCons x xs) = apply f x (hFoldr f acc xs)
```

---

## ขั้นตอนที่ 504: Phantom Types สำหรับ Domain Safety

```haskell
-- Phantom types: type parameters ที่ไม่ปรากฏใน value

-- State machine ด้วย phantom types
data Locked
data Unlocked
data Running
data Stopped

newtype Door (state :: Type) = Door { doorId :: Int }
newtype Server (state :: Type) = Server { serverId :: Int }

-- Functions สามารถ lock state ด้วย type
unlock :: Door Locked -> IO (Door Unlocked)
unlock (Door n) = do
  putStrLn $ "Unlocking door " ++ show n
  return (Door n)

lock :: Door Unlocked -> IO (Door Locked)
lock (Door n) = do
  putStrLn $ "Locking door " ++ show n
  return (Door n)

enter :: Door Unlocked -> IO ()
enter (Door n) = putStrLn $ "Entering door " ++ show n

-- Cannot enter locked door - compile error!
-- enterLocked :: Door Locked -> IO ()
-- enterLocked d = enter d  -- type error!

-- Currency types
data USD
data EUR
data GBP

newtype Money (currency :: Type) = Money { amount :: Decimal }

addMoney :: Money c -> Money c -> Money c
addMoney (Money a) (Money b) = Money (a + b)

-- Cannot add different currencies - compile error!
-- mixCurrency :: Money USD -> Money EUR -> Money USD
-- mixCurrency a b = addMoney a b  -- type error!

convertUsdToEur :: Money USD -> Double -> Money EUR
convertUsdToEur (Money usd) rate = Money (usd * fromRational (toRational rate))

-- Database operations
data ReadOnly
data ReadWrite

newtype Connection (perm :: Type) = Connection { connInner :: PG.Connection }

query :: Connection p -> Query -> IO [Row]
query (Connection conn) q = PG.query_ conn q

execute :: Connection ReadWrite -> Query -> IO ()
execute (Connection conn) q = PG.execute_ conn q >> return ()

-- Read-only connection cannot execute
withReadOnly :: (Connection ReadOnly -> IO a) -> IO a
withReadOnly action = withConn $ \conn ->
  action (Connection conn)
```

---

## ขั้นตอนที่ 505: Dependent Types Simulation

```haskell
-- Simulating dependent types ใน GHC

-- Singleton types: bridge between type and value
data Sing (a :: k) where
  SInt  :: KnownNat n => Sing (n :: Nat)
  SBool :: SBool b    -> Sing (b :: Bool)

data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

-- Decision procedure
data Decision a = Proved a | Disproved (a -> Void)

-- Decidable equality at type level
class DecEq k where
  decEq :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

-- Type equality
data a :~: b where
  Refl :: a :~: a

-- Use in safe indexing
safeIndex :: Vec n a -> Fin n -> a
safeIndex (VCons x _)  FZero    = x
safeIndex (VCons _ xs) (FSucc f) = safeIndex xs f

data Fin (n :: Nat) where
  FZero :: Fin ('Succ n)
  FSucc :: Fin n -> Fin ('Succ n)

-- Bounded integers
data BoundedInt (lo :: Nat) (hi :: Nat) = BoundedInt
  { boundedValue :: Int
  } deriving (Show)

mkBounded :: forall lo hi. (KnownNat lo, KnownNat hi)
          => Int -> Maybe (BoundedInt lo hi)
mkBounded n
  | n >= natVal (Proxy :: Proxy lo) &&
    n <= natVal (Proxy :: Proxy hi) = Just (BoundedInt n)
  | otherwise = Nothing

-- Proof-carrying computations
data Proof p a = Proof { proofValue :: a, proofEvidence :: p }

-- Sorted list (type-level invariant)
data Sorted (xs :: [Nat]) where
  SNil  :: Sorted '[]
  SCons :: All ((:<=:) x) xs => Sing x -> Sorted xs -> Sorted (x ': xs)
```

---

## ขั้นตอนที่ 506: Type-Safe API Design

```haskell
-- Type-safe API ด้วย type-level programming

-- Type-level permissions
data Permission
  = CanRead
  | CanWrite
  | CanDelete
  | CanAdmin

type family HasPermission (p :: Permission) (ps :: [Permission]) :: Constraint where
  HasPermission p (p ': _)  = ()
  HasPermission p (_ ': ps) = HasPermission p ps
  HasPermission p '[]       = TypeError ('Text "Missing permission: " ':<>: 'ShowType p)

-- Role-based access
data AdminRole
data UserRole
data GuestRole

class Role r where
  type Permissions r :: [Permission]

instance Role AdminRole where
  type Permissions AdminRole = '[CanRead, CanWrite, CanDelete, CanAdmin]

instance Role UserRole where
  type Permissions UserRole = '[CanRead, CanWrite]

instance Role GuestRole where
  type Permissions GuestRole = '[CanRead]

-- Capability-restricted actions
data Action (required :: [Permission]) a where
  ReadAction   :: Action '[CanRead]   a
  WriteAction  :: Action '[CanWrite]  a
  DeleteAction :: Action '[CanDelete] a
  AdminAction  :: Action '[CanAdmin]  a

runAction :: (AllIn required (Permissions role))
          => proxy role
          -> Action required a
          -> IO a
runAction _ _ = undefined  -- implementation

-- Type-safe routing
data Route (method :: Method) (path :: Symbol) (req :: Type) (resp :: Type)

data Method = GET | POST | PUT | DELETE

type UsersIndex = Route 'GET "/users" () [User]
type CreateUser = Route 'POST "/users" CreateUserReq User
type GetUser    = Route 'GET "/users/:id" () User
type DeleteUser = Route 'DELETE "/users/:id" () ()

-- Server type (must implement all routes)
type API = UsersIndex
      :<|> CreateUser
      :<|> GetUser
      :<|> DeleteUser

server :: Server API
server = listUsers :<|> createUser :<|> getUser :<|> deleteUser
```

---

## ขั้นตอนที่ 507: Reflection และ Reification

```haskell
-- Reflection: ใช้ type class ดึงค่าจาก type
import Data.Proxy
import GHC.TypeLits

-- KnownNat: reflect type-level Nat to value
portNumber :: forall (n :: Nat). KnownNat n => Int
portNumber = fromInteger (natVal (Proxy :: Proxy n))

-- Usage: portNumber @8080

-- KnownSymbol: reflect type-level String to value
tableName :: forall (s :: Symbol). KnownSymbol s => String
tableName = symbolVal (Proxy :: Proxy s)

-- Reification: convert value to type
import Data.Reflection

-- reify: introduce type class instance at runtime
withKnownInt :: Int -> (forall n. KnownNat n => Proxy n -> r) -> r
withKnownInt n f = reifyNat (fromIntegral n) f

-- Example: runtime-sized vector
createVec :: Int -> [a] -> SomeVec a
createVec n xs = withKnownInt n $ \(_ :: Proxy n) ->
  SomeVec (fromList xs :: Vec n a)

data SomeVec a = forall n. SomeVec (Vec n a)

-- Runtime type equality
eqTypeRep :: TypeRep a -> TypeRep b -> Maybe (a :~: b)
eqTypeRep ta tb
  | ta == tb  = Just (unsafeCoerce Refl)
  | otherwise = Nothing

-- Dynamic types
data Dynamic = forall a. Typeable a => Dynamic a

toDyn :: Typeable a => a -> Dynamic
toDyn = Dynamic

fromDyn :: Typeable a => Dynamic -> Maybe a
fromDyn (Dynamic x) = cast x
```

---

## ขั้นตอนที่ 508: Generic Programming ขั้นสูง

```haskell
-- GHC.Generics สำหรับ generic programming

import GHC.Generics

-- Auto-derive JSON for any Generic type
class GToJson (f :: Type -> Type) where
  gToJson :: f a -> Value

instance GToJson V1 where  -- empty types
  gToJson _ = Null

instance GToJson U1 where  -- unit constructor
  gToJson U1 = object []

instance (GToJson a, GToJson b) => GToJson (a :*: b) where  -- products
  gToJson (a :*: b) = mergeObjects (gToJson a) (gToJson b)

instance (GToJson a, GToJson b) => GToJson (a :+: b) where  -- sums
  gToJson (L1 a) = gToJson a
  gToJson (R1 b) = gToJson b

instance (GToJson a, Selector s) => GToJson (S1 s a) where  -- selectors
  gToJson (M1 a) = object [T.pack (selName (undefined :: M1 s c a p)) .= gToJson a]

instance (ToJSON c) => GToJson (K1 i c) where  -- constants
  gToJson (K1 c) = toJSON c

-- Auto-derive CSV encoder
class GToCSV (f :: Type -> Type) where
  gToCSV :: f a -> [Text]
  gHeaders :: proxy f -> [Text]

-- Deep traversal of data types
class GDepth (f :: Type -> Type) where
  gDepth :: f a -> Int

instance GDepth (K1 i c) where
  gDepth _ = 0

instance GDepth a => GDepth (M1 i c a) where
  gDepth (M1 a) = gDepth a

instance (GDepth a, GDepth b) => GDepth (a :*: b) where
  gDepth (a :*: b) = max (gDepth a) (gDepth b) + 1

-- Generic comparison
genericCompare :: (Generic a, GCompare (Rep a)) => a -> a -> Ordering
genericCompare x y = gCompare (from x) (from y)
```

---

## ขั้นตอนที่ 509: Advanced GADTs

```haskell
-- GADTs สำหรับ type-safe DSLs

-- Type-safe expression language
data Expr (a :: Type) where
  Lit    :: a -> Expr a
  Add    :: Num a => Expr a -> Expr a -> Expr a
  Mul    :: Num a => Expr a -> Expr a -> Expr a
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a
  Eq     :: Eq a => Expr a -> Expr a -> Expr Bool
  Lam    :: (Expr a -> Expr b) -> Expr (a -> b)
  App    :: Expr (a -> b) -> Expr a -> Expr b
  Pair   :: Expr a -> Expr b -> Expr (a, b)
  Fst    :: Expr (a, b) -> Expr a
  Snd    :: Expr (a, b) -> Expr b

-- Evaluator is total (no partial functions!)
eval :: Expr a -> a
eval (Lit x)     = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (If b t f)  = if eval b then eval t else eval f
eval (Eq e1 e2)  = eval e1 == eval e2
eval (Lam f)     = \x -> eval (f (Lit x))
eval (App f x)   = eval f (eval x)
eval (Pair a b)  = (eval a, eval b)
eval (Fst p)     = fst (eval p)
eval (Snd p)     = snd (eval p)

-- Typed stack machine
data Stack (ts :: [Type]) where
  Empty :: Stack '[]
  Push  :: a -> Stack ts -> Stack (a ': ts)

data Instruction (from :: [Type]) (to :: [Type]) where
  IPush :: a -> Instruction ts (a ': ts)
  IPop  :: Instruction (a ': ts) ts
  IAdd  :: Instruction (Int ': Int ': ts) (Int ': ts)
  IDup  :: Instruction (a ': ts) (a ': a ': ts)
  ISwap :: Instruction (a ': b ': ts) (b ': a ': ts)

runInstruction :: Instruction from to -> Stack from -> Stack to
runInstruction (IPush x) s        = Push x s
runInstruction IPop      (Push _ s) = s
runInstruction IAdd      (Push a (Push b s)) = Push (a + b) s
runInstruction IDup      (Push a s) = Push a (Push a s)
runInstruction ISwap     (Push a (Push b s)) = Push b (Push a s)
```

---

## ขั้นตอนที่ 510: Coercible และ Roles

```haskell
-- Coerce: zero-cost conversion between representationally equal types

import Data.Coerce
import GHC.Prim (coerce)

-- Newtype coercions
newtype Age = Age Int
newtype Name = Name Text

age :: Age
age = Age 42

-- Safe coerce between representationally equal types
ageToInt :: Age -> Int
ageToInt = coerce  -- zero cost!

-- Coerce in containers
ageList :: [Age] -> [Int]
ageList = coerce  -- zero cost even in containers!

-- Roles prevent unsafe coercions
-- nominal: cannot coerce (Set uses Ord, coerce would violate invariant)
-- representational: can coerce (safe)
-- phantom: can coerce

-- Set has nominal role for element type
-- Cannot coerce Set Name -> Set Text even if Name = newtype Text
-- because Set's invariant depends on Ord instance

-- Data.Semigroup.Last has phantom role
-- newtype Last a = Last { getLast :: Maybe a }
-- Can coerce Last Name -> Last Text if Name ~ Text

-- Explicit role annotations
type role Set nominal         -- cannot coerce elements
type role Vector nominal      -- cannot coerce elements

-- Use coerce for performance
processAges :: [Age] -> [Age]
processAges ages = coerce . filter (> 0) . coerce $ ages
-- :: [Age] -> [Int] -> [Int] -> [Age]
-- all zero-cost!
```

---

## ขั้นตอนที่ 511: Quantified Constraints

```haskell
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

-- Quantified constraints: constraint สำหรับทุก type

-- "For all a, if Eq a then Eq (f a)"
class (forall a. Eq a => Eq (f a)) => EqF f where
  liftEq :: (a -> a -> Bool) -> f a -> f a -> Bool

-- Example implementation
instance EqF Maybe where
  liftEq _ Nothing  Nothing  = True
  liftEq _ Nothing  (Just _) = False
  liftEq _ (Just _) Nothing  = False
  liftEq f (Just a) (Just b) = f a b

-- Functor with quantified law
class (forall a b. (Eq a, Eq b) => Eq (f a), Functor f) => LawfulFunctor f where
  -- fmap id = id (expressible with quantified constraint)

-- Higher-kinded equality
class HEq (f :: k -> Type) where
  heq :: f a -> f b -> Bool

-- Universal existentials
data AnyShow = forall a. Show a => AnyShow a

showAny :: AnyShow -> String
showAny (AnyShow x) = show x

-- Constraint kinds
type Serializable a = (Show a, Read a, ToJSON a, FromJSON a)

printAndRead :: Serializable a => a -> Maybe a
printAndRead x = readMaybe (show x)
```

---

## ขั้นตอนที่ 512: Type Applications

```haskell
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Type applications: explicit type arguments

-- read ต้องการ type
x :: Int
x = read @Int "42"

-- parse JSON
parseUser :: ByteString -> Maybe User
parseUser = decode @User

-- Proxy-free programming
class Configurable a where
  defaultConfig :: a
  configName    :: String

instance Configurable DatabaseConfig where
  defaultConfig = DatabaseConfig { dbHost = "localhost", dbPort = 5432 }
  configName = "database"

-- Use with TypeApplications instead of Proxy
getConfigName :: forall a. Configurable a => String
getConfigName = configName @a  -- no Proxy needed!

-- Type application in constraints
class HasDefault a where
  def :: a

class (HasDefault a, HasDefault b) => BothDefault a b where
  bothDef :: (a, b)
  bothDef = (def @a, def @b)

-- fromInteger with type application
mkInt :: Integer -> Int
mkInt = fromInteger @Int

mkDouble :: Integer -> Double
mkDouble = fromInteger @Double

-- Explicit type in do-notation
main :: IO ()
main = do
  val <- readIORef @Int someRef  -- explicit type!
  print val
```

---

## ขั้นตอนที่ 513: Linear Types

```haskell
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

-- Linear types: ใช้ค่าได้ครั้งเดียว (Haskell 9.0+)

import Prelude.Linear
import qualified Control.Monad.Linear as Linear

-- Linear function: argument used exactly once
double :: Int %1 -> Int
double x = x + x  -- x used twice! ❌ compiler error

-- Correct
copyAndDouble :: Int %1 -> (Int, Int)
copyAndDouble x = (x, x)  -- x copied then used once each ✓

-- Resource-safe file handling
data LinearFile

openFile :: FilePath -> IO (Ur LinearFile)
closeFile :: LinearFile %1 -> IO ()

withFile :: FilePath -> (LinearFile %1 -> IO b) -> IO b
withFile path action = do
  Ur f <- openFile path
  action f  -- f used exactly once

-- Mutable arrays without freezing
import Data.Array.Mutable.Linear

-- Linear array modification (safe!)
processArray :: Array Int %1 -> (Array Int, Int)
processArray arr =
  let arr1 = set 0 42 arr      -- arr used once (consumed by set)
      arr2 = set 1 43 arr1     -- arr1 used once
      (arr3, v) = get 0 arr2   -- arr2 used once
  in (arr3, v)

-- Prompt resource deallocation
import System.IO.Linear

linearMain :: IO ()
linearMain = do
  (handle, bytes) <- withFile "input.txt" $ \h ->
    let (h', bs) = readFile h  -- linear read
    in (h', bs)
  print bytes
```

---

## ขั้นตอนที่ 514: Visible Forall และ RequiredTypeArguments

```haskell
{-# LANGUAGE RequiredTypeArguments #-}  -- GHC 9.10+

-- Visible forall: type arguments ที่ต้องระบุ

-- Function ที่ต้องการ type argument
typeOf :: forall a -> a -> TypeRep
typeOf a _ = typeRep @a

-- Usage: typeOf Int 42  (pass type as argument)

-- Previously needed Proxy:
typeOfProxy :: Proxy a -> TypeRep
typeOfProxy _ = typeRep @a
-- Usage: typeOfProxy (Proxy :: Proxy Int)

-- More ergonomic type-directed programming
fromJSON :: forall a -> FromJSON a => ByteString -> Maybe a
fromJSON _ = decode

-- Usage: fromJSON User "{ \"name\": \"Alice\" }"

-- Allocate values of any type
allocate :: forall a -> Default a => IO (IORef a)
allocate _ = newIORef def
-- Usage: allocate Int   -> IORef Int with value 0
--        allocate Text  -> IORef Text with value ""
```

---

## ขั้นตอนที่ 515: Effect Polymorphism

```haskell
-- Effect-polymorphic code

-- MTL style
class (Monad m) => MonadAuth m where
  authenticate :: Credentials -> m (Either AuthError User)
  getCurrentUser :: m (Maybe User)
  logout :: m ()

class (Monad m) => MonadLogger m where
  logInfo  :: Text -> m ()
  logError :: Text -> m ()
  logDebug :: Text -> m ()

class (Monad m) => MonadCache m where
  cacheGet :: Text -> m (Maybe ByteString)
  cacheSet :: Text -> ByteString -> Int -> m ()
  cacheDel :: Text -> m ()

-- Function polymorphic over effects
loginUser :: (MonadAuth m, MonadLogger m, MonadCache m)
          => LoginRequest -> m (Either LoginError AuthToken)
loginUser req = do
  logInfo $ "Login attempt: " <> lrEmail req
  result <- authenticate (lrCredentials req)
  case result of
    Left err -> do
      logError $ "Auth failed: " <> T.pack (show err)
      return (Left (InvalidCredentials err))
    Right user -> do
      token <- generateToken user
      cacheSet ("session:" <> tokenId token) (encode user) 3600
      logInfo $ "Login success: " <> userName user
      return (Right token)

-- Concrete implementation
newtype AppM a = AppM { runAppM :: ReaderT AppEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

instance MonadAuth AppM where
  authenticate creds = do
    db <- asks envDB
    liftIO $ verifyCredentials db creds

instance MonadLogger AppM where
  logInfo msg  = liftIO $ putStrLn $ "[INFO] " ++ T.unpack msg
  logError msg = liftIO $ putStrLn $ "[ERROR] " ++ T.unpack msg
  logDebug msg = liftIO $ putStrLn $ "[DEBUG] " ++ T.unpack msg
```

---

## ขั้นตอนที่ 516: Type-Level Strings และ Symbols

```haskell
-- Type-level strings (Symbols) ใน Haskell

import GHC.TypeLits

-- Symbol: type-level string
type MyField = "firstName"

-- Append symbols
type FullName = "first" `AppendSymbol` "Last"

-- Compare symbols
type IsEqual = CmpSymbol "foo" "foo"  -- EQ

-- Symbol operations
type family ToSymbol (n :: Nat) :: Symbol

-- Extract symbol value
fieldName :: forall (s :: Symbol). KnownSymbol s => String
fieldName = symbolVal (Proxy :: Proxy s)

-- Type-level record (row types simulation)
data Field (name :: Symbol) (ty :: Type) = Field { fieldValue :: ty }

data Record (fields :: [(Symbol, Type)]) where
  RNil  :: Record '[]
  RCons :: Field n t -> Record rest -> Record ('(n, t) ': rest)

-- Lookup field by name
type family LookupField (n :: Symbol) (fields :: [(Symbol, Type)]) :: Type where
  LookupField n ('(n, t) ': _) = t
  LookupField n (_ ': rest)    = LookupField n rest
  LookupField n '[]            = TypeError ('Text "Field not found: " ':<>: 'Text n)

getField :: forall n fields. (KnownSymbol n)
         => Record fields -> Field n (LookupField n fields)
getField = undefined  -- implementation with type class

-- Example record type
type PersonRecord = Record
  '[ '("name", Text)
   , '("age",  Int)
   , '("email", Text)
   ]
```

---

## ขั้นตอนที่ 517: Deriving Strategies ขั้นสูง

```haskell
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving      #-}

-- DerivingVia: derive instances via another type

-- Example: Semigroup via First
newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  First Nothing  <> y = y
  First (Just x) <> _ = First (Just x)

-- ConfigValue takes First's Semigroup behavior
newtype ConfigValue a = ConfigValue (Maybe a)
  deriving Semigroup via (First a)

-- Derive newtype behaviors
newtype Percentage = Percentage { getPercent :: Double }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, Floating, NFData)

-- Derive via Generically for standard instances
data Point = Point { x :: Double, y :: Double }
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (GenericJSON Point)

-- Standalone deriving for orphan instances
deriving instance Show (Fix f)
deriving instance Functor f => Eq (Fix f)

-- Newtype deriving through transformers
newtype AppM a = AppM { unAppM :: ReaderT Env (ExceptT AppError IO) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Env
    , MonadError AppError
    , MonadIO
    )

-- DerivingVia for MTL instances
newtype LoggingT m a = LoggingT { runLoggingT :: ReaderT Logger m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- Derive all IO-like classes via LoggingT
deriving via (LoggingT IO) instance MonadIO (LoggingT IO)
```

---

## ขั้นตอนที่ 518: Compiler Plugins

```haskell
-- GHC Compiler Plugins

-- Types of plugins:
-- 1. Core plugins: transform GHC Core IR
-- 2. Typechecker plugins: extend type checker
-- 3. Source plugins: transform syntax tree
-- 4. Frontend plugins: custom pipeline

-- Popular plugins:
-- polysemy-plugin: optimize polysemy effects
-- inspection-testing: verify compiler optimizations
-- record-dot-syntax: dot notation for records

-- Using polysemy-plugin
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

import Polysemy

data Telemetry m a where
  RecordEvent :: Text -> Map Text Value -> Telemetry m ()

makeSem ''Telemetry

runTelemetry :: Member (Embed IO) r
             => Sem (Telemetry ': r) a -> Sem r a
runTelemetry = interpret $ \case
  RecordEvent name props ->
    embed (sendToAnalytics name props)

-- Using inspection-testing
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}

import Test.Inspection

-- Assert that this function compiles without allocations
noAllocation :: [Int] -> Int
noAllocation = foldl' (+) 0

-- ❌ Will fail if function allocates
inspect $ hasNoTypeClasses 'noAllocation
inspect $ 'noAllocation `hasNoType` ''[]  -- no list allocation

-- source-plugin example: custom preprocessing
-- Implement record update syntax sugar
-- oldRecord { field1 = val1, field2 = val2 }
-- becomes
-- oldRecord & #field1 .~ val1 & #field2 .~ val2
```

---

## ขั้นตอนที่ 519: Haskell Metaprogramming ด้วย TH

```haskell
-- Template Haskell สำหรับ metaprogramming

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Generate instances at compile time
-- This generates: instance Storable Foo, SizeOf Foo, etc.
$(deriveStorable ''MyStruct)

-- Compile-time file embedding
import Data.FileEmbed

sqlQuery :: ByteString
sqlQuery = $(embedFile "sql/users.sql")

migrations :: [(FilePath, ByteString)]
migrations = $(embedDir "sql/migrations")

-- Generate routing from record
data Routes = Routes
  { routeUsers    :: !Text
  , routePosts    :: !Text
  , routeComments :: !Text
  }

$(makeRoutes ''Routes)

-- Quasi-quotation for DSLs
import Text.Megaparsec.TH

-- SQL DSL with compile-time checking
query :: Query
query = [sql| SELECT u.name, p.title
              FROM users u
              JOIN posts p ON p.author_id = u.id
              WHERE u.active = true |]

-- Regex DSL
pattern :: Regex
pattern = [re| ^\d{4}-\d{2}-\d{2}$ |]

-- Record lens generation
data User = User
  { _userId   :: Int
  , _userName :: Text
  , _userAge  :: Int
  }

$(makeLenses ''User)
$(makeClassy ''User)
$(makePrisms ''Maybe)
```

---

## ขั้นตอนที่ 520: Type-Level Configuration

```haskell
-- Type-level configuration สำหรับ zero-overhead specialization

-- Type-level flags
data LogLevel = Verbose | Normal | Quiet

data ServerConfig (logLevel :: LogLevel) (maxConnections :: Nat) = ServerConfig
  { scPort :: Int
  , scHost :: Text
  }

-- Specialize behavior based on type
class Loggable (level :: LogLevel) where
  logMessage :: Text -> IO ()

instance Loggable 'Verbose where
  logMessage msg = putStrLn $ "[VERBOSE] " ++ T.unpack msg

instance Loggable 'Normal where
  logMessage msg = putStrLn $ "[INFO] " ++ T.unpack msg

instance Loggable 'Quiet where
  logMessage _ = return ()  -- optimized out!

-- GHC will eliminate dead code with -O2
runServer :: forall (l :: LogLevel) (n :: Nat).
          (Loggable l, KnownNat n)
          => ServerConfig l n -> IO ()
runServer config = do
  logMessage @l "Starting server..."  -- specialized per log level
  let maxConns = natVal (Proxy :: Proxy n)
  putStrLn $ "Max connections: " ++ show maxConns
  -- ... server logic

-- Type-level feature flags
data Feature = FeatureEnabled | FeatureDisabled

newtype FeatureFlag (f :: Feature) = FeatureFlag ()

ifEnabled :: FeatureFlag 'FeatureEnabled -> IO () -> IO ()
ifEnabled _ action = action

ifEnabled' :: FeatureFlag 'FeatureDisabled -> IO () -> IO ()
ifEnabled' _ _ = return ()  -- compiler eliminates this

-- Production configuration
type ProdConfig = ServerConfig 'Normal 1000

prodServer :: ServerConfig 'Normal 1000
prodServer = ServerConfig 8080 "0.0.0.0"
```

---

## โปรเจกต์: Type-Safe Query Builder

```haskell
-- Type-safe SQL query builder ด้วย type-level programming

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- Column type
data Column (table :: Symbol) (name :: Symbol) (ty :: Type)

-- Table type
data Table (name :: Symbol) (cols :: [(Symbol, Type)])

-- Query type (fully type-checked)
data Query (tables :: [Symbol]) (cols :: [(Symbol, Type)]) (params :: [Type]) where
  Select :: [SomeColumn tables] -> Query tables '[] '[]
  From   :: Table name cols -> Query '[name] cols '[]
  Join   :: Table name cols -> JoinCondition -> Query ts cs ps -> Query (name ': ts) (cs <> cols) ps
  Where  :: Condition params -> Query ts cs '[] -> Query ts cs params
  OrderBy :: [SomeColumn ts] -> Query ts cs ps -> Query ts cs ps
  Limit  :: Int -> Query ts cs ps -> Query ts cs ps

-- Type-safe condition
data Condition (params :: [Type]) where
  Eq    :: Column t n a -> a -> Condition '[]
  Param :: Column t n a -> Condition '[a]
  And   :: Condition ps -> Condition qs -> Condition (ps ++ qs)
  Or    :: Condition ps -> Condition qs -> Condition (ps ++ qs)

-- Execute with type-checked parameters
executeQuery :: Connection
             -> Query ts cs ps
             -> HList ps
             -> IO [Record cs]
executeQuery conn q params = do
  let (sql, binds) = renderQuery q params
  rows <- DB.query conn sql binds
  return (map parseRow rows)

-- Example usage
usersQuery :: Query '["users"] '[("name", Text), ("email", Text)] '[Int]
usersQuery =
  Select [col @"users" @"name", col @"users" @"email"]
  `From` (table @"users")
  `Where` (Param (col @"users" @"age"))
  `OrderBy` [col @"users" @"name"]
  `Limit` 10

-- Execute with typed parameter
runQuery :: Connection -> Int -> IO [(Text, Text)]
runQuery conn minAge = do
  results <- executeQuery conn usersQuery (minAge <:> HNil)
  return [(getField @"name" r, getField @"email" r) | r <- results]
```

---

*[← Part 25](part-25.md) | [Part 27 →](part-27.md)*
