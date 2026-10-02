# Part 13: Lenses และ Optics
## ขั้นตอนที่ 241-260: Lens, Prism, Traversal

---

## บทนำ

Lenses เป็น abstraction ที่ช่วยให้สามารถ focus, read, และ modify parts ของ nested data structures อย่างง่ายดาย

---

## ขั้นตอนที่ 241: Lens พื้นฐาน

```haskell
-- Lens: focus on a part of a structure
-- Lens s t a b = (a -> f b) -> s -> f t
-- s: input structure
-- t: output structure
-- a: input focus
-- b: output focus

-- Simple Lens (s = t, a = b)
-- Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

import Control.Lens

-- Record field
data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving (Show)

-- Automatically generate lenses
makeLenses ''Person

-- ใช้ lens
alice :: Person
alice = Person "Alice" 30

-- view: ดูค่า
aliceName :: String
aliceName = view name alice  -- "Alice"

-- หรือใช้ operator ^.
aliceName' :: String
aliceName' = alice ^. name  -- "Alice"

-- set: เปลี่ยนค่า
older :: Person
older = set age 31 alice

-- หรือใช้ operator .~
older' :: Person
older' = alice & age .~ 31

-- over: modify ด้วย function
birthday :: Person
birthday = over age (+1) alice

-- หรือใช้ operator %~
birthday' :: Person
birthday' = alice & age %~ (+1)
```

---

## ขั้นตอนที่ 242: Lens Composition

```haskell
-- Lens composition: chain lenses ด้วย (.)

data Address = Address
  { _street :: String
  , _city   :: String
  , _country :: String
  } deriving (Show)

data Company = Company
  { _companyName :: String
  , _headquarters :: Address
  } deriving (Show)

data Employee = Employee
  { _employee :: Person
  , _company  :: Company
  } deriving (Show)

makeLenses ''Address
makeLenses ''Company
makeLenses ''Employee

-- Nested access
emp :: Employee
emp = Employee
  (Person "Bob" 25)
  (Company "Haskell Corp" (Address "123 Main St" "Dublin" "Ireland"))

-- Access nested field
getCity :: Employee -> String
getCity e = e ^. company . headquarters . city

-- Modify nested field
moveToLondon :: Employee -> Employee
moveToLondon = company . headquarters . city .~ "London"

-- Multiple updates with &
updateEmployee :: Employee -> Employee
updateEmployee emp = emp
  & employee . name .~ "Robert"
  & employee . age  %~ (+1)
  & company . headquarters . city .~ "London"
```

---

## ขั้นตอนที่ 243: Common Lens Operations

```haskell
-- view: (^.) = get value
-- set: (.~) = set value
-- over: (%~) = modify with function
-- preview: (^?) = try to get (for Prism)
-- review: (#) = construct with Prism
-- toListOf: (^..) = collect all focused values
-- traverseOf: traverse with an effect

-- ตัวอย่าง operations

data Config = Config
  { _verbose :: Bool
  , _maxConnections :: Int
  , _hosts :: [String]
  } deriving (Show)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config False 10 ["localhost"]

-- View
isVerbose :: Bool
isVerbose = defaultConfig ^. verbose

-- Set
verboseConfig :: Config
verboseConfig = defaultConfig & verbose .~ True

-- Modify
doubleConnections :: Config
doubleConnections = defaultConfig & maxConnections *~ 2

-- Add to list
addHost :: String -> Config -> Config
addHost h = hosts %~ (h:)

-- lens for map values
import qualified Data.Map as Map

scores :: Map String Int
scores = Map.fromList [("Alice", 90), ("Bob", 85)]

-- ix: lens into container
aliceScore :: Maybe Int
aliceScore = scores ^? ix "Alice"

updateAlice :: Map String Int
updateAlice = scores & ix "Alice" %~ (+5)

-- at: access with default
withDefault :: Map String Int
withDefault = scores & at "Charlie" .~ Just 75
```

---

## ขั้นตอนที่ 244: Prisms

```haskell
-- Prism: work with sum types / partial structure

-- Prism s t a b = (b -> t, s -> Either t a)
-- Prism' s a = Prism s s a a

-- Built-in prisms
_Just :: Prism' (Maybe a) a
_Nothing :: Prism' (Maybe a) ()
_Left :: Prism' (Either a b) a
_Right :: Prism' (Either a b) b

-- ตัวอย่าง
maybeVal :: Maybe Int
maybeVal = Just 42

-- preview: extract if matches
val :: Maybe Int
val = maybeVal ^? _Just  -- Just 42

nothing :: Maybe ()
nothing = maybeVal ^? _Nothing  -- Nothing

-- review: construct
makeJust :: Int -> Maybe Int
makeJust n = _Just # n  -- Just n

-- ตัวอย่าง: Custom Prism
data Shape = Circle Double | Rectangle Double Double | Triangle Double Double Double

_circle :: Prism' Shape Double
_circle = prism' Circle $ \case
  Circle r -> Just r
  _        -> Nothing

_rectangle :: Prism' Shape (Double, Double)
_rectangle = prism' (uncurry Rectangle) $ \case
  Rectangle w h -> Just (w, h)
  _             -> Nothing

-- ใช้ prism
radius :: Maybe Double
radius = Circle 5.0 ^? _circle  -- Just 5.0

area :: Maybe Double
area = Rectangle 3.0 4.0 ^? _rectangle . to (uncurry (*))

-- หา radius ของทุก circles
allRadii :: [Shape] -> [Double]
allRadii = toListOf (folded . _circle)
```

---

## ขั้นตอนที่ 245: Traversals

```haskell
-- Traversal: access multiple elements in a structure

-- Traversal s t a b = (a -> f b) -> s -> f t
-- where f is Applicative

-- each: traverse all elements
nums :: [Int]
nums = [1, 2, 3, 4, 5]

-- double all elements
doubled :: [Int]
doubled = nums & each %~ (*2)

-- collect all
allNums :: [Int]
allNums = nums ^.. each

-- traversed: traverse Foldable
sumAll :: Int
sumAll = sumOf traversed nums

-- ตัวอย่าง: nested traversal
data Team = Team
  { _teamName :: String
  , _members  :: [Person]
  } deriving (Show)

makeLenses ''Team

team :: Team
team = Team "Haskell Team" [Person "Alice" 30, Person "Bob" 25]

-- Access all member names
memberNames :: [String]
memberNames = team ^.. members . each . name

-- Age all members
agingTeam :: Team
agingTeam = team & members . each . age %~ (+1)

-- filtered: traverse only matching elements
youngMembers :: [Person]
youngMembers = team ^.. members . each . filtered (\p -> _age p < 28)

-- ตัวอย่าง: Map traversal
import qualified Data.Map as Map

inventory :: Map String Int
inventory = Map.fromList [("apple", 10), ("banana", 5), ("cherry", 20)]

-- Double all quantities
doubled' :: Map String Int
doubled' = inventory & each %~ (*2)

-- Only items with > 10
highStock :: [Int]
highStock = inventory ^.. each . filtered (> 10)
```

---

## ขั้นตอนที่ 246: Isos

```haskell
-- Iso: isomorphism between two types
-- Iso s t a b = (s <-> a, b <-> t)

-- _1, _2: focus on tuple elements
pair :: (Int, String)
pair = (42, "hello")

first :: Int
first = pair ^. _1  -- 42

second :: String
second = pair ^. _2  -- "hello"

-- swap: swap tuple
swapped :: (String, Int)
swapped = pair & swapped

-- coerced: coerce via Coercible
newtype Celsius = Celsius Double
newtype Kelvin  = Kelvin  Double

kelvinIso :: Iso' Celsius Kelvin
kelvinIso = iso (\(Celsius c) -> Kelvin (c + 273.15))
                (\(Kelvin k)  -> Celsius (k - 273.15))

-- ตัวอย่าง: non-empty list iso
import Data.List.NonEmpty (NonEmpty(..))

listToNonEmpty :: Iso' (a, [a]) (NonEmpty a)
listToNonEmpty = iso (\(x, xs) -> x :| xs) (\(x :| xs) -> (x, xs))

-- Text/String iso
import qualified Data.Text as T

textString :: Iso' T.Text String
textString = iso T.unpack T.pack

-- ใช้
upper :: T.Text -> T.Text
upper t = t & textString %~ map toUpper
  where toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
```

---

## ขั้นตอนที่ 247: Folds

```haskell
-- Fold: read-only traversal (get multiple values)

-- Fold s a = (a -> f a) -> s -> f s
-- where f is Applicative AND Contravariant

-- ตัวอย่าง folds

data BinTree a = Leaf' | Node' (BinTree a) a (BinTree a)

-- Fold over all elements
elements :: Fold (BinTree a) a
elements f (Node' l x r) = elements f l *> f x *> elements f r
elements _ Leaf'         = pure ()

-- ใช้ fold
tree :: BinTree Int
tree = Node' (Node' Leaf' 1 Leaf') 2 (Node' Leaf' 3 Leaf')

allElements :: [Int]
allElements = tree ^.. elements  -- [1, 2, 3]

-- sumOf, productOf, minimumOf, maximumOf
total :: Int
total = sumOf elements tree  -- 6

-- ตัวอย่าง: fold with filtering
data Log = Log
  { _logLevel   :: LogLevel
  , _logMessage :: String
  }

data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Eq, Ord, Show)

makeLenses ''Log

logs :: [Log]
logs = [ Log INFO "Starting"
       , Log WARN "Low memory"
       , Log ERROR "Failed"
       , Log INFO "Done"
       ]

-- Get all error messages
errorMessages :: [String]
errorMessages = logs ^.. folded . filtered (\l -> _logLevel l >= ERROR) . logMessage
```

---

## ขั้นตอนที่ 248: Lens Laws

```haskell
-- Lens Laws (3 laws):

-- 1. You get what you put (set then view):
-- view l (set l v s) = v

-- 2. Putting back what you got (view then set):
-- set l (view l s) s = s

-- 3. Setting twice = setting once (set is idempotent):
-- set l v' (set l v s) = set l v' s

-- ตัวอย่าง: ตรวจสอบ lens law ด้วย QuickCheck
import Test.QuickCheck
import Control.Lens

prop_getLaw :: (Eq a, Show a) => Lens' s a -> s -> a -> Bool
prop_getLaw l s v = view l (set l v s) == v

prop_setGetLaw :: (Eq s, Show s) => Lens' s a -> s -> Bool
prop_setGetLaw l s = set l (view l s) s == s

prop_setSetLaw :: (Eq s, Show s) => Lens' s a -> s -> a -> a -> Bool
prop_setSetLaw l s v1 v2 = set l v2 (set l v1 s) == set l v2 s

-- Prism Laws:
-- 1. review then preview: preview p (review p a) = Just a
-- 2. preview then review: if preview p s = Just a then review p a = ???

-- ISO Laws:
-- 1. from (to x) = x
-- 2. to (from x) = x
```

---

## ขั้นตอนที่ 249: Optics Hierarchy

```haskell
-- Hierarchy ของ Optics:
--
-- Iso
--  |
-- Lens    Prism
--   \    /
--  Traversal
--     |
--    Fold  Setter
--     |
--    Getter

-- การ compose:
-- Lens . Lens       = Lens
-- Lens . Traversal  = Traversal
-- Prism . Prism     = Prism
-- Lens . Prism      = Traversal (ไม่ใช่ Lens)
-- Traversal . Traversal = Traversal

-- ตัวอย่าง
data Config' = Config'
  { _featureFlags :: Map String Bool
  , _serverConfig :: ServerConfig
  } deriving (Show)

data ServerConfig = ServerConfig
  { _host :: String
  , _port :: Int
  } deriving (Show)

makeLenses ''Config'
makeLenses ''ServerConfig

-- Traversal: ไปทุก feature flag
allFlags :: Traversal' Config' Bool
allFlags = featureFlags . each

-- Set all flags
disableAll :: Config' -> Config'
disableAll = allFlags .~ False

-- Fold: collect enabled features
enabledFeatures :: Config' -> [String]
enabledFeatures cfg = 
  [ k 
  | (k, v) <- Map.toList (_featureFlags cfg)
  , v
  ]
```

---

## ขั้นตอนที่ 250: lens vs optics vs microlens

```haskell
-- lens: full-featured, large dependency
-- optics: more type-safe, newer
-- microlens: lightweight, for libraries

-- lens: ใช้ Van Laarhoven encoding
-- optics: ใช้ opaque profunctor encoding

-- ตัวอย่าง: microlens
import Lens.Micro

-- API เหมือนกัน แต่ lighter
data Config = Config
  { _timeout :: Int
  , _retries :: Int
  } deriving (Show)

timeoutL :: Lens' Config Int
timeoutL = lens _timeout (\cfg t -> cfg { _timeout = t })

retriesL :: Lens' Config Int
retriesL = lens _retries (\cfg r -> cfg { _retries = r })

-- ใช้ microlens
cfg :: Config
cfg = Config 30 3

newCfg :: Config
newCfg = cfg & timeoutL .~ 60 & retriesL %~ (+1)

-- optics: ใช้ ix syntax ที่ต่างกัน
import Optics

-- ใน optics ใช้ % แทน .
-- cfg & #timeout %~ (*2)  (ด้วย OverloadedLabels)
```

---

## ขั้นตอนที่ 251: Classy Lenses

```haskell
{-# LANGUAGE FunctionalDependencies #-}

-- Classy Lenses: สร้าง type class สำหรับ lens

-- makeClassy สร้าง type class ด้วย
makeClassy ''Person

-- ผลลัพธ์:
class HasPerson a where
  person :: Lens' a Person
  personName :: Lens' a String
  personAge  :: Lens' a Int

-- Instance สำหรับ Person itself
instance HasPerson Person where
  person = id

-- ตัวอย่าง: Employee ที่มี Person
data Employee = Employee
  { _person :: Person
  , _salary :: Int
  }

makeClassy ''Employee

-- Auto-derive HasPerson
instance HasPerson Employee where
  person = employeePerson

-- ใช้ HasPerson constraint
printName :: HasPerson s => s -> IO ()
printName s = putStrLn (s ^. personName)

-- Works for both Person and Employee
main :: IO ()
main = do
  printName (Person "Alice" 30)
  printName (Employee (Person "Bob" 25) 50000)
```

---

## ขั้นตอนที่ 252: Lenses ใน STM

```haskell
import Control.Lens
import Control.Concurrent.STM

-- TVar + Lens
data AppState = AppState
  { _userCount :: Int
  , _activeConnections :: [String]
  } deriving (Show)

makeLenses ''AppState

type AppStateVar = TVar AppState

-- อ่านค่าด้วย lens
readUserCount :: AppStateVar -> STM Int
readUserCount var = (^. userCount) <$> readTVar var

-- Update ด้วย lens
addConnection :: String -> AppStateVar -> STM ()
addConnection conn var =
  modifyTVar var (activeConnections %~ (conn:))

-- Atomic update multiple fields
updateState :: String -> AppStateVar -> STM ()
updateState conn var = modifyTVar var $
  (userCount %~ (+1)) . (activeConnections %~ (conn:))

-- ตัวอย่าง: State machine ด้วย lens
data RequestState
  = Idle
  | Processing { _requestId :: String }
  | Done { _result :: String }

makePrisms ''RequestState

processRequest :: TVar RequestState -> String -> IO ()
processRequest var reqId = atomically $ do
  state <- readTVar var
  case state ^? _Idle of
    Just _  -> writeTVar var (Processing reqId)
    Nothing -> return ()
```

---

## ขั้นตอนที่ 253: JSON Lenses ด้วย aeson-lens

```haskell
import Data.Aeson.Lens
import Data.Aeson
import qualified Data.Vector as V

-- จัดการ JSON ด้วย lens

jsonData :: Value
jsonData = object
  [ "users" .= array
    [ object ["name" .= "Alice", "age" .= (30 :: Int)]
    , object ["name" .= "Bob",   "age" .= (25 :: Int)]
    ]
  , "count" .= (2 :: Int)
  ]

-- key: access object property
count :: Maybe Int
count = jsonData ^? key "count" . _Integer

-- nth: access array element
firstUser :: Maybe Value
firstUser = jsonData ^? key "users" . nth 0

-- Access nested
firstName :: Maybe String
firstName = jsonData ^? key "users" . nth 0 . key "name" . _String

-- Traverse all users
allNames :: [String]
allNames = jsonData ^.. key "users" . values . key "name" . _String

-- Modify JSON
addUser :: Value -> Value
addUser = key "users" . _Array %~ (`V.snoc` newUser)
  where newUser = object ["name" .= "Charlie", "age" .= (35 :: Int)]
```

---

## ขั้นตอนที่ 254: Algebraic Optics

```haskell
-- Algebraic optics: เข้าใจ optics ในแง่ category theory

-- Profunctor: opaque representation
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

-- Optic: generalized optic
type Optic p s t a b = p a b -> p s t

-- Lens, Prism, Traversal ต่างกันที่ constraint บน p
type Lens      s t a b = forall p. Strong p       => Optic p s t a b
type Prism     s t a b = forall p. Choice p       => Optic p s t a b
type Traversal s t a b = forall p. Traversing p   => Optic p s t a b

-- การ compose: เป็นแค่ function composition!
-- lens1 . lens2 :: Optic p s t a b

-- ตัวอย่าง: Van Laarhoven encoding
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- สร้าง lens ง่ายๆ
_fst :: Lens' (a, b) a
_fst f (x, y) = (\x' -> (x', y)) <$> f x

-- Verification
-- view _fst (1, 2) = 1
-- set _fst 10 (1, 2) = (10, 2)
-- over _fst (*2) (1, 2) = (2, 2)
```

---

## ขั้นตอนที่ 255: Lens Best Practices

```haskell
-- Best practices สำหรับ lens

-- 1. ใช้ makeLenses เพื่อ generate lenses อัตโนมัติ
-- 2. prefix fields ด้วย _
-- 3. ใช้ makeClassy เมื่อต้องการ type class interface
-- 4. หลีกเลี่ยง partial lenses (use Traversal แทน)

-- Anti-pattern: lens ที่ไม่ satisfy laws
-- ตัวอย่าง: "lens" ที่ normalize ค่า
badLens :: Lens' String String
badLens = lens id (\s v -> map toLower v)  -- ไม่ satisfy law 1!

-- ถูกต้อง: แยก normalization ออกมา
normalized :: String -> String
normalized = map toLower

-- หรือใช้ iso
lowerIso :: Iso' String String
lowerIso = iso (map toLower) (map toLower)

-- 5. ใช้ strict lens เมื่อ update บ่อย
-- Control.Lens.Strict มี strict versions
import Control.Lens.Strict

-- (.~!) = strict set
-- (%~!) = strict modify

-- 6. ใช้ lens สำหรับ complex updates แต่อย่า overuse
-- simple updates ไม่ต้องใช้ lens

-- ก่อน
updateName :: Person -> String -> Person
updateName p n = p { _name = n }

-- หลัง (ไม่จำเป็นต้องเปลี่ยน)
-- อย่าใช้ lens เพียงเพื่อ "look modern"
```

---

## ขั้นตอนที่ 256: Optics สำหรับ Domain Modeling

```haskell
-- ใช้ optics ใน domain modeling

data Order = Order
  { _orderId    :: OrderId
  , _orderItems :: [OrderItem]
  , _orderStatus :: OrderStatus
  , _customer   :: Customer
  } deriving (Show)

data OrderItem = OrderItem
  { _itemProduct :: Product
  , _itemQty     :: Int
  , _itemPrice   :: Decimal
  } deriving (Show)

data OrderStatus = Pending | Processing | Shipped | Delivered | Cancelled
  deriving (Show, Eq)

makeLenses ''Order
makeLenses ''OrderItem
makePrisms ''OrderStatus

-- ฟังก์ชัน domain ที่ใช้ lens
totalAmount :: Order -> Decimal
totalAmount order = sumOf (orderItems . each . to itemTotal) order
  where itemTotal item = _itemPrice item * fromIntegral (_itemQty item)

-- ตรวจสอบ inventory
hasItem :: Product -> Order -> Bool
hasItem p order = any (\item -> _itemProduct item == p) (_orderItems order)

-- Update status
shipOrder :: Order -> Order
shipOrder = orderStatus .~ Shipped

-- Cancel order (only if not yet shipped)
cancelOrder :: Order -> Maybe Order
cancelOrder order = case _orderStatus order of
  Pending    -> Just (order & orderStatus .~ Cancelled)
  Processing -> Just (order & orderStatus .~ Cancelled)
  _          -> Nothing

-- ฟังก์ชัน higher-order ด้วย lens
modifyAllPrices :: (Decimal -> Decimal) -> Order -> Order
modifyAllPrices f = orderItems . each . itemPrice %~ f

applyDiscount :: Decimal -> Order -> Order
applyDiscount discount = modifyAllPrices (* (1 - discount))
```

---

## ขั้นตอนที่ 257: Effectful Lens Operations

```haskell
-- Lens ใน IO/Monad context

-- traverseOf: traverse with effect
loadOrder :: Order -> IO Order
loadOrder order = traverseOf (orderItems . each) enrichItem order
  where
    enrichItem :: OrderItem -> IO OrderItem
    enrichItem item = do
      price <- fetchCurrentPrice (_itemProduct item)
      return (item & itemPrice .~ price)

fetchCurrentPrice :: Product -> IO Decimal
fetchCurrentPrice p = return 9.99  -- mock

-- modifyOf: strict modify
updateInventory :: TVar Inventory -> Order -> IO ()
updateInventory inventoryVar order = do
  let items = order ^. orderItems
  atomically $ forM_ items $ \item -> do
    let product = item ^. itemProduct
    modifyTVar inventoryVar (at product . _Just %~ subtract (item ^. itemQty))

-- ioOf: read with IO
printOrderDetails :: Order -> IO ()
printOrderDetails order = do
  forOf_ (orderItems . each) order $ \item -> do
    let product = item ^. itemProduct
    let qty     = item ^. itemQty
    let price   = item ^. itemPrice
    putStrLn $ "  " ++ show product ++ " x" ++ show qty ++ " @ " ++ show price
```

---

## ขั้นตอนที่ 258: Coercible Lenses

```haskell
-- Coercible lens: zero-cost lens ระหว่าง newtypes

import Data.Coerce
import Control.Lens

newtype ProductId = ProductId Int deriving (Show, Eq, Ord)
newtype UserId    = UserId    Int deriving (Show, Eq, Ord)

-- coerced: lens ที่ใช้ Coercible
productIdLens :: Iso' ProductId Int
productIdLens = coerced

userIdLens :: Iso' UserId Int
userIdLens = coerced

-- ตัวอย่าง
pid :: ProductId
pid = ProductId 42

pidInt :: Int
pidInt = pid ^. coerced  -- 42

-- wrapping/unwrapping ด้วย coerced
wrapList :: [Int] -> [ProductId]
wrapList = coerce  -- zero-cost!

-- ตัวอย่าง: newtype wrapping สำหรับ Map keys
type ProductMap v = Map ProductId v

-- ใช้ coerce เพื่อ convert ระหว่าง ProductMap และ Map Int
fromIntMap :: Map Int v -> ProductMap v
fromIntMap = coerce

toIntMap :: ProductMap v -> Map Int v
toIntMap = coerce
```

---

## ขั้นตอนที่ 259: State Operations ด้วย Lens

```haskell
-- ใช้ lens ใน State monad

import Control.Lens
import Control.Monad.State

data GameState = GameState
  { _player :: Player
  , _enemies :: [Enemy]
  , _score   :: Int
  , _level   :: Int
  } deriving (Show)

data Player = Player
  { _health :: Int
  , _position :: (Int, Int)
  , _inventory :: [Item]
  } deriving (Show)

data Enemy = Enemy
  { _enemyHealth :: Int
  , _enemyPos    :: (Int, Int)
  } deriving (Show)

data Item = Sword | Shield | Potion deriving (Show, Eq)

makeLenses ''GameState
makeLenses ''Player
makeLenses ''Enemy

type Game a = State GameState a

-- Actions
movePlayer :: (Int, Int) -> Game ()
movePlayer pos = player . position .= pos

heal :: Int -> Game ()
heal amount = player . health %= min 100 . (+amount)

addScore :: Int -> Game ()
addScore n = score += n

pickupItem :: Item -> Game ()
pickupItem item = player . inventory %= (item:)

-- Complex action
attackEnemy :: Int -> Int -> Game Bool
attackEnemy idx damage = do
  es <- use enemies
  case es ^? ix idx of
    Nothing -> return False
    Just e  -> do
      let newHealth = _enemyHealth e - damage
      if newHealth <= 0
        then do
          enemies %= filter (\e' -> _enemyPos e' /= _enemyPos e)
          addScore 100
          return True
        else do
          enemies . ix idx . enemyHealth .= newHealth
          return False

-- Run game
runGame :: IO ()
runGame = do
  let initial = GameState
        (Player 100 (0,0) [])
        [Enemy 50 (5,5), Enemy 30 (10,10)]
        0 1
  let (_, finalState) = runState gameplay initial
  putStrLn $ "Final score: " ++ show (_score finalState)
  where
    gameplay :: Game ()
    gameplay = do
      movePlayer (5, 5)
      attackEnemy 0 60
      heal 20
      addScore 50
```

---

## ขั้นตอนที่ 260: โปรเจกต์: Config Manager ด้วย Lens

```haskell
module ConfigManager where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Lens

-- Configuration structure
data AppConfig = AppConfig
  { _database  :: DatabaseConfig
  , _server    :: ServerConfig
  , _features  :: Map String Bool
  , _logging   :: LoggingConfig
  } deriving (Show)

data DatabaseConfig = DatabaseConfig
  { _dbHost     :: String
  , _dbPort     :: Int
  , _dbName     :: String
  , _dbPoolSize :: Int
  } deriving (Show)

data ServerConfig = ServerConfig
  { _serverHost    :: String
  , _serverPort    :: Int
  , _maxWorkers    :: Int
  , _requestTimeout :: Int
  } deriving (Show)

data LoggingConfig = LoggingConfig
  { _logLevel   :: String
  , _logFile    :: Maybe FilePath
  , _structured :: Bool
  } deriving (Show)

makeLenses ''AppConfig
makeLenses ''DatabaseConfig
makeLenses ''ServerConfig
makeLenses ''LoggingConfig

-- Default config
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { _database  = DatabaseConfig "localhost" 5432 "myapp" 10
  , _server    = ServerConfig "0.0.0.0" 8080 4 30
  , _features  = Map.fromList [("newUI", False), ("betaAPI", False)]
  , _logging   = LoggingConfig "INFO" Nothing False
  }

-- Config modifications
enableFeature :: String -> AppConfig -> AppConfig
enableFeature f = features . ix f .~ True

setDbPool :: Int -> AppConfig -> AppConfig
setDbPool n = database . dbPoolSize .~ n

setLogging :: String -> Maybe FilePath -> AppConfig -> AppConfig
setLogging level file cfg = cfg
  & logging . logLevel .~ level
  & logging . logFile  .~ file
  & logging . structured .~ True

-- Load from environment
loadFromEnv :: Map String String -> AppConfig -> AppConfig
loadFromEnv env cfg = foldr applyEnv cfg (Map.toList env)
  where
    applyEnv ("DB_HOST", v) = database . dbHost .~ v
    applyEnv ("DB_PORT", v) = database . dbPort .~ read v
    applyEnv ("DB_NAME", v) = database . dbName .~ v
    applyEnv ("SERVER_PORT", v) = server . serverPort .~ read v
    applyEnv _ = id

-- Validate config
validateConfig :: AppConfig -> Either String AppConfig
validateConfig cfg
  | cfg ^. database . dbPort < 1024  = Left "DB port too low"
  | cfg ^. server . serverPort < 1024 = Left "Server port too low"
  | cfg ^. database . dbPoolSize < 1 = Left "Pool size must be >= 1"
  | otherwise = Right cfg

-- Print config summary
printConfig :: AppConfig -> IO ()
printConfig cfg = do
  putStrLn "=== Application Configuration ==="
  putStrLn $ "Database: " ++ (cfg ^. database . dbHost) ++ ":" ++ show (cfg ^. database . dbPort)
  putStrLn $ "DB Name: " ++ (cfg ^. database . dbName)
  putStrLn $ "Pool: " ++ show (cfg ^. database . dbPoolSize)
  putStrLn $ "Server: " ++ (cfg ^. server . serverHost) ++ ":" ++ show (cfg ^. server . serverPort)
  putStrLn $ "Log Level: " ++ (cfg ^. logging . logLevel)
  putStrLn "Features:"
  forOf_ (features . itraversed) cfg $ \(name, enabled) ->
    putStrLn $ "  " ++ name ++ ": " ++ (if enabled then "ON" else "OFF")

main :: IO ()
main = do
  let cfg = defaultConfig
        & enableFeature "newUI"
        & setDbPool 20
        & setLogging "DEBUG" (Just "/var/log/app.log")
  
  case validateConfig cfg of
    Left err  -> putStrLn $ "Config error: " ++ err
    Right cfg -> printConfig cfg
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 14 เราจะเรียนเรื่อง **Concurrency และ Parallelism**:
- Async และ Concurrent
- STM transactions
- Parallel algorithms
- Actor model

---

*[← Part 12](part-12.md) | [Part 14 →](part-14.md)*
