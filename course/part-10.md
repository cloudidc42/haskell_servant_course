# Part 10: Monads เชิงลึก
## ขั้นตอนที่ 181-200: State, Reader, Writer Monads

---

## บทนำ

Monads คือหนึ่งในแนวคิดที่สำคัญที่สุดใน Haskell โดยช่วยให้สามารถเรียงลำดับ computations ที่มี context ได้อย่างเป็นระเบียบ

---

## ขั้นตอนที่ 181: Monad Type Class

```haskell
-- Monad type class definition
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b  -- bind
  return :: a -> m a                   -- pure (same as Applicative pure)

-- Monad Laws:
-- 1. Left identity:  return a >>= f  ≡  f a
-- 2. Right identity: m >>= return    ≡  m
-- 3. Associativity:  (m >>= f) >>= g ≡  m >>= (\x -> f x >>= g)

-- Maybe Monad
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x
  return  = Just

-- Either Monad
instance Monad (Either e) where
  Left  e >>= _ = Left e
  Right x >>= f = f x
  return  = Right

-- List Monad (nondeterminism)
instance Monad [] where
  xs >>= f = concatMap f xs
  return x = [x]

-- ตัวอย่าง: List Monad เป็น nondeterminism
pairs :: [(Int, Int)]
pairs = do
  x <- [1, 2, 3]
  y <- [1, 2, 3]
  return (x, y)
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = do
  c <- [1..n]
  b <- [1..c]
  a <- [1..b]
  if a*a + b*b == c*c
    then return (a, b, c)
    else []
```

---

## ขั้นตอนที่ 182: State Monad

```haskell
-- State Monad: computation ที่มี state
-- State s a = s -> (a, s)

import Control.Monad.State

-- Type alias
type Counter = State Int

-- เพิ่ม counter
increment :: Counter ()
increment = modify (+1)

-- อ่าน counter
getCount :: Counter Int
getCount = get

-- Reset counter
resetCount :: Counter ()
resetCount = put 0

-- ตัวอย่าง: นับ nodes ใน tree
data Tree a = Leaf | Node a (Tree a) (Tree a)

numberNodes :: Tree a -> Tree (Int, a)
numberNodes tree = evalState (label tree) 0
  where
    label Leaf = return Leaf
    label (Node x l r) = do
      n <- get
      put (n + 1)
      l' <- label l
      r' <- label r
      return (Node (n, x) l' r')

-- ตัวอย่าง: Stack implementation
type Stack a = State [a]

push :: a -> Stack a ()
push x = modify (x:)

pop :: Stack a a
pop = do
  xs <- get
  case xs of
    []     -> error "Empty stack"
    (x:xs) -> do
      put xs
      return x

peek :: Stack a a
peek = do
  xs <- get
  case xs of
    []    -> error "Empty stack"
    (x:_) -> return x

-- Run stack operations
stackExample :: [Int]
stackExample = execState operations []
  where
    operations = do
      push 1
      push 2
      push 3
      pop  -- removes 3
      push 10

-- runState, evalState, execState
runExample :: (Int, [Int])
runExample = runState (do { push 5; pop }) []

evalExample :: Int
evalExample = evalState (do { push 5; pop }) []

execExample :: [Int]
execExample = execState (do { push 5; push 10 }) []
```

---

## ขั้นตอนที่ 183: Reader Monad

```haskell
-- Reader Monad: computation ที่มี read-only environment
-- Reader r a = r -> a

import Control.Monad.Reader

-- ตัวอย่าง: Database configuration
data Config = Config
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  } deriving (Show)

type App a = Reader Config a

-- ฟังก์ชันที่ต้องการ Config
getConnectionString :: App String
getConnectionString = do
  cfg <- ask  -- ask :: Reader r r
  return $ dbHost cfg ++ ":" ++ show (dbPort cfg) ++ "/" ++ dbName cfg

-- หรือใช้ asks
getHost :: App String
getHost = asks dbHost

getPort :: App Int
getPort = asks dbPort

-- Nested readers
connectDb :: App String
connectDb = do
  connStr <- getConnectionString
  return $ "Connected to: " ++ connStr

-- Run with environment
result :: String
result = runReader connectDb (Config "localhost" 5432 "mydb")

-- local: run sub-computation with modified environment
withTestConfig :: App a -> App a
withTestConfig = local (\cfg -> cfg { dbHost = "testhost", dbName = "testdb" })

-- ตัวอย่าง: Application configuration
data AppConfig = AppConfig
  { logLevel :: LogLevel
  , maxRetries :: Int
  , timeout :: Int
  }

data LogLevel = Debug | Info | Warn | Error

type AppM = ReaderT AppConfig IO

logInfo :: String -> AppM ()
logInfo msg = do
  level <- asks logLevel
  when (level <= Info) $ liftIO $ putStrLn $ "[INFO] " ++ msg

fetchWithRetry :: String -> AppM String
fetchWithRetry url = do
  retries <- asks maxRetries
  attempt url retries
  where
    attempt _   0 = return "Failed after all retries"
    attempt url n = do
      logInfo $ "Fetching " ++ url ++ " (attempt " ++ show n ++ ")"
      -- mock fetch
      return $ "Content from " ++ url
```

---

## ขั้นตอนที่ 184: Writer Monad

```haskell
-- Writer Monad: computation ที่มี write-only log
-- Writer w a = (a, w)

import Control.Monad.Writer

-- w ต้องเป็น Monoid
type Logger a = Writer [String] a

-- บันทึก log
logStep :: String -> Logger ()
logStep msg = tell [msg]  -- tell :: w -> Writer w ()

-- ตัวอย่าง: Traced computation
factorial :: Int -> Logger Int
factorial 0 = do
  logStep "Base case: 0! = 1"
  return 1
factorial n = do
  logStep $ "Computing " ++ show n ++ "!"
  prev <- factorial (n-1)
  let result = n * prev
  logStep $ show n ++ "! = " ++ show result
  return result

-- Run
runFactorial :: Int -> (Int, [String])
runFactorial n = runWriter (factorial n)

-- ตัวอย่าง
example :: IO ()
example = do
  let (result, logs) = runFactorial 5
  putStrLn $ "Result: " ++ show result
  putStrLn "Log:"
  mapM_ putStrLn logs

-- WriterT transformer
type AppLogger a = WriterT [String] IO a

loggedAction :: AppLogger Int
loggedAction = do
  tell ["Starting action"]
  liftIO $ putStrLn "Running..."
  tell ["Action complete"]
  return 42

runLoggedAction :: IO (Int, [String])
runLoggedAction = runWriterT loggedAction
```

---

## ขั้นตอนที่ 185: Monad Transformers เบื้องต้น

```haskell
-- Monad Transformers: stack multiple monads
-- Pattern: StateT s (ReaderT r (Writer w)) a

import Control.Monad.Trans

-- StateT s m a = s -> m (a, s)
type AppState = StateT AppStateData

-- ReaderT r m a = r -> m a
type AppReader = ReaderT Config

-- Stack example: StateT on top of IO
type App a = StateT AppData IO a

data AppData = AppData
  { counter :: Int
  , history :: [String]
  }

initialState :: AppData
initialState = AppData 0 []

increment :: App ()
increment = modify (\s -> s { counter = counter s + 1 })

record :: String -> App ()
record msg = modify (\s -> s { history = msg : history s })

getCurrentCount :: App Int
getCurrentCount = gets counter

-- liftIO: lift IO actions into transformer stack
printCount :: App ()
printCount = do
  n <- getCurrentCount
  liftIO $ putStrLn $ "Count: " ++ show n

runApp :: App a -> IO (a, AppData)
runApp app = runStateT app initialState

-- Stack: StateT + ReaderT + IO
type FullApp a = StateT AppData (ReaderT Config IO) a

runFullApp :: FullApp a -> Config -> IO (a, AppData)
runFullApp app config = runReaderT (runStateT app initialState) config
```

---

## ขั้นตอนที่ 186: mtl Style vs Transformers

```haskell
-- mtl style: ใช้ type classes เพื่อ abstract over monad stack

-- MonadState type class
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

-- MonadReader type class
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a

-- MonadWriter type class
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell :: w -> m ()
  listen :: m a -> m (a, w)

-- ตัวอย่าง: MonadState style
increment' :: MonadState Int m => m ()
increment' = modify (+1)

getCount' :: MonadState Int m => m Int
getCount' = get

-- สามารถใช้ได้กับ State, StateT, และ custom monads
-- ที่ implement MonadState

-- ตัวอย่าง: ฟังก์ชันที่ไม่ขึ้นกับ transformer stack
runWithConfig :: (MonadReader Config m, MonadState Int m, MonadIO m)
              => m ()
runWithConfig = do
  cfg <- ask
  n <- get
  liftIO $ putStrLn $ "Config: " ++ show cfg ++ ", State: " ++ show n

-- ข้อดี: flexible, เปลี่ยน stack ได้โดยไม่ต้องแก้ logic
```

---

## ขั้นตอนที่ 187: RWS Monad (Reader + Writer + State)

```haskell
import Control.Monad.RWS

-- RWS r w s a = r -> s -> (a, s, w)
-- เหมือนมี Reader, Writer, State ในตัวเดียว

type MyRWS a = RWS Config [String] AppData a

-- ใช้ ask, tell, get, put ได้ทั้งหมด
myComputation :: MyRWS Int
myComputation = do
  cfg <- ask
  tell ["Starting computation"]
  n <- gets counter
  modify (\s -> s { counter = n + 1 })
  tell ["Incremented counter to " ++ show (n+1)]
  return (n + 1)

-- Run RWS
runExample :: (Int, AppData, [String])
runExample = runRWS myComputation config initialState
  where
    config = Config "localhost" 5432 "mydb"
    initialState = AppData 0 []

-- evalRWS: เอา (result, log)
-- execRWS: เอา (state, log)
```

---

## ขั้นตอนที่ 188: Cont Monad

```haskell
-- Cont Monad: continuation-passing style
-- Cont r a = (a -> r) -> r

import Control.Monad.Cont

-- callCC: call with current continuation
pythagorean :: ContT r IO ()
pythagorean = callCC $ \exit -> do
  liftIO $ putStr "a: "
  a <- liftIO $ fmap read getLine
  liftIO $ putStr "b: "
  b <- liftIO $ fmap read getLine
  let c = sqrt (a*a + b*b) :: Double
  when (isNaN c) $ exit ()  -- early exit
  liftIO $ putStrLn $ "c = " ++ show c

-- ตัวอย่าง: Early exit from loop
findFirst :: [Int] -> ContT r IO (Maybe Int)
findFirst xs = callCC $ \found -> do
  forM_ xs $ \x -> do
    when (x > 10) $ found (Just x)
  return Nothing

-- CPS transform
factorial' :: Int -> Int
factorial' n = runCont (factCont n) id
  where
    factCont :: Int -> Cont Int Int
    factCont 0 = return 1
    factCont n = do
      prev <- factCont (n-1)
      return (n * prev)
```

---

## ขั้นตอนที่ 189: Free Monad

```haskell
-- Free Monad: DSL construction
-- สร้าง mini-language ที่ interpret ได้หลายวิธี

data ConsoleF next
  = PutStr String next
  | GetLine (String -> next)
  deriving (Functor)

type Console = Free ConsoleF

-- Smart constructors
putStr' :: String -> Console ()
putStr' s = liftF (PutStr s ())

getLine' :: Console String
getLine' = liftF (GetLine id)

-- DSL program
program :: Console ()
program = do
  putStr' "Enter name: "
  name <- getLine'
  putStr' $ "Hello, " ++ name

-- Interpreter 1: IO
interpretIO :: Console a -> IO a
interpretIO (Pure a) = return a
interpretIO (Free (PutStr s next)) = do
  putStr s
  interpretIO next
interpretIO (Free (GetLine f)) = do
  line <- getLine
  interpretIO (f line)

-- Interpreter 2: Pure (for testing)
interpretPure :: [String] -> Console a -> (a, [String])
interpretPure _      (Pure a) = (a, [])
interpretPure inputs (Free (PutStr s next)) =
  let (result, output) = interpretPure inputs next
  in (result, s : output)
interpretPure []     (Free (GetLine _)) = error "No input"
interpretPure (i:is) (Free (GetLine f)) = interpretPure is (f i)
```

---

## ขั้นตอนที่ 190: Monad vs Applicative

```haskell
-- Applicative: independent effects
-- Monad: sequential, dependent effects

-- Applicative: ทั้ง 2 actions ทำงานอิสระ
validateBoth :: Either String Int -> Either String Int -> Either String (Int, Int)
validateBoth x y = (,) <$> x <*> y

-- ถ้า x fail, y ยังถูก evaluate (แต่ result ถูก discard)

-- Monad: actions ขึ้นกับผล action ก่อนหน้า
validateSequential :: Int -> Int -> Either String (Int, Int)
validateSequential x y = do
  x' <- if x > 0 then Right x else Left "x must be positive"
  y' <- if y > x' then Right y else Left "y must be greater than x"
  return (x', y')

-- ใช้ Applicative เมื่อ effects เป็นอิสระ (compile-time parallelism)
-- ใช้ Monad เมื่อต้องการ dynamic control flow

-- Validation (Applicative, not Monad)
import Data.Validation

validateAge :: Int -> Validation [String] Int
validateAge age
  | age < 0   = Failure ["Age cannot be negative"]
  | age > 150 = Failure ["Age is unrealistically high"]
  | otherwise = Success age

validateName :: String -> Validation [String] String
validateName name
  | null name = Failure ["Name cannot be empty"]
  | length name > 100 = Failure ["Name is too long"]
  | otherwise = Success name

-- Applicative collects all errors
validateUser :: Int -> String -> Validation [String] (Int, String)
validateUser age name = (,) <$> validateAge age <*> validateName name
```

---

## ขั้นตอนที่ 191: IO Monad internals

```haskell
-- IO ใน Haskell ทำงานอย่างไร?

-- IO a เป็น description ของ action ที่จะทำ
-- ไม่ใช่ action จริงๆ
-- main :: IO () คือ description ที่ GHC runtime execute

-- Primitive IO operations (unsafe)
import System.IO.Unsafe (unsafePerformIO)
-- อย่าใช้ใน production code!

-- unsafePerformIO: run IO action ใน pure context
{-# NOINLINE globalRef #-}
globalRef :: IORef Int
globalRef = unsafePerformIO (newIORef 0)

-- Safe alternative: Global mutable state ด้วย IORef + unsafePerformIO
-- สำหรับ global configuration หรือ logging เท่านั้น

-- ตัวอย่างที่ถูกต้อง: global counter (อย่าทำแบบนี้ใน real code)
{-# NOINLINE globalCounter #-}
globalCounter :: IORef Int
globalCounter = unsafePerformIO (newIORef 0)

-- IO และ Laziness
-- IO actions ใน Haskell เป็น lazy: ไม่ทำงานจนกว่าจะถูก execute
thunk :: IO ()
thunk = putStrLn "This runs lazily"

-- IO ถูก execute เฉพาะเมื่อ:
-- 1. อยู่ใน main
-- 2. ถูก run ด้วย runIO หรือ similar
-- 3. ถูก sequence ใน do block ที่ execute
```

---

## ขั้นตอนที่ 192: Exception Handling ใน Monad

```haskell
import Control.Exception
import Control.Monad.Catch

-- try: catch exceptions and return Either
safeDiv :: Int -> Int -> IO (Either SomeException Int)
safeDiv x 0 = return $ Left (SomeException (ErrorCall "Division by zero"))
safeDiv x y = return $ Right (x `div` y)

safeDiv' :: Int -> Int -> IO (Either SomeException Int)
safeDiv' x y = try (evaluate (x `div` y))

-- catch: handle exception
safeDivCatch :: Int -> Int -> IO Int
safeDivCatch x y = catch (evaluate (x `div` y)) handler
  where
    handler :: SomeException -> IO Int
    handler _ = do
      putStrLn "Error: caught exception"
      return 0

-- bracket: ensure cleanup
withResource :: IO resource -> (resource -> IO ()) -> (resource -> IO a) -> IO a
withResource acquire release use = bracket acquire release use

-- throwIO: throw exception in IO
data AppException = NotFound String | PermissionDenied String
  deriving (Show, Typeable)

instance Exception AppException

findUser :: Int -> IO String
findUser 42 = return "Alice"
findUser n  = throwIO (NotFound $ "User " ++ show n ++ " not found")

-- catchJust: catch specific exceptions
safeFindUser :: Int -> IO (Maybe String)
safeFindUser n = catchJust isNotFound (fmap Just (findUser n)) (\_ -> return Nothing)
  where
    isNotFound (NotFound _) = Just ()
    isNotFound _            = Nothing
```

---

## ขั้นตอนที่ 193: Monad Composition

```haskell
-- Kleisli composition: >=>
import Control.Monad ((>=>))

-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

lookupUser :: Int -> Maybe String
lookupUser 1 = Just "Alice"
lookupUser 2 = Just "Bob"
lookupUser _ = Nothing

lookupEmail :: String -> Maybe String
lookupEmail "Alice" = Just "alice@example.com"
lookupEmail "Bob"   = Just "bob@example.com"
lookupEmail _       = Nothing

-- Compose Maybe functions
lookupUserEmail :: Int -> Maybe String
lookupUserEmail = lookupUser >=> lookupEmail

-- ตัวอย่าง: Parser combinators
type Parser a = String -> Maybe (a, String)

charP :: Char -> Parser Char
charP c (x:xs) | c == x = Just (c, xs)
charP _ _               = Nothing

-- Compose parsers
charAB :: Parser (Char, Char)
charAB s = do
  (a, rest1) <- charP 'a' s
  (b, rest2) <- charP 'b' rest1
  return ((a, b), rest2)
```

---

## ขั้นตอนที่ 194: Identity Monad

```haskell
-- Identity Monad: ใช้เพื่อ complete transformer stacks
import Data.Functor.Identity

-- Identity a = a
newtype Identity a = Identity { runIdentity :: a }

-- State a = StateT a Identity
-- Reader a = ReaderT a Identity
-- Writer a = WriterT a Identity

-- ตัวอย่าง
type SimpleState s a = StateT s Identity a

runSimpleState :: SimpleState s a -> s -> (a, s)
runSimpleState m s = runIdentity (runStateT m s)

-- ใช้ Identity เพื่อ test transformer logic ใน pure context
testStateLogic :: SimpleState Int Int
testStateLogic = do
  n <- get
  put (n + 1)
  return (n * 2)

result :: (Int, Int)
result = runSimpleState testStateLogic 5
-- (10, 6)
```

---

## ขั้นตอนที่ 195: Indexed Monads

```haskell
-- Indexed Monad: monad ที่ track state ที่ type level

-- IxState s t a = s -> (a, t)
-- s: input state type
-- t: output state type
-- a: result type

class IxMonad m where
  ireturn :: a -> m i i a
  ibind   :: m i j a -> (a -> m j k b) -> m i k b

newtype IxState s t a = IxState { runIxState :: s -> (a, t) }

instance IxMonad IxState where
  ireturn x = IxState (\s -> (x, s))
  ibind m f = IxState (\s ->
    let (x, s') = runIxState m s
    in runIxState (f x) s')

-- ตัวอย่าง: Type-safe state machine
data Locked
data Unlocked

type Door state = IxState state Locked ()

lock :: Door Unlocked
lock = IxState (\_ -> ((), error "locked"))  -- simplified

-- ตัวอย่างที่ใช้ได้จริง: Socket state machine
data Closed
data Open
data Listening

-- แต่ละ operation เปลี่ยน type state
-- Haskell ตรวจสอบ ordering ที่ compile-time
```

---

## ขั้นตอนที่ 196: Monad Laws และ Verification

```haskell
-- Verify Monad Laws สำหรับ custom types

-- สร้าง simple Monad
newtype Box a = Box { unBox :: a } deriving (Show, Eq, Functor)

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

instance Monad Box where
  Box x >>= f = f x

-- Test Monad Laws
prop_leftIdentity :: Eq (m b) => Monad m => (a -> m b) -> a -> Bool
prop_leftIdentity f x = (return x >>= f) == f x

prop_rightIdentity :: Eq (m a) => Monad m => m a -> Bool
prop_rightIdentity m = (m >>= return) == m

prop_associativity :: Eq (m c) => Monad m => m a -> (a -> m b) -> (b -> m c) -> Bool
prop_associativity m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

-- QuickCheck properties
import Test.QuickCheck

propBoxLeftId :: Int -> Fun Int (Box Int) -> Property
propBoxLeftId x (Fn f) = (return x >>= f) === f x

propBoxRightId :: Box Int -> Property
propBoxRightId m = (m >>= return) === m
```

---

## ขั้นตอนที่ 197: Practical Monad Patterns

```haskell
-- Pattern: Resource monad
class MonadResource m where
  allocate :: IO resource -> (resource -> IO ()) -> m (ReleaseKey, resource)
  release  :: ReleaseKey -> m ()

-- Pattern: MonadFail
class Monad m => MonadFail m where
  fail :: String -> m a

-- Pattern: MonadZero
class Monad m => MonadZero m where
  zero :: m a

-- Pattern: MonadPlus (MonadZero + MonadOr)
class (MonadZero m) => MonadPlus m where
  mplus :: m a -> m a -> m a

-- Maybe MonadPlus
instance MonadPlus Maybe where
  mplus (Just x) _  = Just x
  mplus Nothing  my = my

-- List MonadPlus
instance MonadPlus [] where
  mplus = (++)

-- Alternative (cleaner version)
class Applicative f => Alternative f where
  empty :: f a         -- identity for <|>
  (<|>) :: f a -> f a -> f a

-- ตัวอย่าง: Parser with backtracking
type Parser a = StateT String Maybe a

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  cs <- get
  case cs of
    (c:rest) | p c -> do put rest; return c
    _              -> lift Nothing

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

-- Alternative allows backtracking
alphaNum :: Parser Char
alphaNum = digit <|> letter

-- many: zero or more
many' :: Alternative f => f a -> f [a]
many' v = some' v <|> pure []

some' :: Alternative f => f a -> f [a]
some' v = (:) <$> v <*> many' v
```

---

## ขั้นตอนที่ 198: Bidirectional Monad

```haskell
-- Prism: bidirectional transformation

-- ตัวอย่าง: Serialize/Deserialize
class Monad m => SerializationMonad m where
  serialize   :: (Show a) => a -> m ()
  deserialize :: (Read a) => m a

-- ตัวอย่าง: State and inverse
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Invertible computation
data Rev f a b = Rev (f a b) (f b a)

runRev :: Rev f a b -> f a b
runRev (Rev f _) = f

runInv :: Rev f a b -> f b a
runInv (Rev _ g) = g
```

---

## ขั้นตอนที่ 199: Monad Debugging

```haskell
-- Debug monad: ติดตาม execution ใน monad

import Debug.Trace

-- trace: print debug message
debugState :: State Int Int
debugState = do
  n <- get
  let _ = trace ("Current state: " ++ show n) ()
  modify (+1)
  m <- get
  let _ = trace ("New state: " ++ show m) ()
  return (n + m)

-- traceShow: แสดงค่าด้วย show
debugValue :: Maybe Int
debugValue = do
  x <- Just 42
  let _ = traceShow ("x = ", x) ()
  y <- Just (x * 2)
  return y

-- เพิ่ม logging ใน monad stack
import Control.Monad.Writer

type Logged a = Writer [String] a

loggedFactorial :: Int -> Logged Int
loggedFactorial 0 = do
  tell ["0! = 1"]
  return 1
loggedFactorial n = do
  prev <- loggedFactorial (n-1)
  let result = n * prev
  tell [show n ++ "! = " ++ show result]
  return result
```

---

## ขั้นตอนที่ 200: โปรเจกต์: Task Manager

```haskell
-- Task Manager ใช้ State + Reader + Writer + IO

module TaskManager where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time

data Priority = Low | Medium | High deriving (Show, Eq, Ord)

data Task = Task
  { taskId    :: Int
  , taskTitle :: String
  , taskPri   :: Priority
  , taskDone  :: Bool
  , taskCreated :: UTCTime
  } deriving (Show)

data AppConfig = AppConfig
  { maxTasks :: Int
  , defaultPriority :: Priority
  }

data AppState = AppState
  { tasks   :: Map Int Task
  , nextId  :: Int
  }

type App a = ReaderT AppConfig (StateT AppState (WriterT [String] IO)) a

runApp :: AppConfig -> App a -> IO ((a, AppState), [String])
runApp cfg app =
  runWriterT
    (runStateT
      (runReaderT app cfg)
      (AppState Map.empty 1))

addTask :: String -> Priority -> App Int
addTask title priority = do
  cfg   <- ask
  state <- get
  let taskCount = Map.size (tasks state)
  maxT  <- asks maxTasks
  if taskCount >= maxT
    then do
      tell ["Warning: max tasks reached"]
      return (-1)
    else do
      time <- liftIO getCurrentTime
      let tid  = nextId state
          task = Task tid title priority False time
      modify (\s -> s
        { tasks  = Map.insert tid task (tasks s)
        , nextId = tid + 1 })
      tell ["Task added: " ++ title ++ " (id=" ++ show tid ++ ")"]
      return tid

completeTask :: Int -> App Bool
completeTask tid = do
  state <- get
  case Map.lookup tid (tasks state) of
    Nothing -> do
      tell ["Task not found: " ++ show tid]
      return False
    Just task -> do
      let updated = task { taskDone = True }
      modify (\s -> s { tasks = Map.insert tid updated (tasks s) })
      tell ["Task completed: " ++ taskTitle task]
      return True

listTasks :: Priority -> App [Task]
listTasks minPriority = do
  state <- get
  return $
    filter (\t -> taskPri t >= minPriority && not (taskDone t)) $
    Map.elems (tasks state)

-- Main
main :: IO ()
main = do
  let config = AppConfig 100 Medium
  ((_, finalState), logs) <- runApp config $ do
    id1 <- addTask "Write report" High
    id2 <- addTask "Review code"  Medium
    id3 <- addTask "Fix bug #42"  High
    completeTask id1
    highTasks <- listTasks High
    liftIO $ putStrLn $ "High priority tasks: " ++ show (length highTasks)
  putStrLn "\n=== Application Log ==="
  mapM_ putStrLn logs
  putStrLn $ "\nTotal tasks: " ++ show (Map.size (tasks finalState))
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 11 เราจะเรียนเรื่อง **Laziness และ Strictness**:
- Lazy evaluation
- Thunks และ WHNF
- seq, deepseq, bang patterns
- Performance implications

---

*[← Part 09](part-09.md) | [Part 11 →](part-11.md)*
