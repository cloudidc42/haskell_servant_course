# Part 08: Maybe, Either และ Error Handling
## ขั้นตอนที่ 141-160: การจัดการ Errors อย่างถูกต้อง

---

## บทนำ

Error handling ใน Haskell ต่างจากภาษาอื่นอย่างมาก แทนที่จะใช้ exceptions แบบ Java/Python เราใช้ types เพื่อ represent ความเป็นไปได้ที่จะ fail นี่ทำให้ code มีความชัดเจนและ safe กว่า

---

## ขั้นตอนที่ 141: Maybe Type

```haskell
-- Maybe: ค่าที่อาจมีหรือไม่มีก็ได้
data Maybe a = Nothing | Just a
  deriving (Show, Eq, Ord)

-- ใช้แทน null/undefined ใน Haskell

-- ตัวอย่าง: safe lookup
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _     = Nothing
safeIndex (x:_) 0  = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeSqrt :: Double -> Maybe Double
safeSqrt x
  | x < 0    = Nothing
  | otherwise = Just (sqrt x)

-- ตัวอย่าง
ghci> safeHead [1,2,3]
Just 1

ghci> safeHead []
Nothing

ghci> safeDiv 10 2
Just 5

ghci> safeDiv 10 0
Nothing
```

---

## ขั้นตอนที่ 142: Maybe Operations

```haskell
-- maybe: destructure Maybe value
-- maybe :: b -> (a -> b) -> Maybe a -> b

ghci> maybe 0 (*2) (Just 5)
10

ghci> maybe 0 (*2) Nothing
0

-- fromMaybe: default value สำหรับ Nothing
import Data.Maybe (fromMaybe)

ghci> fromMaybe 0 (Just 5)
5

ghci> fromMaybe 0 Nothing
0

-- isJust, isNothing
import Data.Maybe (isJust, isNothing)

ghci> isJust (Just 5)
True

ghci> isNothing Nothing
True

-- fromJust: unsafe! throw exception on Nothing
import Data.Maybe (fromJust)

ghci> fromJust (Just 5)
5

ghci> fromJust Nothing   -- *** Exception: Maybe.fromJust: Nothing!

-- catMaybes: กรอง Nothing ออก
import Data.Maybe (catMaybes)

ghci> catMaybes [Just 1, Nothing, Just 3, Nothing, Just 5]
[1,3,5]

-- listToMaybe, maybeToList
import Data.Maybe (listToMaybe, maybeToList)

ghci> listToMaybe []
Nothing

ghci> listToMaybe [1,2,3]
Just 1

ghci> maybeToList (Just 5)
[5]

ghci> maybeToList Nothing
[]

-- mapMaybe
import Data.Maybe (mapMaybe)

safeReciprocal :: Double -> Maybe Double
safeReciprocal 0 = Nothing
safeReciprocal x = Just (1/x)

ghci> mapMaybe safeReciprocal [1,2,0,4,0,5]
[1.0,0.5,0.25,0.2]
```

---

## ขั้นตอนที่ 143: Maybe เป็น Functor, Applicative, Monad

```haskell
-- Functor: fmap
ghci> fmap (*2) (Just 5)
Just 10

ghci> fmap (*2) Nothing
Nothing

-- Applicative: <*>
ghci> Just (*2) <*> Just 5
Just 10

ghci> Nothing <*> Just 5
Nothing

ghci> Just (*2) <*> Nothing
Nothing

-- Monad: >>=
ghci> Just 5 >>= \x -> Just (x * 2)
Just 10

ghci> Nothing >>= \x -> Just (x * 2)
Nothing

-- do notation กับ Maybe
safeCompute :: Int -> Int -> Int -> Maybe Int
safeCompute x y z = do
  a <- safeDiv x y      -- ถ้า Nothing จะ abort ทันที
  b <- safeDiv a z
  return (a + b)

ghci> safeCompute 10 2 1
Just 7   -- (10/2) = 5, (5/1) = 5, 5+5 = 10... wait

-- ลองคำนวณ:
-- a = 10 `div` 2 = 5
-- b = 5 `div` 1 = 5
-- a + b = 10
ghci> safeCompute 10 2 1
Just 10

ghci> safeCompute 10 0 1
Nothing   -- y=0 ทำให้ Nothing

ghci> safeCompute 10 2 0
Nothing   -- z=0 ทำให้ Nothing

-- <|>: Alternative (ลอง first, ถ้า fail ลอง second)
import Control.Applicative ((<|>))

ghci> Nothing <|> Just 5
Just 5

ghci> Just 3 <|> Just 5
Just 3   -- ใช้ first ที่ succeed

ghci> Nothing <|> Nothing
Nothing
```

---

## ขั้นตอนที่ 144: Either Type

```haskell
-- Either: ค่าที่เป็น Left (error) หรือ Right (success)
data Either a b = Left a | Right b
  deriving (Show, Eq, Ord)

-- Convention: Left = error, Right = success
-- "Being right is correct"

-- ตัวอย่าง
safeDivEither :: Int -> Int -> Either String Int
safeDivEither _ 0 = Left "Division by zero"
safeDivEither x y = Right (x `div` y)

parseAge :: String -> Either String Int
parseAge s = case reads s of
  [(n, "")] | n >= 0 && n <= 150 -> Right n
  [(n, "")] -> Left $ "Age out of range: " ++ show n
  _          -> Left $ "Invalid number: " ++ s

ghci> safeDivEither 10 2
Right 5

ghci> safeDivEither 10 0
Left "Division by zero"

ghci> parseAge "25"
Right 25

ghci> parseAge "200"
Left "Age out of range: 200"

ghci> parseAge "abc"
Left "Invalid number: abc"
```

---

## ขั้นตอนที่ 145: Either Operations

```haskell
-- either: destructure Either
-- either :: (a -> c) -> (b -> c) -> Either a b -> c

ghci> either (\e -> "Error: " ++ e) show (Right 42 :: Either String Int)
"42"

ghci> either (\e -> "Error: " ++ e) show (Left "oops" :: Either String Int)
"Error: oops"

-- isLeft, isRight
import Data.Either (isLeft, isRight)

ghci> isRight (Right 5)
True

ghci> isLeft (Left "error")
True

-- fromLeft, fromRight (partial!)
import Data.Either (fromLeft, fromRight)

ghci> fromRight 0 (Right 5)
5

ghci> fromRight 0 (Left "error")
0

-- lefts, rights
import Data.Either (lefts, rights, partitionEithers)

results :: [Either String Int]
results = [Right 1, Left "err1", Right 3, Left "err2", Right 5]

ghci> lefts results
["err1","err2"]

ghci> rights results
[1,3,5]

ghci> partitionEithers results
(["err1","err2"],[1,3,5])
```

---

## ขั้นตอนที่ 146: Either เป็น Monad

```haskell
-- Either เป็น Monad (บน Right)
-- Left short-circuits เหมือน exception

-- do notation กับ Either
validate :: String -> Int -> Either String (String, Int)
validate name age = do
  validName <- if null name then Left "Name cannot be empty" else Right name
  validAge  <- if age < 0   then Left "Age must be non-negative" else Right age
  validAge' <- if age > 150  then Left "Age seems too high" else Right validAge
  return (validName, validAge')

ghci> validate "Alice" 25
Right ("Alice",25)

ghci> validate "" 25
Left "Name cannot be empty"

ghci> validate "Alice" (-5)
Left "Age must be non-negative"

ghci> validate "Alice" 200
Left "Age seems too high"

-- Complex computation ที่อาจ fail
data DatabaseError = ConnectionFailed | QueryFailed String | NotFound
  deriving (Show)

type DB a = Either DatabaseError a

findUser :: Int -> DB String
findUser 1 = Right "Alice"
findUser 2 = Right "Bob"
findUser _ = Left NotFound

getUserEmail :: String -> DB String
getUserEmail "Alice" = Right "alice@example.com"
getUserEmail "Bob"   = Right "bob@example.com"
getUserEmail name    = Left (QueryFailed $ "No email for: " ++ name)

getUserEmailById :: Int -> DB String
getUserEmailById userId = do
  name  <- findUser userId
  email <- getUserEmail name
  return email

ghci> getUserEmailById 1
Right "alice@example.com"

ghci> getUserEmailById 99
Left NotFound
```

---

## ขั้นตอนที่ 147: Custom Error Types

```haskell
-- สร้าง error type ที่ expressive

data AppError
  = DatabaseError String
  | ValidationError [(String, String)]  -- field, message
  | NotFoundError String
  | AuthorizationError String
  | NetworkError String
  deriving (Show, Eq)

-- Rendering errors
renderError :: AppError -> String
renderError (DatabaseError msg)       = "Database error: " ++ msg
renderError (ValidationError errors)  =
  "Validation failed:\n" ++ unlines (map formatError errors)
  where formatError (field, msg) = "  - " ++ field ++ ": " ++ msg
renderError (NotFoundError resource)  = resource ++ " not found"
renderError (AuthorizationError msg)  = "Not authorized: " ++ msg
renderError (NetworkError msg)        = "Network error: " ++ msg

-- Type alias สำหรับ Either AppError
type Result a = Either AppError a

-- Functions ที่ใช้ custom error type
lookupUser :: Int -> Result User
lookupUser 1 = Right (User 1 "Alice")
lookupUser _ = Left (NotFoundError "User")

data User = User { userId :: Int, userName :: String }

createUser :: String -> Result User
createUser name
  | null name = Left (ValidationError [("name", "cannot be empty")])
  | length name < 3 = Left (ValidationError [("name", "too short")])
  | otherwise = Right (User 999 name)
```

---

## ขั้นตอนที่ 148: ExceptT Transformer

```haskell
-- ExceptT: Either monad transformer
-- รัน computation ที่อาจ fail ใน context อื่น (เช่น IO)

import Control.Monad.Except

-- ExceptT e m a = m (Either e a)

type AppM a = ExceptT AppError IO a

-- throwError: throw error
-- catchError: catch error

-- ตัวอย่าง
readConfig :: FilePath -> AppM String
readConfig path = do
  contents <- liftIO (readFile path) `catchError` handleIOError
  return contents
  where
    handleIOError _ = throwError (NotFoundError path)

processConfig :: String -> AppM Config
processConfig content = case parseConfig content of
  Left err  -> throwError (ValidationError [("config", err)])
  Right cfg -> return cfg

data Config = Config { configHost :: String, configPort :: Int }

parseConfig :: String -> Either String Config
parseConfig s = Right (Config "localhost" 8080)  -- simplified

runApp :: AppM a -> IO (Either AppError a)
runApp = runExceptT

-- ใช้งาน
main :: IO ()
main = do
  result <- runApp $ do
    content <- readConfig "config.txt"
    cfg     <- processConfig content
    liftIO $ putStrLn $ "Host: " ++ configHost cfg
    return cfg
  case result of
    Left err  -> putStrLn $ "Error: " ++ renderError err
    Right cfg -> putStrLn $ "Config loaded: port " ++ show (configPort cfg)
```

---

## ขั้นตอนที่ 149: Validation vs Either

```haskell
-- Either: fails fast (หยุดที่ error แรก)
-- Validation: accumulates errors (รวบรวม errors ทั้งหมด)

-- Validation type (จาก Part 06)
data Validation e a = Failure [e] | Success a
  deriving (Show)

instance Functor (Validation e) where
  fmap _ (Failure es) = Failure es
  fmap f (Success x)  = Success (f x)

instance Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
  Failure e  <*> _           = Failure e
  _          <*> Failure e   = Failure e
  Success f  <*> Success x   = Success (f x)

-- การใช้งาน
data UserInput = UserInput
  { inputName  :: String
  , inputEmail :: String
  , inputAge   :: Int
  }

validateName :: String -> Validation String String
validateName s
  | null s        = Failure ["name cannot be empty"]
  | length s < 2  = Failure ["name too short"]
  | otherwise     = Success s

validateEmail :: String -> Validation String String
validateEmail s
  | null s            = Failure ["email cannot be empty"]
  | '@' `notElem` s  = Failure ["email must contain @"]
  | otherwise         = Success s

validateAge :: Int -> Validation String Int
validateAge n
  | n < 0    = Failure ["age cannot be negative"]
  | n > 150  = Failure ["age too large"]
  | otherwise = Success n

-- Validate all fields
validateUser :: UserInput -> Validation String (String, String, Int)
validateUser (UserInput name email age) =
  (,,) <$> validateName name
       <*> validateEmail email
       <*> validateAge age

ghci> validateUser (UserInput "" "not-email" (-5))
Failure ["name cannot be empty","email must contain @","age cannot be negative"]

ghci> validateUser (UserInput "Alice" "alice@example.com" 25)
Success ("Alice","alice@example.com",25)
```

---

## ขั้นตอนที่ 150: Exceptions ใน Haskell

```haskell
import Control.Exception
import System.IO.Error

-- Haskell มี exception mechanism ด้วย แต่ใช้เฉพาะใน IO
-- (pure code ไม่ควรใช้ exceptions)

-- throw exception
throwIO :: Exception e => e -> IO a

-- catch exception
catch :: Exception e => IO a -> (e -> IO a) -> IO a

-- try: ดัก exception เป็น Either
try :: Exception e => IO a -> IO (Either e a)

-- ตัวอย่าง
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  result <- try (readFile path)
  return $ case result of
    Left e  -> Left (show (e :: IOException))
    Right s -> Right s

-- หรือใช้ catches สำหรับหลาย exception types
safeOp :: IO Int -> IO Int
safeOp action = catches action
  [ Handler (\e -> do putStrLn $ "IOException: " ++ show (e :: IOException)
                      return (-1))
  , Handler (\e -> do putStrLn $ "ArithException: " ++ show (e :: ArithException)
                      return (-2))
  ]

-- Custom exception
data MyException = MyException String deriving (Show)
instance Exception MyException

throwCustom :: IO ()
throwCustom = throwIO (MyException "something went wrong")

catchCustom :: IO ()
catchCustom = catchCustom' `catch` \(MyException msg) ->
  putStrLn $ "Caught: " ++ msg
  where catchCustom' = do
          putStrLn "About to throw..."
          throwIO (MyException "oops")
          putStrLn "This won't run"
```

---

## ขั้นตอนที่ 151: Safe Exceptions กับ bracket

```haskell
import Control.Exception (bracket, finally, onException)

-- bracket: ensure cleanup ไม่ว่า action จะ succeed หรือ fail
-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- Pattern: acquire resource, use it, release it
withFile :: FilePath -> (Handle -> IO a) -> IO a
withFile path action = bracket
  (openFile path ReadMode)  -- acquire
  hClose                    -- release
  action                    -- use

-- ตัวอย่าง
readFileLines :: FilePath -> IO [String]
readFileLines path = withFile path $ \handle ->
  fmap lines (hGetContents handle)

-- finally: ทำ action แล้ว always ทำ finalizer
finallyExample :: IO ()
finallyExample = do
  putStrLn "Start"
  finally
    (do putStrLn "Main action"
        error "Something failed!")   -- throw exception
    (putStrLn "Cleanup (always runs)")
-- Output:
-- Start
-- Main action
-- Cleanup (always runs)
-- *** Exception: Something failed!

-- onException: ทำ cleanup เฉพาะเมื่อมี exception
onExceptionExample :: IO ()
onExceptionExample = do
  putStrLn "Start"
  onException
    (do putStrLn "Main action"
        return ())                   -- success case
    (putStrLn "Cleanup (only on exception)")
-- Output:
-- Start
-- Main action
-- (cleanup ไม่รัน เพราะสำเร็จ)
```

---

## ขั้นตอนที่ 152: Error Handling Patterns

```haskell
-- Pattern 1: Convert exceptions to Either
tryIO :: IO a -> IO (Either String a)
tryIO action = do
  result <- try action
  return $ case result of
    Left e  -> Left (show (e :: SomeException))
    Right v -> Right v

-- Pattern 2: Default value on failure
withDefault :: a -> IO a -> IO a
withDefault def action = do
  result <- try action
  return $ case result of
    Left (_ :: SomeException) -> def
    Right v -> v

-- Pattern 3: Retry on failure
retry :: Int -> IO a -> IO (Either String a)
retry 0 action = tryIO action
retry n action = do
  result <- tryIO action
  case result of
    Left err -> do
      putStrLn $ "Retry " ++ show n ++ " after error: " ++ err
      retry (n-1) action
    Right v -> return (Right v)

-- Pattern 4: Exponential backoff
import Control.Concurrent (threadDelay)

retryWithBackoff :: Int -> IO a -> IO (Either String a)
retryWithBackoff maxRetries action = go maxRetries 1
  where
    go 0 _ = tryIO action
    go n delay = do
      result <- tryIO action
      case result of
        Left err -> do
          threadDelay (delay * 1000000)  -- delay in microseconds
          go (n-1) (delay * 2)
        Right v -> return (Right v)
```

---

## ขั้นตอนที่ 153: MonadError

```haskell
import Control.Monad.Error.Class

-- MonadError: type class สำหรับ computation ที่อาจ fail

-- throwError :: MonadError e m => e -> m a
-- catchError :: MonadError e m => m a -> (e -> m a) -> m a

-- Either e เป็น instance ของ MonadError e
-- ExceptT e m เป็น instance ของ MonadError e

-- ใช้ MonadError สำหรับ code ที่ reusable
safeDiv' :: MonadError String m => Int -> Int -> m Int
safeDiv' _ 0 = throwError "Division by zero"
safeDiv' x y = return (x `div` y)

-- ทำงานกับทั้ง Either และ ExceptT IO
ghci> safeDiv' 10 2 :: Either String Int
Right 5

ghci> runExceptT (safeDiv' 10 0) :: IO (Either String Int)
Left "Division by zero"

-- ดีกว่า Either direct เพราะ polymorphic
computation :: MonadError String m => m Int
computation = do
  x <- safeDiv' 10 2
  y <- safeDiv' x 0
  return (x + y)

ghci> computation :: Either String Int
Left "Division by zero"
```

---

## ขั้นตอนที่ 154: Error Context

```haskell
-- เพิ่ม context ให้กับ errors

-- annotate: เพิ่ม context message
annotate :: String -> Either String a -> Either String a
annotate ctx (Left err) = Left $ ctx ++ ": " ++ err
annotate _   r           = r

-- withContext: เพิ่ม context ใน do block
withContext :: MonadError String m => String -> m a -> m a
withContext ctx action = action `catchError` \err ->
  throwError (ctx ++ ": " ++ err)

-- ตัวอย่าง
parseUserJson :: String -> Either String User
parseUserJson json = withContext "parseUserJson" $ do
  name <- extractField "name" json
  age  <- extractField "age" json >>= parseNumber
  return (User 0 name)
  where
    extractField _ _ = Right "dummy"
    parseNumber _    = Right 25

-- Error Hierarchy
data AppError
  = ParseError ParseError
  | DatabaseError DbError
  | NetworkError NetError
  deriving (Show)

data ParseError = InvalidJson String | MissingField String
  deriving (Show)

data DbError = ConnectionFailed | QueryTimeout | UniqueViolation String
  deriving (Show)

data NetError = Timeout | ConnectionRefused | DNSFailure
  deriving (Show)

-- Smart constructors
parseErr :: ParseError -> AppError
parseErr = ParseError

dbErr :: DbError -> AppError
dbErr = DatabaseError

netErr :: NetError -> AppError
netErr = NetworkError
```

---

## ขั้นตอนที่ 155: Async Error Handling

```haskell
import Control.Concurrent.Async
import Control.Exception

-- Async: run IO actions concurrently
-- Error handling กับ async actions

-- race: รัน 2 actions, คืน result ของที่เร็วกว่า
-- concurrently: รัน 2 actions concurrently, รอทั้งคู่

safeAsync :: IO a -> IO (Either SomeException a)
safeAsync action = do
  result <- async (try action)
  wait result

-- ตัวอย่าง: parallel error handling
fetchParallel :: [String] -> IO [Either String String]
fetchParallel urls = do
  actions <- mapM (async . fetchUrl) urls
  mapM (\a -> fmap (either (Left . show) Right) (waitCatch a)) actions
  where
    fetchUrl url = return $ "Content of " ++ url  -- mock

-- Timeout
import System.Timeout

withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout microseconds = timeout microseconds

-- ตัวอย่าง
example :: IO ()
example = do
  result <- withTimeout 1000000 (slowOperation)  -- 1 second timeout
  case result of
    Nothing -> putStrLn "Timed out!"
    Just v  -> putStrLn $ "Got: " ++ show v
  where slowOperation = return 42
```

---

## ขั้นตอนที่ 156: Error Logging

```haskell
import System.IO
import Data.Time

-- เพิ่ม error context ด้วย timestamps

data LogEntry = LogEntry
  { entryTime    :: String
  , entryLevel   :: String
  , entryMessage :: String
  } deriving (Show)

formatLog :: LogEntry -> String
formatLog entry = "[" ++ entryTime entry ++ "] [" ++ entryLevel entry ++ "] " ++ entryMessage entry

logError :: Handle -> AppError -> IO ()
logError handle err = do
  time <- getCurrentTime
  let entry = LogEntry
        { entryTime    = show time
        , entryLevel   = "ERROR"
        , entryMessage = renderError err
        }
  hPutStrLn handle (formatLog entry)

-- Structured logging
data LogData = LogData
  { logTime     :: String
  , logLevel    :: String
  , logError    :: Maybe String
  , logDetails  :: [(String, String)]
  } deriving (Show)
```

---

## ขั้นตอนที่ 157: IOError Handling

```haskell
import System.IO.Error

-- IOError handling
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  result <- tryIOError (readFile path)
  return $ case result of
    Left e  -> Left (formatIOError e)
    Right s -> Right s
  where
    formatIOError e
      | isDoesNotExistError e = "File not found: " ++ path
      | isPermissionError e   = "Permission denied: " ++ path
      | otherwise             = "IO error: " ++ show e

-- ตรวจสอบ IOError type
handleIOError :: IOError -> IO ()
handleIOError e
  | isDoesNotExistError e  = putStrLn "File not found"
  | isPermissionError e    = putStrLn "Permission denied"
  | isAlreadyExistsError e = putStrLn "File already exists"
  | isEOFError e           = putStrLn "End of file"
  | otherwise              = putStrLn $ "Unknown error: " ++ show e

-- ใช้ catchIOError
safeCopy :: FilePath -> FilePath -> IO (Either String ())
safeCopy src dst = do
  result <- tryIOError $ do
    content <- readFile src
    writeFile dst content
  return $ case result of
    Left e  -> Left (show e)
    Right _ -> Right ()
```

---

## ขั้นตอนที่ 158: Result Type Pattern

```haskell
-- Pattern: Result type ที่ flexible กว่า Either

data Result err ok
  = Err err
  | Ok ok
  deriving (Show, Eq)

-- Smart constructors
ok :: a -> Result e a
ok = Ok

err :: e -> Result e a
err = Err

-- Conversion
toEither :: Result e a -> Either e a
toEither (Ok x)  = Right x
toEither (Err e) = Left e

fromEither :: Either e a -> Result e a
fromEither (Right x) = Ok x
fromEither (Left e)  = Err e

-- instance Functor, Applicative, Monad
instance Functor (Result e) where
  fmap _ (Err e) = Err e
  fmap f (Ok x)  = Ok (f x)

instance Applicative (Result e) where
  pure = Ok
  Err e <*> _     = Err e
  Ok f  <*> Err e = Err e
  Ok f  <*> Ok x  = Ok (f x)

instance Monad (Result e) where
  return = pure
  Err e >>= _ = Err e
  Ok x  >>= f = f x

-- Practical usage
type Validated a = Result [String] a

validate' :: String -> Validated String
validate' s
  | null s       = Err ["cannot be empty"]
  | length s < 3 = Err ["too short (min 3 chars)"]
  | otherwise    = Ok s
```

---

## ขั้นตอนที่ 159: Error Recovery

```haskell
-- Patterns สำหรับ error recovery

-- Try multiple strategies
(<||>) :: Maybe a -> Maybe a -> Maybe a
Nothing <||> y = y
x       <||> _ = x

-- ตัวอย่าง: หา user จาก database หรือ cache
findUserAnywhere :: Int -> IO (Maybe User)
findUserAnywhere uid = do
  fromCache <- lookupCache uid
  case fromCache of
    Just u -> return (Just u)
    Nothing -> do
      fromDB <- lookupDB uid
      case fromDB of
        Just u -> do
          putInCache uid u  -- cache it
          return (Just u)
        Nothing -> return Nothing
  where
    lookupCache _ = return Nothing  -- mock
    lookupDB uid  = if uid == 1 then return (Just (User 1 "Alice")) else return Nothing
    putInCache _ _ = return ()

-- Partial success
processItems :: [a] -> (a -> Either e b) -> ([e], [b])
processItems items f = partitionEithers (map f items)

-- ตัวอย่าง
parseNumbers :: [String] -> ([String], [Int])
parseNumbers strs = processItems strs parseOne
  where
    parseOne s = case reads s of
      [(n, "")] -> Right n
      _          -> Left $ "Invalid number: " ++ s

ghci> parseNumbers ["1", "abc", "3", "xyz", "5"]
(["Invalid number: abc","Invalid number: xyz"],[1,3,5])
```

---

## ขั้นตอนที่ 160: สรุปและ Best Practices

### Error Handling Best Practices

```haskell
-- 1. ใช้ types แทน exceptions สำหรับ expected failures
-- ไม่ดี:
unsafeDivide :: Int -> Int -> Int
unsafeDivide x 0 = error "division by zero"  -- throw exception
unsafeDivide x y = x `div` y

-- ดี:
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- 2. ใช้ Either สำหรับ errors ที่มีข้อมูล
safeRead :: Read a => String -> Either String a
safeRead s = case reads s of
  [(x, "")] -> Right x
  _          -> Left $ "Parse error: " ++ s

-- 3. ใช้ custom error types
data AppError = DatabaseError String | ValidationError String | NotFound
  deriving (Show)

-- 4. ใช้ ExceptT สำหรับ IO computations ที่อาจ fail
type App a = ExceptT AppError IO a

-- 5. ใช้ Validation สำหรับ collecting errors
-- 6. ใช้ bracket สำหรับ resource management
-- 7. Log errors อย่าละเว้น

-- Project: HTTP Request Handler with Error Handling

data HttpMethod = GET | POST | PUT | DELETE deriving (Show, Eq)
data HttpStatus = OK | NotFound | BadRequest | InternalServerError deriving (Show)

data HttpRequest = HttpRequest
  { method  :: HttpMethod
  , path    :: String
  , body    :: Maybe String
  , headers :: [(String, String)]
  }

data HttpResponse = HttpResponse
  { status  :: HttpStatus
  , resBody :: String
  }

data HandlerError
  = MethodNotAllowed HttpMethod
  | ResourceNotFound String
  | InvalidInput String
  | InternalError String
  deriving (Show)

type Handler = HttpRequest -> ExceptT HandlerError IO HttpResponse

-- Middleware
withLogging :: Handler -> Handler
withLogging h req = do
  liftIO $ putStrLn $ show (method req) ++ " " ++ path req
  result <- h req `catchError` \err -> do
    liftIO $ putStrLn $ "Error: " ++ show err
    throwError err
  liftIO $ putStrLn $ "Response: " ++ show (status result)
  return result

-- Handler implementation
userHandler :: Handler
userHandler req = case method req of
  GET    -> handleGetUser req
  POST   -> handleCreateUser req
  _      -> throwError (MethodNotAllowed (method req))

handleGetUser :: HttpRequest -> ExceptT HandlerError IO HttpResponse
handleGetUser req = case path req of
  "/users/1" -> return (HttpResponse OK "{\"id\":1,\"name\":\"Alice\"}")
  p          -> throwError (ResourceNotFound p)

handleCreateUser :: HttpRequest -> ExceptT HandlerError IO HttpResponse
handleCreateUser req = case body req of
  Nothing -> throwError (InvalidInput "Request body required")
  Just b  -> return (HttpResponse OK b)
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 09 เราจะเรียนเรื่อง **IO Monad** อย่างละเอียด:
- IO Actions
- do Notation
- IORef
- File I/O
- Environment Variables

---

*[← Part 07](part-07.md) | [Part 09 →](part-09.md)*
