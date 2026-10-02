# Part 09: IO Monad และ Side Effects
## ขั้นตอนที่ 161-180: การทำงานกับ IO ใน Haskell

---

## บทนำ

IO Monad คือกลไกที่ Haskell ใช้สำหรับจัดการ side effects (เช่น การอ่าน/เขียนไฟล์, network, console I/O) โดยยังคง purity ของ pure functions ไว้

---

## ขั้นตอนที่ 161: IO พื้นฐาน

```haskell
-- IO a คือ "action" ที่เมื่อรันจะทำ side effects และคืนค่า a

-- main เป็น IO ()
main :: IO ()
main = putStrLn "Hello, World!"

-- IO actions ที่พื้นฐาน
putStr   :: String -> IO ()      -- print without newline
putStrLn :: String -> IO ()      -- print with newline
print    :: Show a => a -> IO () -- print any Show value

getLine  :: IO String            -- read a line
getChar  :: IO Char              -- read a char
getContents :: IO String         -- read all stdin

-- Example
main :: IO ()
main = do
  putStr "Enter your name: "
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

-- interact: อ่าน stdin ทั้งหมด แล้ว process เป็น output
main' :: IO ()
main' = interact $ \input ->
  unlines . map (map toUpper) . lines $ input
  where toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
```

---

## ขั้นตอนที่ 162: do Notation

```haskell
-- do notation คือ syntax sugar สำหรับ Monad operations

-- แบบ explicit
readAndPrint :: IO ()
readAndPrint =
  getLine >>= \line ->
  putStrLn ("You said: " ++ line)

-- แบบ do notation (อ่านง่ายกว่า)
readAndPrint' :: IO ()
readAndPrint' = do
  line <- getLine
  putStrLn ("You said: " ++ line)

-- do block คือ sequence ของ actions
greetUser :: IO ()
greetUser = do
  putStr "First name: "
  first <- getLine
  putStr "Last name: "
  last <- getLine
  let fullName = first ++ " " ++ last   -- let ใน do block
  putStrLn $ "Hello, " ++ fullName ++ "!"

-- Return value จาก do block
askNumber :: IO Int
askNumber = do
  putStr "Enter a number: "
  line <- getLine
  return (read line)  -- return :: a -> IO a

-- ใช้ return ค่า
main :: IO ()
main = do
  n <- askNumber
  putStrLn $ "You entered: " ++ show n
  putStrLn $ "Doubled: " ++ show (n * 2)

-- when: conditional IO action
import Control.Monad (when, unless, forM_, forM, replicateM)

main' :: IO ()
main' = do
  n <- askNumber
  when (n > 0) $ putStrLn "Positive!"
  unless (n > 0) $ putStrLn "Non-positive!"
```

---

## ขั้นตอนที่ 163: IORef - Mutable State

```haskell
import Data.IORef

-- IORef a คือ mutable reference ที่เก็บค่า a
-- ใน IO context เท่านั้น

-- สร้าง IORef
newIORef :: a -> IO (IORef a)

-- อ่านค่า
readIORef :: IORef a -> IO a

-- เขียนค่า
writeIORef :: IORef a -> a -> IO ()

-- Modify
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef' :: IORef a -> (a -> a) -> IO ()  -- strict version

-- ตัวอย่าง: Counter
counter :: IO ()
counter = do
  ref <- newIORef (0 :: Int)
  forM_ [1..5] $ \i -> do
    modifyIORef ref (+1)
    count <- readIORef ref
    putStrLn $ "Count: " ++ show count

-- ตัวอย่าง: Accumulator
sumList :: [Int] -> IO Int
sumList xs = do
  acc <- newIORef 0
  forM_ xs $ \x -> modifyIORef acc (+x)
  readIORef acc

-- ตัวอย่าง: Memoization
type Cache k v = IORef (Map.Map k v)

newCache :: IO (Cache k v)
newCache = newIORef Map.empty

lookupCache :: Ord k => k -> Cache k v -> IO (Maybe v)
lookupCache k cache = do
  m <- readIORef cache
  return (Map.lookup k m)

insertCache :: Ord k => k -> v -> Cache k v -> IO ()
insertCache k v cache = modifyIORef cache (Map.insert k v)

-- Memoized fibonacci
fibCached :: IO (Int -> IO Integer)
fibCached = do
  cache <- newCache
  let fib n = do
        cached <- lookupCache n cache
        case cached of
          Just v  -> return v
          Nothing -> do
            v <- if n <= 1
                   then return (fromIntegral n)
                   else do
                     v1 <- fib (n-1)
                     v2 <- fib (n-2)
                     return (v1 + v2)
            insertCache n v cache
            return v
  return fib
```

---

## ขั้นตอนที่ 164: File I/O

```haskell
import System.IO
import System.Directory
import System.FilePath

-- เปิดไฟล์
openFile :: FilePath -> IOMode -> IO Handle

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- ปิดไฟล์
hClose :: Handle -> IO ()

-- อ่านเขียน
hGetLine      :: Handle -> IO String
hGetContents  :: Handle -> IO String
hPutStr       :: Handle -> String -> IO ()
hPutStrLn     :: Handle -> String -> IO ()
hPrint        :: Show a => Handle -> a -> IO ()

-- Safe file operations ด้วย withFile
import System.IO (withFile)

readLines :: FilePath -> IO [String]
readLines path = withFile path ReadMode $ \handle ->
  fmap lines (hGetContents handle)

writeLines :: FilePath -> [String] -> IO ()
writeLines path content = withFile path WriteMode $ \handle ->
  mapM_ (hPutStrLn handle) content

appendLine :: FilePath -> String -> IO ()
appendLine path line = withFile path AppendMode $ \handle ->
  hPutStrLn handle line

-- ง่ายกว่า: readFile, writeFile, appendFile
simpleRead :: FilePath -> IO String
simpleRead = readFile

simpleWrite :: FilePath -> String -> IO ()
simpleWrite = writeFile

simpleAppend :: FilePath -> String -> IO ()
simpleAppend = appendFile

-- ตัวอย่าง
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' src dst = readFile src >>= writeFile dst

-- ตรวจสอบ file existence
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

-- ลบไฟล์
removeFile' :: FilePath -> IO ()
removeFile' = removeFile  -- from System.Directory

-- List directory contents
listDir :: FilePath -> IO [FilePath]
listDir = listDirectory

-- Create directory
createDir :: FilePath -> IO ()
createDir = createDirectoryIfMissing True  -- True = create parents
```

---

## ขั้นตอนที่ 165: Text I/O

```haskell
-- ใช้ Text แทน String สำหรับ performance
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- TIO.putStrLn แทน putStrLn สำหรับ Text
readTextFile :: FilePath -> IO T.Text
readTextFile = TIO.readFile

writeTextFile :: FilePath -> T.Text -> IO ()
writeTextFile = TIO.writeFile

-- ByteString I/O
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

readBinaryFile :: FilePath -> IO BS.ByteString
readBinaryFile = BS.readFile

writeBinaryFile :: FilePath -> BS.ByteString -> IO ()
writeBinaryFile = BS.writeFile

-- ตัวอย่าง: Count lines in file
countLines :: FilePath -> IO Int
countLines path = do
  content <- TIO.readFile path
  return $ length (T.lines content)

-- ตัวอย่าง: Search in file
searchFile :: T.Text -> FilePath -> IO [Int]
searchFile query path = do
  content <- TIO.readFile path
  let numbered = zip [1..] (T.lines content)
  return [n | (n, line) <- numbered, query `T.isInfixOf` line]
```

---

## ขั้นตอนที่ 166: Environment Variables

```haskell
import System.Environment
import System.Exit

-- getArgs: command line arguments
main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "No arguments"
    else mapM_ putStrLn args

-- getProgName: ชื่อโปรแกรม
main' :: IO ()
main' = do
  name <- getProgName
  putStrLn $ "Running: " ++ name

-- getEnvironment: ตัวแปร environment ทั้งหมด
printEnv :: IO ()
printEnv = do
  env <- getEnvironment
  mapM_ (\(k,v) -> putStrLn $ k ++ "=" ++ v) env

-- lookupEnv: หาตัวแปร environment
getDbUrl :: IO String
getDbUrl = do
  mUrl <- lookupEnv "DATABASE_URL"
  case mUrl of
    Just url -> return url
    Nothing  -> do
      putStrLn "DATABASE_URL not set, using default"
      return "postgresql://localhost:5432/mydb"

-- getEnv: ถ้าไม่มีจะ throw exception
getApiKey :: IO String
getApiKey = getEnv "API_KEY"

-- exitSuccess, exitFailure, exitWith
exitProgram :: IO ()
exitProgram = do
  args <- getArgs
  if "--help" `elem` args
    then do putStrLn "Usage: program [options]"
            exitSuccess
    else if "--version" `elem` args
      then do putStrLn "Version 1.0.0"
              exitSuccess
      else putStrLn "Running..."
```

---

## ขั้นตอนที่ 167: Standard Handles

```haskell
import System.IO

-- Standard handles
stdin  :: Handle  -- standard input
stdout :: Handle  -- standard output
stderr :: Handle  -- standard error

-- Write to stderr (สำหรับ error messages)
writeError :: String -> IO ()
writeError msg = hPutStrLn stderr msg

-- Flush output
flushOutput :: IO ()
flushOutput = hFlush stdout

-- Set buffering mode
setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

-- ตัวอย่าง: interactive prompt
interactive :: IO ()
interactive = do
  hSetBuffering stdout NoBuffering
  loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      line <- getLine
      case line of
        "quit" -> putStrLn "Goodbye!"
        _      -> do
          putStrLn $ "You said: " ++ line
          loop

-- Check if handle is ready
checkReady :: Handle -> IO ()
checkReady handle = do
  ready <- hReady handle
  if ready
    then putStrLn "Data available"
    else putStrLn "No data yet"
```

---

## ขั้นตอนที่ 168: forM_, forM, replicateM

```haskell
import Control.Monad

-- forM_: map IO action over list, discard results
printNumbers :: IO ()
printNumbers = forM_ [1..10] print

-- forM: map IO action over list, collect results
readNumbers :: IO [Int]
readNumbers = forM [1..5] $ \i -> do
  putStr $ "Enter number " ++ show i ++ ": "
  fmap read getLine

-- replicateM: repeat IO action n times
getThreeLines :: IO [String]
getThreeLines = replicateM 3 getLine

-- replicateM_: repeat IO action n times, discard results
printNTimes :: Int -> String -> IO ()
printNTimes n s = replicateM_ n (putStrLn s)

-- sequence_: run list of IO actions
sequence_ :: [IO a] -> IO ()
sequence_ = mapM_ id

-- mapM: same as forM but arguments flipped
mapMExample :: IO ()
mapMExample = do
  results <- mapM (\n -> return (n * 2)) [1..5]
  print results

-- filterM: filter with IO predicate
import Control.Monad (filterM)

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM doesFileExist

-- foldM: fold with IO function
import Control.Monad (foldM)

safeProduct :: [Int] -> IO Int
safeProduct = foldM safeMultiply 1
  where
    safeMultiply acc x
      | x == 0    = do putStrLn "Zero encountered!"
                       return 0
      | otherwise = return (acc * x)
```

---

## ขั้นตอนที่ 169: MVar - Mutable Concurrent Variable

```haskell
import Control.Concurrent.MVar

-- MVar: thread-safe mutable variable
-- อ่านได้เฉพาะเมื่อมีค่า, เขียนได้เฉพาะเมื่อว่าง

-- สร้าง MVar
newMVar   :: a -> IO (MVar a)    -- เริ่มด้วยค่า
newEmptyMVar :: IO (MVar a)      -- เริ่มว่าง

-- อ่านค่า (blocks ถ้าว่าง)
takeMVar :: MVar a -> IO a      -- removes value
readMVar :: MVar a -> IO a      -- peeks without removing

-- ใส่ค่า (blocks ถ้ามีค่าแล้ว)
putMVar :: MVar a -> a -> IO ()

-- Modify atomically
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar  :: MVar a -> (a -> IO (a, b)) -> IO b

-- ตัวอย่าง: Thread-safe counter
import Control.Concurrent

counterMVar :: IO ()
counterMVar = do
  counter <- newMVar (0 :: Int)
  threads <- forM [1..5] $ \_ -> do
    forkIO $ replicateM_ 100 $ modifyMVar_ counter (\n -> return (n + 1))
  threadDelay 100000  -- wait for threads
  final <- readMVar counter
  print final  -- should be 500

-- ตัวอย่าง: Simple lock
withLock :: MVar () -> IO a -> IO a
withLock lock action = do
  takeMVar lock   -- acquire
  result <- action `onException` putMVar lock ()  -- release on exception
  putMVar lock () -- release
  return result

-- ตัวอย่าง: Shared state
data AppState = AppState
  { stateCount :: Int
  , stateUsers :: [String]
  }

type SharedState = MVar AppState

newSharedState :: IO SharedState
newSharedState = newMVar (AppState 0 [])

addUser :: SharedState -> String -> IO ()
addUser state name = modifyMVar_ state $ \s ->
  return s { stateCount = stateCount s + 1
           , stateUsers = name : stateUsers s }
```

---

## ขั้นตอนที่ 170: IORef vs MVar vs STM

```haskell
-- IORef: ใช้ใน single thread
-- MVar: ใช้สำหรับ mutual exclusion ระหว่าง threads
-- STM: ใช้สำหรับ complex concurrent state

import Control.Concurrent.STM

-- STM: Software Transactional Memory
-- atomically :: STM a -> IO a

-- TVar: transactional variable
type TCounter = TVar Int

newCounter :: IO TCounter
newCounter = newTVarIO 0

increment :: TCounter -> STM ()
increment counter = modifyTVar counter (+1)

readCounter :: TCounter -> IO Int
readCounter = readTVarIO

-- ตัวอย่าง: atomic bank transfer
transfer :: TVar Int -> TVar Int -> Int -> STM ()
transfer from to amount = do
  fromBalance <- readTVar from
  if fromBalance < amount
    then retry  -- retry ถ้า balance ไม่พอ
    else do
      modifyTVar from (subtract amount)
      modifyTVar to   (+ amount)

-- atomically รัน STM transaction
doTransfer :: TVar Int -> TVar Int -> Int -> IO Bool
doTransfer from to amount = do
  result <- atomically $ do
    fromBalance <- readTVar from
    if fromBalance >= amount
      then do transfer from to amount
              return True
      else return False
  return result

-- TQueue: thread-safe queue
import Control.Concurrent.STM.TQueue

type JobQueue = TQueue String

newJobQueue :: IO JobQueue
newJobQueue = newTQueueIO

addJob :: JobQueue -> String -> IO ()
addJob q job = atomically (writeTQueue q job)

getJob :: JobQueue -> IO String
getJob q = atomically (readTQueue q)  -- blocks if empty
```

---

## ขั้นตอนที่ 171: Haskell Program Structure

```haskell
-- โครงสร้างโปรแกรม Haskell ที่ดี

module Main where

import qualified Config
import qualified App
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Entry point
main :: IO ()
main = do
  args <- getArgs
  config <- loadConfig args
  case config of
    Left err -> do
      putStrLn $ "Configuration error: " ++ err
      exitFailure
    Right cfg -> App.run cfg

-- Config loading
loadConfig :: [String] -> IO (Either String Config.Config)
loadConfig args = Config.fromArgs args

-- App module
module App where

import Config (Config)

run :: Config -> IO ()
run cfg = do
  putStrLn "Application starting..."
  -- ... rest of app
```

---

## ขั้นตอนที่ 172: Lazy IO และ Streaming

```haskell
-- Lazy IO: readFile ใน Haskell เป็น lazy
-- content จะถูกอ่านเมื่อต้องการจริงๆ เท่านั้น

processLargeFile :: FilePath -> IO Int
processLargeFile path = do
  content <- readFile path  -- lazy: ไม่อ่านทั้งหมดทันที
  return $ length (lines content)  -- อ่าน on demand

-- ปัญหา: lazy IO อาจทำให้ handle ถูกปิดก่อนอ่านเสร็จ
-- วิธีแก้: ใช้ streaming library เช่น conduit หรือ pipes

-- conduit: streaming I/O
import Conduit

processFile :: FilePath -> IO ()
processFile path = runConduitRes $
  sourceFile path
  .| decodeUtf8C
  .| linesUnboundedC
  .| mapC T.toUpper
  .| encodeUtf8C
  .| sinkFile (path ++ ".upper")

-- pipes: another streaming library
import Pipes
import qualified Pipes.Prelude as P

printLines :: IO ()
printLines = runEffect $
  P.stdinLn >-> P.map (map toUpper) >-> P.stdoutLn

-- Streaming without library: strict reading
processFileStrict :: FilePath -> IO [String]
processFileStrict path = do
  content <- readFile path
  let ls = lines content
  length ls `seq` return ls  -- force evaluation
```

---

## ขั้นตอนที่ 173: Concurrent IO

```haskell
import Control.Concurrent
import Control.Concurrent.Async

-- forkIO: spawn lightweight thread
simpleThread :: IO ()
simpleThread = do
  tid <- forkIO $ do
    putStrLn "Thread started"
    threadDelay 1000000  -- 1 second
    putStrLn "Thread done"
  putStrLn "Main thread"
  threadDelay 2000000  -- wait for thread

-- Async: better interface for concurrent tasks
fetchAll :: [URL] -> IO [String]
fetchAll urls = do
  asyncs <- mapM (async . fetchUrl) urls
  mapM wait asyncs

type URL = String
fetchUrl :: URL -> IO String
fetchUrl url = return $ "Content of " ++ url  -- mock

-- concurrently: run 2 tasks in parallel
runParallel :: IO ()
runParallel = do
  (result1, result2) <- concurrently
    (fetchUrl "http://example1.com")
    (fetchUrl "http://example2.com")
  putStrLn result1
  putStrLn result2

-- race: first to finish wins
fastest :: IO ()
fastest = do
  result <- race
    (do threadDelay 100000; return "fast")
    (do threadDelay 200000; return "slow")
  case result of
    Left  r -> putStrLn $ "First finished: " ++ r
    Right r -> putStrLn $ "Second finished: " ++ r

-- mapConcurrently: parallel map
processAll :: [Int] -> IO [Int]
processAll = mapConcurrently (\n -> do
  threadDelay 100000
  return (n * 2))
```

---

## ขั้นตอนที่ 174: STM ขั้นสูง

```haskell
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

-- TChan: transactional channel
type Chan a = TChan a

newChan :: IO (Chan a)
newChan = newTChanIO

send :: Chan a -> a -> IO ()
send chan x = atomically (writeTChan chan x)

receive :: Chan a -> IO a
receive chan = atomically (readTChan chan)

-- ตัวอย่าง: Producer-Consumer
producer :: Chan Int -> IO ()
producer chan = forM_ [1..10] $ \i -> do
  send chan i
  putStrLn $ "Produced: " ++ show i
  threadDelay 100000

consumer :: Chan Int -> IO ()
consumer chan = replicateM_ 10 $ do
  item <- receive chan
  putStrLn $ "Consumed: " ++ show item
  threadDelay 200000

-- STM retry
-- retry :: STM a  -- ยกเลิก transaction แล้วรอจนกว่าจะมีการเปลี่ยนแปลง

waitForValue :: TVar (Maybe a) -> STM a
waitForValue var = do
  mValue <- readTVar var
  case mValue of
    Nothing -> retry  -- รอจนกว่าจะมีค่า
    Just v  -> return v

-- orElse: ลอง first ถ้า retry ลอง second
withTimeout :: Int -> STM a -> IO (Maybe a)
withTimeout micros action = do
  delay <- registerDelay micros
  atomically $
    fmap Just action `orElse` do
      expired <- readTVar delay
      if expired then return Nothing else retry
```

---

## ขั้นตอนที่ 175: Network I/O พื้นฐาน

```haskell
import Network.Socket
import Network.Socket.ByteString

-- TCP Server
startServer :: Int -> IO ()
startServer port = do
  addr <- resolve port
  sock <- open addr
  putStrLn $ "Listening on port " ++ show port
  serve sock
  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      addrInfos <- getAddrInfo (Just hints) Nothing (Just (show port))
      return (head addrInfos)
    
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock
    
    serve sock = do
      (conn, _) <- accept sock
      forkIO (handleClient conn)
      serve sock
    
    handleClient conn = do
      msg <- recv conn 1024
      send conn msg  -- echo server
      close conn

-- TCP Client
connectToServer :: String -> Int -> IO ()
connectToServer host port = do
  addr <- resolve host port
  sock <- open addr
  send sock "Hello, server!"
  response <- recv sock 1024
  putStrLn $ "Got: " ++ show response
  close sock
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addrInfos <- getAddrInfo (Just hints) (Just host) (Just (show port))
      return (head addrInfos)
    
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock
```

---

## ขั้นตอนที่ 176: HTTP Requests

```haskell
-- ใช้ http-client หรือ wreq

-- http-client
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- GET request
fetchPage :: String -> IO String
fetchPage url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ show (responseStatus response)

-- wreq (higher-level)
import Network.Wreq

getJson :: String -> IO String
getJson url = do
  r <- get url
  return (show (r ^. responseBody))

-- POST with JSON
import Data.Aeson
import Control.Lens

postData :: String -> Value -> IO String
postData url payload = do
  r <- post url (toJSON payload)
  return (show (r ^. responseStatus))

-- ตัวอย่าง: GitHub API
fetchGitHubUser :: String -> IO String
fetchGitHubUser username = do
  let url = "https://api.github.com/users/" ++ username
  r <- getWith opts url
  return (show (r ^. responseBody))
  where
    opts = defaults & header "User-Agent" .~ ["Haskell-App"]
```

---

## ขั้นตอนที่ 177: Logging

```haskell
-- ใช้ fast-logger หรือ katip สำหรับ production logging

-- Simple logging ด้วย IORef
import Data.IORef
import Data.Time

data LogLevel = DEBUG | INFO | WARN | ERROR deriving (Show, Eq, Ord)

data Logger = Logger
  { logLevel :: LogLevel
  , logHandle :: Handle
  }

newLogger :: LogLevel -> Handle -> IO Logger
newLogger level handle = return (Logger level handle)

logMessage :: Logger -> LogLevel -> String -> IO ()
logMessage logger level msg = do
  when (level >= logLevel logger) $ do
    time <- getCurrentTime
    let formatted = "[" ++ show time ++ "] [" ++ show level ++ "] " ++ msg
    hPutStrLn (logHandle logger) formatted

-- ใช้
example :: IO ()
example = do
  logger <- newLogger INFO stdout
  logMessage logger DEBUG "This won't show"
  logMessage logger INFO "Application started"
  logMessage logger WARN "Low memory"
  logMessage logger ERROR "Database connection failed"
```

---

## ขั้นตอนที่ 178: Serialization

```haskell
-- Serialization ด้วย binary package
import Data.Binary

-- Instances
instance Binary Point where
  put (Point x y) = put x >> put y
  get = Point <$> get <*> get

-- Serialize
encodePoint :: Point -> BS.ByteString
encodePoint = encode

-- Deserialize
decodePoint :: BS.ByteString -> Point
decodePoint = decode

-- ใช้ generic deriving
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Binary

data Config = Config
  { host :: String
  , port :: Int
  } deriving (Generic, Show)

instance Binary Config

-- ตัวอย่าง
saveConfig :: Config -> FilePath -> IO ()
saveConfig cfg path = BS.writeFile path (encode cfg)

loadConfig :: FilePath -> IO Config
loadConfig path = do
  bs <- BS.readFile path
  return (decode bs)

-- JSON Serialization ด้วย Aeson
import Data.Aeson

data Person = Person { name :: String, age :: Int } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person

-- Encode
personToJson :: Person -> BS.ByteString
personToJson = encode

-- Decode
jsonToPerson :: BS.ByteString -> Maybe Person
jsonToPerson = decode
```

---

## ขั้นตอนที่ 179: Testing IO Code

```haskell
-- การ test IO code

-- Mock ด้วย type class
class Monad m => MonadIO' m where
  readFile' :: FilePath -> m String
  writeFile' :: FilePath -> String -> IO ()

-- Real implementation
instance MonadIO' IO where
  readFile' = readFile

-- Mock implementation
newtype MockIO a = MockIO { runMock :: State MockState a }
  deriving (Functor, Applicative, Monad)

data MockState = MockState
  { mockFiles :: Map.Map FilePath String
  }

instance MonadIO' MockIO where
  readFile' path = MockIO $ do
    state <- get
    case Map.lookup path (mockFiles state) of
      Just content -> return content
      Nothing      -> error $ "File not found: " ++ path

-- Test
testWithMock :: MockIO a -> MockState -> a
testWithMock action state = evalState (runMock action) state

-- ตัวอย่าง
processFile :: MonadIO' m => FilePath -> m Int
processFile path = do
  content <- readFile' path
  return (length (lines content))

test1 :: Bool
test1 = testWithMock
  (processFile "test.txt")
  (MockState (Map.fromList [("test.txt", "line1\nline2\nline3")]))
  == 3
```

---

## ขั้นตอนที่ 180: สรุปและ Best Practices

### IO Best Practices

```haskell
-- 1. ใช้ withFile แทน openFile/hClose
-- 2. ใช้ Text/ByteString แทน String สำหรับ file I/O
-- 3. ใช้ bracket สำหรับ resource management
-- 4. ใช้ Async สำหรับ concurrent I/O
-- 5. Avoid lazy I/O ใน production code
-- 6. ใช้ STM สำหรับ shared mutable state

-- Project: File Statistics Tool

module FileStats where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (isAlpha, toLower)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

data FileStats = FileStats
  { filePath  :: FilePath
  , lineCount :: Int
  , wordCount :: Int
  , charCount :: Int
  , topWords  :: [(T.Text, Int)]
  } deriving (Show)

analyzeFile :: FilePath -> IO FileStats
analyzeFile path = do
  content <- TIO.readFile path
  let ls = T.lines content
      ws = concatMap T.words ls
      cleanWords = map (T.map toLower . T.filter isAlpha) ws
      nonEmpty = filter (not . T.null) cleanWords
      freqMap = foldl (\m w -> Map.insertWith (+) w 1 m) Map.empty nonEmpty
      top10 = take 10 . sortBy (comparing (Down . snd)) . Map.toList $ freqMap
  return FileStats
    { filePath  = path
    , lineCount = length ls
    , wordCount = length ws
    , charCount = T.length content
    , topWords  = top10
    }

analyzeDirectory :: FilePath -> IO [FileStats]
analyzeDirectory dir = do
  entries <- listDirectory dir
  let haskellFiles = filter (\f -> takeExtension f == ".hs") entries
      fullPaths = map (dir </>) haskellFiles
  mapM analyzeFile fullPaths

printStats :: FileStats -> IO ()
printStats stats = do
  putStrLn $ "\n=== " ++ filePath stats ++ " ==="
  putStrLn $ "Lines: " ++ show (lineCount stats)
  putStrLn $ "Words: " ++ show (wordCount stats)
  putStrLn $ "Chars: " ++ show (charCount stats)
  putStrLn "Top words:"
  mapM_ (\(w, c) -> putStrLn $ "  " ++ T.unpack w ++ ": " ++ show c) (topWords stats)

main :: IO ()
main = do
  statsList <- analyzeDirectory "src"
  mapM_ printStats statsList
  putStrLn "\n=== Summary ==="
  putStrLn $ "Total files: " ++ show (length statsList)
  putStrLn $ "Total lines: " ++ show (sum (map lineCount statsList))
  putStrLn $ "Total words: " ++ show (sum (map wordCount statsList))
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 10 เราจะเรียนเรื่อง **Monads เชิงลึก**:
- Monad definition
- do notation
- Common Monads (State, Reader, Writer)
- Monad Laws

---

*[← Part 08](part-08.md) | [Part 10 →](part-10.md)*
