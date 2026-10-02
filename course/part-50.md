# Part 50: WebAssembly & Cross-Platform
## ขั้นตอนที่ 981-1000

---

## ขั้นตอนที่ 981: Haskell to WebAssembly

```haskell
-- Compiling Haskell to WebAssembly with GHCJS and GHC-WASM

-- Using GHC-WASM (GHC 9.6+)
-- Install: ghcup install ghc 9.8.1 --set
-- Compile: wasm32-wasi-ghc -o app.wasm Main.hs

module Main where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- Export function to JavaScript
foreign export ccall "add_numbers" addNumbers :: CInt -> CInt -> CInt
addNumbers :: CInt -> CInt -> CInt
addNumbers x y = x + y

-- Access DOM via WASM imports
foreign import ccall "log_to_js" logToJS :: Ptr CChar -> IO ()

logMessage :: Text -> IO ()
logMessage msg = withCString (T.unpack msg) logToJS

-- Main entry point
main :: IO ()
main = do
  logMessage "Haskell WASM running!"
  let result = addNumbers 40 2
  logMessage ("40 + 2 = " <> T.pack (show result))
```

```javascript
// JavaScript integration
// index.js

import { WASI } from '@wasmer/wasi';
import { WasmFs } from '@wasmer/wasmfs';

const wasmFs = new WasmFs();
const wasi = new WASI({
  args: [],
  env: {},
  bindings: { ...WASI.defaultBindings, fs: wasmFs.fs }
});

// Load and run Haskell WASM
async function loadHaskell() {
  const response = await fetch('app.wasm');
  const wasmBuffer = await response.arrayBuffer();
  const wasmModule = await WebAssembly.compile(wasmBuffer);
  
  const imports = {
    wasi_snapshot_preview1: wasi.wasiImport,
    env: {
      log_to_js: (ptr) => {
        const message = readCString(memory.buffer, ptr);
        console.log('[Haskell]', message);
      }
    }
  };
  
  const instance = await WebAssembly.instantiate(wasmModule, imports);
  const { memory, add_numbers } = instance.exports;
  
  wasi.start(instance);
  
  // Call Haskell function from JavaScript
  const result = add_numbers(40, 2);
  console.log('Result from Haskell:', result);  // 42
}

loadHaskell().catch(console.error);
```

---

## ขั้นตอนที่ 982: GHCJS for Web Development

```haskell
-- GHCJS: Haskell compiled to JavaScript

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module Main where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.EventTarget
import GHCJS.DOM.HTMLButtonElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

-- DOM manipulation
main :: IO ()
main = do
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  
  -- Create elements
  heading <- createElement doc "h1"
  setTextContent heading ("Hello from Haskell!" :: Text)
  
  btn <- createElement doc "button"
  setTextContent btn ("Click me" :: Text)
  
  -- Add to DOM
  appendChild_ body heading
  appendChild_ body btn
  
  -- Event handler
  counter <- newIORef (0 :: Int)
  addEventListener btn "click" True $ do
    n <- readIORef counter
    let n' = n + 1
    writeIORef counter n'
    setTextContent heading ("Clicked " <> tshow n' <> " times!" :: Text)

-- JavaScript FFI
foreign import javascript unsafe "console.log($1)"
  js_log :: JSString -> IO ()

foreign import javascript unsafe "window.location.href"
  js_location :: IO JSString

foreign import javascript unsafe "JSON.stringify($1)"
  js_stringify :: JSVal -> IO JSString

-- Fetch API
foreign import javascript interruptible
  "fetch($1).then(r => r.text()).then($c, $c)"
  js_fetch :: JSString -> IO JSString

fetchUrl :: Text -> IO Text
fetchUrl url = fromJSString <$> js_fetch (toJSString url)
```

---

## ขั้นตอนที่ 983: Reflex-FRP Web Framework

```haskell
-- Reflex-FRP: Functional reactive web framework

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Counter where

import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map

-- Simple counter app
counterApp :: MonadWidget t m => m ()
counterApp = el "div" $ do
  el "h1" (text "Counter")
  
  -- Buttons that fire events
  increment <- button "+"
  decrement <- button "-"
  
  -- Fold events into state
  let delta = leftmost
        [ 1  <$ increment
        , -1 <$ decrement
        ]
  
  count <- foldDyn (+) (0 :: Int) delta
  
  -- Display count
  el "p" $ dynText (fmap (("Count: " <>) . tshow) count)

-- Todo list app
todoApp :: MonadWidget t m => m ()
todoApp = el "div" $ do
  el "h1" (text "Todo List")
  
  -- Input for new item
  rec
    let addEv = ffilter (not . T.null) (tag (current input) addBtn)
    input <- _inputElement_value <$>
      inputElement (def & inputElementConfig_setValue .~ ("" <$ addBtn))
    addBtn <- button "Add"
    
    -- Todo state
    todos <- foldDyn id [] $ mergeWith (.)
      [ (:) <$> addEv          -- add new item
      , filter . (/=) <$> deleteEv  -- delete item
      ]
    
    -- Display todos
    deleteEv <- el "ul" $ do
      evs <- simpleList todos $ \todo -> el "li" $ do
        dynText todo
        fmap (tag (current todo)) (button "Delete")
      return (switchDyn (leftmost <$> evs))
  
  return ()

-- Dynamic form
data FormData = FormData { fdName :: Text, fdEmail :: Text }

dynamicForm :: MonadWidget t m => m (Event t FormData)
dynamicForm = el "form" $ do
  nameInput  <- labeledInput "Name"
  emailInput <- labeledInput "Email"
  submit     <- button "Submit"
  
  let formData = FormData
        <$> current (_inputElement_value nameInput)
        <*> current (_inputElement_value emailInput)
  
  return (tag formData submit)

labeledInput :: MonadWidget t m => Text -> m (InputElement EventResult GhcjsDomSpace t)
labeledInput label = do
  el "label" (text label)
  inputElement def
```

---

## ขั้นตอนที่ 984: Miso Web Framework

```haskell
-- Miso: Elm-like Haskell web framework

{-# LANGUAGE OverloadedStrings #-}

module App where

import Miso
import Miso.String (MisoString, ms, fromMisoString)

-- Application state
data Model = Model
  { counter    :: Int
  , inputText  :: MisoString
  , todos      :: [MisoString]
} deriving (Show, Eq)

initialModel :: Model
initialModel = Model 0 "" []

-- Actions
data Action
  = Increment
  | Decrement
  | Reset
  | UpdateInput MisoString
  | AddTodo
  | DeleteTodo Int
  | NoOp
  deriving (Show, Eq)

-- Update function
update :: Action -> Model -> Effect Action Model
update Increment m = noEff m { counter = counter m + 1 }
update Decrement m = noEff m { counter = counter m - 1 }
update Reset     m = noEff m { counter = 0 }
update (UpdateInput t) m = noEff m { inputText = t }
update AddTodo m
  | inputText m == "" = noEff m
  | otherwise = noEff m
      { todos = todos m ++ [inputText m]
      , inputText = ""
      }
update (DeleteTodo i) m = noEff m
  { todos = [t | (j, t) <- zip [0..] (todos m), j /= i] }
update NoOp m = noEff m

-- View function
view :: Model -> View Action
view m = div_ []
  [ h1_ [] [text "Miso Todo App"]
  , counterView m
  , todoView m
  ]

counterView :: Model -> View Action
counterView m = div_ [class_ "counter"]
  [ h2_ [] [text "Counter"]
  , button_ [onClick Decrement] [text "-"]
  , span_   [] [text (ms (show (counter m)))]
  , button_ [onClick Increment] [text "+"]
  , button_ [onClick Reset]     [text "Reset"]
  ]

todoView :: Model -> View Action
todoView m = div_ [class_ "todo-list"]
  [ h2_ [] [text "Todos"]
  , div_ [class_ "input-group"]
      [ input_  [value_ (inputText m), onInput UpdateInput, placeholder_ "New todo"]
      , button_ [onClick AddTodo] [text "Add"]
      ]
  , ul_ [] (zipWith todoItem [0..] (todos m))
  ]

todoItem :: Int -> MisoString -> View Action
todoItem i todo = li_ []
  [ text todo
  , button_ [onClick (DeleteTodo i)] [text "×"]
  ]

-- Main
main :: IO ()
main = startApp App
  { model         = initialModel
  , update        = fromTransition . update
  , view          = view
  , events        = defaultEvents
  , subs          = []
  , mountPoint    = Nothing
  , logLevel      = Off
  }
```

---

## ขั้นตอนที่ 985: Mobile Development with Haskell

```haskell
-- Mobile development: Android/iOS with Haskell

-- Using Android NDK + Haskell
-- Platform: armv7a-linux-androideabi, aarch64-linux-android

-- iOS cross-compilation
-- Platform: aarch64-apple-ios, x86_64-apple-ios

-- React Native Bridge
{-# LANGUAGE ForeignFunctionInterface #-}

module HaskellBridge where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Control.DeepSeq (force)
import Control.Exception (evaluate)

-- Export functions callable from React Native
foreign export ccall "hs_compute" hsCompute :: CInt -> IO CInt
hsCompute :: CInt -> IO CInt
hsCompute n = do
  let result = fibonacciNaive (fromIntegral n) :: Int
  return (fromIntegral result)

foreign export ccall "hs_process_json" hsProcessJson :: CString -> IO CString
hsProcessJson :: CString -> IO CString
hsProcessJson cstr = do
  str <- peekCString cstr
  let result = processJSON (T.pack str)
  newCString (T.unpack result)

fibonacciNaive :: Int -> Int
fibonacciNaive 0 = 0
fibonacciNaive 1 = 1
fibonacciNaive n = fibonacciNaive (n-1) + fibonacciNaive (n-2)

processJSON :: Text -> Text
processJSON input = case eitherDecode (BSL.fromStrict (T.encodeUtf8 input)) of
  Left err -> T.pack ("Error: " ++ err)
  Right (v :: Value) -> T.decodeUtf8 (BSL.toStrict (encode (transformValue v)))

transformValue :: Value -> Value
transformValue (Object o) = Object (fmap transformValue o)
transformValue (Array  a) = Array  (fmap transformValue a)
transformValue (String s) = String (T.toUpper s)
transformValue v          = v
```

---

## ขั้นตอนที่ 986: Desktop GUI with Haskell

```haskell
-- Desktop GUI using gi-gtk (GTK+ bindings)

{-# LANGUAGE OverloadedStrings #-}

module GUI where

import qualified GI.Gtk as Gtk
import GI.Gtk (AttrOp(..))
import Data.IORef
import Control.Monad (void)

-- Simple GTK application
main :: IO ()
main = do
  Gtk.init Nothing
  
  -- Create window
  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.set win
    [ #title   := "Haskell GTK Application"
    , #defaultWidth  := 400
    , #defaultHeight := 300
    ]
  
  -- Create main layout
  vbox <- Gtk.boxNew Gtk.OrientationVertical 10
  Gtk.containerAdd win vbox
  
  -- Header
  label <- Gtk.labelNew (Just "Hello from Haskell GTK!")
  Gtk.boxPackStart vbox label True True 10
  
  -- Counter
  counter <- newIORef (0 :: Int)
  countLabel <- Gtk.labelNew (Just "Count: 0")
  Gtk.boxPackStart vbox countLabel False False 5
  
  -- Buttons
  hbox <- Gtk.boxNew Gtk.OrientationHorizontal 5
  Gtk.boxPackStart vbox hbox False False 5
  
  incBtn <- Gtk.buttonNewWithLabel "Increment"
  decBtn <- Gtk.buttonNewWithLabel "Decrement"
  Gtk.boxPackStart hbox incBtn True True 0
  Gtk.boxPackStart hbox decBtn True True 0
  
  -- Event handlers
  void $ Gtk.onButtonClicked incBtn $ do
    n <- modifyIORef counter (+1) >> readIORef counter
    Gtk.labelSetText countLabel ("Count: " <> tshow n)
  
  void $ Gtk.onButtonClicked decBtn $ do
    n <- modifyIORef counter (subtract 1) >> readIORef counter
    Gtk.labelSetText countLabel ("Count: " <> tshow n)
  
  -- Quit handler
  void $ Gtk.onWidgetDestroy win Gtk.mainQuit
  
  Gtk.widgetShowAll win
  Gtk.main

-- Brick TUI (Terminal User Interface)
import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center
import Graphics.Vty

data AppState = AppState
  { asCounter :: Int
  , asInput   :: Text
  }

data AppEvent = Tick

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
  where
    ui = center $ border $ vBox
      [ str "Haskell TUI Application"
      , hBorder
      , str ("Counter: " ++ show (asCounter s))
      , str ("Input: " ++ T.unpack (asInput s))
      ]

handleEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
handleEvent s (VtyEvent (EvKey KUp   [])) = continue s { asCounter = asCounter s + 1 }
handleEvent s (VtyEvent (EvKey KDown [])) = continue s { asCounter = asCounter s - 1 }
handleEvent s (VtyEvent (EvKey KEsc  [])) = halt s
handleEvent s _ = continue s

app :: App AppState AppEvent ()
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])
  }
```

---

## ขั้นตอนที่ 987: Electron + Haskell Backend

```haskell
-- Haskell backend for Electron apps

{-# LANGUAGE OverloadedStrings #-}

module ElectronBackend where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Concurrent (forkIO)
import System.Process (ProcessHandle)

-- API exposed to Electron frontend
type ElectronAPI
  = "api" :> "files" :> Get '[JSON] [FileInfo]
  :<|> "api" :> "files" :> ReqBody '[JSON] FilePath :> Post '[JSON] FileContent
  :<|> "api" :> "process" :> ReqBody '[JSON] ProcessRequest :> Post '[JSON] ProcessResult
  :<|> "api" :> "system" :> Get '[JSON] SystemInfo

data FileInfo = FileInfo
  { fiName    :: Text
  , fiPath    :: FilePath
  , fiSize    :: Int64
  , fiModified :: UTCTime
} deriving (Generic, ToJSON)

-- Server that starts with Electron
startBackend :: IO Int  -- returns port number
startBackend = do
  -- Find available port
  port <- findFreePort
  
  -- Start server
  forkIO $ do
    let app' = serve (Proxy @ElectronAPI) electronServer
    run port app'
  
  return port

electronServer :: Server ElectronAPI
electronServer =
  listFiles
  :<|> readFile'
  :<|> processFile
  :<|> getSystemInfo

listFiles :: Handler [FileInfo]
listFiles = do
  entries <- liftIO (listDirectory ".")
  fileInfos <- liftIO $ forM entries $ \e -> do
    stat <- getFileStatus e
    return FileInfo
      { fiName     = T.pack e
      , fiPath     = e
      , fiSize     = fromIntegral (fileSize stat)
      , fiModified = posixSecondsToUTCTime (modificationTime stat)
      }
  return fileInfos
```

```javascript
// Electron main.js
const { app, BrowserWindow } = require('electron');
const { spawn } = require('child_process');
const fetch = require('node-fetch');

let haskellProcess;
let backendPort;

app.whenReady().then(async () => {
  // Start Haskell backend
  haskellProcess = spawn('./backend-server', []);
  
  haskellProcess.stdout.on('data', (data) => {
    const portMatch = data.toString().match(/PORT:(\d+)/);
    if (portMatch) {
      backendPort = parseInt(portMatch[1]);
      createWindow();
    }
  });
});

async function createWindow() {
  const win = new BrowserWindow({
    width: 1200, height: 800,
    webPreferences: { nodeIntegration: false, contextIsolation: true }
  });
  
  win.loadFile('index.html');
  
  // Expose backend URL to renderer
  win.webContents.on('did-finish-load', () => {
    win.webContents.executeJavaScript(
      `window.BACKEND_URL = 'http://localhost:${backendPort}'`
    );
  });
}

app.on('before-quit', () => {
  haskellProcess?.kill();
});
```

---

## ขั้นตอนที่ 988: Portable Libraries

```haskell
-- Writing portable Haskell libraries

module Portable where

-- Use portability pragma
{-# LANGUAGE PortableFFI #-}

-- Platform-specific implementations with fallbacks
class PlatformClock a where
  getTimeNS :: IO Integer  -- nanoseconds

#ifdef linux_HOST_OS
instance PlatformClock LinuxClock where
  getTimeNS = do
    tp <- alloca $ \p -> do
      _ <- clock_gettime 1 p  -- CLOCK_MONOTONIC = 1
      peek p
    return (toNanoSecs tp)
#endif

#ifdef darwin_HOST_OS
instance PlatformClock DarwinClock where
  getTimeNS = fromIntegral <$> mach_absolute_time
#endif

-- Fallback using GHC runtime
getTimeNanoseconds :: IO Integer
getTimeNanoseconds = do
  t <- getMonotonicTimeNSec
  return (fromIntegral t)

-- Portable file system operations
readFileSafely :: FilePath -> IO (Either Text BSL.ByteString)
readFileSafely path = do
  exists <- doesFileExist path
  if not exists
    then return (Left ("File not found: " <> T.pack path))
    else try (BSL.readFile path) >>= \case
      Left (e :: IOException) -> return (Left (T.pack (show e)))
      Right content           -> return (Right content)

-- Cross-platform path handling
normalizePath :: FilePath -> FilePath
normalizePath = case currentPlatform of
  Windows -> map (\c -> if c == '/' then '\\' else c)
  _       -> map (\c -> if c == '\\' then '/' else c)

-- Portable environment variables
getEnvOptional :: Text -> IO (Maybe Text)
getEnvOptional key = do
  value <- lookupEnv (T.unpack key)
  return (fmap T.pack value)
```

---

## ขั้นตอนที่ 989: WebSocket Real-Time Apps

```haskell
-- Full-duplex WebSocket applications

{-# LANGUAGE OverloadedStrings #-}

module WebSocketApp where

import Network.WebSockets
import Control.Concurrent.STM
import Data.Map.Strict (Map)

-- Client management
type ClientId = Int
type Clients = Map ClientId Connection

data ChatServer = ChatServer
  { csClients  :: TVar Clients
  , csMessages :: TVar [ChatMessage]
  , csNextId   :: TVar ClientId
}

data ChatMessage = ChatMessage
  { cmSender    :: ClientId
  , cmContent   :: Text
  , cmTimestamp :: UTCTime
} deriving (Generic, ToJSON, FromJSON)

-- WebSocket server
chatServer :: ChatServer -> ServerApp
chatServer server pendingConn = do
  conn     <- acceptRequest pendingConn
  clientId <- atomically $ do
    i <- readTVar (csNextId server)
    writeTVar (csNextId server) (i + 1)
    modifyTVar' (csClients server) (Map.insert i conn)
    return i
  
  -- Keep alive ping
  forkPingThread conn 30
  
  -- Send message history
  history <- readTVarIO (csMessages server)
  forM_ history $ \msg -> sendTextData conn (encode msg)
  
  -- Message loop
  let loop = do
        msg <- receiveData conn :: IO BSL.ByteString
        case eitherDecode msg :: Either String ChatMessage of
          Left err -> sendTextData conn ("Error: " <> T.pack err)
          Right chatMsg -> do
            let chatMsg' = chatMsg { cmSender = clientId }
            atomically $ modifyTVar' (csMessages server) (++ [chatMsg'])
            -- Broadcast to all clients
            clients <- readTVarIO (csClients server)
            forM_ (Map.elems clients) $ \c ->
              sendTextData c (encode chatMsg') `catch` (\(_ :: ConnectionException) -> return ())
        loop
  
  loop `finally` atomically (modifyTVar' (csClients server) (Map.delete clientId))

-- Run WebSocket server
main :: IO ()
main = do
  server <- ChatServer <$> newTVarIO Map.empty <*> newTVarIO [] <*> newTVarIO 0
  runServer "0.0.0.0" 9160 (chatServer server)
```

---

## ขั้นตอนที่ 990: Progressive Web Apps (PWA)

```haskell
-- Service worker via JavaScript interop

-- Generate service worker JavaScript
generateServiceWorker :: [Text] -> Text
generateServiceWorker routes = T.unlines
  [ "const CACHE_NAME = 'v1';"
  , "const URLS_TO_CACHE = " <> encode routes <> ";"
  , ""
  , "self.addEventListener('install', event => {"
  , "  event.waitUntil("
  , "    caches.open(CACHE_NAME)"
  , "      .then(cache => cache.addAll(URLS_TO_CACHE))"
  , "  );"
  , "});"
  , ""
  , "self.addEventListener('fetch', event => {"
  , "  event.respondWith("
  , "    caches.match(event.request)"
  , "      .then(response => response || fetch(event.request))"
  , "  );"
  , "});"
  ]

-- Web App Manifest
data Manifest = Manifest
  { mName            :: Text
  , mShortName       :: Text
  , mStartUrl        :: Text
  , mDisplay         :: Text
  , mThemeColor      :: Text
  , mBackgroundColor :: Text
  , mIcons           :: [Icon]
} deriving (Generic, ToJSON)

data Icon = Icon
  { iconSrc     :: Text
  , iconSizes   :: Text
  , iconType    :: Text
} deriving (Generic, ToJSON)

appManifest :: Manifest
appManifest = Manifest
  { mName            = "Haskell PWA"
  , mShortName       = "HaskellApp"
  , mStartUrl        = "/"
  , mDisplay         = "standalone"
  , mThemeColor      = "#4a90e2"
  , mBackgroundColor = "#ffffff"
  , mIcons           =
      [ Icon "/icons/icon-72x72.png"  "72x72"   "image/png"
      , Icon "/icons/icon-96x96.png"  "96x96"   "image/png"
      , Icon "/icons/icon-192x192.png" "192x192" "image/png"
      , Icon "/icons/icon-512x512.png" "512x512" "image/png"
      ]
  }

-- Servant endpoint for PWA assets
type PWAApi
  = "manifest.json"  :> Get '[JSON]   Manifest
  :<|> "sw.js"        :> Get '[OctetStream] Text
  :<|> "offline.html" :> Get '[HTML] Html

pwaServer :: Server PWAApi
pwaServer =
  return appManifest
  :<|> return (generateServiceWorker ["/", "/about", "/contact"])
  :<|> return offlinePage
```

---

## ขั้นตอนที่ 991: WASM Component Model

```haskell
-- WebAssembly Component Model (WIT interfaces)

-- component.wit (WebAssembly Interface Types)
{- 
package example:calculator@0.1.0;

interface math {
  add: func(a: s64, b: s64) -> s64;
  multiply: func(a: s64, b: s64) -> s64;
  fibonacci: func(n: u32) -> u64;
}

world calculator {
  export math;
}
-}

-- Implement WIT interface in Haskell
module Calculator where

-- Add (exported to WASM)
foreign export ccall "add" add :: Int64 -> Int64 -> Int64
add :: Int64 -> Int64 -> Int64
add = (+)

foreign export ccall "multiply" multiply :: Int64 -> Int64 -> Int64
multiply :: Int64 -> Int64 -> Int64
multiply = (*)

foreign export ccall "fibonacci" fibonacci :: Word32 -> Word64
fibonacci :: Word32 -> Word64
fibonacci n = go (fromIntegral n)
  where
    go 0 = 0
    go 1 = 1
    go k = go (k-1) + go (k-2)

-- Host imports from WIT
foreign import ccall "host_log" hostLog :: Ptr CChar -> IO ()

logMessage :: Text -> IO ()
logMessage msg = withCString (T.unpack msg) hostLog
```

---

## ขั้นตอนที่ 992: Full-Stack Type Safety

```haskell
-- Sharing types between frontend (Miso/GHCJS) and backend (Servant)

-- Shared types module (compiled for both targets)
module Shared.Types where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

-- Shared data types
data User = User
  { userId    :: Int
  , userName  :: Text
  , userEmail :: Text
  , userRole  :: Role
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Role = Admin | Editor | Viewer
  deriving (Show, Eq, Enum, Bounded, Generic, ToJSON, FromJSON)

data ApiResponse a = ApiResponse
  { respData    :: Maybe a
  , respError   :: Maybe Text
  , respSuccess :: Bool
} deriving (Show, Generic, ToJSON, FromJSON)

-- Servant API type (shared between client and server)
module Shared.API where

import Servant.API
import Shared.Types

type UserAPI
  = "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "users" :> Capture "id" Int :> ReqBody '[JSON] User :> Put '[JSON] User

-- Backend server
module Server where
import Shared.API
import Servant

userServer :: Server UserAPI
userServer = getAllUsers :<|> getUser :<|> createUser :<|> updateUser

-- Frontend client (GHCJS/Miso)
module Client where
import Shared.API
import Servant.Client.JS  -- GHCJS-specific client

getAllUsersClient :: ClientM [User]
getUserClient :: Int -> ClientM User
createUserClient :: User -> ClientM User
updateUserClient :: Int -> User -> ClientM User
(getAllUsersClient :<|> getUserClient :<|> createUserClient :<|> updateUserClient)
  = client (Proxy @UserAPI)
```

---

## ขั้นตอนที่ 993: Native Modules & FFI

```haskell
-- Foreign Function Interface deep dive

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

module FFI where

import Foreign
import Foreign.C
import Foreign.C.Types
import System.IO.Unsafe

-- C types
type CDouble = C'double
type CSize   = C'size_t

-- Import C functions
foreign import ccall "math.h sin"  c_sin  :: CDouble -> CDouble
foreign import ccall "math.h cos"  c_cos  :: CDouble -> CDouble
foreign import ccall "string.h strlen" c_strlen :: CString -> IO CSize
foreign import ccall "stdlib.h malloc" c_malloc :: CSize -> IO (Ptr ())
foreign import ccall "stdlib.h free"   c_free   :: Ptr () -> IO ()

-- Safe wrapper for C functions
sin' :: Double -> Double
sin' = realToFrac . c_sin . realToFrac

-- Using foreign structs
data CVector = CVector { cvX :: CDouble, cvY :: CDouble, cvZ :: CDouble }

instance Storable CVector where
  sizeOf    _ = sizeOf (undefined :: CDouble) * 3
  alignment _ = alignment (undefined :: CDouble)
  
  peek ptr = do
    x <- peekElemOff (castPtr ptr) 0
    y <- peekElemOff (castPtr ptr) 1
    z <- peekElemOff (castPtr ptr) 2
    return (CVector x y z)
  
  poke ptr (CVector x y z) = do
    pokeElemOff (castPtr ptr) 0 x
    pokeElemOff (castPtr ptr) 1 y
    pokeElemOff (castPtr ptr) 2 z

-- Using with alloca
dotProduct :: (Double, Double, Double) -> (Double, Double, Double) -> IO Double
dotProduct (x1,y1,z1) (x2,y2,z2) = alloca $ \v1ptr -> alloca $ \v2ptr -> do
  poke v1ptr (CVector (realToFrac x1) (realToFrac y1) (realToFrac z1))
  poke v2ptr (CVector (realToFrac x2) (realToFrac y2) (realToFrac z2))
  result <- c_dot_product v1ptr v2ptr
  return (realToFrac result)

foreign import ccall "dot_product" c_dot_product :: Ptr CVector -> Ptr CVector -> IO CDouble
```

---

## ขั้นตอนที่ 994: WebRTC & P2P Browser Apps

```haskell
-- WebRTC signaling server in Haskell

module WebRTCSignaling where

import Network.WebSockets
import Control.Concurrent.STM

data Room = Room
  { roomId      :: Text
  , roomPeers   :: TVar (Map Text Connection)
}

data Rooms = Rooms
  { rooms :: TVar (Map Text Room)
}

data SignalMessage
  = Join      { msgRoom :: Text, msgPeerId :: Text }
  | Leave     { msgRoom :: Text, msgPeerId :: Text }
  | Offer     { msgFrom :: Text, msgTo :: Text, msgSdp :: Text }
  | Answer    { msgFrom :: Text, msgTo :: Text, msgSdp :: Text }
  | Candidate { msgFrom :: Text, msgTo :: Text, msgCandidate :: Text }
  deriving (Generic, FromJSON, ToJSON)

-- Handle WebRTC signaling
handleSignaling :: Rooms -> Connection -> IO ()
handleSignaling rooms conn = do
  peerId <- nextPeerId
  
  let loop currentRoom = do
        msgBytes <- receiveData conn :: IO BSL.ByteString
        case eitherDecode msgBytes of
          Left err  -> sendTextData conn ("Error: " <> err)
          Right msg -> case msg of
            Join roomId' _ -> do
              room <- getOrCreateRoom rooms roomId'
              atomically (modifyTVar' (roomPeers room) (Map.insert peerId conn))
              -- Notify others
              notifyOthers room peerId (toJSON msg)
              loop (Just room)
            
            Leave roomId' _ -> do
              mapM_ (\r -> atomically (modifyTVar' (roomPeers r) (Map.delete peerId))) currentRoom
              loop Nothing
            
            Offer  { msgTo } -> forwardTo rooms msgTo (toJSON msg)    >> loop currentRoom
            Answer { msgTo } -> forwardTo rooms msgTo (toJSON msg)    >> loop currentRoom
            Candidate { msgTo } -> forwardTo rooms msgTo (toJSON msg) >> loop currentRoom
  
  loop Nothing

forwardTo :: Rooms -> Text -> Value -> IO ()
forwardTo rooms targetId msg = do
  rs  <- readTVarIO (rooms rooms)
  let connections = concatMap (Map.elems . unsafePerformIO . readTVarIO . roomPeers) (Map.elems rs)
  -- simplified: would need to track peerId -> connection mapping
  return ()
```

---

## ขั้นตอนที่ 995: Offline-First Architecture

```haskell
-- Offline-first Haskell + WASM application

module OfflineFirst where

-- Local storage API via WASM
foreign import javascript "localStorage.setItem($1, $2)" jsSetItem :: JSString -> JSString -> IO ()
foreign import javascript "localStorage.getItem($1)"     jsGetItem :: JSString -> IO (Nullable JSString)
foreign import javascript "localStorage.removeItem($1)"  jsRemoveItem :: JSString -> IO ()

-- Offline data store
class LocalStore m where
  lsSet    :: Text -> Text -> m ()
  lsGet    :: Text -> m (Maybe Text)
  lsDelete :: Text -> m ()
  lsKeys   :: m [Text]

-- WASM implementation
instance LocalStore IO where
  lsSet k v = jsSetItem (toJSString k) (toJSString v)
  lsGet k   = do
    v <- jsGetItem (toJSString k)
    return (fmap fromJSString (nullableToMaybe v))
  lsDelete k = jsRemoveItem (toJSString k)
  lsKeys     = return []  -- simplified

-- Sync queue for offline operations
data SyncOp
  = SyncCreate Text Value  -- collection, data
  | SyncUpdate Text Text Value  -- collection, id, data
  | SyncDelete Text Text  -- collection, id

data SyncQueue = SyncQueue
  { sqOps :: IORef [SyncOp]
}

enqueue :: SyncQueue -> SyncOp -> IO ()
enqueue q op = do
  modifyIORef' (sqOps q) (++ [op])
  serializeSyncQueue q  -- persist to localStorage

syncWithServer :: SyncQueue -> Text -> IO ()
syncWithServer q serverUrl = do
  ops <- readIORef (sqOps q)
  when (not (null ops)) $ do
    result <- try (sendToServer serverUrl ops) :: IO (Either SomeException ())
    case result of
      Right () -> writeIORef (sqOps q) []
      Left err -> logError err  -- retry later
```

---

## ขั้นตอนที่ 996: Browser Extensions

```haskell
-- Browser extension with Haskell/GHCJS backend

-- Content script (runs in page context)
module ContentScript where

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element

-- Inject functionality into any webpage
main :: IO ()
main = do
  doc  <- currentDocumentUnchecked
  
  -- Listen for messages from background script
  addEventListener (window doc) "message" True $ \(e :: MessageEvent) -> do
    let data' = getData e
    handleMessage data'
  
  -- Send page data to background
  let pageData = collectPageData doc
  postMessage window pageData

-- Background service worker
module BackgroundScript where

import GHCJS.Foreign.Callback

-- Handle extension events
setupHandlers :: IO ()
setupHandlers = do
  -- Intercept network requests
  onBeforeRequest addListener $ \details -> do
    let url = reqUrl details
    blocked <- shouldBlock url
    return (if blocked then BlockRequest else AllowRequest)
  
  -- Context menu
  addContextMenu "Analyze with Haskell" $ \info tab -> do
    text <- getSelectedText info
    result <- analyzeText text
    showBadge (tshow (length result))

-- Extension popup (HTML + GHCJS)
module Popup where

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  
  -- Get current tab info from background
  tab  <- getCurrentTab
  data' <- fetchPageAnalysis (tabUrl tab)
  
  -- Display results
  container <- getElementById doc "results"
  forM_ data' $ \item -> do
    el <- createElement doc "div"
    setTextContent el (displayItem item)
    appendChild_ container el
```

---

## ขั้นตอนที่ 997: Edge Computing

```haskell
-- Haskell on edge (Cloudflare Workers-like via WASM)

module EdgeFunction where

-- Edge function type
type EdgeHandler = EdgeRequest -> IO EdgeResponse

data EdgeRequest = EdgeRequest
  { edgeMethod  :: Text
  , edgePath    :: Text
  , edgeHeaders :: Map Text Text
  , edgeBody    :: Maybe BSL.ByteString
  , edgeCf      :: CloudflareData  -- CF-specific
}

data EdgeResponse = EdgeResponse
  { edgeStatus  :: Int
  , edgeRespHeaders :: Map Text Text
  , edgeRespBody :: BSL.ByteString
}

data CloudflareData = CloudflareData
  { cfCountry :: Text
  , cfCity    :: Text
  , cfColo    :: Text  -- datacenter
}

-- Handler
handler :: EdgeHandler
handler req = case edgeMethod req of
  "GET" -> handleGet req
  "POST" -> handlePost req
  _     -> return (EdgeResponse 405 Map.empty "Method not allowed")

handleGet :: EdgeRequest -> IO EdgeResponse
handleGet req = case edgePath req of
  "/" -> return (EdgeResponse 200
    (Map.singleton "Content-Type" "application/json")
    (encode (object ["message" .= ("Hello from Haskell edge!" :: Text)
                    ,"country" .= cfCountry (edgeCf req)])))
  
  "/headers" -> return (EdgeResponse 200
    (Map.singleton "Content-Type" "application/json")
    (encode (edgeHeaders req)))
  
  _ -> return (EdgeResponse 404 Map.empty "Not found")

-- Compile with:
-- wasm32-wasi-ghc -o handler.wasm EdgeFunction.hs
-- Then deploy to Cloudflare Workers
```

---

## ขั้นตอนที่ 998: Performance Optimization for WASM

```haskell
-- WASM performance optimization

module WasmOpt where

-- Avoid heap allocation in hot paths
-- Use unboxed types and strict evaluation

-- Tight loop with no allocation
sumArray :: Ptr Word64 -> Int -> IO Word64
sumArray ptr n = go 0 0
  where
    go !acc !i
      | i >= n    = return acc
      | otherwise = do
          v <- peekElemOff ptr i
          go (acc + v) (i + 1)

-- SIMD-like operations via foreign imports
foreign import ccall unsafe "simd_dot_product"
  simdDotProduct :: Ptr Float -> Ptr Float -> Int -> IO Float

-- Manual memory management for performance
withTemporaryBuffer :: Int -> (Ptr Word8 -> IO a) -> IO a
withTemporaryBuffer size action = do
  ptr <- mallocBytes size
  result <- action ptr
  free ptr
  return result

-- Streaming data processing
processStreamWasm :: [Word8] -> IO BSL.ByteString
processStreamWasm input = do
  let n = length input
  withTemporaryBuffer n $ \inputBuf ->
    withTemporaryBuffer (n * 2) $ \outputBuf -> do
      pokeArray inputBuf input
      outLen <- process_data inputBuf (fromIntegral n) outputBuf
      result <- peekArray (fromIntegral outLen) outputBuf
      return (BSL.pack result)

foreign import ccall "process_data"
  process_data :: Ptr Word8 -> CInt -> Ptr Word8 -> IO CInt

-- Optimize GC pressure in WASM
-- Use IntMap/HashMap over Map
-- Use Vector over List for numeric data
-- Use ByteString over String

numericOps :: V.Vector Double -> Double
numericOps v = V.foldl' (+) 0 v + V.minimum v + V.maximum v
```

---

## ขั้นตอนที่ 999: Full-Stack Application

```haskell
-- Complete full-stack Haskell application
-- Backend: Servant API
-- Frontend: Miso (GHCJS/WASM)
-- Database: PostgreSQL
-- Realtime: WebSockets

-- shared/Types.hs
module Shared.Types where

data Post = Post
  { postId      :: Int
  , postTitle   :: Text
  , postContent :: Text
  , postAuthor  :: Text
  , postCreated :: UTCTime
} deriving (Show, Generic, ToJSON, FromJSON)

data Comment = Comment
  { commentId     :: Int
  , commentPostId :: Int
  , commentText   :: Text
  , commentAuthor :: Text
} deriving (Show, Generic, ToJSON, FromJSON)

-- backend/API.hs
type BlogAPI
  = "posts"    :> Get '[JSON] [Post]
  :<|> "posts" :> Capture "id" Int :> Get '[JSON] Post
  :<|> "posts" :> ReqBody '[JSON] Post :> Post '[JSON] Post
  :<|> "posts" :> Capture "id" Int :> "comments" :> Get '[JSON] [Comment]
  :<|> "ws"    :> WebSocket

-- backend/Server.hs
module Backend.Server where

runApp :: IO ()
runApp = do
  db <- initDb
  ws <- initWebSocket
  
  let srv = serve (Proxy @BlogAPI)
              (listPosts db :<|> getPost db :<|> createPost db ws :<|> getComments db :<|> wsHandler ws)
  
  run 8080 srv

-- frontend/App.hs (GHCJS/Miso)
module Frontend.App where

data Model = Model
  { posts          :: [Post]
  , selectedPost   :: Maybe Post
  , wsConnection   :: Maybe WebSocket
  , loading        :: Bool
  , newPostTitle   :: Text
  , newPostContent :: Text
}

data Action
  = LoadPosts
  | PostsLoaded [Post]
  | SelectPost Post
  | CreatePost
  | PostCreated Post
  | WebSocketMessage Text
  | Noop

update :: Action -> Model -> Effect Action Model
update LoadPosts m = m { loading = True } <# do
  posts <- fetchPosts
  return (PostsLoaded posts)
update (PostsLoaded ps) m = noEff m { posts = ps, loading = False }
update (SelectPost p)  m = noEff m { selectedPost = Just p }
update CreatePost m = m <# do
  let newPost = Post 0 (newPostTitle m) (newPostContent m) "user" (UTCTime undefined 0)
  created <- submitPost newPost
  return (PostCreated created)
update (PostCreated p) m = noEff m { posts = p : posts m }
update Noop m = noEff m

view :: Model -> View Action
view m = div_ [class_ "app"]
  [ header_ [] [h1_ [] [text "Haskell Blog"]]
  , main_ []
      [ sidebar m
      , content m
      ]
  ]
```

---

## ขั้นตอนที่ 1000: โปรเจกต์สุดท้าย: World-Class Haskell Application

```haskell
-- Step 1000: The Complete World-Class Haskell Application
-- Combining everything learned in this course

module WorldClassApp where

{- This final project demonstrates mastery of:
   ✓ Type-safe REST API with Servant
   ✓ Full-stack development with Miso/GHCJS
   ✓ Real-time WebSockets
   ✓ Database integration (PostgreSQL)
   ✓ Authentication & Authorization
   ✓ Caching (Redis)
   ✓ Distributed systems patterns
   ✓ ML/AI integration
   ✓ Performance optimization
   ✓ Type-level programming
   ✓ Effect systems (Polysemy)
   ✓ Formal verification (Liquid Haskell)
   ✓ Meta-programming (Template Haskell)
   ✓ Advanced concurrency (STM, actors)
   ✓ WebAssembly deployment
   ✓ Nix infrastructure
-}

-- Application configuration
data AppConfig = AppConfig
  { cfgDb           :: DatabaseConfig
  , cfgRedis        :: RedisConfig
  , cfgAuth         :: AuthConfig
  , cfgML           :: MLConfig
  , cfgPort         :: Int
  , cfgEnvironment  :: Environment
} deriving (Show, Generic, FromJSON)

data Environment = Development | Staging | Production
  deriving (Show, Generic, FromJSON)

-- Main application type using Polysemy effects
type AppEffects =
  '[ Database
   , Cache
   , Auth
   , MLService
   , Logger
   , Metrics
   , Error AppError
   , Embed IO
   ]

type App a = Sem AppEffects a

-- API combining all features
type MasterAPI
  = AuthAPI
  :<|> UsersAPI
  :<|> ContentAPI
  :<|> AnalyticsAPI
  :<|> MLAPI
  :<|> WebSocketAPI
  :<|> AdminAPI
  :<|> HealthAPI

-- Main entry point
main :: IO ()
main = do
  cfg <- loadConfig
  
  -- Initialize all services
  db       <- initDb (cfgDb cfg)
  redis    <- initRedis (cfgRedis cfg)
  mlClient <- initML (cfgML cfg)
  metrics  <- initMetrics
  logger   <- initLogger (cfgEnvironment cfg)
  
  let interpreters = runDatabase db
                   . runCache redis
                   . runAuth (cfgAuth cfg)
                   . runMLService mlClient
                   . runLogger logger
                   . runMetrics metrics
                   . runError
  
  let server = hoistServer (Proxy @MasterAPI)
                 (liftIO . interpreters . runM)
                 masterServer
  
  let app = cors (const corsPolicy) . requestLogger logger
          $ serve (Proxy @MasterAPI) server
  
  logInfo logger "World-Class Haskell Application Starting..."
  logInfo logger ("Environment: " <> tshow (cfgEnvironment cfg))
  logInfo logger ("Port: " <> tshow (cfgPort cfg))
  
  -- Run with graceful shutdown
  withAsync (run (cfgPort cfg) app) $ \serverThread -> do
    -- Setup shutdown handler
    setupGracefulShutdown serverThread db redis
    wait serverThread

-- The journey: 1000 steps from beginner to world-class
-- ขั้นตอนที่ 1-1000 สำเร็จแล้ว!
-- คุณได้เรียนรู้ Haskell ตั้งแต่พื้นฐานจนถึงระดับโลก!
--
-- Topics covered:
-- Steps 1-40:    Haskell basics (types, functions, typeclasses)
-- Steps 41-80:   IO, error handling, concurrency
-- Steps 81-120:  Type classes advanced, Functor/Applicative/Monad
-- Steps 121-160: Servant REST API basics
-- Steps 161-200: Servant advanced (auth, middleware, OpenAPI)
-- Steps 201-240: Database (PostgreSQL, SQLite)
-- Steps 241-280: Yesod framework
-- Steps 281-320: Testing (HUnit, QuickCheck, Hedgehog)
-- Steps 321-360: Production patterns
-- Steps 361-400: Advanced types (GADTs, DataKinds, TypeFamilies)
-- Steps 401-440: Performance and profiling
-- Steps 441-480: Parsers and DSLs
-- Steps 481-520: Category theory for programmers
-- Steps 521-560: Distributed systems
-- Steps 561-600: Microservices
-- Steps 601-640: Security and authentication
-- Steps 641-680: Cloud deployment
-- Steps 681-720: Monitoring and observability
-- Steps 721-760: Architecture patterns
-- Steps 761-780: ML/AI integration
-- Steps 781-800: Advanced algorithms
-- Steps 801-820: Compiler design
-- Steps 821-840: Distributed systems advanced
-- Steps 841-860: Formal verification
-- Steps 861-880: Performance engineering
-- Steps 881-900: Meta-programming
-- Steps 901-920: DSL design
-- Steps 921-940: Concurrency patterns
-- Steps 941-960: Web3 and blockchain
-- Steps 961-980: Nix and build systems
-- Steps 981-1000: WebAssembly and cross-platform
```

---

## สรุป: เส้นทางสู่การเป็น Haskell Expert

**หลักสูตรนี้ครอบคลุม 1000 ขั้นตอนที่พาคุณจากผู้เริ่มต้นสู่ระดับโลก:**

### ทักษะที่ได้รับ
1. **ภาษา Haskell** — Syntax, Types, Type Classes, Advanced Types
2. **Servant Framework** — Type-safe REST APIs, middleware, authentication  
3. **Yesod Framework** — Full-stack web development
4. **Database** — PostgreSQL, SQLite, connection pooling
5. **Concurrency** — STM, async, actors, work-stealing
6. **Distributed Systems** — Raft, CRDTs, gossip protocols
7. **ML/AI** — Neural networks, NLP, RAG, OpenAI API
8. **Algorithms** — Red-Black Trees, Dijkstra, KMP, Bloom filters
9. **Compiler Design** — Lexers, parsers, type inference, LLVM
10. **Formal Verification** — Liquid Haskell, dependent types
11. **Performance** — Profiling, cache optimization, SIMD
12. **Meta-programming** — Template Haskell, GHC.Generics
13. **Web3** — Blockchain, smart contracts, DeFi
14. **Cross-platform** — WASM, mobile, desktop, PWA
15. **Infrastructure** — Nix, Docker, CI/CD, deployment

**ยินดีด้วย! คุณสำเร็จหลักสูตร Haskell ระดับโลก! 🎓**

*[← Part 49](part-49.md)*
