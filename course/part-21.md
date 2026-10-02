# Part 21: Advanced Haskell Patterns
## ขั้นตอนที่ 401-420: Free Monads และ Effect Systems

---

## ขั้นตอนที่ 401: Free Monads

```haskell
-- Free Monad: แยก description ออกจาก interpretation

{-# LANGUAGE DeriveFunctor #-}

import Control.Monad.Free

-- DSL functor
data FileOp next
  = ReadFile  FilePath (String -> next)
  | WriteFile FilePath String next
  | DeleteFile FilePath next
  | ListDir    FilePath ([FilePath] -> next)
  deriving Functor

-- Free monad type
type FileM = Free FileOp

-- Smart constructors
readFile' :: FilePath -> FileM String
readFile' path = liftF (ReadFile path id)

writeFile' :: FilePath -> String -> FileM ()
writeFile' path content = liftF (WriteFile path content ())

deleteFile' :: FilePath -> FileM ()
deleteFile' path = liftF (DeleteFile path ())

listDir' :: FilePath -> FileM [FilePath]
listDir' path = liftF (ListDir path id)

-- Program using the DSL
processFiles :: FileM ()
processFiles = do
  files <- listDir' "/tmp"
  forM_ files $ \f -> do
    content <- readFile' f
    let processed = map toUpper content
    writeFile' (f ++ ".processed") processed

-- Real interpreter
runFileIO :: FileM a -> IO a
runFileIO (Pure a) = return a
runFileIO (Free op) = case op of
  ReadFile path next -> do
    content <- Prelude.readFile path
    runFileIO (next content)
  WriteFile path content next -> do
    Prelude.writeFile path content
    runFileIO next
  DeleteFile path next -> do
    removeFile path
    runFileIO next
  ListDir path next -> do
    files <- getDirectoryContents path
    runFileIO (next files)

-- Test interpreter (pure)
runFilePure :: Map FilePath String -> FileM a -> (a, Map FilePath String)
runFilePure fs (Pure a) = (a, fs)
runFilePure fs (Free op) = case op of
  ReadFile path next ->
    let content = fromMaybe "" (Map.lookup path fs)
    in runFilePure fs (next content)
  WriteFile path content next ->
    runFilePure (Map.insert path content fs) next
  DeleteFile path next ->
    runFilePure (Map.delete path fs) next
  ListDir path next ->
    let files = Map.keys (Map.filterWithKey (\k _ -> takeDirectory k == path) fs)
    in runFilePure fs (next files)
```

---

## ขั้นตอนที่ 402: Freer Monad

```haskell
-- Freer Monad: แก้ปัญหา Free Monad (Functor constraint)

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Control.Monad.Freer.Error

-- Effects
data Database r where
  Query  :: Text -> Database [Row]
  Insert :: Table -> Row -> Database Key
  Update :: Table -> Key -> Row -> Database ()
  Delete :: Table -> Key -> Database ()

data Logger r where
  Log :: LogLevel -> Text -> Logger ()

-- Smart constructors
query :: Member Database effs => Text -> Eff effs [Row]
query sql = send (Query sql)

logMsg :: Member Logger effs => LogLevel -> Text -> Eff effs ()
logMsg level msg = send (Log level msg)

-- Program
fetchUsers :: (Member Database effs, Member Logger effs) => Eff effs [User]
fetchUsers = do
  logMsg Info "Fetching all users"
  rows <- query "SELECT * FROM users"
  logMsg Info $ "Found " <> T.pack (show (length rows)) <> " users"
  return (map rowToUser rows)

-- Interpreters
runDatabase :: Eff (Database ': effs) a -> Eff effs a
runDatabase = interpret $ \case
  Query sql       -> liftIO $ executeQuery sql
  Insert t row    -> liftIO $ executeInsert t row
  Update t k row  -> liftIO $ executeUpdate t k row
  Delete t k      -> liftIO $ executeDelete t k

runLogger :: Eff (Logger ': effs) a -> Eff effs a
runLogger = interpret $ \case
  Log level msg -> liftIO $ logToFile level msg

-- Pure interpreter for testing
runTestDatabase :: [Row] -> Eff (Database ': effs) a -> Eff effs a
runTestDatabase testRows = interpret $ \case
  Query _   -> return testRows
  Insert _ _ -> return (toSqlKey 1)
  Update _ _ _ -> return ()
  Delete _ _ -> return ()
```

---

## ขั้นตอนที่ 403: Effect System ด้วย Polysemy

```haskell
-- polysemy: type-safe effects

{-# LANGUAGE TemplateHaskell #-}

import Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Error
import Polysemy.Output

-- Define effects
data Cache m a where
  CacheGet :: Text -> Cache m (Maybe Text)
  CacheSet :: Text -> Text -> Cache m ()
  CacheDelete :: Text -> Cache m ()

makeSem ''Cache  -- Template Haskell สร้าง smart constructors

-- Effect combination
type AppEffects = '[Cache, State AppState, Reader AppConfig, Error AppError, Embed IO]

-- Program using effects
getOrFetch :: Members AppEffects r => Text -> Sem r Text
getOrFetch key = do
  mCached <- cacheGet key
  case mCached of
    Just val -> return val
    Nothing  -> do
      val <- embed (fetchFromDB key)
      cacheSet key val
      return val

-- Interpreter: Redis
runCacheRedis :: Member (Embed IO) r => Redis.Connection -> Sem (Cache ': r) a -> Sem r a
runCacheRedis conn = interpret $ \case
  CacheGet key -> embed $ do
    result <- Redis.runRedis conn (Redis.get (encodeUtf8 key))
    return $ case result of
      Right (Just v) -> Just (decodeUtf8 v)
      _              -> Nothing
  CacheSet key val -> embed $ do
    Redis.runRedis conn $ Redis.setex (encodeUtf8 key) 3600 (encodeUtf8 val)
    return ()
  CacheDelete key -> embed $ do
    Redis.runRedis conn $ Redis.del [encodeUtf8 key]
    return ()

-- Interpreter: In-memory (for testing)
runCacheMap :: Sem (Cache ': r) a -> Sem r a
runCacheMap = evalState Map.empty . reinterpret $ \case
  CacheGet key -> gets (Map.lookup key)
  CacheSet key val -> modify (Map.insert key val)
  CacheDelete key -> modify (Map.delete key)

-- Run the full application
runApp :: AppConfig -> Redis.Connection -> Sem AppEffects a -> IO (Either AppError a)
runApp config redisConn =
  runM
  . runError
  . runReader config
  . evalState initialState
  . runCacheRedis redisConn
```

---

## ขั้นตอนที่ 404: Template Haskell

```haskell
-- Template Haskell: metaprogramming

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Simple TH macro
hello :: Q Exp
hello = stringE "Hello, Template Haskell!"

-- Usage: $(hello)  -- สร้าง "Hello, Template Haskell!"

-- Generate getter functions
makeGetters :: Name -> Q [Dec]
makeGetters typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
      mapM makeGetter fields
    _ -> fail "Expected a record type"
  where
    makeGetter (fieldName, _, fieldType) = do
      let getterName = mkName (nameBase fieldName ++ "'")
      return $ FunD getterName
        [ Clause
          [VarP (mkName "x")]
          (NormalB (AppE (VarE fieldName) (VarE (mkName "x"))))
          []
        ]

-- Data type
data Person = Person
  { personName :: Text
  , personAge  :: Int
  } deriving Show

-- Generate getters
$(makeGetters ''Person)
-- สร้าง:
-- personName' :: Person -> Text
-- personAge'  :: Person -> Int

-- Quasi-quoter
sql :: QuasiQuoter
sql = QuasiQuoter
  { quoteExp  = \s -> [| runQuery $(stringE s) |]
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

-- Usage: [sql| SELECT * FROM users |]

-- Type-safe printf
printf :: String -> Q Exp
printf fmt = do
  let parts = parseFmt fmt
  buildExpr parts
  where
    parseFmt = ...
    buildExpr = ...
```

---

## ขั้นตอนที่ 405: Generic Programming ด้วย GHC.Generics

```haskell
-- Generic programming

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}

import GHC.Generics

-- Custom type class ที่รองรับ Generic
class Serialize a where
  serialize :: a -> Text
  deserialize :: Text -> Maybe a
  
  default serialize :: (Generic a, GSerialize (Rep a)) => a -> Text
  serialize = gSerialize . from
  
  default deserialize :: (Generic a, GSerialize (Rep a)) => Text -> Maybe a
  deserialize = fmap to . gDeserialize

class GSerialize f where
  gSerialize :: f a -> Text
  gDeserialize :: Text -> Maybe (f a)

-- Generic instances
instance GSerialize V1 where
  gSerialize _ = ""
  gDeserialize _ = Nothing

instance GSerialize U1 where
  gSerialize _ = "()"
  gDeserialize "()" = Just U1
  gDeserialize _ = Nothing

instance (GSerialize f, GSerialize g) => GSerialize (f :*: g) where
  gSerialize (x :*: y) = gSerialize x <> "," <> gSerialize y
  gDeserialize s = do
    let (sx, sy) = T.breakOn "," s
    x <- gDeserialize sx
    y <- gDeserialize (T.drop 1 sy)
    return (x :*: y)

-- Data types using Generic
data Color = Red | Green | Blue
  deriving (Show, Eq, Generic)

instance Serialize Color

data Point = Point { px :: Double, py :: Double }
  deriving (Show, Eq, Generic)

instance Serialize Point

-- สามารถ serialize/deserialize ได้อัตโนมัติ
-- serialize (Point 1.0 2.0)  -> "1.0,2.0"
-- deserialize "3.0,4.0" :: Maybe Point -> Just (Point 3.0 4.0)
```

---

## ขั้นตอนที่ 406: Type-Safe Builder Pattern

```haskell
-- Builder pattern ด้วย type-level state

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Type-level tags
data Present
data Missing

-- HTTP Request builder
data Request (hasUrl :: *) (hasMethod :: *) (hasBody :: *) = Request
  { reqUrl     :: Maybe Text
  , reqMethod  :: Maybe Method
  , reqBody    :: Maybe ByteString
  , reqHeaders :: [(Text, Text)]
  }

emptyRequest :: Request Missing Missing Missing
emptyRequest = Request Nothing Nothing Nothing []

setUrl :: Text -> Request Missing m b -> Request Present m b
setUrl url req = req { reqUrl = Just url }

setMethod :: Method -> Request u Missing b -> Request u Present b
setMethod method req = req { reqMethod = Just method }

setBody :: ByteString -> Request u m Missing -> Request u m Present
setBody body req = req { reqBody = Just body }

addHeader :: Text -> Text -> Request u m b -> Request u m b
addHeader k v req = req { reqHeaders = (k,v) : reqHeaders req }

-- Can only send when url and method are set
sendRequest :: Request Present Present b -> IO Response
sendRequest req = do
  let url    = fromJust (reqUrl req)
      method = fromJust (reqMethod req)
  httpRequest method url (reqBody req) (reqHeaders req)

-- Usage (type error if url or method missing)
example :: IO Response
example = sendRequest $
  setUrl "https://api.example.com/users"
  . setMethod GET
  . addHeader "Authorization" "Bearer token"
  $ emptyRequest
```

---

## ขั้นตอนที่ 407: Servant Type-Level API Composition

```haskell
-- Advanced Servant type-level API

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- Versioned API
type V1API = "v1" :> V1Routes
type V2API = "v2" :> V2Routes

type FullAPI = V1API :<|> V2API

-- Nested routes
type UserRoutes = Get '[JSON] [User]
            :<|> Capture "id" UserId :> Get '[JSON] User
            :<|> Capture "id" UserId :> "posts" :> Get '[JSON] [Post]

type AdminRoutes = "admin" :> BasicAuth "admin" Admin :>
  ( "users" :> UserRoutes
  :<|> "stats" :> Get '[JSON] Stats
  )

-- API with custom combinators
data RequireAdmin

type SecureRoute a = Auth '[JWT] UserClaims :> a

type ProtectedUserAPI = SecureRoute (
  "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" UserId :> RequireOwner :> Get '[JSON] User
  )

-- Custom combinator implementation
data RequireOwner

instance HasServer api ctx => HasServer (RequireOwner :> api) ctx where
  type ServerT (RequireOwner :> api) m = UserId -> ServerT api m
  route _ ctx sub = route (Proxy :: Proxy api) ctx $
    \req respond ->
      case lookupHeader "X-User-Id" req of
        Nothing -> respond (failWith err401)
        Just uid -> route (Proxy :: Proxy api) ctx (sub uid) req respond
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)
```

---

## ขั้นตอนที่ 408: Dependent Types

```haskell
-- Dependent-like types ใน Haskell

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- Type-level Nat
data Nat = Z | S Nat

-- Vector ที่มีขนาดใน type
data Vec (n :: Nat) a where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

-- Type-safe operations
head' :: Vec (S n) a -> a
head' (Cons x _) = x

tail' :: Vec (S n) a -> Vec n a
tail' (Cons _ xs) = xs

-- ไม่ compile: head' Nil (เพราะ S n ≠ Z)

-- Safe zip (ต้องยาวเท่ากัน)
zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec Nil         Nil         = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x, y) (zipVec xs ys)

-- Singleton types: bridge between type-level and value-level
data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- Create vector of given size
replicate' :: SNat n -> a -> Vec n a
replicate' SZ     _ = Nil
replicate' (SS n) x = Cons x (replicate' n x)

-- IndexOf: type-safe index
type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[]     = False
  Elem x (x:_)  = True
  Elem x (_:xs) = Elem x xs

-- Type-safe lookup in HList
data HList xs where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

class HLookup x xs where
  hlookup :: HList xs -> x

instance HLookup x (x ': xs) where
  hlookup (HCons x _) = x

instance HLookup x xs => HLookup x (y ': xs) where
  hlookup (HCons _ rest) = hlookup rest
```

---

## ขั้นตอนที่ 409: Comonad

```haskell
-- Comonad: dual of Monad

class Functor w => Comonad w where
  extract   :: w a -> a       -- dual of return
  duplicate :: w a -> w (w a) -- dual of join
  extend    :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- Store comonad
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- Zipper: infinite list with focus
data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
  fmap f (Zipper ls x rs) = Zipper (map f ls) (f x) (map f rs)

instance Comonad Zipper where
  extract (Zipper _ x _) = x
  duplicate z@(Zipper ls _ rs) =
    Zipper (tail $ iterate goLeft z)
           z
           (tail $ iterate goRight z)

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
goLeft z = z

goRight :: Zipper a -> Zipper a
goRight (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
goRight z = z

-- Conway's Game of Life ด้วย Comonad
type World = Zipper (Zipper Bool)

step :: World -> Bool
step w =
  let neighbors = map extract . filter (/= w) $
        [ goLeft . extract $ w
        , goRight . extract $ w
        , extract . goLeft $ w
        , extract . goRight $ w
        , goLeft . extract . goLeft $ w
        , goRight . extract . goLeft $ w
        , goLeft . extract . goRight $ w
        , goRight . extract . goRight $ w
        ]
      alive = extract (extract w)
      liveNeighbors = length (filter id neighbors)
  in (alive && liveNeighbors `elem` [2,3])
     || (not alive && liveNeighbors == 3)

evolve :: World -> World
evolve = extend (extend step)
```

---

## ขั้นตอนที่ 410: Arrow

```haskell
-- Arrow: generalization of functions

import Control.Arrow

-- Arrow ทำงานเหมือน function แต่ generalized
-- arr :: (b -> c) -> a b c
-- (>>>) :: a b c -> a c d -> a b d
-- first :: a b c -> a (b, d) (c, d)

-- Parser combinator ด้วย Arrow
newtype Parser a b = Parser { runParser :: a -> Maybe (b, a) }

instance Category Parser where
  id    = Parser $ \x -> Just (x, x)
  f . g = Parser $ \x -> do
    (y, x') <- runParser g x
    runParser f y

instance Arrow Parser where
  arr f = Parser $ \x -> Just (f x, x)
  first p = Parser $ \(x, d) -> do
    (y, x') <- runParser p x
    return ((y, d), (x', d))

-- ตัวอย่าง: circuit simulation ด้วย ArrowLoop
import Control.Arrow (ArrowLoop(..))

-- Feedback loop
counter :: ArrowLoop a => a () Int
counter = loop $ arr (\(_, n) -> (n, n+1)) <<< second (delay 0)
  where
    delay :: a b b
    delay x = x  -- simplification

-- Arrow notation
sumInputs :: (Arrow a, ArrowChoice a) => a Int Int
sumInputs = proc n -> do
  total <- sumInputs -< n
  returnA -< total + n
```

---

## ขั้นตอนที่ 411: Recursion Schemes

```haskell
-- Recursion schemes: generalized recursion patterns

import Data.Functor.Foldable

-- Fixed point combinator
newtype Fix f = Fix { unFix :: f (Fix f) }

-- Catamorphism (fold)
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- Anamorphism (unfold)
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

-- Hylomorphism (unfold then fold)
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = cata alg . ana coalg

-- Expression tree
data ExprF a
  = LitF Int
  | AddF a a
  | MulF a a
  deriving Functor

type Expr = Fix ExprF

-- Smart constructors
lit :: Int -> Expr
lit = Fix . LitF

add :: Expr -> Expr -> Expr
add x y = Fix (AddF x y)

mul :: Expr -> Expr -> Expr
mul x y = Fix (MulF x y)

-- Evaluator (catamorphism)
eval :: Expr -> Int
eval = cata alg
  where
    alg (LitF n)   = n
    alg (AddF x y) = x + y
    alg (MulF x y) = x * y

-- Pretty printer
prettyPrint :: Expr -> String
prettyPrint = cata alg
  where
    alg (LitF n)   = show n
    alg (AddF x y) = "(" ++ x ++ " + " ++ y ++ ")"
    alg (MulF x y) = "(" ++ x ++ " * " ++ y ++ ")"

-- Example: (2 + 3) * (4 + 1)
expr :: Expr
expr = mul (add (lit 2) (lit 3)) (add (lit 4) (lit 1))
-- eval expr == 25
-- prettyPrint expr == "((2 + 3) * (4 + 1))"
```

---

## ขั้นตอนที่ 412: Category Theory Patterns

```haskell
-- Category Theory ใน Haskell

-- Bifunctor
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  bimap f g = first f . second g
  
  first :: (a -> c) -> f a b -> f c b
  first f = bimap f id
  
  second :: (b -> d) -> f a b -> f a d
  second = bimap id

instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)

instance Bifunctor Either where
  bimap f _ (Left x)  = Left (f x)
  bimap _ g (Right y) = Right (g y)

-- Profunctor
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id

instance Profunctor (->) where
  dimap f g h = g . h . f

-- Natural transformation
type f ~> g = forall a. f a -> g a

listToMaybe' :: [] ~> Maybe
listToMaybe' []    = Nothing
listToMaybe' (x:_) = Just x

-- Adjunction
class (Functor f, Functor g) => Adjunction f g where
  unit   :: a -> g (f a)
  counit :: f (g a) -> a
  
  leftAdj  :: (f a -> b) -> (a -> g b)
  leftAdj f = fmap f . unit
  
  rightAdj :: (a -> g b) -> (f a -> b)
  rightAdj g = counit . fmap g

-- (,) e ⊣ (->) e
instance Adjunction ((,) e) ((->) e) where
  unit a = \e -> (e, a)
  counit (e, f) = f e
```

---

## ขั้นตอนที่ 413: Streaming

```haskell
-- Streaming data ด้วย Conduit

import Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL

-- Producer
numbersSource :: Source IO Int
numbersSource = CL.sourceList [1..1000000]

-- Transformation
doubleC :: Conduit Int IO Int
doubleC = mapC (*2)

filterEvenC :: Conduit Int IO Int
filterEvenC = filterC even

-- Consumer
sumSink :: Sink Int IO Int
sumSink = foldlC (+) 0

-- Compose and run
processNumbers :: IO Int
processNumbers = runConduit $
  numbersSource
  .| filterEvenC
  .| doubleC
  .| sumSink

-- File streaming
processLargeFile :: FilePath -> FilePath -> IO ()
processLargeFile inFile outFile =
  runConduitRes $
    sourceFile inFile
    .| decodeUtf8C
    .| linesUnboundedC
    .| filterC (\line -> not (T.null line))
    .| mapC T.toUpper
    .| unlines'C
    .| encodeUtf8C
    .| sinkFile outFile

-- Resource safe streaming
withFileConduit :: FilePath -> ConduitT i ByteString (ResourceT IO) () -> IO ()
withFileConduit path action = runConduitRes $ action .| sinkFile path

-- Streaming CSV
import Data.Csv.Streaming

streamCsv :: FilePath -> IO ()
streamCsv path = do
  bs <- BS.readFile path
  let records = decode NoHeader bs :: Records (Text, Int, Double)
  mapM_ processRecord records
  where
    processRecord (Left err) = putStrLn $ "Error: " ++ err
    processRecord (Right r)  = putStrLn $ "Record: " ++ show r
```

---

## ขั้นตอนที่ 414: Parser Combinators

```haskell
-- Parser combinators ด้วย Megaparsec

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

-- Expression parser ด้วย precedence
data Expr
  = Num Double
  | Var Text
  | BinOp Op Expr Expr
  | UnOp  UOp Expr
  | App   Text [Expr]
  deriving Show

data Op  = Add | Sub | Mul | Div | Pow deriving Show
data UOp = Neg | Not deriving Show

expr :: Parser Expr
expr = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [Prefix (UnOp Neg <$ symbol "-")]
  , [InfixR (BinOp Pow <$ symbol "^")]
  , [InfixL (BinOp Mul <$ symbol "*"), InfixL (BinOp Div <$ symbol "/")]
  , [InfixL (BinOp Add <$ symbol "+"), InfixL (BinOp Sub <$ symbol "-")]
  ]

term :: Parser Expr
term = choice
  [ try (App <$> identifier <*> parens (expr `sepBy` symbol ","))
  , Num <$> float
  , Num . fromIntegral <$> integer
  , Var <$> identifier
  , parens expr
  ]

identifier :: Parser Text
identifier = lexeme $ T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parse and evaluate
parseAndEval :: Text -> Either String Double
parseAndEval input =
  case parse expr "<input>" input of
    Left err  -> Left (errorBundlePretty err)
    Right ast -> Right (evaluate ast)
  where
    evaluate (Num n)        = n
    evaluate (Var _)        = 0  -- undefined variable
    evaluate (BinOp Add x y) = evaluate x + evaluate y
    evaluate (BinOp Sub x y) = evaluate x - evaluate y
    evaluate (BinOp Mul x y) = evaluate x * evaluate y
    evaluate (BinOp Div x y) = evaluate x / evaluate y
    evaluate (BinOp Pow x y) = evaluate x ** evaluate y
    evaluate (UnOp Neg x)   = -(evaluate x)
    evaluate _              = 0
```

---

## ขั้นตอนที่ 415: Rewrite Rules และ SPECIALIZE

```haskell
-- GHC optimization pragmas

-- SPECIALIZE: สร้าง specialized version ของ generic function
{-# SPECIALIZE myFunc :: [Int] -> Int #-}
{-# SPECIALIZE myFunc :: [Double] -> Double #-}

myFunc :: Num a => [a] -> a
myFunc = sum

-- INLINE: บอก GHC ให้ inline function
{-# INLINE fastAdd #-}
fastAdd :: Int -> Int -> Int
fastAdd x y = x + y

-- NOINLINE: ห้าม inline
{-# NOINLINE expensiveSetup #-}
expensiveSetup :: IO Config
expensiveSetup = ...

-- RULES: rewrite rules สำหรับ optimization
{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
"filter/filter" forall p q xs. filter p (filter q xs) = filter (\x -> q x && p x) xs
"foldr/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b).
    foldr k z (build g) = g k z
  #-}

-- Stream fusion: อัตโนมัติด้วย RULES
-- sum [1..n] -- ไม่สร้าง list จริง, fuse เป็น loop

-- BangPattern กับ strictness
{-# LANGUAGE BangPatterns #-}

strictSum :: [Int] -> Int
strictSum = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs
```

---

## ขั้นตอนที่ 416: Profiling และ Performance Analysis

```haskell
-- Performance profiling

-- 1. Compile with profiling
-- ghc -prof -fprof-auto -rtsopts MyApp.hs

-- 2. Run with profiling
-- ./MyApp +RTS -p -s -RTS
-- สร้าง MyApp.prof และ stats ใน stderr

-- 3. Heap profiling
-- ./MyApp +RTS -h -RTS
-- hp2ps MyApp.hp > MyApp.ps

-- Criterion benchmarking
import Criterion
import Criterion.Main

benchmarks :: Benchmark
benchmarks = bgroup "sort algorithms"
  [ bench "insertion sort" $ nf insertionSort testData
  , bench "merge sort"     $ nf mergeSort testData
  , bench "quicksort"      $ nf quickSort testData
  , bench "Data.List.sort" $ nf sort testData
  ]
  where testData = [1000,999..1] :: [Int]

main :: IO ()
main = defaultMain [benchmarks]

-- Weigh: memory usage
import Weigh

main :: IO ()
main = mainWith $ do
  func "list [1..100]" ([1..100 :: Int]) length
  func "list [1..1000]" ([1..1000 :: Int]) length
  action "allocate Map" $ do
    let m = Map.fromList [(i, i) | i <- [1..1000 :: Int]]
    return $! Map.size m

-- ThreadScope: concurrent profiling
-- Compile: ghc -eventlog -rtsopts
-- Run: ./MyApp +RTS -l-agu -RTS
-- View: threadscope MyApp.eventlog
```

---

## ขั้นตอนที่ 417: Safe Haskell

```haskell
-- Safe Haskell: subset ที่ปลอดภัย

{-# LANGUAGE Safe #-}
{-# LANGUAGE Trustworthy #-}

-- Safe Haskell ป้องกัน:
-- - unsafePerformIO
-- - unsafeCoerce
-- - Foreign imports
-- - Overlapping instances (บางส่วน)

-- Trustworthy: module ที่ใช้ unsafe internally แต่ safe externally
{-# LANGUAGE Trustworthy #-}
module SafeWrapper where

import qualified System.IO.Unsafe (unsafePerformIO)

-- wrap unsafe operation ให้ safe
globalConfig :: Config
globalConfig = System.IO.Unsafe.unsafePerformIO readConfig
{-# NOINLINE globalConfig #-}

-- Safe alternatives
-- แทน unsafePerformIO ใช้ IORef + initialization
data SafeGlobal a = SafeGlobal (IORef (Maybe a)) (IO a)

newSafeGlobal :: IO a -> IO (SafeGlobal a)
newSafeGlobal init = SafeGlobal <$> newIORef Nothing <*> pure init

getGlobal :: SafeGlobal a -> IO a
getGlobal (SafeGlobal ref init) = do
  mVal <- readIORef ref
  case mVal of
    Just v  -> return v
    Nothing -> do
      v <- init
      writeIORef ref (Just v)
      return v
```

---

## ขั้นตอนที่ 418: Foreign Function Interface

```haskell
-- FFI: calling C from Haskell

{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

-- Import C function
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

foreign import ccall "string.h strlen"
  c_strlen :: CString -> IO CSize

-- Use C function
haskellSin :: Double -> Double
haskellSin = realToFrac . c_sin . realToFrac

getLength :: String -> IO Int
getLength s = withCString s $ \cs -> do
  n <- c_strlen cs
  return (fromIntegral n)

-- Export Haskell function to C
foreign export ccall haskell_add
  :: CInt -> CInt -> CInt

haskell_add :: CInt -> CInt -> CInt
haskell_add x y = x + y

-- Marshaling
marshalList :: [Int] -> IO (Ptr CInt, CInt)
marshalList xs = do
  ptr <- mallocArray (length xs)
  pokeArray ptr (map fromIntegral xs)
  return (ptr, fromIntegral (length xs))

unmarshalList :: Ptr CInt -> CInt -> IO [Int]
unmarshalList ptr n = do
  arr <- peekArray (fromIntegral n) ptr
  return (map fromIntegral arr)
```

---

## ขั้นตอนที่ 419: Type Class Deriving Advanced

```haskell
-- Advanced deriving strategies

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

-- DerivingStrategies: เลือก strategy ชัดเจน
data Foo = Foo Int
  deriving stock    (Show, Eq, Ord, Generic)  -- standard deriving
  deriving anyclass (ToJSON, FromJSON)          -- use generic instances
  deriving newtype  ()                          -- N/A for data

-- DerivingVia: derive instance ผ่าน type ที่มี instance แล้ว
newtype Sum a = Sum a

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

newtype Age = Age Int
  deriving (Show, Eq, Ord)
  deriving (Semigroup, Monoid) via Sum Int

-- Coercible constraint
newtype Wrapper a = Wrapper { unwrap :: a }
  deriving newtype (Show, Eq, Ord, Functor)

-- ใช้ coerce เพื่อ zero-cost conversion
import Data.Coerce

sumWrapper :: [Wrapper Int] -> Wrapper Int
sumWrapper = coerce sum  -- coerce :: [Int] -> Int ไป [Wrapper Int] -> Wrapper Int

-- Stock deriving for records
data Config = Config
  { configHost :: Text
  , configPort :: Int
  } deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via (Config)
```

---

## ขั้นตอนที่ 420: โปรเจกต์: DSL สำหรับ Query Builder

```haskell
-- Type-safe SQL query builder DSL

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- Column type
data Column (t :: *) = Column
  { colTable :: Text
  , colName  :: Text
  }

-- Value type
data Val (t :: *) where
  VInt  :: Int  -> Val Int
  VText :: Text -> Val Text
  VBool :: Bool -> Val Bool
  VNull :: Val (Maybe a)

-- Condition
data Condition where
  Eq    :: Column t -> Val t -> Condition
  Ne    :: Column t -> Val t -> Condition
  Gt    :: Ord t => Column t -> Val t -> Condition
  Lt    :: Ord t => Column t -> Val t -> Condition
  And   :: Condition -> Condition -> Condition
  Or    :: Condition -> Condition -> Condition
  Not   :: Condition -> Condition

-- Query DSL
data Select a where
  SelectAll :: Text -> Select [Row]
  SelectCols :: [AnyColumn] -> Text -> Select [Row]
  Where :: Select a -> Condition -> Select a
  Limit :: Select a -> Int -> Select a
  Offset :: Select a -> Int -> Select a
  OrderBy :: Select a -> [OrderSpec] -> Select a
  Join :: Select a -> Text -> JoinType -> Condition -> Select a

data JoinType = InnerJoin | LeftJoin | RightJoin
data OrderSpec = Asc AnyColumn | Desc AnyColumn
data AnyColumn = forall t. AnyColumn (Column t)

-- Smart constructors
from :: Text -> Select [Row]
from = SelectAll

select :: [AnyColumn] -> Text -> Select [Row]
select = SelectCols

where' :: Select a -> Condition -> Select a
where' = Where

(==.) :: Column t -> Val t -> Condition
(==.) = Eq

(!=.) :: Column t -> Val t -> Condition
(!=.) = Ne

limit :: Select a -> Int -> Select a
limit = Limit

-- SQL renderer
renderSelect :: Select a -> (Text, [SqlParam])
renderSelect q = case q of
  SelectAll table ->
    ("SELECT * FROM " <> table, [])
  SelectCols cols table ->
    ("SELECT " <> renderCols cols <> " FROM " <> table, [])
  Where inner cond ->
    let (sql, params) = renderSelect inner
        (condSql, condParams) = renderCondition cond
    in (sql <> " WHERE " <> condSql, params ++ condParams)
  Limit inner n ->
    let (sql, params) = renderSelect inner
    in (sql <> " LIMIT ?", params ++ [SqlInt n])
  Offset inner n ->
    let (sql, params) = renderSelect inner
    in (sql <> " OFFSET ?", params ++ [SqlInt n])

renderCondition :: Condition -> (Text, [SqlParam])
renderCondition (Eq col val) =
  (colTable col <> "." <> colName col <> " = ?", [valToParam val])
renderCondition (And c1 c2) =
  let (s1, p1) = renderCondition c1
      (s2, p2) = renderCondition c2
  in ("(" <> s1 <> " AND " <> s2 <> ")", p1 ++ p2)
renderCondition (Or c1 c2) =
  let (s1, p1) = renderCondition c1
      (s2, p2) = renderCondition c2
  in ("(" <> s1 <> " OR " <> s2 <> ")", p1 ++ p2)

-- Example usage
usersCol :: Column Text
usersCol = Column "users" "name"

ageCol :: Column Int
ageCol = Column "users" "age"

exampleQuery :: Select [Row]
exampleQuery = from "users"
  `where'` (ageCol `Gt` VInt 18 `And` usersCol `Ne` VText "admin")
  `limit` 10
  `offset` 0

-- renderSelect exampleQuery
-- ("SELECT * FROM users WHERE (users.age > ? AND users.name != ?) LIMIT ? OFFSET ?",
--  [SqlInt 18, SqlText "admin", SqlInt 10, SqlInt 0])
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 22 เราจะเรียน **Production Architecture Patterns**:
- Clean Architecture ใน Haskell
- Domain-Driven Design
- CQRS/Event Sourcing
- Hexagonal Architecture

---

*[← Part 20](part-20.md) | [Part 22 →](part-22.md)*
