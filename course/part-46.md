# Part 46: Domain-Specific Languages (DSL)
## ขั้นตอนที่ 901-920

---

## ขั้นตอนที่ 901: Embedded DSL Basics

```haskell
-- Embedded DSL patterns

-- Simple arithmetic DSL (deep embedding)
data AExpr
  = ANum   Double
  | AVar   Text
  | AAdd   AExpr AExpr
  | AMul   AExpr AExpr
  | ADiv   AExpr AExpr
  | APow   AExpr AExpr
  | ASqrt  AExpr
  | ALn    AExpr
  | AExp   AExpr
  deriving (Show, Eq)

-- Smart constructors
num :: Double -> AExpr
num = ANum

(.+.) :: AExpr -> AExpr -> AExpr
(.+.) = AAdd

(.*.) :: AExpr -> AExpr -> AExpr
(.*.) = AMul

(./) :: AExpr -> AExpr -> AExpr
(./) = ADiv

-- Evaluation
evalAExpr :: Map Text Double -> AExpr -> Maybe Double
evalAExpr _ (ANum n)   = Just n
evalAExpr env (AVar x) = Map.lookup x env
evalAExpr env (AAdd l r) = (+) <$> evalAExpr env l <*> evalAExpr env r
evalAExpr env (AMul l r) = (*) <$> evalAExpr env l <*> evalAExpr env r
evalAExpr env (ADiv l r) = do
  rv <- evalAExpr env r
  guard (rv /= 0)
  lv <- evalAExpr env l
  return (lv / rv)
evalAExpr env (ASqrt e)  = sqrt <$> evalAExpr env e

-- Symbolic differentiation
diff :: Text -> AExpr -> AExpr
diff x (ANum _)   = num 0
diff x (AVar y)   = if x == y then num 1 else num 0
diff x (AAdd l r) = diff x l .+. diff x r
diff x (AMul l r) = (diff x l .*. r) .+. (l .*. diff x r)
diff x (APow e n) = n .*. APow e (n .+. num (-1)) .*. diff x e
diff x (ASqrt e)  = diff x e ./ (num 2 .*. ASqrt e)
```

---

## ขั้นตอนที่ 902: Query DSL

```haskell
-- Type-safe query DSL

data Query table result where
  Select  :: [Field table r] -> Query table [r]
  Where   :: Query table [r] -> Condition table -> Query table [r]
  OrderBy :: Query table [r] -> Field table a -> SortOrder -> Query table [r]
  Limit   :: Query table [r] -> Int -> Query table [r]
  Join    :: Query t1 [r1] -> Query t2 [r2] -> JoinCondition t1 t2 -> Query (t1, t2) [(r1, r2)]

data Field (table :: *) a where
  FId      :: Field User Int
  FName    :: Field User Text
  FEmail   :: Field User Text
  FCreated :: Field User UTCTime

data Condition table where
  CEq   :: Field table a -> a -> Condition table
  CGt   :: Ord a => Field table a -> a -> Condition table
  CLt   :: Ord a => Field table a -> a -> Condition table
  CAnd  :: Condition table -> Condition table -> Condition table
  COr   :: Condition table -> Condition table -> Condition table

-- DSL usage
userQuery :: Query User [Text]
userQuery = Limit
  (OrderBy
    (Where
      (Select [FName])
      (CAnd (CEq FEmail "admin@example.com") (CGt FId 0)))
    FName Ascending)
  10

-- Translate to SQL
toSQL :: Query table result -> (Text, [Value])
toSQL (Select fields) = ("SELECT " <> fieldList, [])
  where fieldList = T.intercalate ", " (map fieldName fields)
toSQL (Where q cond) =
  let (sql, params) = toSQL q
      (condSql, condParams) = condToSQL cond
  in (sql <> " WHERE " <> condSql, params ++ condParams)
toSQL (Limit q n) =
  let (sql, params) = toSQL q
  in (sql <> " LIMIT ?", params ++ [toValue n])
```

---

## ขั้นตอนที่ 903: Configuration DSL

```haskell
-- Configuration DSL with validation

data ConfigDsl a where
  CRequired :: Text -> ConfigDsl a
  COptional :: Text -> a -> ConfigDsl a
  CWith     :: ConfigDsl a -> (a -> Either Text b) -> ConfigDsl b
  CMap      :: ConfigDsl a -> (a -> b) -> ConfigDsl b
  CBoth     :: ConfigDsl a -> ConfigDsl b -> ConfigDsl (a, b)

instance Functor ConfigDsl where
  fmap = CMap

instance Applicative ConfigDsl where
  pure a = CMap (CRequired "unused") (\_ -> a)  -- simplified
  liftA2 f ca cb = CMap (CBoth ca cb) (uncurry f)

-- Build server config
serverConfig :: ConfigDsl ServerConfig
serverConfig = ServerConfig
  <$> CWith (COptional "PORT" "8080") parsePort
  <*> COptional "HOST" "0.0.0.0"
  <*> CWith (COptional "TIMEOUT" "30") parseInt
  <*> CWith (CRequired "DATABASE_URL") parseDatabaseUrl
  where
    parsePort s = case readMaybe s of
      Just n | n > 0 && n < 65536 -> Right n
      _                            -> Left "Invalid port number"
    parseInt s  = maybe (Left "Invalid integer") Right (readMaybe s)

-- Evaluate config from environment
evalConfig :: ConfigDsl a -> Map Text Text -> Either [Text] a
evalConfig (CRequired key) env = case Map.lookup key env of
  Nothing -> Left ["Missing required config: " <> key]
  Just v  -> Right v
evalConfig (COptional key def) env = Right (fromMaybe def (Map.lookup key env))
evalConfig (CWith cfg validate) env = do
  v <- evalConfig cfg env
  case validate v of
    Left err -> Left [err]
    Right v' -> Right v'
evalConfig (CBoth ca cb) env =
  case (evalConfig ca env, evalConfig cb env) of
    (Right a, Right b)  -> Right (a, b)
    (Left ea, Left eb)  -> Left (ea ++ eb)  -- collect all errors
    (Left ea, _)        -> Left ea
    (_, Left eb)        -> Left eb
```

---

## ขั้นตอนที่ 904: HTML DSL

```haskell
-- Type-safe HTML DSL

{-# LANGUAGE OverloadedStrings #-}

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- Blaze-html DSL
page :: Html
page = docTypeHtml $ do
  H.head $ do
    title "My Page"
    link ! rel "stylesheet" ! href "style.css"
  body $ do
    div ! class_ "container" $ do
      h1 ! id "title" $ "Hello, World!"
      p  ! class_ "intro" $ "Welcome to my page."
      ul $ mapM_ (\item -> li (toHtml item)) ["Item 1", "Item 2", "Item 3"]

-- Custom component
buttonComponent :: Text -> Text -> Html
buttonComponent label href' = do
  a ! href (toValue href') ! class_ "btn" $ toHtml label

-- Form DSL
loginForm :: Html
loginForm = form ! method "post" ! action "/login" $ do
  fieldset $ do
    legend "Login"
    label ! for "email"    $ "Email:"
    input ! type_ "email" ! id "email" ! name "email" ! required ""
    br
    label ! for "password" $ "Password:"
    input ! type_ "password" ! id "password" ! name "password" ! required ""
    br
    input ! type_ "submit" ! value "Login"

-- Template with data
userProfile :: User -> Html
userProfile user = div ! class_ "profile" $ do
  h2 (toHtml (userName user))
  p  (toHtml (userEmail user))
  when (userIsAdmin user) $
    span ! class_ "badge badge-admin" $ "Admin"
```

---

## ขั้นตอนที่ 905: Parser Combinator DSL

```haskell
-- Parser DSL using megaparsec

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Lexer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Tokens
integer :: Parser Int
integer = lexeme L.decimal

float' :: Parser Double
float' = lexeme L.float

identifier :: Parser Text
identifier = lexeme (T.cons <$> letterChar <*> (T.pack <$> many alphaNumChar))

reserved :: Text -> Parser ()
reserved word = void (lexeme (string word <* notFollowedBy alphaNumChar))

-- Expression parser
data Expr'' = ELit' Int | EVar' Text | EBinOp' Op' Expr'' Expr'' | ECall' Text [Expr'']
  deriving (Show)

data Op' = Add' | Sub' | Mul' | Div' deriving (Show)

parseExpr' :: Parser Expr''
parseExpr' = makeExprParser parseTerm
  [ [ InfixL (EBinOp' Mul' <$ symbol "*")
    , InfixL (EBinOp' Div' <$ symbol "/") ]
  , [ InfixL (EBinOp' Add' <$ symbol "+")
    , InfixL (EBinOp' Sub' <$ symbol "-") ]
  ]

parseTerm :: Parser Expr''
parseTerm = choice
  [ ELit' <$> integer
  , try (ECall' <$> identifier <*> (symbol "(" *> parseArgs <* symbol ")"))
  , EVar' <$> identifier
  , symbol "(" *> parseExpr' <* symbol ")"
  ]

parseArgs :: Parser [Expr'']
parseArgs = parseExpr' `sepBy` symbol ","
```

---

## ขั้นตอนที่ 906: Workflow DSL

```haskell
-- Workflow DSL for business processes

data Step result
  = Action  Text (IO result)
  | Wait    Text (TVar (Maybe result)) 
  | Branch  (IO Bool) (Workflow ()) (Workflow ())
  | Parallel [Workflow ()] 
  | Retry   Int (Workflow result)
  | Timeout Int (Workflow result)

data Workflow a = Workflow { runWorkflow :: IO (WorkflowResult a) }

data WorkflowResult a
  = WCompleted a
  | WFailed    Text
  | WTimedOut
  | WSuspended SuspendedState

-- Smart constructors
action :: Text -> IO a -> Workflow a
action name io = Workflow $ do
  result <- try io
  case result of
    Right v  -> return (WCompleted v)
    Left err -> return (WFailed (tshow (err :: SomeException)))

await :: Text -> TVar (Maybe a) -> Workflow a
await name var = Workflow $ do
  v <- atomically $ do
    mv <- readTVar var
    maybe retry return mv  -- STM retry until value available
  return (WCompleted v)

-- Workflow for order processing
orderWorkflow :: Order -> Workflow OrderResult
orderWorkflow order = do
  payment <- action "Process Payment" (processPayment (ordPayment order))
  unless (paymentSuccess payment) (fail "Payment failed")
  
  inventory <- action "Reserve Inventory" (reserveItems (ordItems order))
  unless (reservationSuccess inventory) $ do
    action "Refund Payment" (refundPayment payment)
    fail "Inventory unavailable"
  
  action "Send Confirmation" (sendEmail (ordCustomerEmail order) "Order confirmed")
  action "Ship Order"        (createShipment order)
```

---

## ขั้นตอนที่ 907: Rule Engine DSL

```haskell
-- Rule engine DSL

data Rule fact result = Rule
  { ruleName      :: Text
  , ruleCondition :: fact -> Bool
  , ruleAction    :: fact -> result
  , rulePriority  :: Int
  }

-- Rule DSL
data RuleM fact result a = RuleM { runRuleM :: [Rule fact result] -> (a, [Rule fact result]) }

instance Functor (RuleM fact result) where
  fmap f (RuleM run) = RuleM (\rs -> let (a, rs') = run rs in (f a, rs'))

instance Monad (RuleM fact result) where
  return a = RuleM (\rs -> (a, rs))
  RuleM run >>= f = RuleM (\rs -> 
    let (a, rs') = run rs
        (b, rs'') = runRuleM (f a) rs'
    in (b, rs''))

-- Add a rule
addRule :: Text -> (fact -> Bool) -> (fact -> result) -> Int -> RuleM fact result ()
addRule name cond action priority = RuleM (\rs -> ((), rs ++ [Rule name cond action priority]))

-- Run rule engine (first match)
runRules :: fact -> [Rule fact result] -> Maybe result
runRules fact = fmap (flip ruleAction fact) . listToMaybe . sortBy (flip compare `on` rulePriority) . filter (\r -> ruleCondition r fact)

-- Pricing rules
pricingRules :: RuleM Order Double ()
pricingRules = do
  addRule "VIP Discount"      (isVipCustomer . ordCustomer)    (\o -> orderTotal o * 0.8)   100
  addRule "Large Order"       (\o -> orderTotal o > 1000)      (\o -> orderTotal o * 0.9)    50
  addRule "Weekend Promotion" (isWeekend . ordDate)             (\o -> orderTotal o * 0.95)   30
  addRule "Default Price"     (const True)                      orderTotal                     0
```

---

## ขั้นตอนที่ 908: Migration DSL

```haskell
-- Database migration DSL

data Migration = Migration
  { migVersion  :: Int
  , migName     :: Text
  , migUp       :: [MigrationOp]
  , migDown     :: [MigrationOp]
  }

data MigrationOp
  = CreateTable  TableName [ColumnDef]
  | DropTable    TableName
  | AddColumn    TableName ColumnDef
  | DropColumn   TableName ColumnName
  | AddIndex     TableName IndexDef
  | DropIndex    TableName IndexName
  | CreateEnum   Text [Text]
  | RawSQL       Text

data ColumnDef = ColumnDef
  { colName     :: ColumnName
  , colType     :: ColumnType
  , colNullable :: Bool
  , colDefault  :: Maybe Text
  , colUnique   :: Bool
  }

-- DSL builders
createTable :: TableName -> [ColumnDef] -> MigrationOp
createTable = CreateTable

col :: ColumnName -> ColumnType -> ColumnDef
col name typ = ColumnDef name typ True Nothing False

notNull :: ColumnDef -> ColumnDef
notNull c = c { colNullable = False }

unique :: ColumnDef -> ColumnDef
unique c = c { colUnique = True }

withDefault :: Text -> ColumnDef -> ColumnDef
withDefault def c = c { colDefault = Just def }

-- Migration definition
migration001 :: Migration
migration001 = Migration
  { migVersion = 1
  , migName    = "create_users"
  , migUp      =
      [ createTable "users"
          [ col "id"         PgSerial  |> notNull
          , col "name"       PgText    |> notNull
          , col "email"      PgText    |> notNull |> unique
          , col "created_at" PgTimestamp |> notNull |> withDefault "NOW()"
          ]
      , AddIndex "users" (IndexDef "users_email_idx" ["email"] Unique)
      ]
  , migDown    = [DropTable "users"]
  }
```

---

## ขั้นตอนที่ 909: Test DSL

```haskell
-- Test DSL with custom assertions

import Test.Hspec
import Test.Hspec.Expectations.Contrib

-- Custom matchers
shouldBePositive :: (Show a, Num a, Ord a) => a -> Expectation
shouldBePositive x = x `shouldSatisfy` (> 0)

shouldBeInRange :: (Show a, Ord a) => (a, a) -> a -> Expectation
shouldBeInRange (lo, hi) x =
  unless (lo <= x && x <= hi) $
    expectationFailure ("Expected " ++ show x ++ " to be in range [" ++ show lo ++ ", " ++ show hi ++ "]")

shouldBeSubsetOf :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldBeSubsetOf xs ys =
  let missing = filter (`notElem` ys) xs
  in unless (null missing) $
       expectationFailure ("Missing elements: " ++ show missing)

-- Test builder DSL
data TestSuite = TestSuite
  { tsName  :: Text
  , tsTests :: [TestCase]
  }

data TestCase = TestCase
  { tcName   :: Text
  , tcSetup  :: IO ()
  , tcRun    :: IO ()
  , tcTeardown :: IO ()
  }

-- Test DSL
mkTest :: Text -> IO () -> TestCase
mkTest name action = TestCase name (return ()) action (return ())

withSetup :: IO () -> TestCase -> TestCase
withSetup setup tc = tc { tcSetup = setup }

withTeardown :: IO () -> TestCase -> TestCase
withTeardown teardown tc = tc { tcTeardown = teardown }

-- Run test suite
runTestSuite :: TestSuite -> SpecWith ()
runTestSuite suite = describe (T.unpack (tsName suite)) $
  mapM_ (\tc -> it (T.unpack (tcName tc)) $ do
    tcSetup tc
    tcRun tc `finally` tcTeardown tc) (tsTests suite)
```

---

## ขั้นตอนที่ 910: Protocol DSL

```haskell
-- Network protocol DSL

data ProtocolM state a where
  Send    :: Message -> ProtocolM state ()
  Receive :: ProtocolM state Message
  Transition :: state -> ProtocolM state ()
  GetState :: ProtocolM state state
  Fail :: Text -> ProtocolM state a

-- Interpret as state machine
runProtocol :: ProtocolM state a -> state -> Connection -> IO (Either ProtocolError (a, state))
runProtocol (Send msg) state conn = do
  sendMessage conn msg
  return (Right ((), state))
runProtocol Receive state conn = do
  msg <- receiveMessage conn
  return (Right (msg, state))
runProtocol (Transition newState) _ conn = return (Right ((), newState))
runProtocol GetState state _ = return (Right (state, state))
runProtocol (Fail err) _ _ = return (Left (ProtocolError err))

-- SMTP protocol DSL
data SmtpState = SmtpInit | SmtpGreeted | SmtpFromSet | SmtpRcptSet | SmtpData | SmtpDone

smtpSession :: Text -> Text -> Text -> ProtocolM SmtpState ()
smtpSession from to body = do
  greeting <- Receive
  assertResponse greeting 220
  Send (EHLO "my-client")
  resp <- Receive
  assertResponse resp 250
  Transition SmtpGreeted
  
  Send (MAIL_FROM from)
  resp2 <- Receive
  assertResponse resp2 250
  Transition SmtpFromSet
  
  Send (RCPT_TO to)
  resp3 <- Receive
  assertResponse resp3 250
  Transition SmtpRcptSet
  
  Send DATA
  resp4 <- Receive
  assertResponse resp4 354
  Transition SmtpData
  
  Send (BODY body)
  Send END_DATA
  resp5 <- Receive
  assertResponse resp5 250
  Transition SmtpDone
```

---

## ขั้นตอนที่ 911: Build System DSL

```haskell
-- Build system DSL

data Build a = Build
  { buildRules  :: Map Target (BuildRule a)
  , buildPhony  :: [Target]
  }

data BuildRule a = BuildRule
  { ruleDeps    :: [Target]
  , ruleAction  :: [FilePath] -> IO a
  , rulePHONY   :: Bool
  }

type Target = Text

-- DSL for build rules
data BuildM a = BuildM { runBuildM :: Build () -> (a, Build ()) }

rule :: Target -> [Target] -> ([FilePath] -> IO ()) -> BuildM ()
rule target deps action = BuildM (\b -> 
  ((), b { buildRules = Map.insert target (BuildRule deps action False) (buildRules b) }))

phony :: Target -> [Target] -> IO () -> BuildM ()
phony target deps action = BuildM (\b ->
  ((), b { buildRules = Map.insert target (BuildRule deps (\_ -> action) True) (buildRules b)
          , buildPhony = target : buildPhony b }))

-- Build file
myBuild :: BuildM ()
myBuild = do
  rule "app" ["src/Main.hs", "src/Lib.hs"] $ \_ -> do
    callProcess "ghc" ["-o", "app", "src/Main.hs"]
  
  rule "test" ["test/Main.hs", "app"] $ \_ -> do
    callProcess "runhaskell" ["test/Main.hs"]
  
  phony "clean" [] $ do
    removeFile "app"
    removeFile "app.hi"
    removeFile "app.o"
  
  phony "all" ["app", "test"] (return ())
```

---

## ขั้นตอนที่ 912: Animation DSL

```haskell
-- Animation/graphics DSL

data Anim a = Anim { runAnim :: Time -> a }

instance Functor Anim where
  fmap f (Anim a) = Anim (f . a)

instance Applicative Anim where
  pure a = Anim (const a)
  Anim f <*> Anim a = Anim (\t -> f t (a t))

-- Basic animations
constant :: a -> Anim a
constant = pure

linear :: Double -> Double -> Anim Double
linear start end = Anim (\t -> start + (end - start) * t)

-- Easing functions
easeIn :: Anim Double -> Anim Double
easeIn (Anim f) = Anim ((\t -> t * t) . f)

easeOut :: Anim Double -> Anim Double
easeOut (Anim f) = Anim ((\t -> t * (2 - t)) . f)

-- Temporal combinators
delay :: Time -> Anim a -> Anim (Maybe a)
delay d (Anim a) = Anim (\t -> if t < d then Nothing else Just (a (t - d)))

trim :: Time -> Time -> Anim a -> Anim a
trim start end (Anim a) = Anim (\t -> a (max start (min end t)))

loop :: Time -> Anim a -> Anim a
loop period (Anim a) = Anim (\t -> a (t `mod'` period))

-- Sequence animations
sequential :: [(Time, Anim a)] -> Anim a
sequential anims = Anim (\t -> 
  let relevant = filter (\(start, _) -> start <= t) anims
      (start, Anim a) = last relevant
  in a (t - start))

-- Particle system DSL
data Particle = Particle
  { pPos :: (Double, Double)
  , pVel :: (Double, Double)
  , pLife :: Double
  }

particleSystem :: Int -> [(Double, Double)] -> [Anim Particle]
particleSystem n initialVelocities = zipWith makeParticle [0..n-1] initialVelocities
  where
    makeParticle i (vx, vy) = Anim (\t ->
      Particle { pPos  = (vx * t, vy * t - 0.5 * 9.8 * t * t)
               , pVel  = (vx, vy - 9.8 * t)
               , pLife = 1.0 - t / 3.0 })
```

---

## ขั้นตอนที่ 913: Shader DSL

```haskell
-- GLSL-like shader DSL

-- Types
data Vec2 = Vec2 Float Float
data Vec3 = Vec3 Float Float Float
data Vec4 = Vec4 Float Float Float Float
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4

-- Shader expression
data ShaderExpr t where
  SEFloat :: Float -> ShaderExpr Float
  SEVec2  :: ShaderExpr Float -> ShaderExpr Float -> ShaderExpr Vec2
  SEVec3  :: ShaderExpr Float -> ShaderExpr Float -> ShaderExpr Float -> ShaderExpr Vec3
  SEAdd   :: Num t => ShaderExpr t -> ShaderExpr t -> ShaderExpr t
  SEMul   :: Num t => ShaderExpr t -> ShaderExpr t -> ShaderExpr t
  SEDot   :: ShaderExpr Vec3 -> ShaderExpr Vec3 -> ShaderExpr Float
  SENorm  :: ShaderExpr Vec3 -> ShaderExpr Vec3
  SEMix   :: ShaderExpr t -> ShaderExpr t -> ShaderExpr Float -> ShaderExpr t
  SEUniform :: Text -> ShaderExpr t  -- uniform variable
  SEVarying :: Text -> ShaderExpr t  -- varying variable

-- Fragment shader DSL
fragmentShader :: ShaderExpr Vec3 -> ShaderExpr Vec3 -> ShaderExpr Vec4
fragmentShader normal lightDir =
  let nNormal    = SENorm normal
      nLightDir  = SENorm lightDir
      diffuse    = SEDot nNormal nLightDir
      ambient    = SEFloat 0.1
      light      = SEAdd diffuse ambient
      baseColor  = SEVec3 (SEFloat 0.8) (SEFloat 0.5) (SEFloat 0.2)
      litColor   = SEMul baseColor (SEVec3 light light light)
  in SEVec4 (r litColor) (g litColor) (b litColor) (SEFloat 1.0)

-- Compile shader DSL to GLSL
toGLSL :: ShaderExpr t -> Text
toGLSL (SEFloat n)     = T.pack (show n)
toGLSL (SEAdd l r)     = "(" <> toGLSL l <> " + " <> toGLSL r <> ")"
toGLSL (SEMul l r)     = "(" <> toGLSL l <> " * " <> toGLSL r <> ")"
toGLSL (SEDot l r)     = "dot(" <> toGLSL l <> ", " <> toGLSL r <> ")"
toGLSL (SENorm v)      = "normalize(" <> toGLSL v <> ")"
toGLSL (SEUniform n)   = n
```

---

## ขั้นตอนที่ 914: Contract DSL

```haskell
-- Smart contract DSL

data ContractM a = ContractM
  { runContract :: ContractState -> IO (Either ContractError a, ContractState)
  }

data ContractState = ContractState
  { csBalances    :: Map Address Amount
  , csStorage     :: Map Key Value
  , csEvents      :: [Event]
  , csGasUsed     :: Gas
  }

data ContractError
  = InsufficientFunds Address Amount
  | InvalidState Text
  | OutOfGas
  | Revert Text

-- Transfer funds
transfer :: Address -> Address -> Amount -> ContractM ()
transfer from to amount = ContractM $ \state -> do
  let balance = fromMaybe 0 (Map.lookup from (csBalances state))
  if balance < amount
    then return (Left (InsufficientFunds from amount), state)
    else do
      let state' = state
            { csBalances = Map.insert from (balance - amount)
                         $ Map.insertWith (+) to amount
                         $ csBalances state
            }
      return (Right (), state')

-- Emit event
emit :: Event -> ContractM ()
emit event = ContractM $ \state ->
  return (Right (), state { csEvents = event : csEvents state })

-- Require condition
require :: Bool -> Text -> ContractM ()
require True  _   = ContractM (\s -> return (Right (), s))
require False msg = ContractM (\s -> return (Left (Revert msg), s))

-- Simple token contract
tokenTransfer :: Address -> Address -> Amount -> ContractM ()
tokenTransfer sender recipient amount = do
  balance <- getBalance sender
  require (balance >= amount) "Insufficient balance"
  transfer sender recipient amount
  emit (TransferEvent sender recipient amount)
```

---

## ขั้นตอนที่ 915: Music DSL

```haskell
-- Music notation DSL (Euterpea-inspired)

data Pitch = C | D | E | F | G | A | B deriving (Show, Eq, Ord, Enum)
data Octave = Oct3 | Oct4 | Oct5 deriving (Show, Eq)
data Duration = Whole | Half | Quarter | Eighth | Sixteenth deriving (Show, Eq)

data Note = Note Pitch Octave Duration | Rest Duration deriving (Show)

data Music
  = Single Note
  | Sequential Music Music   -- one after another
  | Parallel  Music Music   -- simultaneously (chord)
  | Modify    Modifier Music
  deriving (Show)

data Modifier
  = Tempo Double    -- multiply tempo
  | Transpose Int   -- semitones
  | Volume Int      -- 0-127
  deriving (Show)

-- Music combinators
(>:>) :: Music -> Music -> Music
(>:>) = Sequential

(<:>) :: Music -> Music -> Music
(<:>) = Parallel

note :: Pitch -> Octave -> Duration -> Music
note p o d = Single (Note p o d)

rest :: Duration -> Music
rest = Single . Rest

-- Chord
chord :: [(Pitch, Octave)] -> Duration -> Music
chord notes dur = foldl1 (<:>) [note p o dur | (p, o) <- notes]

-- Scale
cMajorScale :: [Music]
cMajorScale = [note p Oct4 Quarter | p <- [C, D, E, F, G, A, B, C]]

-- Twinkle Twinkle
twinkle :: Music
twinkle = foldl1 (>:>)
  [ note C Oct4 Quarter, note C Oct4 Quarter
  , note G Oct4 Quarter, note G Oct4 Quarter
  , note A Oct4 Quarter, note A Oct4 Quarter
  , note G Oct4 Half
  ]

-- Render to MIDI events
toMidi :: Music -> [(Time, MidiEvent)]
toMidi = go 0
  where
    go t (Single (Note p o d)) = [(t, NoteOn (pitchMidi p o)), (t + durTime d, NoteOff (pitchMidi p o))]
    go t (Sequential a b)      = let evs = go t a; t' = t + musicDuration a in evs ++ go t' b
    go t (Parallel a b)        = go t a ++ go t b
```

---

## ขั้นตอนที่ 916: Pipeline DSL

```haskell
-- Data transformation pipeline DSL

data Pipeline a b where
  Id       :: Pipeline a a
  Compose  :: Pipeline b c -> Pipeline a b -> Pipeline a c
  Map      :: (a -> b) -> Pipeline a b
  Filter   :: (a -> Bool) -> Pipeline a a
  Flatten  :: Pipeline [a] a
  Group    :: Ord k => (a -> k) -> Pipeline a [(k, [a])]
  Sort     :: Ord a => Pipeline a a
  Take     :: Int -> Pipeline a a
  Drop     :: Int -> Pipeline a a
  Zip      :: Pipeline b c -> Pipeline a (b, c)
  Parallel :: [Pipeline a b] -> Pipeline a [b]

-- Run pipeline
runPipeline :: Pipeline a b -> [a] -> [b]
runPipeline Id xs = xs
runPipeline (Compose g f) xs = runPipeline g (runPipeline f xs)
runPipeline (Map f) xs = map f xs
runPipeline (Filter p) xs = filter p xs
runPipeline Flatten xs = concatMap id xs
runPipeline (Group f) xs =
  let grouped = Map.toList (Map.fromListWith (++) [(f x, [x]) | x <- xs])
  in [grouped]  -- returns one element: the grouped list
runPipeline (Sort) xs = sort xs
runPipeline (Take n) xs = take n xs
runPipeline (Drop n) xs = drop n xs

-- Infix operator for pipeline composition
(|>) :: Pipeline a b -> Pipeline b c -> Pipeline a c
(|>) = flip Compose

-- Example pipeline
processOrders :: Pipeline Order ProcessedOrder
processOrders =
  Filter (\o -> orderStatus o == Pending)
  |> Sort  -- by date
  |> Take 100  -- process up to 100
  |> Map processOrder
```

---

## ขั้นตอนที่ 917: Validation DSL

```haskell
-- Validation DSL

newtype Validator a = Validator { runValidator :: a -> Validation [ValidationError] a }

data Validation e a = Failure e | Success a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative (Validation [e]) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)  -- collect all errors!
  Failure e  <*> Success _  = Failure e
  Success _  <*> Failure e  = Failure e
  Success f  <*> Success a  = Success (f a)

-- Primitive validators
notEmpty :: Text -> Validator Text
notEmpty fieldName = Validator $ \v ->
  if T.null v then Failure [fieldName <> " cannot be empty"]
  else Success v

minLength :: Int -> Text -> Validator Text
minLength n fieldName = Validator $ \v ->
  if T.length v < n then Failure [fieldName <> " must be at least " <> tshow n <> " characters"]
  else Success v

maxLength :: Int -> Text -> Validator Text
maxLength n fieldName = Validator $ \v ->
  if T.length v > n then Failure [fieldName <> " cannot exceed " <> tshow n <> " characters"]
  else Success v

isEmail :: Validator Text
isEmail = Validator $ \v ->
  if "@" `T.isInfixOf` v then Success v
  else Failure ["Invalid email address"]

isPositive :: (Num a, Ord a) => Text -> Validator a
isPositive fieldName = Validator $ \v ->
  if v > 0 then Success v
  else Failure [fieldName <> " must be positive"]

-- Compose validators
(.&&.) :: Validator a -> Validator a -> Validator a
v1 .&&. v2 = Validator $ \x ->
  case (runValidator v1 x, runValidator v2 x) of
    (Success _, res) -> res
    (err, _)         -> err

-- Validate a user form
data UserForm = UserForm { ufName :: Text, ufEmail :: Text, ufAge :: Int }

validateUserForm :: UserForm -> Validation [ValidationError] UserForm
validateUserForm form = UserForm
  <$> runValidator (notEmpty "name" .&&. maxLength 100 "name") (ufName form)
  <*> runValidator (notEmpty "email" .&&. isEmail) (ufEmail form)
  <*> runValidator (isPositive "age") (ufAge form)
```

---

## ขั้นตอนที่ 918: CLI DSL

```haskell
-- Command-line interface DSL

import Options.Applicative

-- Command type
data Command
  = CmdServer ServerOpts
  | CmdMigrate MigrateOpts
  | CmdSeed
  | CmdVersion

data ServerOpts = ServerOpts
  { soPort    :: Int
  , soHost    :: Text
  , soWorkers :: Int
  }

data MigrateOpts = MigrateOpts
  { moDirection :: Direction
  , moSteps     :: Maybe Int
  }

data Direction = MigrateUp | MigrateDown deriving (Show, Read)

-- Parser combinators
serverParser :: Parser Command
serverParser = CmdServer <$> (ServerOpts
  <$> option auto (long "port" <> short 'p' <> value 8080 <> metavar "PORT" <> help "Port to listen on")
  <*> strOption   (long "host" <> short 'H' <> value "0.0.0.0" <> metavar "HOST" <> help "Host to bind to")
  <*> option auto (long "workers" <> short 'w' <> value 4 <> metavar "N" <> help "Number of worker threads"))

migrateParser :: Parser Command
migrateParser = CmdMigrate <$> (MigrateOpts
  <$> option auto (long "direction" <> short 'd' <> value MigrateUp <> metavar "up|down" <> help "Migration direction")
  <*> optional (option auto (long "steps" <> short 'n' <> metavar "N" <> help "Number of steps")))

-- Main command parser
commandParser :: Parser Command
commandParser = hsubparser
  ( command "server"  (info serverParser  (progDesc "Start the server"))
  <> command "migrate" (info migrateParser (progDesc "Run database migrations"))
  <> command "seed"    (info (pure CmdSeed) (progDesc "Seed the database"))
  <> command "version" (info (pure CmdVersion) (progDesc "Show version"))
  )

main' :: IO ()
main' = execParser opts >>= runCommand
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
      <> progDesc "My Application"
      <> header "myapp - a simple web application" )
```

---

## ขั้นตอนที่ 919: State Machine DSL

```haskell
-- Type-safe state machine DSL

{-# LANGUAGE DataKinds, TypeFamilies, GADTs #-}

-- States as types
data TrafficLightState = Red | Yellow | Green

-- State machine transitions
data SM from to where
  ToGreen  :: SM 'Red    'Green
  ToYellow :: SM 'Green  'Yellow
  ToRed    :: SM 'Yellow 'Red

-- Typed state machine
data TSM (s :: TrafficLightState) = TSM

-- Only valid transitions are expressible
transition :: SM from to -> TSM from -> TSM to
transition _ _ = TSM

-- Example: valid sequence
validSequence :: TSM 'Red
validSequence =
  let green  = transition ToGreen  (TSM :: TSM 'Red)
      yellow = transition ToYellow green
      red    = transition ToRed    yellow
  in red

-- Invalid: cannot go directly Red -> Yellow (type error)
-- invalidSequence :: TSM 'Yellow
-- invalidSequence = transition ToYellow (TSM :: TSM 'Red)  -- TYPE ERROR

-- Process state machine DSL
data Process state result where
  Done    :: result -> Process state result
  Step    :: state -> IO newState -> (newState -> Process newState result) -> Process state result

processOrder :: Process 'Pending OrderResult
processOrder = Step Pending validatePayment $ \payResult ->
  if paymentValid payResult
    then Step Processing fulfillOrder $ \_ -> Done OrderComplete
    else Done OrderFailed
```

---

## ขั้นตอนที่ 920: โปรเจกต์: Complete DSL Framework

```haskell
-- Complete DSL framework: from spec to running application

module DslFramework where

-- Universal DSL type
data DSL effect input output where
  DSLPure   :: output -> DSL effect input output
  DSLEffect :: effect -> (a -> DSL effect input output) -> DSL effect input output  
  DSLInput  :: (input -> DSL effect input output) -> DSL effect input output
  DSLFail   :: Text -> DSL effect input output

instance Functor (DSL effect input) where
  fmap f (DSLPure a)       = DSLPure (f a)
  fmap f (DSLEffect e k)   = DSLEffect e (fmap f . k)
  fmap f (DSLInput k)      = DSLInput (fmap f . k)
  fmap _ (DSLFail err)     = DSLFail err

instance Monad (DSL effect input) where
  return = DSLPure
  DSLPure a       >>= f = f a
  DSLEffect e k   >>= f = DSLEffect e (\a -> k a >>= f)
  DSLInput k      >>= f = DSLInput (\i -> k i >>= f)
  DSLFail err     >>= _ = DSLFail err

-- Interpreter interface
class Interpreter interp where
  type EffectType interp :: *
  interpretEffect :: interp -> EffectType interp -> IO a

-- Run DSL with interpreter
runDSL :: Interpreter interp
       => interp
       -> DSL (EffectType interp) i o
       -> [i]
       -> IO (Either Text o, [i])
runDSL interp (DSLPure a) inputs = return (Right a, inputs)
runDSL interp (DSLFail e) inputs = return (Left e, inputs)
runDSL interp (DSLEffect e k) inputs = do
  result <- interpretEffect interp e
  runDSL interp (k result) inputs
runDSL interp (DSLInput k) [] = return (Left "Unexpected end of input", [])
runDSL interp (DSLInput k) (i:is) = runDSL interp (k i) is

-- Example: complete pipeline DSL
data PipelineEffect
  = ReadData  FilePath
  | WriteData FilePath ByteString
  | Transform TransformSpec
  | Validate  ValidationSpec
  | Notify    Text

userPipeline :: DSL PipelineEffect FilePath ()
userPipeline = do
  DSLEffect (ReadData "users.csv") $ \csvData ->
  DSLEffect (Transform (CsvToJson csvData)) $ \jsonData ->
  DSLEffect (Validate UserSchema) $ \validationResult ->
  DSLEffect (WriteData "users.json" jsonData) $ \() ->
  DSLEffect (Notify "Pipeline complete") $ \() ->
  DSLPure ()
```

---

*[← Part 45](part-45.md) | [Part 47 →](part-47.md)*
