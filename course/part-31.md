# Part 31: Advanced Testing Strategies
## ขั้นตอนที่ 601-620

---

## ขั้นตอนที่ 601: Property-Based Testing ขั้นสูง

```haskell
-- Property-based testing ด้วย QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Modifiers

-- Custom generators
data User = User
  { userId    :: Int
  , userName  :: Text
  , userEmail :: Text
  , userAge   :: Int
  }

instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
    <*> genName
    <*> genEmail
    <*> choose (18, 100)

genName :: Gen Text
genName = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ [' ']))

genEmail :: Gen Text
genEmail = do
  local  <- listOf1 (elements ['a'..'z'])
  domain <- listOf1 (elements ['a'..'z'])
  tld    <- elements ["com", "org", "net", "io"]
  return $ T.pack $ local ++ "@" ++ domain ++ "." ++ tld

-- Shrinking
instance Arbitrary User where
  arbitrary = ...
  shrink user = 
    [ user { userName = name' }
    | name' <- shrink (userName user)
    ] ++
    [ user { userAge = age' }
    | age' <- shrink (userAge user), age' >= 18
    ]

-- Properties
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

prop_sort_length :: [Int] -> Bool
prop_sort_length xs = length (sort xs) == length xs

prop_sort_ordered :: [Int] -> Bool
prop_sort_ordered xs = all (\(a,b) -> a <= b) (zip sorted (tail sorted))
  where sorted = sort xs

-- Conditional properties
prop_positive_sqrt :: Property
prop_positive_sqrt = forAll (choose (0.0, 1000.0 :: Double)) $ \n ->
  sqrt (n * n) `almostEqual` n

-- Labeling
prop_insert :: Map Int Int -> Int -> Int -> Property
prop_insert m k v = label category (Map.lookup k result == Just v)
  where
    result   = Map.insert k v m
    category = case Map.lookup k m of
      Nothing -> "new key"
      Just _  -> "update"
```

---

## ขั้นตอนที่ 602: QuickCheck State Machine

```haskell
-- State machine testing ด้วย quickcheck-state-machine

import Test.StateMachine
import Test.StateMachine.Types

-- Model the state machine
data Command r
  = Create CreateUserReq
  | Delete (Reference UserId r)
  | GetUser (Reference UserId r)
  | UpdateUser (Reference UserId r) UpdateUserReq
  deriving (Show, Generic)

data Response r
  = Created (Reference UserId r)
  | Deleted
  | GotUser (Maybe User)
  | Updated (Maybe User)
  deriving (Show, Generic)

-- Model state
data Model r = Model
  { modelUsers :: Map (Reference UserId r) User
  } deriving (Show, Generic)

initialModel :: Model r
initialModel = Model Map.empty

-- Preconditions
preconditions :: Model Symbolic -> Command Symbolic -> Logic
preconditions model (Delete ref)     = ref `elem` Map.keys (modelUsers model)
preconditions model (GetUser ref)    = ref `elem` Map.keys (modelUsers model)
preconditions model (UpdateUser ref _) = ref `elem` Map.keys (modelUsers model)
preconditions _ _                    = Top

-- Transitions
transitions :: Model r -> Command r -> Response r -> Model r
transitions model (Create req)     (Created ref) = 
  model { modelUsers = Map.insert ref (reqToUser req) (modelUsers model) }
transitions model (Delete ref)     Deleted      = 
  model { modelUsers = Map.delete ref (modelUsers model) }
transitions model _                _            = model

-- Postconditions
postconditions :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postconditions model (GetUser ref) (GotUser mUser) =
  Map.lookup ref (modelUsers model) .== mUser
postconditions _ _ _ = Top

-- Run state machine
prop_userStateMachine :: Property
prop_userStateMachine = forAllCommands sm Nothing $ \cmds ->
  monadicIO $ do
    (hist, model, res) <- runCommands sm cmds
    prettyCommands sm hist (checkCommandNames cmds (res === Ok))
```

---

## ขั้นตอนที่ 603: Hedgehog Property Testing

```haskell
-- Hedgehog: modern property testing

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- Generators
genUser :: Gen User
genUser = do
  uid   <- Gen.int (Range.linear 1 10000)
  name  <- Gen.text (Range.linear 2 50) Gen.alpha
  email <- genEmail
  age   <- Gen.int (Range.linear 18 100)
  return (User uid name email age)

genEmail :: Gen Text
genEmail = do
  local  <- Gen.text (Range.linear 3 20) Gen.alphaNum
  domain <- Gen.text (Range.linear 2 15) Gen.alpha
  tld    <- Gen.element ["com", "org", "net", "io"]
  return (local <> "@" <> domain <> "." <> tld)

-- Properties
prop_roundtrip :: Property
prop_roundtrip = property $ do
  user <- forAll genUser
  -- JSON round-trip
  tripping user encode (decode . BSL.toStrict)

prop_sortStable :: Property
prop_sortStable = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) (Gen.int (Range.linearBounded))
  let sorted = sort xs
  sort sorted === sorted  -- idempotent

-- Integration with Tasty
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Properties"
  [ testProperty "JSON roundtrip" prop_roundtrip
  , testProperty "sort stable" prop_sortStable
  ]
```

---

## ขั้นตอนที่ 604: Mutation Testing

```haskell
-- Mutation testing: ตรวจสอบว่า test จับ bug ได้

-- MuCheck library หรือ manual mutation

-- Original code
validateEmail :: Text -> Bool
validateEmail email = "@" `T.isInfixOf` email && "." `T.isInfixOf` email

-- Mutations (ใส่ bugs เพื่อทดสอบ):
-- Mutation 1: change && to ||
validateEmail1 :: Text -> Bool
validateEmail1 email = "@" `T.isInfixOf` email || "." `T.isInfixOf` email

-- Mutation 2: negate condition
validateEmail2 :: Text -> Bool
validateEmail2 email = not ("@" `T.isInfixOf` email) && "." `T.isInfixOf` email

-- Mutation 3: remove one check
validateEmail3 :: Text -> Bool
validateEmail3 email = "@" `T.isInfixOf` email

-- Good tests kill all mutations:
prop_validateEmail :: Property
prop_validateEmail = property $ do
  -- Valid emails pass
  forAll genValidEmail $ \email ->
    validateEmail email === True
  
  -- Invalid emails fail
  forAll genInvalidEmail $ \email ->
    validateEmail email === False

-- A test suite with high mutation score is well-tested!
-- Tool: run with MuCheck, check mutation score

-- Schema mutation testing for APIs
-- Test that changing validation rules breaks tests
mutationSuite :: Spec
mutationSuite = do
  describe "Email validation mutations" $ do
    it "catches mutation 1" $ validateEmail1 "nodot.com" `shouldBe` True   -- catches bug!
    it "catches mutation 2" $ validateEmail2 "valid@test.com" `shouldBe` False  -- catches!
    it "catches mutation 3" $ validateEmail3 "nodotatsign" `shouldBe` False  -- catches!
```

---

## ขั้นตอนที่ 605: Contract Testing

```haskell
-- Contract testing สำหรับ microservices

-- Consumer-Driven Contract Testing (Pact)
-- Service A (consumer) defines expectations
-- Service B (provider) verifies it meets them

-- Consumer side (User Service -> Post Service)
module UserService.Contracts where

import Test.Hspec
import Data.Pact.Consumer

postServiceContract :: Spec
postServiceContract = describe "Post Service" $ do
  describe "GET /posts/:userId" $ do
    it "returns posts for user" $ do
      -- Define interaction
      let interaction = Interaction
            { description = "get posts by user"
            , state = Just "user 1 has posts"
            , request = Request
                { method = "GET"
                , path   = "/posts/user/1"
                , headers = [("Accept", "application/json")]
                }
            , response = Response
                { status  = 200
                , headers = [("Content-Type", "application/json")]
                , body    = Just (encode [examplePost])
                }
            }
      
      -- Record interaction
      recordInteraction postServicePact interaction
      
      -- Make call to mock server
      posts <- fetchUserPosts mockServerUrl 1
      
      -- Verify
      length posts `shouldBe` 1
      postTitle (head posts) `shouldBe` "Test Post"

-- Provider side (Post Service verifies contract)
module PostService.ContractVerification where

verifyContracts :: IO ()
verifyContracts = do
  pacts <- loadPactFiles "pacts/"
  forM_ pacts $ \pact -> do
    results <- verifyPact postServiceApp pact
    mapM_ assertVerified results
```

---

## ขั้นตอนที่ 606: Fuzz Testing

```haskell
-- Fuzz testing ด้วย AFL/libFuzzer

import LlvmFFI.LibFuzzer

-- Fuzz target function
{-# ANN fuzzTarget LibFuzzerEntry #-}
fuzzTarget :: ByteString -> IO ()
fuzzTarget input = do
  -- ลอง parse input ทุกประเภท
  let _ = decode @Value input      -- JSON
  let _ = decode @User input       -- User
  case parseQuery input of         -- SQL query
    Left _ -> return ()
    Right q -> validateQuery q >>= \_ -> return ()

-- Property fuzzing
fuzzJsonRoundtrip :: ByteString -> IO ()
fuzzJsonRoundtrip input = do
  case decode @Value input of
    Nothing  -> return ()  -- not valid JSON, skip
    Just val ->
      -- JSON must round-trip
      let reEncoded = encode val
      in case decode @Value reEncoded of
        Nothing  -> error "Re-encoded invalid JSON!"
        Just val2 -> when (val /= val2) $
          error "JSON round-trip failed!"

-- Structure-aware fuzzing
import Test.QuickCheck.Gen (generate)

structuredFuzz :: IO ()
structuredFuzz = forever $ do
  -- Generate semi-valid input
  input <- generate $ frequency
    [ (70, genValidRequest)   -- 70% valid
    , (20, genMalformedRequest)  -- 20% malformed
    , (10, genRandomBytes)       -- 10% random
    ]
  
  -- Should not crash
  E.try (processRequest input) >>= \case
    Left (e :: SomeException) -> logCrash input e
    Right _ -> return ()
```

---

## ขั้นตอนที่ 607: Golden Tests

```haskell
-- Golden testing: test output matches golden file

import Test.Golden

-- Simple golden test
goldenTest :: TestTree
goldenTest = goldenVsString
  "format user output"
  "test/golden/user-output.txt"  -- golden file
  (return $ formatUser exampleUser)

-- Golden JSON tests
goldenJsonTest :: TestTree
goldenJsonTest = goldenVsString
  "user JSON serialization"
  "test/golden/user.json"
  (return $ BSL.toStrict $ encodePretty exampleUser)

-- Update golden files
-- Run tests with: --accept (creates/updates golden files)
-- Normal run: compare against existing golden files

-- Directory-based golden tests
goldenDirectory :: TestTree
goldenDirectory = goldenVsDirectory
  "template rendering"
  "test/golden/templates/"
  "test/input/templates/"
  renderAllTemplates

renderAllTemplates :: FilePath -> FilePath -> IO ()
renderAllTemplates inputDir outputDir = do
  templates <- listDirectory inputDir
  forM_ templates $ \template -> do
    input  <- readFile (inputDir </> template)
    output <- renderTemplate input
    writeFile (outputDir </> template) output

-- Snapshot testing
snapshotTest :: Spec
snapshotTest = do
  it "renders user card" $ do
    let html = renderUserCard exampleUser
    matchSnapshot "user-card" html

matchSnapshot :: Text -> Text -> IO ()
matchSnapshot name actual = do
  let snapshotPath = "test/snapshots/" <> T.unpack name <> ".html"
  exists <- doesFileExist snapshotPath
  if exists
    then do
      expected <- readFile snapshotPath
      actual `shouldBe` T.pack expected
    else do
      writeFile snapshotPath (T.unpack actual)
      pendingWith "Snapshot created, run again to verify"
```

---

## ขั้นตอนที่ 608: Load Testing

```haskell
-- Load testing ด้วย Haskell

import Control.Concurrent.Async
import Data.Time.Clock

-- Load test configuration
data LoadConfig = LoadConfig
  { lcConcurrency :: Int     -- concurrent users
  , lcDuration    :: Int     -- seconds
  , lcRampUp      :: Int     -- ramp up seconds
  , lcTargetRPS   :: Maybe Int  -- target requests/second
  }

data LoadResult = LoadResult
  { lrTotalRequests  :: Int
  , lrSuccessful     :: Int
  , lrFailed         :: Int
  , lrAvgLatency     :: Double
  , lrP99Latency     :: Double
  , lrMaxLatency     :: Double
  , lrRequestsPerSec :: Double
  }

-- Run load test
runLoadTest :: LoadConfig -> (IO (Either Text ())) -> IO LoadResult
runLoadTest config request = do
  latencies   <- newTVarIO []
  errors      <- newTVarIO 0
  successes   <- newTVarIO 0
  
  start <- getCurrentTime
  let deadline = addUTCTime (fromIntegral (lcDuration config)) start
  
  -- Create workers
  workers <- replicateM (lcConcurrency config) $ async $
    workerLoop deadline request latencies errors successes
  
  mapM_ wait workers
  
  -- Compute stats
  lats  <- readTVarIO latencies
  errs  <- readTVarIO errors
  succs <- readTVarIO successes
  
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  
  return LoadResult
    { lrTotalRequests  = errs + succs
    , lrSuccessful     = succs
    , lrFailed         = errs
    , lrAvgLatency     = if null lats then 0 else sum lats / fromIntegral (length lats)
    , lrP99Latency     = percentile 99 (sort lats)
    , lrMaxLatency     = if null lats then 0 else maximum lats
    , lrRequestsPerSec = fromIntegral (errs + succs) / elapsed
    }

workerLoop :: UTCTime -> IO (Either Text ()) -> TVar [Double] -> TVar Int -> TVar Int -> IO ()
workerLoop deadline request latencies errors successes = do
  now <- getCurrentTime
  when (now < deadline) $ do
    start  <- getCurrentTime
    result <- request
    end    <- getCurrentTime
    
    let latency = realToFrac (diffUTCTime end start) * 1000
    atomically $ modifyTVar latencies (latency:)
    
    case result of
      Left  _ -> atomically $ modifyTVar errors (+1)
      Right _ -> atomically $ modifyTVar successes (+1)
    
    workerLoop deadline request latencies errors successes
```

---

## ขั้นตอนที่ 609: Integration Testing Pipeline

```haskell
-- Integration test pipeline

import Test.Hspec
import Database.PostgreSQL.Simple

-- Test environment
data TestEnv = TestEnv
  { teDb       :: ConnectionPool
  , teRedis    :: RedisConn
  , teApp      :: Application
  , teManager  :: Manager
  }

-- Setup test environment
setupTestEnv :: IO TestEnv
setupTestEnv = do
  -- Fresh test database
  conn <- connectPostgreSQL "dbname=test_db"
  pool <- createPool (connectPostgreSQL "dbname=test_db") close 1 10 5
  
  -- Run migrations
  withConnection pool runMigrations
  
  -- Create app
  let app = makeTestApp pool
  
  manager <- newManager defaultManagerSettings
  redis   <- connectRedis
  
  return TestEnv
    { teDb      = pool
    , teRedis   = redis
    , teApp     = app
    , teManager = manager
    }

-- Cleanup
teardownTestEnv :: TestEnv -> IO ()
teardownTestEnv env = do
  -- Truncate all tables
  withConnection (teDb env) $ \conn ->
    execute_ conn "TRUNCATE users, posts, comments CASCADE"
  
  -- Flush Redis test namespace
  Redis.runRedis (teRedis env) $ Redis.flushdb

-- Test with clean state
withCleanEnv :: TestEnv -> IO () -> IO ()
withCleanEnv env action = do
  action
  teardownTestEnv env

-- Full integration test
integrationSpec :: TestEnv -> Spec
integrationSpec env = do
  beforeEach (teardownTestEnv env) $ do
    describe "User registration flow" $ do
      it "registers and logs in" $ do
        -- 1. Register
        let regReq = RegisterRequest "alice" "alice@test.com" "password123"
        regResp <- post (teApp env) "/api/v1/auth/register" regReq
        regResp `statusShouldBe` 201
        
        -- 2. Login
        let loginReq = LoginRequest "alice@test.com" "password123"
        loginResp <- post (teApp env) "/api/v1/auth/login" loginReq
        loginResp `statusShouldBe` 200
        
        let token = responseToken loginResp
        
        -- 3. Access protected resource
        meResp <- getWithAuth (teApp env) "/api/v1/me" token
        meResp `statusShouldBe` 200
        (responseUser meResp) `nameShouldBe` "alice"
```

---

## ขั้นตอนที่ 610: Test Data Management

```haskell
-- Test data management

-- Factory pattern
class Factory a where
  defaultFactory :: Gen a
  build :: Gen a
  build = defaultFactory

instance Factory User where
  defaultFactory = User
    <$> pure 0  -- auto-assigned
    <*> Gen.text (Range.linear 2 50) Gen.alpha
    <*> genEmail
    <*> Gen.int (Range.linear 18 80)

-- Override specific fields
buildUser :: Partial User -> Gen User
buildUser overrides = do
  base <- build @User
  return $ applyOverrides base overrides

-- Fixtures (static test data)
fixtures :: IO TestFixtures
fixtures = do
  let admin = User 1 "Admin" "admin@test.com" 30
  let user1 = User 2 "Alice" "alice@test.com" 25
  let user2 = User 3 "Bob"   "bob@test.com"   28
  let posts = [ Post 1 2 "First Post" "Content..." True
              , Post 2 2 "Second Post" "More..." True
              , Post 3 3 "Bob's Post" "Hello" False
              ]
  return TestFixtures
    { tfAdmin = admin
    , tfUsers = [user1, user2]
    , tfPosts = posts
    }

-- Seed test database
seedDatabase :: TestEnv -> IO TestFixtures
seedDatabase env = do
  f <- fixtures
  runDB (teDb env) $ do
    insertMany_ [tfAdmin f, tfUsers f !! 0, tfUsers f !! 1]
    insertMany_ (tfPosts f)
  return f

-- Random data generation
generateTestScenario :: IO TestScenario
generateTestScenario = do
  users    <- replicateM 10 (generate genUser)
  posts    <- replicateM 30 (generate genPost)
  comments <- replicateM 100 (generate genComment)
  return TestScenario{..}
```

---

## ขั้นตอนที่ 611: API Testing DSL

```haskell
-- DSL สำหรับ API testing

-- Type-safe API test DSL
data ApiTest = ApiTest
  { atMethod  :: Method
  , atPath    :: Text
  , atBody    :: Maybe Value
  , atHeaders :: [(Text, Text)]
  , atAsserts :: [Assert]
  }

data Assert
  = StatusIs Int
  | BodyContains Text
  | BodyMatches (Value -> Bool)
  | HeaderIs Text Text
  | ResponseTimeLt Double  -- milliseconds

-- DSL functions
get' :: Text -> ApiTest
get' path = ApiTest GET path Nothing [] []

post' :: ToJSON a => Text -> a -> ApiTest
post' path body = ApiTest POST path (Just (toJSON body)) [] []

withAuth :: Text -> ApiTest -> ApiTest
withAuth token test = test
  { atHeaders = ("Authorization", "Bearer " <> token) : atHeaders test }

expectStatus :: Int -> ApiTest -> ApiTest
expectStatus n test = test { atAsserts = StatusIs n : atAsserts test }

expectBody :: ToJSON a => a -> ApiTest -> ApiTest
expectBody body test = test
  { atAsserts = BodyMatches (\v -> v == toJSON body) : atAsserts test }

-- Run tests
runApiTest :: Application -> ApiTest -> IO TestResult
runApiTest app test = do
  let req = buildRequest test
  resp <- runAppRequest app req
  results <- mapM (runAssert resp) (atAsserts test)
  return (TestResult (and results) results)

-- Example test
exampleTests :: Application -> Spec
exampleTests app = do
  it "creates user" $ runApiTest app $
    post' "/api/v1/users" createUserReq
    `expectStatus` 201
    `expectBodyContains` "id"
  
  it "gets user" $ runApiTest app $
    get' "/api/v1/users/1"
    `expectStatus` 200
    `expectBody` exampleUser
```

---

## ขั้นตอนที่ 612: Database Testing

```haskell
-- Database testing patterns

-- In-memory SQLite for unit tests
import Database.SQLite.Simple

withTestDb :: (Connection -> IO a) -> IO a
withTestDb action = do
  conn <- open ":memory:"
  runMigrations conn
  result <- action conn
  close conn
  return result

-- Transaction rollback pattern (keep DB clean)
withRollback :: ConnectionPool -> DB a -> IO a
withRollback pool action = do
  conn <- takeConnection pool
  result <- withTransaction conn $ do
    r <- action
    rollback  -- always rollback after test!
    return r
  putConnection pool conn
  return result

-- Test-specific data isolation
withTestSchema :: ConnectionPool -> (ConnectionPool -> IO a) -> IO a
withTestSchema pool action = do
  schemaName <- T.pack . show <$> newUUID
  
  runSQL pool $ "CREATE SCHEMA " <> schemaName
  runSQL pool $ "SET search_path TO " <> schemaName
  
  result <- action pool
  
  runSQL pool $ "DROP SCHEMA " <> schemaName <> " CASCADE"
  
  return result

-- Migration testing
migrationsSpec :: Spec
migrationsSpec = do
  describe "Database migrations" $ do
    it "can migrate up and down" $ withTestDb $ \conn -> do
      -- Run all migrations
      migrateUp conn
      -- Verify schema
      tables <- getTableNames conn
      tables `shouldContain` ["users", "posts", "comments"]
      -- Roll back
      migrateDown conn
      -- Verify clean
      tables' <- getTableNames conn
      tables' `shouldBe` []
    
    it "each migration is idempotent" $ withTestDb $ \conn -> do
      migrateUp conn
      migrateUp conn  -- run again, should not fail
      tables <- getTableNames conn
      length tables `shouldBe` 10  -- same count
```

---

## ขั้นตอนที่ 613: Test Coverage Analysis

```haskell
-- Test coverage ด้วย HPC (Haskell Program Coverage)

-- Enable coverage:
-- cabal test --enable-coverage

-- HPC tools:
-- hpc report main.tix        -- coverage report
-- hpc markup main.tix        -- HTML report
-- hpc combine main1.tix main2.tix --union > combined.tix

-- Code to test (with coverage markers)
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)

-- HPC will track which branches are exercised
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Empty              = Node x Empty Empty  -- branch 1
insert x (Node y left right)
  | x < y    = Node y (insert x left) right   -- branch 2
  | x > y    = Node y left (insert x right)  -- branch 3
  | otherwise = Node y left right              -- branch 4

-- Tests for full coverage
insertSpec :: Spec
insertSpec = do
  it "inserts into empty tree"    $ ... -- covers branch 1
  it "inserts left"               $ ... -- covers branch 2
  it "inserts right"              $ ... -- covers branch 3
  it "ignores duplicate"          $ ... -- covers branch 4

-- Coverage thresholds in CI
checkCoverage :: IO ()
checkCoverage = do
  coverageFile <- readFile "main.tix"
  let coverage = parseCoverage coverageFile
  when (coverage < 80.0) $
    die $ "Coverage " ++ show coverage ++ "% is below 80% threshold"
```

---

## ขั้นตอนที่ 614: Concurrent Testing

```haskell
-- Testing concurrent code

import Test.Concur

-- Test for race conditions
prop_noRaceCondition :: Property
prop_noRaceCondition = monadicIO $ do
  counter <- liftIO (newTVarIO 0)
  
  -- 100 concurrent increments
  liftIO $ replicateConcurrently_ 100 $
    atomically (modifyTVar counter (+1))
  
  result <- liftIO (readTVarIO counter)
  
  -- Should be exactly 100 if no race condition
  assert (result == 100)

-- Test with controlled concurrency (dejafu)
import Control.Concurrent.Classy
import Test.DejaFu

-- Test that two threads produce consistent results
consistencyTest :: MonadConc m => m (Int, Int)
consistencyTest = do
  var <- newMVar 0
  
  t1 <- fork $ do
    modifyMVar_ var (\n -> return (n + 1))
    readMVar var
  
  t2 <- fork $ do
    modifyMVar_ var (\n -> return (n + 1))
    readMVar var
  
  r1 <- readMVar =<< return t1
  r2 <- readMVar =<< return t2
  return (r1, r2)

-- DejaFu explores all interleavings
prop_consistency :: Property
prop_consistency = dejafuProp consistencyTest $ \result ->
  case result of
    (1, 2) -> True  -- t1 first
    (2, 2) -> True  -- t2 first
    _      -> False  -- invalid!
```

---

## ขั้นตอนที่ 615: API Snapshot Testing

```haskell
-- API snapshot testing: catch unexpected changes

-- Record API responses
snapshotApi :: Text -> Handler Value -> Text -> IO ()
snapshotApi name handler path = do
  response <- runTestRequest handler path
  
  let snapshotPath = "test/api-snapshots/" <> T.unpack name <> ".json"
  exists <- doesFileExist snapshotPath
  
  if exists
    then do
      existing <- decode <$> readFile snapshotPath
      when (existing /= Just response) $
        fail $ "API response changed for " ++ T.unpack name ++ "!\n" ++
               "Expected: " ++ show existing ++ "\n" ++
               "Got: " ++ show response
    else do
      createDirectoryIfMissing True "test/api-snapshots"
      writeFile snapshotPath (BSL.unpack (encodePretty response))
      putStrLn $ "Snapshot created: " ++ T.unpack name

-- Automated API changelog
generateChangelog :: IO ()
generateChangelog = do
  currentSpec <- generateOpenApiSpec
  oldSpec     <- readFile "api-spec.json"
  
  let changes = diffOpenApiSpec oldSpec currentSpec
  
  unless (null changes) $ do
    putStrLn "API Changes detected:"
    forM_ changes $ \change -> do
      putStrLn $ "  " ++ showChange change
    
    writeFile "CHANGELOG.md" (formatChangelog changes)
```

---

## ขั้นตอนที่ 616: Correctness Proofs (Liquid Haskell)

```haskell
-- LiquidHaskell: refinement types สำหรับ proofs

{-# ANN module "LH" #-}

import Data.List (sort)

-- Specify invariants with refinement types
{-@ type SortedList a = {v:[a] | isSorted v} @-}

{-@ isSorted :: Ord a => [a] -> Bool @-}
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Prove sort returns sorted list
{-@ sort :: Ord a => [a] -> SortedList a @-}
sort :: Ord a => [a] -> [a]
sort = Data.List.sort  -- LiquidHaskell will verify!

-- Safe indexing
{-@ safeHead :: {v:[a] | len v > 0} -> a @-}
safeHead :: [a] -> a
safeHead (x:_) = x
safeHead []    = error "impossible"  -- LiquidHaskell knows this is unreachable!

-- Bounds checking
{-@ safeIndex :: xs:[a] -> {i:Int | 0 <= i && i < len xs} -> a @-}
safeIndex :: [a] -> Int -> a
safeIndex (x:_) 0  = x
safeIndex (_:xs) i = safeIndex xs (i-1)
safeIndex _ _      = error "impossible"

-- Non-negative numbers
{-@ type NonNeg = {v:Int | v >= 0} @-}
{-@ factorial :: NonNeg -> NonNeg @-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

---

## ขั้นตอนที่ 617: Test Automation

```haskell
-- Test automation ใน CI/CD

-- Test runner with reporting
data TestReport = TestReport
  { trPassed  :: Int
  , trFailed  :: Int
  , trSkipped :: Int
  , trTime    :: Double
  , trFailures :: [TestFailure]
  }

data TestFailure = TestFailure
  { tfName    :: Text
  , tfError   :: Text
  , tfLocation :: Text
  }

-- JUnit XML output สำหรับ CI
generateJUnitXml :: TestReport -> Text
generateJUnitXml report = 
  "<?xml version=\"1.0\"?>\n" <>
  "<testsuite tests=\"" <> T.pack (show total) <> "\" " <>
  "failures=\"" <> T.pack (show (trFailed report)) <> "\" " <>
  "time=\"" <> T.pack (show (trTime report)) <> "\">\n" <>
  T.concat (map failureToXml (trFailures report)) <>
  "</testsuite>"
  where
    total = trPassed report + trFailed report + trSkipped report

failureToXml :: TestFailure -> Text
failureToXml tf =
  "<testcase name=\"" <> tfName tf <> "\">\n" <>
  "  <failure>" <> escapeXml (tfError tf) <> "</failure>\n" <>
  "</testcase>\n"

-- Parallel test execution
runTestsParallel :: [Spec] -> IO TestReport
runTestsParallel specs = do
  start   <- getCurrentTime
  results <- mapConcurrently runSpec specs
  end     <- getCurrentTime
  
  let allResults = concat results
  let passed     = length (filter isPass allResults)
  let failed     = length (filter isFail allResults)
  
  return TestReport
    { trPassed  = passed
    , trFailed  = failed
    , trSkipped = 0
    , trTime    = realToFrac (diffUTCTime end start)
    , trFailures = extractFailures allResults
    }
```

---

## ขั้นตอนที่ 618: Behavioral Testing (BDD)

```haskell
-- BDD-style testing

import Test.Hspec

-- Feature: User registration
-- Scenario: Valid registration
-- Given: no existing user with this email
-- When: user submits registration form with valid data
-- Then: account is created and welcome email sent

registrationSpec :: Spec
registrationSpec = do
  describe "User registration feature" $ do
    context "when registering with valid email and password" $ do
      before (setupCleanState >> setupSmtpMock) $ do
        it "creates the user account" $ \env -> do
          let req = RegisterReq "alice@test.com" "Password123!"
          result <- register env req
          result `shouldSatisfy` isSuccess
        
        it "sends welcome email" $ \env -> do
          let req = RegisterReq "alice@test.com" "Password123!"
          register env req
          emailsSent <- getSentEmails env
          any (isWelcomeEmail "alice@test.com") emailsSent `shouldBe` True
        
        it "allows login after registration" $ \env -> do
          let req = RegisterReq "alice@test.com" "Password123!"
          register env req
          let loginReq = LoginReq "alice@test.com" "Password123!"
          loginResult <- login env loginReq
          loginResult `shouldSatisfy` isSuccess
    
    context "when registering with duplicate email" $ do
      before (setupCleanState >> seedUser "alice@test.com") $ do
        it "returns conflict error" $ \env -> do
          let req = RegisterReq "alice@test.com" "NewPassword123!"
          result <- register env req
          result `shouldSatisfy` isEmailConflict
```

---

## ขั้นตอนที่ 619: E2E Testing

```haskell
-- End-to-end testing ด้วย Playwright (หรือ Selenium)

import Test.WebDriver
import Test.WebDriver.Commands

-- E2E test
loginE2E :: WD ()
loginE2E = do
  openPage "http://localhost:3000/login"
  
  emailInput <- findElem (ByName "email")
  sendKeys "alice@test.com" emailInput
  
  passwordInput <- findElem (ByName "password")
  sendKeys "password123" passwordInput
  
  submitBtn <- findElem (ByXPath "//button[@type='submit']")
  click submitBtn
  
  -- Wait for redirect
  waitFor 5000 (isElem (ById "dashboard"))
  
  -- Verify logged in
  title <- getTitle
  title `shouldContain` "Dashboard"
  
  -- Get user name displayed
  userEl  <- findElem (ByClassName "user-name")
  userName <- getText userEl
  userName `shouldBe` "Alice"

-- Run E2E tests
e2eSpec :: Spec
e2eSpec = do
  describe "User flows" $ do
    it "completes login flow" $ runWD chromeConfig loginE2E
    it "completes registration" $ runWD chromeConfig registrationE2E
    it "creates post" $ runWD chromeConfig createPostE2E

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig
  { wdHost     = "localhost"
  , wdPort     = 4444
  , wdBasePath = "/wd/hub"
  }
```

---

## ขั้นตอนที่ 620: โปรเจกต์: Complete Test Suite

```haskell
-- Complete test suite สำหรับ production app

-- test/Spec.hs
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HSpec

main :: IO ()
main = do
  env <- setupTestEnv
  defaultMain (allTests env)

allTests :: TestEnv -> TestTree
allTests env = testGroup "All Tests"
  [ unitTests
  , propertyTests
  , integrationTests env
  , apiTests env
  , performanceTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "validateEmail valid"   $ validateEmail "a@b.com" @?= True
  , testCase "validateEmail invalid" $ validateEmail "notvalid" @?= False
  , testCase "hashPassword"          $ do
      hash1 <- hashPassword "secret"
      hash2 <- hashPassword "secret"
      hash1 @?/= hash2  -- salted, should differ!
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ QC.testProperty "sort idempotent"   prop_sort_idempotent
  , QC.testProperty "encode/decode"     prop_roundtrip
  , QC.testProperty "pagination valid"  prop_pagination
  ]

integrationTests :: TestEnv -> TestTree
integrationTests env = testGroup "Integration Tests"
  [ testCase "user CRUD" $ withCleanEnv env $ userCrudTest env
  , testCase "post flow"  $ withCleanEnv env $ postFlowTest env
  ]

apiTests :: TestEnv -> TestTree
apiTests env = testGroup "API Tests"
  [ testCase "GET /users"  $ apiTest env (get' "/api/v1/users") (expectStatus 200)
  , testCase "POST /users" $ apiTest env (post' "/api/v1/users" createUserReq) (expectStatus 201)
  ]

performanceTests :: TestTree
performanceTests = testGroup "Performance Tests"
  [ bench "sort 10k ints" $ nf sort ([1..10000] :: [Int])
  , bench "JSON encode user" $ nf encode exampleUser
  ]
```

---

*[← Part 30](part-30.md) | [Part 32 →](part-32.md)*
