# Part 17: Database Integration ขั้นสูง
## ขั้นตอนที่ 321-340: Persistent, Esqueleto, Migrations

---

## บทนำ

Database integration ใน Haskell มี library หลักๆ คือ Persistent (ORM), Esqueleto (type-safe SQL), และ hasql/postgresql-simple (low-level)

---

## ขั้นตอนที่ 321: Persistent ORM Overview

```haskell
-- Persistent: type-safe database access

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Database.Persist
import Database.Persist.TH

-- Schema definition
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name    Text
    email   Text
    age     Int
    active  Bool  default=True
    created UTCTime  default=now()
    UniqueEmail email
    deriving Show

Post
    title   Text
    content Text
    userId  UserId
    published Bool default=False
    created UTCTime default=now()
    deriving Show

Tag
    name    Text
    UniqueTag name
    deriving Show

PostTag
    postId  PostId
    tagId   TagId
    Primary postId tagId
    deriving Show
|]

-- Generates:
-- - data User = User { userName :: Text, userEmail :: Text, ... }
-- - data UserId
-- - instance PersistEntity User
-- - etc.
```

---

## ขั้นตอนที่ 322: Database Connection Pool

```haskell
import Database.Persist.Postgresql
import Database.Persist.Sqlite
import Control.Monad.Logger

-- PostgreSQL connection
connectPostgres :: Text -> IO ConnectionPool
connectPostgres connStr = runStderrLoggingT $
  createPostgresqlPool connStr 10  -- 10 connections

-- SQLite (สำหรับ development/testing)
connectSqlite :: Text -> IO ConnectionPool
connectSqlite path = runStderrLoggingT $
  createSqlitePool path 1  -- 1 connection (SQLite ไม่รองรับ concurrent writes)

-- Run migrations
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runSqlPool (runMigration migrateAll) pool

-- Environment-based connection
connectFromEnv :: IO ConnectionPool
connectFromEnv = do
  env <- getEnvironment
  let connStr = fromMaybe "postgresql://localhost/myapp" (lookup "DATABASE_URL" env)
  connectPostgres (T.pack connStr)

-- ตัวอย่าง: Full setup
setupDatabase :: IO ConnectionPool
setupDatabase = do
  pool <- connectFromEnv
  runMigrations pool
  return pool
```

---

## ขั้นตอนที่ 323: CRUD Operations

```haskell
import Database.Persist
import Database.Persist.Postgresql

-- Create
createUser :: ConnectionPool -> Text -> Text -> Int -> IO UserId
createUser pool name email age = runSqlPool pool $
  insert (User name email age True)
-- ถ้า UniqueEmail conflict: ได้ constraint violation exception

-- insertUnique: return Nothing ถ้า conflict
createUserSafe :: ConnectionPool -> Text -> Text -> Int -> IO (Maybe UserId)
createUserSafe pool name email age = runSqlPool pool $
  insertUnique (User name email age True)

-- Read
getUserById :: ConnectionPool -> UserId -> IO (Maybe User)
getUserById pool uid = runSqlPool pool $
  get uid  -- get :: Key record -> SqlPersistT IO (Maybe record)

-- Read with Entity (includes key)
getUserEntity :: ConnectionPool -> UserId -> IO (Maybe (Entity User))
getUserEntity pool uid = runSqlPool pool $
  selectFirst [UserId ==. uid] []

-- List all
listUsers :: ConnectionPool -> IO [Entity User]
listUsers pool = runSqlPool pool $
  selectList [] [Asc UserId]

-- Update
updateUser :: ConnectionPool -> UserId -> Text -> IO ()
updateUser pool uid newName = runSqlPool pool $
  update uid [UserName =. newName]

-- Update multiple fields
updateMultiple :: ConnectionPool -> UserId -> Text -> Int -> IO ()
updateMultiple pool uid name age = runSqlPool pool $
  update uid [UserName =. name, UserAge =. age]

-- Delete
deleteUser :: ConnectionPool -> UserId -> IO ()
deleteUser pool uid = runSqlPool pool $
  delete uid

-- Delete by condition
deleteInactiveUsers :: ConnectionPool -> IO ()
deleteInactiveUsers pool = runSqlPool pool $
  deleteWhere [UserActive ==. False]
```

---

## ขั้นตอนที่ 324: Querying

```haskell
-- Persistent query combinators

-- Filter operators
-- ==. /=. <. <=. >. >=. : comparison
-- &&. ||.                : boolean
-- In. NotIn.             : list membership
-- Like. ILike.           : pattern matching
-- IsNull. NotNull.       : null checks

-- ตัวอย่าง queries
import Database.Persist

-- Find active users
activeUsers :: ConnectionPool -> IO [Entity User]
activeUsers pool = runSqlPool pool $
  selectList [UserActive ==. True] []

-- Age range
usersInAgeRange :: ConnectionPool -> Int -> Int -> IO [Entity User]
usersInAgeRange pool minAge maxAge = runSqlPool pool $
  selectList [UserAge >=. minAge, UserAge <=. maxAge] []

-- OR condition
findByNameOrEmail :: ConnectionPool -> Text -> IO [Entity User]
findByNameOrEmail pool query = runSqlPool pool $
  selectList [UserName ==. query ||. UserEmail ==. query] []

-- In list
getUsersByIds :: ConnectionPool -> [UserId] -> IO [Entity User]
getUsersByIds pool uids = runSqlPool pool $
  selectList [UserId <-. uids] []

-- Sort and limit
topUsers :: ConnectionPool -> Int -> IO [Entity User]
topUsers pool n = runSqlPool pool $
  selectList [] [Asc UserName, LimitTo n]

-- Pagination
paginateUsers :: ConnectionPool -> Int -> Int -> IO [Entity User]
paginateUsers pool page size = runSqlPool pool $
  selectList [] [Asc UserId, OffsetBy ((page-1)*size), LimitTo size]

-- Count
countActiveUsers :: ConnectionPool -> IO Int
countActiveUsers pool = runSqlPool pool $
  count [UserActive ==. True]
```

---

## ขั้นตอนที่ 325: Esqueleto (Type-Safe SQL)

```haskell
-- Esqueleto: type-safe SQL joins และ complex queries

import Database.Esqueleto.Experimental

-- JOIN query
getUsersWithPosts :: ConnectionPool -> IO [(Entity User, Entity Post)]
getUsersWithPosts pool = runSqlPool pool $
  select $ do
    (user :& post) <- from $ table @User
      `innerJoin` table @Post
      `on` (\(u :& p) -> u ^. UserId ==. p ^. PostUserId)
    return (user, post)

-- LEFT JOIN
getUsersWithOptionalPosts :: ConnectionPool -> IO [(Entity User, Maybe (Entity Post))]
getUsersWithOptionalPosts pool = runSqlPool pool $
  select $ do
    (user :& mPost) <- from $ table @User
      `leftJoin` table @Post
      `on` (\(u :& p) -> just (u ^. UserId) ==. p ?. PostUserId)
    return (user, mPost)

-- WHERE clause
activeUsersWithPosts :: ConnectionPool -> IO [(Entity User, Entity Post)]
activeUsersWithPosts pool = runSqlPool pool $
  select $ do
    (user :& post) <- from $ table @User
      `innerJoin` table @Post
      `on` (\(u :& p) -> u ^. UserId ==. p ^. PostUserId)
    where_ (user ^. UserActive ==. val True)
    where_ (post ^. PostPublished ==. val True)
    return (user, post)

-- GROUP BY และ aggregate
postCountByUser :: ConnectionPool -> IO [(Entity User, Value Int)]
postCountByUser pool = runSqlPool pool $
  select $ do
    (user :& post) <- from $ table @User
      `leftJoin` table @Post
      `on` (\(u :& p) -> just (u ^. UserId) ==. p ?. PostUserId)
    groupBy (user ^. UserId)
    return (user, countRows)

-- ORDER BY
sortedUsers :: ConnectionPool -> IO [Entity User]
sortedUsers pool = runSqlPool pool $
  select $ do
    user <- from (table @User)
    orderBy [asc (user ^. UserName), desc (user ^. UserAge)]
    return user
```

---

## ขั้นตอนที่ 326: Complex Queries

```haskell
-- Subqueries และ complex expressions

-- Subquery: users ที่มีอย่างน้อย 5 posts
usersWithManyPosts :: ConnectionPool -> IO [Entity User]
usersWithManyPosts pool = runSqlPool pool $
  select $ do
    user <- from (table @User)
    let postCount = subSelect $ do
          post <- from (table @Post)
          where_ (post ^. PostUserId ==. user ^. UserId)
          return countRows
    having (postCount >. val (5 :: Int))
    return user

-- CASE WHEN
userActivity :: ConnectionPool -> IO [(Value Text, Value Text)]
userActivity pool = runSqlPool pool $
  select $ do
    user <- from (table @User)
    let status = case_
          [ when_ (user ^. UserActive ==. val True)  then_ (val "Active")
          , when_ (user ^. UserActive ==. val False) then_ (val "Inactive")
          ]
          (else_ (val "Unknown"))
    return (user ^. UserName, status)

-- Complex filter
searchUsers :: ConnectionPool -> Text -> Maybe Int -> Maybe Bool -> IO [Entity User]
searchUsers pool query mAge mActive = runSqlPool pool $
  select $ do
    user <- from (table @User)
    let conditions = catMaybes
          [ Just (user ^. UserName `like` val ("%" <> query <> "%"))
          , fmap (\age -> user ^. UserAge ==. val age) mAge
          , fmap (\active -> user ^. UserActive ==. val active) mActive
          ]
    when_ (foldr1 (&&.) conditions)
    return user
```

---

## ขั้นตอนที่ 327: Transactions

```haskell
-- Database transactions

import Database.Persist

-- runSqlPool ทุก operation อยู่ใน single transaction
-- ถ้า exception: rollback อัตโนมัติ

-- Explicit transaction
transferMoney :: ConnectionPool -> UserId -> UserId -> Int -> IO ()
transferMoney pool fromUser toUser amount = runSqlPool pool $ do
  from <- get fromUser
  to   <- get toUser
  
  case (from, to) of
    (Just f, Just t) -> do
      -- Both exist: do transfer
      update fromUser [UserBalance -=. amount]
      update toUser   [UserBalance +=. amount]
    _ -> liftIO $ throwIO (userError "User not found")

-- Savepoints (nested transactions)
nestedTransaction :: ConnectionPool -> IO ()
nestedTransaction pool = runSqlPool pool $ do
  -- Outer transaction
  uid <- insert (User "Alice" "alice@example.com" 30 True)
  
  -- Try inner operation (might fail)
  savepoint <- createSavepoint
  result <- try $ insert (Post "Alice's Post" "Content" uid False)
  
  case result of
    Left  _ -> rollbackToSavepoint savepoint
    Right _ -> return ()

-- transactionSave: flush without committing
batchInsert :: ConnectionPool -> [User] -> IO [UserId]
batchInsert pool users = runSqlPool pool $
  forM users $ \user -> do
    uid <- insert user
    transactionSave  -- flush to allow other reads to see progress
    return uid
```

---

## ขั้นตอนที่ 328: Migration Management

```haskell
-- Database Migrations

-- 1. Persistent auto-migration (development)
autoMigrate :: ConnectionPool -> IO ()
autoMigrate pool = runSqlPool pool $ do
  runMigration migrateAll

-- 2. Check migration safety
checkMigration :: ConnectionPool -> IO [Text]
checkMigration pool = runSqlPool pool $ do
  stmts <- getMigration migrateAll
  return stmts

-- 3. Manual migration (production)
-- ใช้ sql-migrate หรือ flyway สำหรับ production

-- Migration file structure:
-- migrations/
--   001_create_users.sql
--   002_add_posts.sql
--   003_add_indexes.sql

-- ตัวอย่าง migration SQL
{-
-- 001_create_users.sql
CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    age INTEGER NOT NULL,
    active BOOLEAN NOT NULL DEFAULT true,
    created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_users_active ON users(active) WHERE active = true;

-- 002_add_posts.sql
CREATE TABLE IF NOT EXISTS posts (
    id SERIAL PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    content TEXT NOT NULL,
    user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    published BOOLEAN NOT NULL DEFAULT false,
    created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE INDEX idx_posts_published ON posts(published) WHERE published = true;
-}

-- Run migrations with migrate library
import Database.Migrate

runManualMigrations :: Connection -> IO ()
runManualMigrations conn = do
  migrate conn "migrations"
```

---

## ขั้นตอนที่ 329: Connection Pool Configuration

```haskell
-- ปรับ connection pool settings

import Database.Persist.Postgresql
import Data.Pool

-- Custom pool settings
createCustomPool :: ConnectionString -> IO ConnectionPool
createCustomPool connStr = runStderrLoggingT $
  createPostgresqlPoolWithConf connStr poolSettings
  where
    poolSettings = PostgresConf
      { pgConnStr     = connStr
      , pgPoolSize    = 20           -- max connections
      , pgPoolIdleTimeout = 300      -- 5 minutes
      , pgPoolStripes = 1
      }

-- Pool statistics
checkPool :: Pool Connection -> IO ()
checkPool pool = do
  stats <- Pool.getPoolStats pool  -- not in standard API, illustrative
  putStrLn $ "Pool stats: " ++ show stats

-- ใช้ raw connection
withRawConnection :: ConnectionPool -> (Connection -> IO a) -> IO a
withRawConnection pool action = 
  withResource pool $ \backend -> do
    conn <- rawConn backend  -- get underlying connection
    action conn

-- Health check for pool
pingDatabase :: ConnectionPool -> IO Bool
pingDatabase pool = do
  result <- try $ runSqlPool pool $ do
    rawExecute "SELECT 1" []
    return True
  return $ case result of
    Left  (_ :: SomeException) -> False
    Right _                    -> True
```

---

## ขั้นตอนที่ 330: Raw SQL

```haskell
-- Raw SQL สำหรับ complex queries

import Database.Persist.Postgresql
import Database.Persist.Sql

-- rawSql: execute SQL and parse results
getUsersRaw :: ConnectionPool -> IO [Single Text]
getUsersRaw pool = runSqlPool pool $
  rawSql "SELECT name FROM users WHERE active = TRUE" []

-- Interpolation ด้วย PersistValue
getUserByEmail :: ConnectionPool -> Text -> IO [Single Text]
getUserByEmail pool email = runSqlPool pool $
  rawSql "SELECT name FROM users WHERE email = ?" [PersistText email]

-- Multiple columns
userInfo :: ConnectionPool -> IO [(Single Int, Single Text, Single Int)]
userInfo pool = runSqlPool pool $
  rawSql "SELECT id, name, age FROM users ORDER BY name" []

-- Execute without result
createIndex :: ConnectionPool -> IO ()
createIndex pool = runSqlPool pool $
  rawExecute "CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_users_name ON users(name)" []

-- Full-text search (PostgreSQL specific)
searchFullText :: ConnectionPool -> Text -> IO [Entity User]
searchFullText pool query = runSqlPool pool $
  rawSql
    "SELECT ?? FROM users WHERE to_tsvector(name || ' ' || email) @@ plainto_tsquery(?)"
    [PersistText query]
```

---

## ขั้นตอนที่ 331: hasql (High-Performance PostgreSQL)

```haskell
-- hasql: low-level, high-performance PostgreSQL

import Hasql.Session
import Hasql.Statement
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

-- Define statement
getUserStatement :: Statement UserId (Maybe User)
getUserStatement = Statement sql encoder decoder True
  where
    sql = "SELECT name, email, age FROM users WHERE id = $1"
    encoder = E.param (E.nonNullable E.int4)
    decoder = D.rowMaybe $ User
      <$> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.int4)

-- Session
getUser :: UserId -> Session (Maybe User)
getUser uid = statement uid getUserStatement

-- Run session
runGetUser :: HasqlPool -> UserId -> IO (Either QueryError (Maybe User))
runGetUser pool uid = use pool (getUser uid)

-- Batch operations
insertManyUsers :: [(Text, Text, Int)] -> Session ()
insertManyUsers users = for_ users $ \(name, email, age) -> do
  statement (name, email, age) insertUserStatement

insertUserStatement :: Statement (Text, Text, Int) ()
insertUserStatement = Statement sql encoder decoder False
  where
    sql = "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)"
    encoder = (E.param (E.nonNullable E.text))
           <> (E.param (E.nonNullable E.text))
           <> (E.param (E.nonNullable E.int4))
    decoder = D.noResult
```

---

## ขั้นตอนที่ 332: postgresql-simple

```haskell
-- postgresql-simple: balanced PostgreSQL library

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

-- Queries
getUserSimple :: Connection -> Int -> IO [User]
getUserSimple conn uid = do
  query conn "SELECT id, name, email, age FROM users WHERE id = ?" (Only uid)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

-- Insert
insertUserSimple :: Connection -> Text -> Text -> Int -> IO Int64
insertUserSimple conn name email age =
  execute conn "INSERT INTO users (name, email, age) VALUES (?, ?, ?)" (name, email, age)

instance ToRow (Text, Text, Int) where
  toRow (a, b, c) = [toField a, toField b, toField c]

-- Transaction
transferSimple :: Connection -> Int -> Int -> Int -> IO ()
transferSimple conn from to amount = withTransaction conn $ do
  execute conn "UPDATE accounts SET balance = balance - ? WHERE id = ?" (amount, from)
  execute conn "UPDATE accounts SET balance = balance + ? WHERE id = ?" (amount, to)

-- executemany: batch insert
batchInsertSimple :: Connection -> [(Text, Text, Int)] -> IO Int64
batchInsertSimple conn users =
  executeMany conn "INSERT INTO users (name, email, age) VALUES (?, ?, ?)" users

-- Notification (PostgreSQL LISTEN/NOTIFY)
listenForChanges :: Connection -> IO ()
listenForChanges conn = do
  execute_ conn "LISTEN user_changes"
  forever $ do
    note <- getNotification conn
    putStrLn $ "Got notification: " ++ show (notificationData note)
```

---

## ขั้นตอนที่ 333: Caching Layer

```haskell
-- Database caching ด้วย Redis

import Database.Redis

-- Redis connection
connectRedis :: IO Connection
connectRedis = connect defaultConnectInfo

-- Cache get/set
cacheGet :: Connection -> Text -> IO (Maybe BS.ByteString)
cacheGet conn key = do
  result <- runRedis conn $ get (encodeUtf8 key)
  return $ case result of
    Right (Just v) -> Just v
    _              -> Nothing

cacheSet :: Connection -> Text -> BS.ByteString -> Int -> IO ()
cacheSet conn key value ttl = do
  runRedis conn $ setex (encodeUtf8 key) (fromIntegral ttl) value
  return ()

-- Cache delete
cacheInvalidate :: Connection -> Text -> IO ()
cacheInvalidate conn key = do
  runRedis conn $ del [encodeUtf8 key]
  return ()

-- Cache-aside pattern
getWithCache :: (FromJSON a, ToJSON a) => Connection -> Text -> IO a -> IO a
getWithCache redis key fetchFn = do
  cached <- cacheGet redis key
  case cached >>= decode . BL.fromStrict of
    Just val -> return val
    Nothing  -> do
      val <- fetchFn
      cacheSet redis key (BL.toStrict (encode val)) 3600  -- 1 hour
      return val

-- ตัวอย่าง
getUserWithCache :: Connection -> Connection -> Int -> IO (Maybe User)
getUserWithCache db redis uid = do
  let key = "user:" <> T.pack (show uid)
  getWithCache redis key (getUserFromDB db uid)
```

---

## ขั้นตอนที่ 334: Database Testing

```haskell
-- Testing database code

import Test.Hspec
import Database.Persist.Sqlite

-- Test ด้วย in-memory SQLite
withTestDB :: (ConnectionPool -> IO ()) -> IO ()
withTestDB action = do
  pool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  runSqlPool pool (runMigration migrateAll)
  action pool

spec :: Spec
spec = do
  describe "User CRUD" $ do
    it "creates and retrieves user" $ withTestDB $ \pool -> do
      uid <- runSqlPool pool $ insert (User "Alice" "alice@example.com" 30 True)
      mUser <- runSqlPool pool $ get uid
      mUser `shouldBe` Just (User "Alice" "alice@example.com" 30 True)
    
    it "updates user" $ withTestDB $ \pool -> do
      uid <- runSqlPool pool $ insert (User "Alice" "alice@example.com" 30 True)
      runSqlPool pool $ update uid [UserName =. "Bob"]
      mUser <- runSqlPool pool $ get uid
      fmap userName mUser `shouldBe` Just "Bob"
    
    it "deletes user" $ withTestDB $ \pool -> do
      uid <- runSqlPool pool $ insert (User "Alice" "alice@example.com" 30 True)
      runSqlPool pool $ delete uid
      mUser <- runSqlPool pool $ get uid
      mUser `shouldBe` Nothing

  describe "Query" $ do
    it "filters by age" $ withTestDB $ \pool -> do
      runSqlPool pool $ do
        insert (User "Alice" "alice@example.com" 30 True)
        insert (User "Bob" "bob@example.com" 25 True)
        insert (User "Charlie" "charlie@example.com" 35 True)
      
      users <- runSqlPool pool $ 
        selectList [UserAge >=. 30] [Asc UserId]
      length users `shouldBe` 2
```

---

## ขั้นตอนที่ 335: Soft Delete Pattern

```haskell
-- Soft delete: mark records as deleted instead of removing them

-- Schema ที่รองรับ soft delete
share [mkPersist sqlSettings] [persistLowerCase|
SoftDeleteUser
    name    Text
    email   Text
    age     Int
    deleted Bool default=False
    deletedAt UTCTime Maybe
    deriving Show
|]

-- Soft delete functions
softDelete :: ConnectionPool -> SoftDeleteUserId -> IO ()
softDelete pool uid = do
  now <- getCurrentTime
  runSqlPool pool $
    update uid [SoftDeleteUserDeleted =. True, SoftDeleteUserDeletedAt =. Just now]

-- Active only query
getActiveUsers :: ConnectionPool -> IO [Entity SoftDeleteUser]
getActiveUsers pool = runSqlPool pool $
  selectList [SoftDeleteUserDeleted ==. False] []

-- Filter ใน Esqueleto
activeUsersEsq :: ConnectionPool -> IO [Entity SoftDeleteUser]
activeUsersEsq pool = runSqlPool pool $
  select $ do
    u <- from (table @SoftDeleteUser)
    where_ (u ^. SoftDeleteUserDeleted ==. val False)
    return u

-- Undelete
restore :: ConnectionPool -> SoftDeleteUserId -> IO ()
restore pool uid = runSqlPool pool $
  update uid [SoftDeleteUserDeleted =. False, SoftDeleteUserDeletedAt =. Nothing]

-- Auto-filter ด้วย custom query
newtype ActiveOnly a = ActiveOnly { unActiveOnly :: a }

class HasDeletedField a where
  deletedField :: EntityField a Bool

instance HasDeletedField SoftDeleteUser where
  deletedField = SoftDeleteUserDeleted

queryActive :: (PersistEntity a, HasDeletedField a, PersistEntityBackend a ~ SqlBackend)
            => SqlPersistT IO [Entity a]
queryActive = selectList [deletedField ==. False] []
```

---

## ขั้นตอนที่ 336: Audit Trail

```haskell
-- Audit trail: track all changes to records

share [mkPersist sqlSettings] [persistLowerCase|
AuditLog
    entityType  Text
    entityId    Int
    action      Text  -- CREATE, UPDATE, DELETE
    oldValue    Text Maybe
    newValue    Text Maybe
    changedBy   Int
    changedAt   UTCTime default=now()
    deriving Show
|]

-- Audit wrapper
auditCreate :: (ToJSON a, PersistEntity a) 
            => ConnectionPool -> Int -> a -> IO (Key a)
auditCreate pool userId entity = runSqlPool pool $ do
  key <- insert entity
  let entityId = fromIntegral (keyToId key)
  insert_ $ AuditLog
    { auditLogEntityType = entityTypeName entity
    , auditLogEntityId   = entityId
    , auditLogAction     = "CREATE"
    , auditLogOldValue   = Nothing
    , auditLogNewValue   = Just (T.pack (show (encode entity)))
    , auditLogChangedBy  = userId
    }
  return key

auditUpdate :: (ToJSON a, PersistEntity a)
            => ConnectionPool -> Int -> Key a -> a -> [Update a] -> IO ()
auditUpdate pool userId key oldVal updates = runSqlPool pool $ do
  update key updates
  mNewVal <- get key
  insert_ $ AuditLog
    { auditLogEntityType = entityTypeName oldVal
    , auditLogEntityId   = fromIntegral (keyToId key)
    , auditLogAction     = "UPDATE"
    , auditLogOldValue   = Just (T.pack (show (encode oldVal)))
    , auditLogNewValue   = fmap (T.pack . show . encode) mNewVal
    , auditLogChangedBy  = userId
    }

entityTypeName :: PersistEntity a => a -> Text
entityTypeName _ = T.pack (show (entityDef (Nothing :: Maybe a)))
```

---

## ขั้นตอนที่ 337: Full-Text Search

```haskell
-- Full-text search ด้วย PostgreSQL

-- สร้าง tsvector column
{-
ALTER TABLE posts ADD COLUMN search_vector tsvector;

CREATE FUNCTION update_search_vector() RETURNS trigger AS $$
BEGIN
  NEW.search_vector := to_tsvector('english', NEW.title || ' ' || NEW.content);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER posts_search_update
BEFORE INSERT OR UPDATE ON posts
FOR EACH ROW EXECUTE FUNCTION update_search_vector();

CREATE INDEX idx_posts_search ON posts USING GIN(search_vector);
-}

-- Search ใน Haskell
searchPosts :: ConnectionPool -> Text -> IO [Entity Post]
searchPosts pool query = runSqlPool pool $
  rawSql
    "SELECT ?? FROM posts \
    \WHERE search_vector @@ plainto_tsquery('english', ?) \
    \ORDER BY ts_rank(search_vector, plainto_tsquery('english', ?)) DESC"
    [PersistText query, PersistText query]

-- Highlight matches
searchWithHighlight :: ConnectionPool -> Text -> IO [(Text, Text)]
searchWithHighlight pool query = runSqlPool pool $ do
  results <- rawSql
    "SELECT title, ts_headline('english', content, plainto_tsquery(?)) \
    \FROM posts WHERE search_vector @@ plainto_tsquery(?)"
    [PersistText query, PersistText query]
  return [(t, h) | (Single t, Single h) <- results]
```

---

## ขั้นตอนที่ 338: Optimistic Locking

```haskell
-- Optimistic locking: ป้องกัน concurrent updates

share [mkPersist sqlSettings] [persistLowerCase|
VersionedUser
    name    Text
    email   Text
    version Int default=1
    deriving Show
|]

-- Update ด้วย version check
updateWithVersion :: ConnectionPool -> VersionedUserId -> Int -> [Update VersionedUser] -> IO Bool
updateWithVersion pool uid version updates = do
  n <- runSqlPool pool $
    updateWhere
      [VersionedUserId ==. uid, VersionedUserVersion ==. version]
      (VersionedUserVersion =. version + 1 : updates)
  return (n > 0)  -- True ถ้า update สำเร็จ

-- ถ้า return False แสดงว่า conflict

-- ตัวอย่าง: retry loop
updateWithRetry :: ConnectionPool -> VersionedUserId -> (VersionedUser -> VersionedUser) -> IO ()
updateWithRetry pool uid updateFn = go 3  -- max 3 retries
  where
    go 0 = throwIO (userError "Optimistic locking failed after retries")
    go n = do
      mUser <- runSqlPool pool $ get uid
      case mUser of
        Nothing -> throwIO (userError "User not found")
        Just user -> do
          let updated = updateFn user
              version = versionedUserVersion user
          success <- updateWithVersion pool uid version
            [ VersionedUserName  =. versionedUserName updated
            , VersionedUserEmail =. versionedUserEmail updated
            ]
          unless success $ do
            threadDelay (100000 * (4 - n))  -- backoff
            go (n - 1)
```

---

## ขั้นตอนที่ 339: Database Seeding

```haskell
-- Database seeding สำหรับ development/testing

seedDatabase :: ConnectionPool -> IO ()
seedDatabase pool = runSqlPool pool $ do
  -- ตรวจสอบว่ามีข้อมูลแล้ว
  n <- count ([] :: [Filter User])
  when (n == 0) $ do
    -- Categories
    cat1 <- insert (Category "Technology")
    cat2 <- insert (Category "Science")
    
    -- Users
    alice <- insert (User "Alice Johnson" "alice@example.com" 30 True)
    bob   <- insert (User "Bob Smith" "bob@example.com" 25 True)
    charlie <- insert (User "Charlie Brown" "charlie@example.com" 35 True)
    
    -- Posts
    post1 <- insert (Post "Introduction to Haskell" "Learn Haskell..." alice True)
    post2 <- insert (Post "Database Design" "Best practices..." bob True)
    post3 <- insert (Post "Draft Post" "Work in progress..." charlie False)
    
    -- Tags
    tag1 <- insert (Tag "haskell")
    tag2 <- insert (Tag "database")
    tag3 <- insert (Tag "programming")
    
    -- PostTags
    insert_ (PostTag post1 tag1)
    insert_ (PostTag post1 tag3)
    insert_ (PostTag post2 tag2)
    insert_ (PostTag post2 tag3)
    
    liftIO $ putStrLn "Database seeded successfully"

-- Faker-like data generation
generateUsers :: Int -> IO [User]
generateUsers n = forM [1..n] $ \i -> do
  let name  = "User" <> T.pack (show i)
      email = "user" <> T.pack (show i) <> "@example.com"
      age   = 20 + (i `mod` 40)
  return (User name email age True)
```

---

## ขั้นตอนที่ 340: โปรเจกต์: Blog ด้วย Database

```haskell
module BlogDB where

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Esqueleto.Experimental
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Time

-- Schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlogUser
    name    Text
    email   Text
    bio     Text Maybe
    active  Bool default=True
    created UTCTime default=now()
    UniqueEmail email
    deriving Show Generic

BlogPost
    title     Text
    slug      Text
    content   Text
    excerpt   Text Maybe
    authorId  BlogUserId
    published Bool default=False
    viewCount Int  default=0
    created   UTCTime default=now()
    updated   UTCTime default=now()
    UniqueSlug slug
    deriving Show Generic

BlogComment
    postId    BlogPostId
    authorId  BlogUserId
    content   Text
    approved  Bool default=False
    created   UTCTime default=now()
    deriving Show Generic

BlogTag
    name    Text
    UniqueTagName name
    deriving Show Generic

BlogPostTag
    postId  BlogPostId
    tagId   BlogTagId
    Primary postId tagId
    deriving Show
|]

-- JSON instances
instance ToJSON BlogUser
instance ToJSON BlogPost
instance ToJSON BlogComment

-- Repository
type DBPool = ConnectionPool

-- User operations
getUserById :: DBPool -> BlogUserId -> IO (Maybe BlogUser)
getUserById pool uid = runSqlPool pool (get uid)

getUserByEmail :: DBPool -> Text -> IO (Maybe (Entity BlogUser))
getUserByEmail pool email = runSqlPool pool $
  selectFirst [BlogUserEmail ==. email] []

createBlogUser :: DBPool -> Text -> Text -> IO BlogUserId
createBlogUser pool name email = runSqlPool pool $
  insert (BlogUser name email Nothing True)

-- Post operations
getPostBySlug :: DBPool -> Text -> IO (Maybe (Entity BlogPost))
getPostBySlug pool slug = runSqlPool pool $
  selectFirst [BlogPostSlug ==. slug] []

getPublishedPosts :: DBPool -> Int -> Int -> IO [Entity BlogPost]
getPublishedPosts pool page size = runSqlPool pool $
  selectList
    [BlogPostPublished ==. True]
    [Desc BlogPostCreated, OffsetBy ((page-1)*size), LimitTo size]

createPost :: DBPool -> BlogUserId -> Text -> Text -> Text -> IO BlogPostId
createPost pool authorId title slug content = runSqlPool pool $ do
  now <- liftIO getCurrentTime
  insert (BlogPost title slug content Nothing authorId False 0 now now)

publishPost :: DBPool -> BlogPostId -> IO ()
publishPost pool pid = runSqlPool pool $
  update pid [BlogPostPublished =. True]

incrementView :: DBPool -> BlogPostId -> IO ()
incrementView pool pid = runSqlPool pool $
  update pid [BlogPostViewCount +=. 1]

-- Join: post with author
data PostWithAuthor = PostWithAuthor
  { pwaPost   :: Entity BlogPost
  , pwaAuthor :: Entity BlogUser
  } deriving Show

getPostsWithAuthors :: DBPool -> IO [PostWithAuthor]
getPostsWithAuthors pool = do
  results <- runSqlPool pool $
    select $ do
      (post :& author) <- from $ table @BlogPost
        `innerJoin` table @BlogUser
        `on` (\(p :& u) -> p ^. BlogPostAuthorId ==. u ^. BlogUserId)
      where_ (post ^. BlogPostPublished ==. val True)
      orderBy [desc (post ^. BlogPostCreated)]
      return (post, author)
  return [PostWithAuthor p a | (p, a) <- results]

-- Comment operations
addComment :: DBPool -> BlogPostId -> BlogUserId -> Text -> IO BlogCommentId
addComment pool postId authorId content = runSqlPool pool $ do
  now <- liftIO getCurrentTime
  insert (BlogComment postId authorId content False now)

approveComment :: DBPool -> BlogCommentId -> IO ()
approveComment pool cid = runSqlPool pool $
  update cid [BlogCommentApproved =. True]

getApprovedComments :: DBPool -> BlogPostId -> IO [Entity BlogComment]
getApprovedComments pool postId = runSqlPool pool $
  selectList
    [BlogCommentPostId ==. postId, BlogCommentApproved ==. True]
    [Asc BlogCommentCreated]

-- Tag operations
addTag :: DBPool -> Text -> IO BlogTagId
addTag pool name = runSqlPool pool $ do
  mTag <- getBy (UniqueTagName name)
  case mTag of
    Just (Entity tid _) -> return tid
    Nothing             -> insert (BlogTag name)

tagPost :: DBPool -> BlogPostId -> [Text] -> IO ()
tagPost pool postId tagNames = runSqlPool pool $ do
  -- Remove old tags
  deleteWhere [BlogPostTagPostId ==. postId]
  -- Add new tags
  forM_ tagNames $ \name -> do
    mTag <- selectFirst [BlogTagName ==. name] []
    case mTag of
      Nothing          -> return ()
      Just (Entity tid _) -> insert_ (BlogPostTag postId tid)

getPostTags :: DBPool -> BlogPostId -> IO [BlogTag]
getPostTags pool postId = runSqlPool pool $ do
  tags <- select $ do
    (pt :& tag) <- from $ table @BlogPostTag
      `innerJoin` table @BlogTag
      `on` (\(pt :& t) -> pt ^. BlogPostTagTagId ==. t ^. BlogTagId)
    where_ (pt ^. BlogPostTagPostId ==. val postId)
    return tag
  return (map entityVal tags)

-- Statistics
getBlogStats :: DBPool -> IO (Int, Int, Int)
getBlogStats pool = runSqlPool pool $ do
  userCount <- count ([] :: [Filter BlogUser])
  postCount <- count [BlogPostPublished ==. True]
  commentCount <- count [BlogCommentApproved ==. True]
  return (userCount, postCount, commentCount)
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 18 เราจะเรียนเรื่อง **Yesod Framework เบื้องต้น**:
- Yesod overview
- Routes
- Handlers
- Templates

---

*[← Part 16](part-16.md) | [Part 18 →](part-18.md)*
