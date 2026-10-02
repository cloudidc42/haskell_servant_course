# Part 35: Cloud Infrastructure & DevOps
## ขั้นตอนที่ 681-700

---

## ขั้นตอนที่ 681: Docker Multi-Stage Builds

```dockerfile
# Optimized Docker build สำหรับ Haskell

# Stage 1: Build dependencies
FROM haskell:9.4-buster AS deps
WORKDIR /deps

# Copy only dependency files first (better cache)
COPY package.yaml stack.yaml ./
RUN stack setup && stack build --only-dependencies

# Stage 2: Build application
FROM deps AS builder
WORKDIR /app
COPY . .
RUN stack build --copy-bins

# Stage 3: Runtime image (minimal)
FROM debian:buster-slim AS runtime

RUN apt-get update && apt-get install -y \
    libgmp10 \
    libffi7 \
    ca-certificates \
    netbase \
    && rm -rf /var/lib/apt/lists/*

# Copy only the executable
COPY --from=builder /root/.local/bin/myapp /usr/local/bin/myapp

# Non-root user
RUN useradd -r -s /bin/false appuser
USER appuser

EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

ENTRYPOINT ["/usr/local/bin/myapp"]
```

```haskell
-- Graceful shutdown ที่ Kubernetes ต้องการ

import Control.Concurrent
import System.Posix.Signals

main :: IO ()
main = do
  shutdownVar <- newEmptyMVar
  
  -- Handle SIGTERM from Kubernetes
  installHandler sigTERM (Catch (putMVar shutdownVar ())) Nothing
  installHandler sigINT  (Catch (putMVar shutdownVar ())) Nothing
  
  -- Start server
  app <- buildApp
  serverThread <- forkIO (runApp app 8080)
  
  -- Wait for shutdown signal
  takeMVar shutdownVar
  
  -- Graceful shutdown: stop accepting new requests
  putStrLn "Shutting down gracefully..."
  stopAccepting app
  
  -- Wait for in-flight requests (max 30s)
  withTimeout 30000 (waitForDraining app)
  
  putStrLn "Shutdown complete"
```

---

## ขั้นตอนที่ 682: Kubernetes Deployment

```yaml
# Kubernetes deployment manifests

apiVersion: apps/v1
kind: Deployment
metadata:
  name: haskell-api
  labels:
    app: haskell-api
    version: "1.0.0"
spec:
  replicas: 3
  selector:
    matchLabels:
      app: haskell-api
  template:
    metadata:
      labels:
        app: haskell-api
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
    spec:
      containers:
      - name: api
        image: registry.example.com/haskell-api:1.0.0
        ports:
        - containerPort: 8080
        - containerPort: 9090  # metrics
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: db-credentials
              key: url
        - name: REDIS_URL
          valueFrom:
            configMapKeyRef:
              name: app-config
              key: redis_url
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 30
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 10
        lifecycle:
          preStop:
            exec:
              command: ["/bin/sleep", "5"]
      terminationGracePeriodSeconds: 60
```

---

## ขั้นตอนที่ 683: CI/CD Pipeline

```yaml
# GitHub Actions CI/CD

name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  test:
    name: Test
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:15
        env:
          POSTGRES_PASSWORD: test
          POSTGRES_DB: testdb
        ports: ["5432:5432"]
      redis:
        image: redis:7
        ports: ["6379:6379"]
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Cache stack
      uses: actions/cache@v3
      with:
        path: ~/.stack
        key: ${{ runner.os }}-stack-${{ hashFiles('**/stack.yaml.lock') }}
    
    - name: Setup Haskell
      uses: haskell-actions/setup@v2
      with:
        ghc-version: '9.4.7'
        cabal-version: 'latest'
    
    - name: Run tests
      run: |
        stack test --fast --no-terminal
      env:
        TEST_DATABASE_URL: postgresql://postgres:test@localhost/testdb
    
    - name: Security scan
      run: |
        cabal-audit --format=json --output=audit.json
        cat audit.json
  
  build-and-push:
    name: Build & Push
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Build Docker image
      run: docker build -t $IMAGE_TAG .
      env:
        IMAGE_TAG: registry.example.com/api:${{ github.sha }}
    
    - name: Push to registry
      run: docker push $IMAGE_TAG
    
    - name: Deploy to staging
      run: |
        kubectl set image deployment/haskell-api \
          api=registry.example.com/api:${{ github.sha }} \
          -n staging
        kubectl rollout status deployment/haskell-api -n staging
```

---

## ขั้นตอนที่ 684: Infrastructure as Code

```haskell
-- Pulumi infrastructure ใน Haskell

import Pulumi

-- Define infrastructure
main :: IO ()
main = runPulumi $ do
  -- VPC
  vpc <- newResource "main-vpc" VpcArgs
    { vpcCidrBlock           = "10.0.0.0/16"
    , vpcEnableDnsHostnames  = True
    }
  
  -- ECS Cluster
  cluster <- newResource "app-cluster" ClusterArgs
    { clusterName = "production"
    }
  
  -- RDS PostgreSQL
  dbSg <- newResource "db-sg" SecurityGroupArgs
    { sgVpcId = vpc.id
    , sgIngress = [IngressRule 5432 5432 "tcp" ["10.0.0.0/16"]]
    }
  
  db <- newResource "main-db" DbInstanceArgs
    { dbInstanceClass      = "db.t3.medium"
    , dbAllocatedStorage   = 100
    , dbEngine             = "postgres"
    , dbEngineVersion      = "15"
    , dbName               = "appdb"
    , dbUsername           = "appuser"
    , dbPassword           = Output.secret "changeme"
    , dbMultiAz            = True
    , dbBackupRetention    = 7
    , dbStorageEncrypted   = True
    , dbVpcSecurityGroups  = [dbSg.id]
    }
  
  -- ElastiCache Redis
  redis <- newResource "cache" CacheClusterArgs
    { ccCacheNodeType  = "cache.t3.small"
    , ccEngine         = "redis"
    , ccNumCacheNodes  = 1
    }
  
  -- Output
  export "db_endpoint" (db.endpoint)
  export "redis_endpoint" (redis.configurationEndpoint)
```

---

## ขั้นตอนที่ 685: Auto-scaling

```haskell
-- Auto-scaling logic

data ScalingConfig = ScalingConfig
  { scMinReplicas     :: Int
  , scMaxReplicas     :: Int
  , scTargetCpu       :: Double  -- 0-100
  , scTargetLatency   :: Double  -- ms
  , scScaleUpDelay    :: Int     -- seconds
  , scScaleDownDelay  :: Int     -- seconds
  }

data ScalingDecision
  = ScaleUp   Int  -- by N replicas
  | ScaleDown Int  -- by N replicas
  | NoChange

-- Horizontal Pod Autoscaler logic
computeScalingDecision :: ScalingConfig -> [MetricsSnapshot] -> Int -> ScalingDecision
computeScalingDecision cfg snapshots currentReplicas =
  let avgCpu     = avg (map snapshotCpuUsage snapshots)
      avgLatency = avg (map snapshotP99Latency snapshots)
      
      cpuRatio     = avgCpu / scTargetCpu cfg
      latencyRatio = avgLatency / scTargetLatency cfg
      
      ratio        = max cpuRatio latencyRatio
      desired      = ceiling (fromIntegral currentReplicas * ratio)
      clamped      = clamp (scMinReplicas cfg) (scMaxReplicas cfg) desired
  in if clamped > currentReplicas
     then ScaleUp   (clamped - currentReplicas)
     else if clamped < currentReplicas
          then ScaleDown (currentReplicas - clamped)
          else NoChange

-- Scale cooldown tracking
data ScalerState = ScalerState
  { ssLastScaleUp   :: Maybe UTCTime
  , ssLastScaleDown :: Maybe UTCTime
  }

canScale :: ScalingConfig -> ScalerState -> ScalingDecision -> IO Bool
canScale cfg state decision = do
  now <- getCurrentTime
  return $ case decision of
    ScaleUp _ -> case ssLastScaleUp state of
      Nothing -> True
      Just t  -> diffUTCTime now t > fromIntegral (scScaleUpDelay cfg)
    ScaleDown _ -> case ssLastScaleDown state of
      Nothing -> True
      Just t  -> diffUTCTime now t > fromIntegral (scScaleDownDelay cfg)
    NoChange -> False
```

---

## ขั้นตอนที่ 686: Blue-Green Deployments

```haskell
-- Blue-green deployment automation

data DeploymentConfig = DeploymentConfig
  { dcNamespace    :: Text
  , dcServiceName  :: Text
  , dcTimeout      :: Int
  , dcVerification :: VerificationConfig
  }

-- Blue-green deployment steps
blueGreenDeploy :: KubeClient -> DeploymentConfig -> Text -> IO DeployResult
blueGreenDeploy k8s cfg newImage = do
  -- 1. Determine current active color
  activeColor <- getActiveColor k8s cfg
  let inactiveColor = if activeColor == Blue then Green else Blue
  
  -- 2. Deploy new version to inactive slot
  putStrLn $ "Deploying " <> newImage <> " to " <> show inactiveColor
  deployToSlot k8s cfg inactiveColor newImage
  
  -- 3. Wait for deployment to be ready
  waitForReady k8s cfg inactiveColor (dcTimeout cfg)
  
  -- 4. Run verification tests against inactive slot
  testResult <- verifySlot k8s cfg inactiveColor (dcVerification cfg)
  
  case testResult of
    VerificationFailed reasons -> do
      putStrLn "Verification failed, rolling back"
      rollbackSlot k8s cfg inactiveColor
      return (DeployFailed reasons)
    
    VerificationPassed -> do
      -- 5. Switch traffic to new version
      switchTrafficTo k8s cfg inactiveColor
      putStrLn $ "Traffic switched to " <> show inactiveColor
      
      -- 6. Keep old version for quick rollback
      putStrLn $ "Old version (" <> show activeColor <> ") kept for rollback"
      return DeploySuccess

-- Traffic shifting (canary)
shiftTraffic :: KubeClient -> Text -> Int -> IO ()
shiftTraffic k8s serviceName newPercent = do
  current <- getCurrentTrafficSplit k8s serviceName
  let newSplit = TrafficSplit
        { tsBlueWeight  = 100 - newPercent
        , tsGreenWeight = newPercent
        }
  updateTrafficSplit k8s serviceName newSplit
```

---

## ขั้นตอนที่ 687: Database Migrations

```haskell
-- Zero-downtime database migrations

import Database.Persist.Migration

-- Migration with backward compatibility
data Migration = Migration
  { migName    :: Text
  , migVersion :: Int
  , migUp      :: SqlPersistT IO ()
  , migDown    :: SqlPersistT IO ()
  , migSafe    :: Bool  -- can run on live database
  }

-- Expand-Contract pattern
--
-- Phase 1: Add new column (nullable, backward compatible)
addEmailColumn :: Migration
addEmailColumn = Migration
  { migName    = "add_email_column"
  , migVersion = 20240101
  , migUp      = rawExecute "ALTER TABLE users ADD COLUMN email_new TEXT" []
  , migDown    = rawExecute "ALTER TABLE users DROP COLUMN email_new" []
  , migSafe    = True  -- online safe
  }

-- Phase 2: Populate new column
backfillEmailColumn :: Migration
backfillEmailColumn = Migration
  { migName    = "backfill_email"
  , migVersion = 20240102
  , migUp      = do
      -- Batch update to avoid locking
      let batchSize = 1000
      batchUpdate "UPDATE users SET email_new = email WHERE email_new IS NULL LIMIT ?"
                  [batchSize]
  , migDown    = rawExecute "UPDATE users SET email_new = NULL" []
  , migSafe    = True
  }

-- Phase 3: Make column not null
makeEmailNotNull :: Migration
makeEmailNotNull = Migration
  { migName    = "email_not_null"
  , migVersion = 20240103
  , migUp      = rawExecute "ALTER TABLE users ALTER COLUMN email_new SET NOT NULL" []
  , migDown    = rawExecute "ALTER TABLE users ALTER COLUMN email_new DROP NOT NULL" []
  , migSafe    = True  -- only after all rows populated
  }

-- Run migrations safely
runMigrations :: ConnectionPool -> [Migration] -> IO ()
runMigrations pool migrations = do
  applied <- getAppliedMigrations pool
  let pending = filter (\m -> migVersion m `notElem` applied) migrations
  
  forM_ pending $ \mig -> do
    putStrLn $ "Applying migration: " <> T.unpack (migName mig)
    runSqlPool (migUp mig) pool
    recordMigration pool (migVersion mig)
```

---

## ขั้นตอนที่ 688: Container Registry

```haskell
-- Container registry and image management

data ImageTag = ImageTag
  { itRegistry :: Text
  , itRepo     :: Text
  , itTag      :: Text
  } deriving (Show, Eq)

formatImageTag :: ImageTag -> Text
formatImageTag it = mconcat
  [ itRegistry it, "/", itRepo it, ":", itTag it ]

-- Build and push pipeline
buildAndPush :: BuildConfig -> IO BuildResult
buildAndPush cfg = do
  -- Calculate image tags
  let gitSha = bcGitSha cfg
  let branch = bcBranch cfg
  let tags   = [ ImageTag (bcRegistry cfg) (bcRepo cfg) gitSha
               , ImageTag (bcRegistry cfg) (bcRepo cfg) (sanitizeBranch branch)
               ] ++ [ ImageTag (bcRegistry cfg) (bcRepo cfg) "latest"
                    | branch == "main" ]
  
  -- Build
  let buildArgs = concatMap (\t -> ["--tag", T.unpack (formatImageTag t)]) tags
  runProcess (proc "docker" (["build"] ++ buildArgs ++ ["."]))
  
  -- Scan for vulnerabilities
  scanResult <- scanImage (formatImageTag (head tags))
  when (hasCriticalVulns scanResult) $
    throwIO (CriticalVulnerability scanResult)
  
  -- Push all tags
  forM_ tags $ \tag ->
    runProcess (proc "docker" ["push", T.unpack (formatImageTag tag)])
  
  return (BuildSuccess tags)

-- Image vulnerability scanning
scanImage :: Text -> IO ScanResult
scanImage image = do
  (code, out, _) <- readProcessWithExitCode "trivy"
    ["image", "--format", "json", T.unpack image] ""
  case code of
    ExitSuccess -> case eitherDecode (BS.pack out) of
      Right result -> return result
      Left err     -> throwIO (ScanParseError err)
    ExitFailure _ -> throwIO (ScanFailed image)
```

---

## ขั้นตอนที่ 689: GitOps Workflow

```haskell
-- GitOps deployment automation

data GitOpsConfig = GitOpsConfig
  { gocManifestRepo :: Text
  , gocBranch       :: Text
  , gocEnv          :: Text
  }

-- Update manifest and create PR
updateDeployment :: GitOpsConfig -> Text -> Text -> IO ()
updateDeployment cfg service newImage = do
  -- Clone manifest repo
  tmpDir <- mktempdir "/tmp" "gitops"
  runProcess (proc "git" ["clone", T.unpack (gocManifestRepo cfg), tmpDir])
  
  -- Update image in manifest
  let manifestPath = tmpDir </> T.unpack (gocEnv cfg) </> T.unpack service <> ".yaml"
  updateImageInManifest manifestPath newImage
  
  -- Create branch
  let branch = "deploy/" <> service <> "/" <> extractTag newImage
  runProcess (proc "git"
    ["-C", tmpDir, "checkout", "-b", T.unpack branch])
  
  -- Commit and push
  runProcess (proc "git"
    ["-C", tmpDir, "add", "-A"])
  runProcess (proc "git"
    ["-C", tmpDir, "commit", "-m", "Deploy " <> T.unpack service <> " to " <> T.unpack (gocEnv cfg)])
  runProcess (proc "git"
    ["-C", tmpDir, "push", "origin", T.unpack branch])
  
  -- Create PR (via GitHub API)
  createDeployPR cfg service branch

updateImageInManifest :: FilePath -> Text -> IO ()
updateImageInManifest path newImage = do
  content <- readFile path
  let updated = T.replace (extractCurrentImage content) newImage content
  writeFile path updated
```

---

## ขั้นตอนที่ 690: Load Testing with k6

```haskell
-- Generate k6 load test scripts from Haskell

data LoadTestConfig = LoadTestConfig
  { ltcBaseUrl   :: Text
  , ltcScenarios :: [Scenario]
  , ltcThreshold :: [Threshold]
  }

data Scenario = Scenario
  { scName       :: Text
  , scVus        :: Int  -- virtual users
  , scDuration   :: Text
  , scRequests   :: [Request']
  }

data Threshold = Threshold
  { thMetric :: Text
  , thValue  :: Text  -- e.g., "p(99) < 1000"
  }

generateK6Script :: LoadTestConfig -> Text
generateK6Script cfg = T.unlines
  [ "import http from 'k6/http';"
  , "import { check, sleep } from 'k6';"
  , ""
  , "export const options = {"
  , "  scenarios: " <> encode (scenariosToJson (ltcScenarios cfg)) <> ","
  , "  thresholds: " <> encode (thresholdsToJson (ltcThreshold cfg))
  , "};"
  , ""
  , "const BASE_URL = '" <> ltcBaseUrl cfg <> "';"
  , ""
  , "export default function() {"
  , T.unlines (concatMap generateScenarioCode (ltcScenarios cfg))
  , "}"
  ]

-- Load test report analysis
analyzeLoadTestReport :: LoadTestReport -> LoadTestAnalysis
analyzeLoadTestReport report = LoadTestAnalysis
  { ltaPassedThresholds = all checkThreshold (ltrThresholds report)
  , ltaP99Latency       = ltrP99Latency report
  , ltaErrorRate        = ltrErrorRate report
  , ltaMaxRps           = ltrMaxRps report
  , ltaBottleneck       = detectBottleneck report
  }
```

---

## ขั้นตอนที่ 691: Secrets Rotation

```haskell
-- Automated secrets rotation

data RotationSchedule = RotationSchedule
  { rsSecretId  :: Text
  , rsFrequency :: NominalDiffTime
  , rsRotator   :: IO ByteString
  , rsValidator :: ByteString -> IO Bool
  }

-- Rotate a secret
rotateSecret :: VaultClient -> RotationSchedule -> IO ()
rotateSecret vault sched = do
  -- Generate new secret
  newSecret <- rsRotator sched
  
  -- Validate it works
  valid <- rsValidator sched newSecret
  unless valid $ throwIO (RotationValidationFailed (rsSecretId sched))
  
  -- Store in Vault with versioning
  writeSecretVersion vault (rsSecretId sched) newSecret
  
  -- Wait for propagation
  threadDelay 5000000
  
  -- Invalidate old secret
  invalidateOldVersion vault (rsSecretId sched)
  
  logInfo ("Rotated secret: " <> rsSecretId sched)

-- Specific rotators
dbPasswordRotator :: ConnectionString -> IO ByteString
dbPasswordRotator connStr = do
  newPass <- generateSecurePassword
  conn    <- connect connStr
  execute conn "ALTER USER appuser WITH PASSWORD ?" [newPass]
  return (encodeUtf8 newPass)

apiKeyRotator :: ApiProvider -> IO ByteString
apiKeyRotator provider = do
  newKey <- issueNewApiKey provider
  return (encodeUtf8 newKey)

-- Rotation scheduler
scheduleRotations :: [RotationSchedule] -> IO ()
scheduleRotations schedules = do
  forM_ schedules $ \sched -> forkIO $ forever $ do
    threadDelay (round (rsFrequency sched * 1e6))
    vault <- connectVault vaultConfig
    rotateSecret vault sched
```

---

## ขั้นตอนที่ 692: Terraform Integration

```haskell
-- Terraform state management

data TerraformState = TerraformState
  { tsVersion   :: Int
  , tsResources :: [TerraformResource]
  , tsOutputs   :: Map Text TerraformOutput
  }

data TerraformResource = TerraformResource
  { trType       :: Text
  , trName       :: Text
  , trProvider   :: Text
  , trAttributes :: Map Text Value
  }

-- Parse Terraform state
parseTerraformState :: FilePath -> IO TerraformState
parseTerraformState path = do
  bs <- BSL.readFile path
  case eitherDecode bs of
    Left err  -> throwIO (StateParseError err)
    Right st  -> return st

-- Generate Terraform variables from Haskell config
generateTerraformVars :: AppConfig -> Map Text Text
generateTerraformVars cfg = Map.fromList
  [ ("region",         acRegion cfg)
  , ("environment",    acEnvironment cfg)
  , ("app_name",       acAppName cfg)
  , ("db_size",        dbInstanceSize (acDbConfig cfg))
  , ("min_instances",  T.pack (show (acMinInstances cfg)))
  , ("max_instances",  T.pack (show (acMaxInstances cfg)))
  ]

-- Terraform runner
runTerraform :: Text -> [Text] -> Map Text Text -> IO TerraformResult
runTerraform command args vars = do
  let varArgs = concatMap (\(k,v) -> ["-var", T.unpack k <> "=" <> T.unpack v])
                          (Map.toList vars)
  (code, out, err) <- readProcessWithExitCode "terraform"
    (T.unpack command : (T.unpack <$> args) ++ varArgs) ""
  case code of
    ExitSuccess   -> return (TerraformSuccess out)
    ExitFailure c -> return (TerraformFailed c err)
```

---

## ขั้นตอนที่ 693: Multi-Region Deployment

```haskell
-- Multi-region deployment strategy

data Region = USEast1 | EUWest1 | APSoutheast1

data MultiRegionConfig = MultiRegionConfig
  { mrcPrimary    :: Region
  , mrcSecondary  :: [Region]
  , mrcStrategy   :: DeployStrategy
  }

data DeployStrategy
  = Sequential      -- Deploy to regions one at a time
  | Parallel        -- Deploy to all regions simultaneously
  | PrimaryFirst    -- Deploy primary first, then secondaries

-- Deploy to multiple regions
deployMultiRegion :: MultiRegionConfig -> Text -> IO MultiRegionResult
deployMultiRegion cfg image = do
  case mrcStrategy cfg of
    Sequential -> do
      -- Deploy primary
      primaryResult <- deployToRegion (mrcPrimary cfg) image
      case primaryResult of
        DeployFailed err -> return (MultiRegionFailed (mrcPrimary cfg) err)
        DeploySuccess    -> do
          -- Deploy secondaries one by one
          results <- forM (mrcSecondary cfg) $ \region -> do
            result <- deployToRegion region image
            return (region, result)
          return (MultiRegionSuccess results)
    
    Parallel -> do
      results <- mapConcurrently (\r -> deployToRegion r image >>= \res -> return (r, res))
        (mrcPrimary cfg : mrcSecondary cfg)
      return (MultiRegionSuccess results)

-- Database replication check
checkReplicationLag :: Region -> Region -> IO Double
checkReplicationLag primary replica = do
  primaryLsn <- getWriteLsn primary
  replicaLsn <- getApplyLsn replica
  return (fromIntegral (primaryLsn - replicaLsn) / bytesPerSecond)
  where bytesPerSecond = 1024 * 1024  -- approximate
```

---

## ขั้นตอนที่ 694: Feature Flags in Production

```haskell
-- Production feature flags

import Data.IORef

data FlagStore = FlagStore
  { fsFlags :: TVar (Map Text FlagConfig)
  }

data FlagConfig = FlagConfig
  { fcEnabled      :: Bool
  , fcRolloutPct   :: Int     -- 0-100
  , fcTargetUsers  :: [Text]  -- specific user IDs
  , fcTargetGroups :: [Text]  -- user groups/roles
  , fcMetadata     :: Map Text Text
  }

-- Check if feature is enabled for user
isEnabled :: FlagStore -> Text -> UserId -> [Text] -> IO Bool
isEnabled store flagName userId userGroups = do
  flags <- readTVarIO (fsFlags store)
  case Map.lookup flagName flags of
    Nothing -> return False
    Just cfg -> do
      if not (fcEnabled cfg) then return False
      else if T.pack (show userId) `elem` fcTargetUsers cfg then return True
      else if any (`elem` fcTargetGroups cfg) userGroups then return True
      else checkRolloutPercentage cfg userId

checkRolloutPercentage :: FlagConfig -> UserId -> IO Bool
checkRolloutPercentage cfg uid = do
  let hash     = hashUserId uid
  let bucket   = hash `mod` 100
  return (bucket < fcRolloutPct cfg)

-- Update flag from remote config
syncFlags :: FlagStore -> ConfigService -> IO ()
syncFlags store svc = forever $ do
  threadDelay 30000000  -- 30 seconds
  newFlags <- getFlagsFromService svc
  atomically $ writeTVar (fsFlags store) newFlags

-- Metrics per flag
data FlagMetrics = FlagMetrics
  { fmChecks  :: TVar (Map Text Int)  -- flag -> count
  , fmEnabled :: TVar (Map Text Int)  -- flag -> enabled count
  }

trackFlagUsage :: FlagMetrics -> Text -> Bool -> IO ()
trackFlagUsage metrics flagName enabled = atomically $ do
  modifyTVar (fmChecks  metrics) (Map.insertWith (+) flagName 1)
  when enabled $ modifyTVar (fmEnabled metrics) (Map.insertWith (+) flagName 1)
```

---

## ขั้นตอนที่ 695: Service Mesh Configuration

```haskell
-- Istio/Envoy service mesh configuration

-- Generate Istio VirtualService
generateVirtualService :: ServiceConfig -> Text
generateVirtualService cfg = T.unlines
  [ "apiVersion: networking.istio.io/v1alpha3"
  , "kind: VirtualService"
  , "metadata:"
  , "  name: " <> scName cfg
  , "spec:"
  , "  hosts:"
  , "  - " <> scHost cfg
  , "  http:"
  , "  - match:"
  , "    - headers:"
  , "        x-canary:"
  , "          exact: 'true'"
  , "    route:"
  , "    - destination:"
  , "        host: " <> scName cfg
  , "        subset: canary"
  , "  - route:"
  , "    - destination:"
  , "        host: " <> scName cfg
  , "        subset: stable"
  , "      weight: " <> T.pack (show (100 - scCanaryWeight cfg))
  , "    - destination:"
  , "        host: " <> scName cfg
  , "        subset: canary"
  , "      weight: " <> T.pack (show (scCanaryWeight cfg))
  ]

-- Retry and circuit breaker via DestinationRule
generateDestinationRule :: ServiceConfig -> Text
generateDestinationRule cfg = T.unlines
  [ "apiVersion: networking.istio.io/v1alpha3"
  , "kind: DestinationRule"
  , "metadata:"
  , "  name: " <> scName cfg
  , "spec:"
  , "  host: " <> scHost cfg
  , "  trafficPolicy:"
  , "    connectionPool:"
  , "      tcp:"
  , "        maxConnections: 100"
  , "      http:"
  , "        http1MaxPendingRequests: 1000"
  , "    outlierDetection:"
  , "      consecutiveErrors: 5"
  , "      interval: 10s"
  , "      baseEjectionTime: 30s"
  ]
```

---

## ขั้นตอนที่ 696: Cloud Cost Management

```haskell
-- Cloud cost tracking and optimization

data CostRecord = CostRecord
  { crDate     :: Day
  , crService  :: Text
  , crRegion   :: Text
  , crAmount   :: Double
  , crCurrency :: Text
  , crTags     :: Map Text Text
  }

-- Cost anomaly detection
detectCostAnomalies :: [CostRecord] -> [CostAnomaly]
detectCostAnomalies records = concatMap checkService (groupByService records)
  where
    checkService (service, recs) =
      let daily   = aggregateByDay recs
          recent  = last7Days daily
          avg     = weeklyAverage daily
          stddev  = weeklyStdDev daily
          current = lastDay daily
      in [ CostAnomaly service current avg
         | current > avg + 2 * stddev ]

-- Budget alerts
data Budget = Budget
  { budgetName   :: Text
  , budgetAmount :: Double
  , budgetPeriod :: BudgetPeriod
  , budgetAlerts :: [BudgetAlert]
  }

data BudgetAlert = BudgetAlert
  { baThreshold :: Double  -- 0.0-1.0
  , baNotify    :: Text -> IO ()
  }

checkBudget :: Budget -> Double -> IO ()
checkBudget budget spent = do
  let ratio = spent / budgetAmount budget
  forM_ (budgetAlerts budget) $ \alert ->
    when (ratio >= baThreshold alert) $
      baNotify alert $ "Budget alert: " <> budgetName budget <>
                       " at " <> formatPercent ratio <> " of budget"

-- Resource right-sizing
suggestRightSizing :: [MetricRecord] -> [RightSizingSuggestion]
suggestRightSizing records =
  mapMaybe suggest (groupByInstance records)
  where
    suggest (instance', metrics) =
      let avgCpu = avg (map mCpuUsage metrics)
          maxCpu = maximum (map mCpuUsage metrics)
      in if maxCpu < 30
         then Just (RightSizingSuggestion instance' (currentType instance') (smallerType instance') (estimatedSavings instance'))
         else Nothing
```

---

## ขั้นตอนที่ 697: Observability-Driven Development

```haskell
-- Instrumented development approach

-- Every function that matters is instrumented
tracedDbQuery :: Text -> DB a -> ReaderT AppEnv IO a
tracedDbQuery queryName action = do
  env <- ask
  liftIO $ withSpan (envTracer env) ("db." <> queryName) [] $ do
    start <- getMonotonicTime
    result <- runSqlPool action (envDbPool env)
    end   <- getMonotonicTime
    let duration = (end - start) * 1000
    observe duration (amDbQueryDuration (envMetrics env))
    return result

-- Automatic instrumentation via Template Haskell
makeInstrumented :: Name -> Q [Dec]
makeInstrumented funcName = do
  info <- reify funcName
  case info of
    VarI _ funcType _ -> generateWrapper funcName funcType
    _                 -> fail "Not a function"

-- SLO-aware function
withSlo :: Text -> Double -> IO a -> IO a
withSlo sloName target action = do
  start  <- getMonotonicTime
  result <- action
  end    <- getMonotonicTime
  let duration = end - start
  
  when (duration > target) $
    recordSloViolation sloName duration target
  
  return result

-- Business metric tracking
trackBusinessEvent :: Text -> Map Text Value -> IO ()
trackBusinessEvent eventName attrs = do
  now <- getCurrentTime
  let event = object
        [ "event"     .= eventName
        , "timestamp" .= now
        , "attrs"     .= attrs
        ]
  emitBusinessEvent event
```

---

## ขั้นตอนที่ 698: Infrastructure Testing

```haskell
-- Test infrastructure automatically

-- Smoke tests
data InfraTest = InfraTest
  { itName  :: Text
  , itCheck :: IO TestResult
  }

infraSmokeTests :: [InfraTest]
infraSmokeTests =
  [ InfraTest "database_connection" testDbConnection
  , InfraTest "redis_connection"    testRedisConnection
  , InfraTest "api_health"          testApiHealth
  , InfraTest "dns_resolution"      testDnsResolution
  , InfraTest "ssl_certificate"     testSslCertificate
  ]

testSslCertificate :: IO TestResult
testSslCertificate = do
  cert <- getCertificate "api.example.com" 443
  now  <- getCurrentTime
  let expiry = certExpiry cert
  let daysLeft = diffUTCTime expiry now / 86400
  
  if daysLeft < 30
    then return (TestFailed ("SSL cert expires in " <> T.pack (show (floor daysLeft)) <> " days"))
    else return TestPassed

testDnsResolution :: IO TestResult
testDnsResolution = do
  result <- E.try @SomeException $ resolve "api.example.com"
  case result of
    Left err -> return (TestFailed (T.pack (show err)))
    Right ip -> return (TestPassed' ("Resolves to " <> T.pack (show ip)))

-- Run all and report
runInfraTests :: [InfraTest] -> IO InfraTestReport
runInfraTests tests = do
  results <- forM tests $ \t -> do
    result <- itCheck t
    return (itName t, result)
  
  return InfraTestReport
    { itrResults = results
    , itrPassed  = all (isPassed . snd) results
    }
```

---

## ขั้นตอนที่ 699: Production Readiness Checklist

```haskell
-- Production readiness validation

data ReadinessCheck = ReadinessCheck
  { rcCategory :: Text
  , rcName     :: Text
  , rcCheck    :: IO CheckStatus
  , rcRequired :: Bool
  }

data CheckStatus = Green | Yellow Text | Red Text

-- Comprehensive checklist
readinessChecks :: AppConfig -> [ReadinessCheck]
readinessChecks cfg =
  -- Security
  [ ReadinessCheck "security" "tls_enabled"          (checkTls cfg)             True
  , ReadinessCheck "security" "rate_limiting"        (checkRateLimit cfg)       True
  , ReadinessCheck "security" "auth_configured"      (checkAuth cfg)            True
  , ReadinessCheck "security" "secrets_in_vault"     (checkSecretsVault cfg)    True
  
  -- Reliability
  , ReadinessCheck "reliability" "health_endpoint"   (checkHealthEndpoint cfg)  True
  , ReadinessCheck "reliability" "db_pool_size"      (checkDbPool cfg)          True
  , ReadinessCheck "reliability" "circuit_breakers"  (checkCircuitBreakers cfg) False
  
  -- Observability
  , ReadinessCheck "observability" "metrics_enabled" (checkMetrics cfg)         True
  , ReadinessCheck "observability" "logging"         (checkLogging cfg)         True
  , ReadinessCheck "observability" "tracing"         (checkTracing cfg)         False
  
  -- Performance
  , ReadinessCheck "performance" "caching"           (checkCaching cfg)         False
  , ReadinessCheck "performance" "indexes"           (checkIndexes cfg)         True
  ]

runReadinessCheck :: [ReadinessCheck] -> IO ReadinessReport
runReadinessCheck checks = do
  results <- forM checks $ \c -> do
    status <- rcCheck c
    return (c, status)
  
  let failed  = filter (\(c, s) -> rcRequired c && not (isGreen s)) results
  let ready   = null failed
  
  return ReadinessReport { rrReady = ready, rrResults = results }
```

---

## ขั้นตอนที่ 700: โปรเจกต์: Complete DevOps Platform

```haskell
-- Complete DevOps platform ครบวงจร

-- Deployment pipeline orchestrator
data Pipeline = Pipeline
  { pName    :: Text
  , pStages  :: [PipelineStage]
  , pOnFail  :: FailureHandler
  }

data PipelineStage = PipelineStage
  { psName     :: Text
  , psSteps    :: [PipelineStep]
  , psParallel :: Bool
  }

data PipelineStep
  = TestStep    { tsCommand :: Text }
  | BuildStep   { bsDockerfile :: FilePath }
  | PushStep    { psTags :: [Text] }
  | ScanStep    { ssScanType :: ScanType }
  | DeployStep  { dsEnv :: Text, dsService :: Text }
  | VerifyStep  { vsChecks :: [VerificationCheck] }
  | NotifyStep  { nsChannels :: [NotifyChannel] }

-- Full pipeline execution
executePipeline :: Pipeline -> PipelineContext -> IO PipelineResult
executePipeline pipeline ctx = do
  logInfo ("Starting pipeline: " <> pName pipeline)
  
  stageResults <- runStages (pStages pipeline) ctx
  
  let success = all psrPassed stageResults
  
  unless success (pOnFail pipeline ctx stageResults)
  
  return PipelineResult
    { prSuccess = success
    , prStages  = stageResults
    , prDuration = sum (map psrDuration stageResults)
    }

runStages :: [PipelineStage] -> PipelineContext -> IO [StageResult]
runStages stages ctx = do
  foldM (\results stage -> do
    result <- runStage stage ctx
    if psrPassed result
      then return (results ++ [result])
      else return (results ++ [result])  -- continue to collect all
  ) [] stages

-- Summary: 700 steps complete!
-- ขั้นตอนที่ 1-700 ครอบคลุม:
-- - พื้นฐาน Haskell (1-200)
-- - Servant Framework (201-350)
-- - Yesod Framework (351-450)
-- - Advanced Haskell (451-550)
-- - Microservices & Distributed (551-600)
-- - Testing (601-620)
-- - Data Analytics (621-640)
-- - Observability (641-660)
-- - Security (661-680)
-- - DevOps & Cloud (681-700)
```

---

*[← Part 34](part-34.md) | [Part 36 →](part-36.md)*
