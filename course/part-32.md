# Part 32: Data Processing & Analytics
## ขั้นตอนที่ 621-640

---

## ขั้นตอนที่ 621: CSV Processing

```haskell
-- CSV processing ขั้นสูง

import Data.Csv
import qualified Data.Csv.Streaming as CSV

-- Parse CSV
data SalesRecord = SalesRecord
  { srDate     :: Day
  , srProduct  :: Text
  , srQuantity :: Int
  , srRevenue  :: Double
  , srRegion   :: Text
  } deriving (Show, Generic)

instance FromRecord SalesRecord where
  parseRecord v = SalesRecord
    <$> v .! 0
    <*> v .! 1
    <*> v .! 2
    <*> v .! 3
    <*> v .! 4

instance ToRecord SalesRecord where
  toRecord sr = record
    [ toField (srDate sr)
    , toField (srProduct sr)
    , toField (srQuantity sr)
    , toField (srRevenue sr)
    , toField (srRegion sr)
    ]

-- Streaming CSV processing (memory efficient)
processSalesFile :: FilePath -> IO SalesSummary
processSalesFile path = do
  bs <- BSL.readFile path
  let records = CSV.decode HasHeader bs :: CSV.Records SalesRecord
  
  foldM accumulate emptySummary records
  where
    accumulate summary (Left err) = do
      putStrLn $ "Skipping bad row: " ++ err
      return summary
    accumulate summary (Right record) =
      return (updateSummary summary record)

data SalesSummary = SalesSummary
  { totalRevenue  :: Double
  , totalUnits    :: Int
  , byProduct     :: Map Text Double
  , byRegion      :: Map Text Double
  , byDate        :: Map Day Double
  }

updateSummary :: SalesSummary -> SalesRecord -> SalesSummary
updateSummary s r = s
  { totalRevenue = totalRevenue s + srRevenue r
  , totalUnits   = totalUnits s + srQuantity r
  , byProduct    = Map.insertWith (+) (srProduct r) (srRevenue r) (byProduct s)
  , byRegion     = Map.insertWith (+) (srRegion r) (srRevenue r) (byRegion s)
  , byDate       = Map.insertWith (+) (srDate r) (srRevenue r) (byDate s)
  }
```

---

## ขั้นตอนที่ 622: Data Transformation Pipelines

```haskell
-- Data transformation pipeline

import Conduit
import Data.Conduit.Combinators (sinkList)

-- ETL pipeline
type Pipeline m i o = ConduitT i o m ()

-- Extract
extractFromDB :: MonadIO m => ConnectionPool -> Pipeline m Void (Entity Record)
extractFromDB pool = do
  records <- liftIO $ runSqlPool (selectList [] []) pool
  yieldMany records

-- Transform
transformRecord :: Monad m => Pipeline m (Entity Record) ProcessedRecord
transformRecord = mapC (\(Entity _ r) -> process r)
  where
    process r = ProcessedRecord
      { prId      = recordId r
      , prName    = T.toUpper (recordName r)
      , prValue   = normalizeValue (recordValue r)
      , prCreated = recordCreatedAt r
      }

-- Load
loadToWarehouse :: MonadIO m => WarehouseConn -> Pipeline m ProcessedRecord Void
loadToWarehouse conn = mapM_C $ \record ->
  liftIO $ warehouseInsert conn record

-- Run full ETL
runEtl :: ConnectionPool -> WarehouseConn -> IO ()
runEtl pool warehouse = do
  runConduit $
    extractFromDB pool
    .| transformRecord
    .| loadToWarehouse warehouse

-- Windowed aggregation
windowedSum :: Monad m => Int -> Pipeline m Double Double
windowedSum windowSize = go []
  where
    go window = do
      mx <- await
      case mx of
        Nothing  -> when (not (null window)) (yield (sum window))
        Just x   -> do
          let newWindow = take windowSize (x : window)
          when (length newWindow == windowSize) (yield (sum newWindow))
          go newWindow
```

---

## ขั้นตอนที่ 623: JSON Data Processing

```haskell
-- Large JSON processing

import Data.Aeson.Lens
import Control.Lens

-- Navigate nested JSON
type Path = [Text]

getNestedValue :: Path -> Value -> Maybe Value
getNestedValue [] v     = Just v
getNestedValue (k:ks) v = v ^? key k >>= getNestedValue ks

-- Lens-based JSON manipulation
transformUserJson :: Value -> Value
transformUserJson = 
    over (key "name" . _String) T.toUpper
  . over (key "email" . _String) T.toLower
  . set  (key "active") (Bool True)
  . deleteKey "password"

deleteKey :: Text -> Value -> Value
deleteKey k (Object o) = Object (KM.delete (Key.fromText k) o)
deleteKey _ v          = v

-- Aggregate JSON data
aggregateUsers :: [Value] -> Value
aggregateUsers users = object
  [ "total"     .= length users
  , "by_region" .= groupByRegion users
  , "avg_age"   .= avgAge users
  ]
  where
    groupByRegion = Map.toList
      . Map.map length
      . groupBy (\u -> u ^? key "region" . _String)
    
    avgAge us =
      let ages = mapMaybe (^? key "age" . _Number) us
      in if null ages
         then Null
         else Number (sum ages / fromIntegral (length ages))

-- JMESPATH-like queries
{-# LANGUAGE QuasiQuotes #-}
query :: Value -> [Value]
query = [jmespath| users[?age > `18`].name |]

-- JSON schema validation
validateUserJson :: Value -> Either [Text] ()
validateUserJson v = do
  requireField "name"  v
  requireField "email" v
  validateEmail (v ^. key "email" . _String)
  validateAge   (v ^. key "age" . _Number)
```

---

## ขั้นตอนที่ 624: Statistical Analysis

```haskell
-- Statistical analysis ใน Haskell

import Statistics.Sample
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Test.KolmogorovSmirnov

-- Basic statistics
analyzeData :: [Double] -> Statistics
analyzeData xs = Statistics
  { statMean     = mean (fromList xs)
  , statVariance = variance (fromList xs)
  , statStdDev   = stdDev (fromList xs)
  , statMedian   = median (fromList xs)
  , statMin      = minimum xs
  , statMax      = maximum xs
  , statQ1       = quantile 0.25 (fromList xs)
  , statQ3       = quantile 0.75 (fromList xs)
  }

-- Histogram
histogram :: [Double] -> Int -> [(Double, Double, Int)]
histogram xs numBins =
  let minVal = minimum xs
      maxVal = maximum xs
      binWidth = (maxVal - minVal) / fromIntegral numBins
      bins = [(minVal + i * binWidth, minVal + (i+1) * binWidth) | i <- [0..numBins-1]]
  in map (\(lo, hi) -> (lo, hi, countInBin xs lo hi)) bins

countInBin :: [Double] -> Double -> Double -> Int
countInBin xs lo hi = length (filter (\x -> x >= lo && x < hi) xs)

-- Correlation
pearsonCorrelation :: [Double] -> [Double] -> Double
pearsonCorrelation xs ys =
  let n    = fromIntegral (length xs)
      xBar = sum xs / n
      yBar = sum ys / n
      num  = sum (zipWith (\x y -> (x - xBar) * (y - yBar)) xs ys)
      denom = sqrt (sum (map (\x -> (x - xBar)^2) xs) *
                    sum (map (\y -> (y - yBar)^2) ys))
  in num / denom

-- Linear regression
simpleLinearRegression :: [(Double, Double)] -> (Double, Double)
simpleLinearRegression points =
  let n    = fromIntegral (length points)
      xs   = map fst points
      ys   = map snd points
      xBar = sum xs / n
      yBar = sum ys / n
      slope = sum (zipWith (\x y -> (x - xBar) * (y - yBar)) xs ys)
            / sum (map (\x -> (x - xBar)^2) xs)
      intercept = yBar - slope * xBar
  in (slope, intercept)
```

---

## ขั้นตอนที่ 625: Time Series Analysis

```haskell
-- Time series analysis

import Data.Time.Calendar
import Data.Map.Strict (Map)

-- Time series data
data TimeSeries = TimeSeries
  { tsData   :: Map UTCTime Double
  , tsName   :: Text
  , tsUnit   :: Text
  }

-- Moving averages
simpleMovingAverage :: Int -> TimeSeries -> TimeSeries
simpleMovingAverage n ts = ts
  { tsData = Map.fromList $ zipWith (\t v -> (t, v))
      (drop (n-1) times)
      (map avg (windows (n-1) values))
  }
  where
    sorted = Map.toAscList (tsData ts)
    times  = map fst sorted
    values = map snd sorted
    windows k vs = [take k (drop i vs) | i <- [0..length vs - k - 1]]
    avg xs = sum xs / fromIntegral (length xs)

-- Exponential moving average
exponentialMovingAverage :: Double -> TimeSeries -> TimeSeries
exponentialMovingAverage alpha ts = ts
  { tsData = Map.fromList (zip times emas)
  }
  where
    sorted = Map.toAscList (tsData ts)
    times  = map fst sorted
    values = map snd sorted
    emas   = scanl1 (\prev cur -> alpha * cur + (1 - alpha) * prev) values

-- Trend detection
detectTrend :: TimeSeries -> TrendResult
detectTrend ts =
  let points = Map.toAscList (tsData ts)
      xs = map (realToFrac . toModifiedJulianDay . utctDay . fst) points
      ys = map snd points
      (slope, _) = simpleLinearRegression (zip xs ys)
  in TrendResult
       { trendDirection = if slope > 0 then Upward else Downward
       , trendStrength  = abs slope
       }

-- Seasonality decomposition
decomposeSeasonality :: TimeSeries -> Int -> (TimeSeries, TimeSeries, TimeSeries)
decomposeSeasonality ts period = (trend, seasonal, residual)
  where
    trend    = simpleMovingAverage period ts
    detrended = subtractTs ts trend
    seasonal = computeSeasonalComponent detrended period
    residual = subtractTs detrended seasonal
```

---

## ขั้นตอนที่ 626: Data Aggregation

```haskell
-- Data aggregation patterns

-- Group and aggregate
groupBy' :: Ord k => (a -> k) -> (a -> b) -> ([b] -> c) -> [a] -> Map k c
groupBy' key val agg = Map.map agg . Map.fromListWith (++) . map (\x -> (key x, [val x]))

-- Example: sales by region
salesByRegion :: [Sale] -> Map Text Double
salesByRegion = groupBy' saleRegion saleAmount sum

-- Pivot table
data PivotTable = PivotTable
  { ptRows    :: [Text]    -- row dimensions
  , ptCols    :: [Text]    -- column dimensions
  , ptValues  :: [[Double]] -- aggregated values
  }

buildPivotTable :: [SalesRecord] -> PivotTable
buildPivotTable records = PivotTable
  { ptRows   = products
  , ptCols   = regions
  , ptValues = [[sumFor p r | r <- regions] | p <- products]
  }
  where
    products = nub (map srProduct records)
    regions  = nub (map srRegion records)
    sumFor p r = sum [srRevenue rec | rec <- records
                     , srProduct rec == p, srRegion rec == r]

-- Hierarchical aggregation
data Hierarchy a = Leaf Text a | Branch Text [Hierarchy a]

aggregateHierarchy :: Monoid a => Hierarchy a -> a
aggregateHierarchy (Leaf _ v)    = v
aggregateHierarchy (Branch _ hs) = mconcat (map aggregateHierarchy hs)

-- Rolling window computations
rollingWindow :: Int -> [a] -> [[a]]
rollingWindow n xs
  | length xs < n = []
  | otherwise     = take n xs : rollingWindow n (tail xs)

rollingAverage :: Int -> [Double] -> [Double]
rollingAverage n = map avg . rollingWindow n
  where avg xs = sum xs / fromIntegral (length xs)
```

---

## ขั้นตอนที่ 627: Report Generation

```haskell
-- Report generation

-- HTML reports ด้วย Blaze HTML
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

generateSalesReport :: SalesSummary -> Html
generateSalesReport summary = docTypeHtml $ do
  H.head $ do
    H.title "Sales Report"
    H.style $ H.toHtml reportCss
  
  H.body $ do
    h1 "Sales Report"
    
    div ! class_ "metrics-grid" $ do
      metricCard "Total Revenue" ("$" <> formatMoney (totalRevenue summary))
      metricCard "Units Sold"    (formatInt (totalUnits summary))
      metricCard "Avg Order"     ("$" <> formatMoney (avgOrderValue summary))
    
    h2 "Revenue by Product"
    table ! class_ "data-table" $ do
      tr $ do
        th "Product"
        th "Revenue"
        th "% Total"
      
      forM_ (Map.toDescList (byProduct summary)) $ \(product, revenue) -> tr $ do
        td $ H.toHtml product
        td $ H.toHtml ("$" <> formatMoney revenue)
        td $ H.toHtml (formatPercent (revenue / totalRevenue summary))
    
    h2 "Revenue Trend"
    div ! id_ "trend-chart" $ ""
    
    script ! src_ "chart.js" $ ""
    script $ H.toHtml (chartData summary)

metricCard :: Text -> Text -> Html
metricCard label value = div ! class_ "metric-card" $ do
  p ! class_ "metric-label" $ H.toHtml label
  p ! class_ "metric-value" $ H.toHtml value

-- PDF generation ด้วย LaTeX
generatePdfReport :: SalesSummary -> IO ByteString
generatePdfReport summary = do
  let latex = renderLatex summary
  withTempDirectory "/tmp" "report" $ \dir -> do
    writeFile (dir </> "report.tex") latex
    callProcess "pdflatex" [dir </> "report.tex"]
    readFile (dir </> "report.pdf")
```

---

## ขั้นตอนที่ 628: Data Validation Pipeline

```haskell
-- Data validation pipeline

import Data.Validation

-- Validation rules
data ValidationRule a = ValidationRule
  { ruleName  :: Text
  , ruleCheck :: a -> Either Text ()
  }

-- Apply multiple rules
validateWith :: [ValidationRule a] -> a -> Validation [Text] a
validateWith rules value = 
  case foldMap (runRule value) rules of
    [] -> Success value
    es -> Failure es
  where
    runRule v rule = case ruleCheck rule v of
      Right () -> []
      Left err -> [ruleName rule <> ": " <> err]

-- Common rules
notEmpty :: ValidationRule Text
notEmpty = ValidationRule "not_empty" $ \t ->
  if T.null t then Left "must not be empty" else Right ()

maxLength :: Int -> ValidationRule Text
maxLength n = ValidationRule ("max_length_" <> T.pack (show n)) $ \t ->
  if T.length t > n then Left ("too long (max " <> T.pack (show n) <> ")") else Right ()

isEmail :: ValidationRule Text
isEmail = ValidationRule "is_email" $ \t ->
  if "@" `T.isInfixOf` t && "." `T.isInfixOf` t
  then Right ()
  else Left "invalid email format"

inRange :: Ord a => a -> a -> ValidationRule a
inRange lo hi = ValidationRule "in_range" $ \v ->
  if v >= lo && v <= hi then Right () else Left "out of range"

-- Example usage
validateUser :: CreateUserReq -> Validation [Text] ValidUser
validateUser req =
  ValidUser
    <$> validate (curName req)  [notEmpty, maxLength 100]
    <*> validate (curEmail req) [notEmpty, isEmail, maxLength 255]
    <*> validate (curAge req)   [inRange 0 150]
  where
    validate v rules = validateWith rules v
```

---

## ขั้นตอนที่ 629: Data Export

```haskell
-- Data export formats

-- Excel export ด้วย xlsx
import Codec.Xlsx

generateExcel :: [User] -> ByteString
generateExcel users =
  let sheet = Worksheet
        { _wsName = "Users"
        , _wsCells = Map.fromList (header : dataRows)
        }
      workbook = Workbook
        { _wbSheets = [sheet]
        }
  in renderLazy workbook
  where
    header = ((1, 1), def
      { _cellValue = Just (CellText "Name") })
    
    dataRows = concat
      [ [ ((row, 1), def { _cellValue = Just (CellText (userName u)) })
        , ((row, 2), def { _cellValue = Just (CellText (userEmail u)) })
        , ((row, 3), def { _cellValue = Just (CellDouble (fromIntegral (userAge u))) })
        ]
      | (i, u) <- zip [2..] users
      , let row = i
      ]

-- XML export
generateXml :: [User] -> Text
generateXml users = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<users>"
  , T.unlines (map userToXml users)
  , "</users>"
  ]

userToXml :: User -> Text
userToXml u = T.unlines
  [ "  <user id=\"" <> T.pack (show (userId u)) <> "\">"
  , "    <name>" <> escapeXml (userName u) <> "</name>"
  , "    <email>" <> escapeXml (userEmail u) <> "</email>"
  , "    <age>" <> T.pack (show (userAge u)) <> "</age>"
  , "  </user>"
  ]

-- Parquet-style columnar format
writeColumnar :: [User] -> FilePath -> IO ()
writeColumnar users path = do
  let names   = map userName users
  let emails  = map userEmail users
  let ages    = map userAge users
  
  BSL.writeFile path $ encode ColumnarData
    { cdColumns = Map.fromList
        [ ("name",  ColumnText names)
        , ("email", ColumnText emails)
        , ("age",   ColumnInt ages)
        ]
    , cdRowCount = length users
    }
```

---

## ขั้นตอนที่ 630: Stream Analytics

```haskell
-- Real-time stream analytics

import Data.Map.Strict (Map)

-- Sliding window analytics
data SlidingWindow a = SlidingWindow
  { swValues  :: Seq a
  , swMaxSize :: Int
  }

addToWindow :: a -> SlidingWindow a -> SlidingWindow a
addToWindow x sw = sw
  { swValues = if Seq.length (swValues sw) >= swMaxSize sw
                  then Seq.drop 1 (swValues sw) |> x
                  else swValues sw |> x
  }

-- Real-time metrics
data RealTimeMetrics = RealTimeMetrics
  { rtmRequestCount  :: TVar Int
  , rtmErrorCount    :: TVar Int
  , rtmLatencies     :: TVar (SlidingWindow Double)
  , rtmLastMinute    :: TVar (Map Int Int)  -- minute -> count
  }

recordRequest :: RealTimeMetrics -> Double -> Bool -> IO ()
recordRequest rtm latency isError = atomically $ do
  modifyTVar (rtmRequestCount rtm) (+1)
  when isError $ modifyTVar (rtmErrorCount rtm) (+1)
  modifyTVar (rtmLatencies rtm) (addToWindow latency)

-- Compute percentiles from sliding window
getP99Latency :: RealTimeMetrics -> IO Double
getP99Latency rtm = do
  window  <- readTVarIO (rtmLatencies rtm)
  let sorted = Seq.sort (swValues window)
  let n      = Seq.length sorted
  if n == 0
    then return 0
    else return (Seq.index sorted (floor (fromIntegral n * 0.99)))

-- Alert rules
data AlertRule = AlertRule
  { arName      :: Text
  , arCondition :: RealTimeMetrics -> IO Bool
  , arMessage   :: Text
  , arCooldown  :: Int  -- seconds between alerts
  }

checkAlerts :: [AlertRule] -> RealTimeMetrics -> IO [Alert]
checkAlerts rules rtm = do
  catMaybes <$> forM rules $ \rule -> do
    triggered <- arCondition rule rtm
    if triggered
      then return (Just (Alert (arName rule) (arMessage rule)))
      else return Nothing
```

---

## ขั้นตอนที่ 631: Data Lake Architecture

```haskell
-- Data lake pattern ใน Haskell

-- Ingestion layer
data IngestionConfig = IngestionConfig
  { icSources   :: [DataSource]
  , icFormat    :: DataFormat
  , icPartition :: PartitionStrategy
  }

data DataSource
  = HttpSource   { hsUrl :: Text }
  | KafkaSource  { ksTopics :: [Text] }
  | FileSource   { fsPath :: FilePath, fsPattern :: Text }
  | DbSource     { dbsQuery :: Text }

data PartitionStrategy
  = ByDate      -- /year=2024/month=01/day=15/
  | ByRegion    -- /region=us-east/
  | ByDateAndId -- /year=2024/month=01/id_bucket=00/

-- Write partitioned data
writePartitioned :: IngestionConfig -> [Record] -> IO ()
writePartitioned config records = do
  let partitioned = groupByPartition (icPartition config) records
  forM_ partitioned $ \(partition, batch) -> do
    let path = buildPath partition
    createDirectoryIfMissing True (takeDirectory path)
    writeFormat (icFormat config) path batch

-- Query layer
data Query = Query
  { qTable   :: Text
  , qSelect  :: [Text]
  , qWhere   :: [Condition]
  , qGroupBy :: [Text]
  , qOrderBy :: [Text]
  , qLimit   :: Maybe Int
  }

-- Execute query against data lake
executeQuery :: DataLakeConfig -> Query -> IO [Row]
executeQuery config q = do
  let paths = discoverPartitions config (qTable q) (qWhere q)
  rows <- concat <$> mapM (readPartition (qSelect q)) paths
  let filtered  = applyFilters (qWhere q) rows
  let grouped   = applyGroupBy (qGroupBy q) filtered
  let ordered   = applyOrderBy (qOrderBy q) grouped
  return $ maybe id take (qLimit q) ordered
```

---

## ขั้นตอนที่ 632: ETL with Conduit

```haskell
-- Full ETL pipeline ด้วย Conduit

import Conduit
import Data.Conduit.Combinators

-- Type-safe ETL stage
type Stage m i o = ConduitT i o m ()

-- Extract stages
fromPostgresStage :: MonadIO m => ConnectionPool -> Text -> Stage m Void (Vector BS.ByteString)
fromPostgresStage pool query = do
  rows <- liftIO $ runSqlPool (rawSql query []) pool
  yieldMany rows

fromKafkaStage :: MonadIO m => KafkaConsumer -> [TopicName] -> Stage m Void ByteString
fromKafkaStage consumer topics = do
  subscribe consumer topics
  forever $ do
    msgs <- liftIO $ pollMessages consumer (Timeout 100)
    yieldMany (mapMaybe messageValue msgs)

-- Transform stages
parseJsonStage :: (MonadIO m, FromJSON a) => Stage m ByteString a
parseJsonStage = mapMC $ \bs ->
  case eitherDecode bs of
    Left err -> do
      liftIO $ putStrLn $ "Parse error: " ++ err
      return Nothing  -- filtered by catMaybes
    Right val -> return (Just val)

validateStage :: Monad m => (a -> Either Text a) -> Stage m a a
validateStage validate = mapMC $ \val ->
  case validate val of
    Left err -> do
      logWarn ("Validation failed: " <> err)
      return Nothing
    Right valid -> return (Just valid)

-- Load stages
toPostgresStage :: MonadIO m => ConnectionPool -> (a -> DB ()) -> Stage m a Void
toPostgresStage pool inserter = mapM_C $ \val ->
  liftIO $ runSqlPool (inserter val) pool

-- Compose full ETL
runFullEtl :: IO ()
runFullEtl = runConduit $
  fromPostgresStage sourcePool "SELECT * FROM raw_data"
  .| parseJsonStage @RawRecord
  .| catMaybes
  .| validateStage validateRecord
  .| catMaybes
  .| mapC transformRecord
  .| batchC 1000
  .| mapM_C (bulkInsert targetPool)
```

---

## ขั้นตอนที่ 633: Text Analytics

```haskell
-- Text analytics

-- Word frequency
wordFrequency :: [Text] -> Map Text Int
wordFrequency = Map.fromListWith (+) . map (\w -> (w, 1)) . concatMap tokenize

tokenize :: Text -> [Text]
tokenize = filter (not . T.null)
  . map (T.toLower . T.filter isAlpha)
  . T.words

-- TF-IDF
type DocId = Int
type TermFreq = Map Text Double
type InvDocFreq = Map Text Double

-- Term frequency in single document
tf :: Text -> TermFreq
tf doc =
  let words' = tokenize doc
      n      = fromIntegral (length words')
  in Map.map (/ n) (wordFrequency words')

-- Inverse document frequency
idf :: [Text] -> InvDocFreq
idf docs =
  let n          = fromIntegral (length docs)
      docCount   = Map.fromListWith (+) 
        [ (word, 1)
        | doc  <- docs
        , word <- nub (tokenize doc)
        ]
  in Map.map (\cnt -> log (n / cnt)) docCount

-- TF-IDF score
tfidf :: InvDocFreq -> Text -> Map Text Double
tfidf idfScores doc =
  Map.intersectionWith (*) (tf doc) idfScores

-- Document similarity (cosine)
cosineSimilarity :: Map Text Double -> Map Text Double -> Double
cosineSimilarity a b =
  let dot     = Map.foldlWithKey' (\acc k v ->
                  acc + v * Map.findWithDefault 0 k b) 0 a
      normA   = sqrt (Map.foldl' (\acc v -> acc + v*v) 0 a)
      normB   = sqrt (Map.foldl' (\acc v -> acc + v*v) 0 b)
  in if normA * normB == 0 then 0 else dot / (normA * normB)

-- Simple sentiment analysis
analyzeSentiment :: Text -> Sentiment
analyzeSentiment text =
  let words'    = tokenize text
      posScore  = length (filter (`S.member` positiveWords) words')
      negScore  = length (filter (`S.member` negativeWords) words')
  in case compare posScore negScore of
       GT -> Positive
       LT -> Negative
       EQ -> Neutral
```

---

## ขั้นตอนที่ 634: Network Analysis

```haskell
-- Network/Graph analysis

import Data.Graph
import qualified Data.IntMap as IM

-- Social network graph
data SocialNetwork = SocialNetwork
  { snUsers     :: Map UserId User
  , snFollowing :: Map UserId [UserId]
  , snFollowers :: Map UserId [UserId]
  }

-- PageRank algorithm
pageRank :: SocialNetwork -> Int -> Map UserId Double
pageRank network iterations =
  let n   = Map.size (snUsers network)
      d   = 0.85  -- damping factor
      init = Map.map (\_ -> 1.0 / fromIntegral n) (snUsers network)
  in iterate (updateRanks d) init !! iterations
  where
    updateRanks d ranks =
      Map.mapWithKey (\uid _ ->
        let inLinks = fromMaybe [] (Map.lookup uid (snFollowers network))
            inboundSum = sum [ranks Map.! f / outDegree f | f <- inLinks]
        in (1 - d) / fromIntegral (Map.size (snUsers network)) + d * inboundSum
      ) ranks
    
    outDegree uid = fromIntegral . length $ fromMaybe [] (Map.lookup uid (snFollowing network))

-- Betweenness centrality (simplified)
betweennessCentrality :: Graph -> Map Vertex Double
betweennessCentrality g = Map.fromList
  [ (v, betweenness v g) | v <- vertices g ]
  where
    betweenness v g =
      let paths = [shortestPath g s t | s <- vertices g, t <- vertices g, s /= t]
          throughV = length (filter (v `elem`) (catMaybes paths))
          total    = length (catMaybes paths)
      in if total == 0 then 0 else fromIntegral throughV / fromIntegral total

-- Community detection (simple label propagation)
detectCommunities :: Graph -> Map Vertex Int
detectCommunities g = iterate propagate initialLabels !! 10
  where
    initialLabels = Map.fromList (zip (vertices g) [0..])
    propagate labels = Map.mapWithKey (\v _ ->
      let neighborLabels = map (labels Map.!) (g ! v)
          mostCommon = head . maximumBy (comparing length) . group . sort
      in if null neighborLabels then labels Map.! v else mostCommon neighborLabels
      ) labels
```

---

## ขั้นตอนที่ 635: Data Quality Monitoring

```haskell
-- Data quality checks

data DataQualityRule = DataQualityRule
  { dqrName    :: Text
  , dqrCheck   :: [Row] -> QualityResult
  , dqrSeverity :: Severity
  }

data QualityResult = QualityResult
  { qrPassed       :: Bool
  , qrPassRate     :: Double  -- 0.0 - 1.0
  , qrFailedRows   :: [Row]
  , qrDescription  :: Text
  }

data Severity = Critical | High | Medium | Low

-- Common quality rules
notNullRule :: Text -> DataQualityRule
notNullRule fieldName = DataQualityRule
  { dqrName = fieldName <> "_not_null"
  , dqrCheck = \rows ->
      let nullRows   = filter (isNull fieldName) rows
          passRate   = 1.0 - fromIntegral (length nullRows) / fromIntegral (length rows)
      in QualityResult (null nullRows) passRate nullRows ("Null values in " <> fieldName)
  , dqrSeverity = Critical
  }

uniquenessRule :: Text -> DataQualityRule
uniquenessRule fieldName = DataQualityRule
  { dqrName     = fieldName <> "_unique"
  , dqrCheck    = \rows ->
      let values    = map (getField fieldName) rows
          duplicates = findDuplicates values
          passRate   = 1.0 - fromIntegral (length duplicates) / fromIntegral (length values)
      in QualityResult (null duplicates) passRate [] ("Duplicate values in " <> fieldName)
  , dqrSeverity = High
  }

-- Run quality checks
runQualityChecks :: [DataQualityRule] -> [Row] -> QualityReport
runQualityChecks rules rows = QualityReport
  { qrResults  = map (\r -> (dqrName r, dqrCheck r rows)) rules
  , qrOverall  = all (qrPassed . snd) results
  , qrScore    = avg (map (qrPassRate . snd) results)
  }
  where results = map (\r -> (dqrName r, dqrCheck r rows)) rules
```

---

## ขั้นตอนที่ 636: Recommendation Engine

```haskell
-- Collaborative filtering recommendation

-- User-item matrix
type Rating = Double
type UserItemMatrix = Map UserId (Map ItemId Rating)

-- Cosine similarity between users
userSimilarity :: UserItemMatrix -> UserId -> UserId -> Double
userSimilarity matrix u1 u2 =
  let r1 = Map.findWithDefault Map.empty u1 matrix
      r2 = Map.findWithDefault Map.empty u2 matrix
      common = Map.intersectionWith (*) r1 r2
      dot    = sum (Map.elems common)
      norm1  = sqrt (sum (map (^2) (Map.elems r1)))
      norm2  = sqrt (sum (map (^2) (Map.elems r2)))
  in if norm1 * norm2 == 0 then 0 else dot / (norm1 * norm2)

-- Find similar users
findSimilarUsers :: UserItemMatrix -> UserId -> Int -> [(UserId, Double)]
findSimilarUsers matrix targetUser n =
  take n . sortBy (flip compare `on` snd) $
  [ (uid, userSimilarity matrix targetUser uid)
  | uid <- Map.keys matrix
  , uid /= targetUser
  ]

-- Predict rating
predictRating :: UserItemMatrix -> UserId -> ItemId -> Double
predictRating matrix uid iid =
  let similarUsers = findSimilarUsers matrix uid 10
      weighted = sum
        [ sim * rating
        | (su, sim) <- similarUsers
        , Just rating <- [Map.lookup iid =<< Map.lookup su matrix]
        ]
      totalSim = sum
        [ sim
        | (su, sim) <- similarUsers
        , Map.member iid (Map.findWithDefault Map.empty su matrix)
        ]
  in if totalSim == 0 then 3.0 else weighted / totalSim  -- default 3.0

-- Get top-N recommendations
getRecommendations :: UserItemMatrix -> UserId -> Int -> [ItemId]
getRecommendations matrix uid n =
  let userRatings = Map.findWithDefault Map.empty uid matrix
      allItems    = nub (concatMap Map.keys (Map.elems matrix))
      unrated     = filter (`Map.notMember` userRatings) allItems
      predictions = [(iid, predictRating matrix uid iid) | iid <- unrated]
  in take n . map fst . sortBy (flip compare `on` snd) $ predictions
```

---

## ขั้นตอนที่ 637: Data Synchronization

```haskell
-- Data sync patterns

-- Incremental sync
data SyncState = SyncState
  { ssLastSyncAt   :: UTCTime
  , ssCheckpoint   :: Maybe Text
  , ssSyncedCount  :: Int
  }

-- Sync with change tracking
incrementalSync :: Source -> Target -> SyncState -> IO SyncState
incrementalSync source target state = do
  changes <- getChanges source (ssLastSyncAt state)
  
  newState <- foldM (applyChange target) state changes
  
  now <- getCurrentTime
  return newState { ssLastSyncAt = now }

applyChange :: Target -> SyncState -> Change -> IO SyncState
applyChange target state change = do
  case changeType change of
    Insert -> targetInsert target (changeData change)
    Update -> targetUpdate target (changeId change) (changeData change)
    Delete -> targetDelete target (changeId change)
  
  return state { ssSyncedCount = ssSyncedCount state + 1
               , ssCheckpoint  = Just (changeId change) }

-- Conflict resolution
data ConflictStrategy
  = LastWriteWins
  | FirstWriteWins
  | MergeFields (Map Text MergeRule)
  | CustomMerge (Value -> Value -> Value)

resolveConflict :: ConflictStrategy -> Value -> Value -> Value
resolveConflict LastWriteWins _ newer = newer
resolveConflict FirstWriteWins older _ = older
resolveConflict (MergeFields rules) older newer =
  mergeByRules rules older newer
resolveConflict (CustomMerge f) older newer = f older newer
```

---

## ขั้นตอนที่ 638: OLAP Queries

```haskell
-- OLAP (Online Analytical Processing) queries

-- Cube operations
data Cube = Cube
  { cubeDimensions :: [Dimension]
  , cubeMeasures   :: [Measure]
  , cubeData       :: Map CellKey [Double]
  }

data Dimension = Dimension
  { dimName :: Text
  , dimValues :: [Text]
  , dimHierarchy :: Maybe [Text]
  }

-- Drill down
drillDown :: Cube -> Text -> Text -> Cube
drillDown cube dimName parentValue = cube
  { cubeData = Map.filterWithKey
      (\key _ -> keyMatchesDim key dimName parentValue)
      (cubeData cube)
  }

-- Roll up
rollUp :: Cube -> Text -> Cube
rollUp cube dimName = cube
  { cubeData = Map.fromListWith (zipWith (+))
      [ (stripDim key dimName, values)
      | (key, values) <- Map.toList (cubeData cube)
      ]
  }

-- Slice (filter on one dimension)
slice :: Cube -> Text -> Text -> Cube
slice cube dimName value = cube
  { cubeData = Map.filterWithKey
      (\key _ -> getDimValue key dimName == Just value)
      (cubeData cube)
  }

-- Dice (filter on multiple dimensions)
dice :: Cube -> [(Text, [Text])] -> Cube
dice cube conditions = cube
  { cubeData = Map.filterWithKey
      (\key _ -> all (\(dim, vals) ->
        case getDimValue key dim of
          Nothing  -> False
          Just val -> val `elem` vals)
        conditions)
      (cubeData cube)
  }

-- Pivot
pivot :: Cube -> Text -> Cube
pivot cube dimName = cube
  { cubeData = Map.fromListWith (zipWith (+))
      [ (pivotKey key dimName, values)
      | (key, values) <- Map.toList (cubeData cube)
      ]
  }
```

---

## ขั้นตอนที่ 639: Real-time Dashboard API

```haskell
-- Real-time dashboard API

-- WebSocket-based real-time metrics
data DashboardConfig = DashboardConfig
  { dcRefreshRate  :: Int  -- seconds
  , dcWidgets      :: [Widget]
  }

data Widget
  = MetricWidget   { mwMetric :: Text }
  | ChartWidget    { cwType :: ChartType, cwMetric :: Text }
  | TableWidget    { twQuery :: Text }
  | AlertsWidget   { awSeverity :: Severity }

-- Push updates to connected clients
type Client = Connection  -- WebSocket connection

broadcastMetrics :: TVar (Set Client) -> MetricsService -> IO ()
broadcastMetrics clientsVar metrics = forever $ do
  threadDelay (5 * 1000000)  -- 5 seconds
  
  update <- collectMetrics metrics
  clients <- readTVarIO clientsVar
  
  forM_ clients $ \client ->
    E.try @SomeException (sendTextData client (encode update)) >>= \case
      Left _  -> atomically $ modifyTVar clientsVar (S.delete client)
      Right _ -> return ()

-- Dashboard API endpoints
type DashboardAPI
  = "dashboard" :> "metrics"  :> Get '[JSON] MetricsSnapshot
  :<|> "dashboard" :> "ws"     :> WebSocket
  :<|> "dashboard" :> "charts" :> Capture "id" ChartId :> Get '[JSON] ChartData

-- Metrics snapshot
collectSnapshot :: MetricsService -> IO MetricsSnapshot
collectSnapshot svc = MetricsSnapshot
  <$> getCurrentTime
  <*> getRequestRate svc
  <*> getErrorRate svc
  <*> getP99Latency svc
  <*> getActiveConnections svc
  <*> getDbPoolUsage svc
  <*> getCacheHitRate svc
```

---

## ขั้นตอนที่ 640: โปรเจกต์: Analytics Platform

```haskell
-- Complete analytics platform

-- Data ingestion
data IngestionPipeline = IngestionPipeline
  { ipSources   :: [DataSource]
  , ipParser    :: ByteString -> Either Text [Event]
  , ipValidator :: Event -> Either Text Event
  , ipEnricher  :: Event -> IO Event
  , ipSink      :: Event -> IO ()
  }

runIngestion :: IngestionPipeline -> IO ()
runIngestion pipeline = do
  chans <- forM (ipSources pipeline) $ \source -> do
    chan <- newChan
    forkIO $ sourceLoop source chan
    return chan
  
  workers <- replicateM 4 $ forkIO $ processLoop pipeline chans
  
  mapM_ wait workers
  where
    processLoop p chans = forever $ do
      bs <- readFromAny chans
      case ipParser p bs of
        Left err     -> logError ("Parse error: " <> err)
        Right events -> forM_ events $ \e -> do
          case ipValidator p e of
            Left err   -> logError ("Validation error: " <> err)
            Right valid -> ipEnricher p valid >>= ipSink p

-- Query engine
data QueryEngine = QueryEngine
  { qeStorage :: AnalyticsStorage
  , qeCache   :: QueryCache
  , qeOptimizer :: QueryPlan -> QueryPlan
  }

executeAnalyticsQuery :: QueryEngine -> AnalyticsQuery -> IO QueryResult
executeAnalyticsQuery engine query = do
  -- Check cache
  mCached <- queryCacheGet (qeCache engine) query
  case mCached of
    Just result -> return result
    Nothing -> do
      -- Optimize and execute
      let plan     = buildQueryPlan query
      let optimized = qeOptimizer engine plan
      result <- executeQueryPlan (qeStorage engine) optimized
      -- Cache result
      queryCacheSet (qeCache engine) query result 300
      return result
```

---

*[← Part 31](part-31.md) | [Part 33 →](part-33.md)*
