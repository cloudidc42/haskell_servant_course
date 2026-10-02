# Part 39: Machine Learning & AI Integration
## ขั้นตอนที่ 761-780

---

## ขั้นตอนที่ 761: Linear Algebra with HMatrix

```haskell
-- Linear algebra สำหรับ ML

import Numeric.LinearAlgebra

-- Matrix operations
matrixOps :: IO ()
matrixOps = do
  let a = (3><3) [1,2,3, 4,5,6, 7,8,9] :: Matrix Double
  let b = (3><1) [1,0,1] :: Matrix Double
  
  -- Matrix multiplication
  let c = a <> b
  putStrLn $ "Matrix product: " ++ show c
  
  -- Eigenvalues
  let (eigenvals, eigenvecs) = eig a
  putStrLn $ "Eigenvalues: " ++ show eigenvals
  
  -- SVD decomposition
  let (u, s, v) = svd a
  putStrLn $ "Singular values: " ++ show s
  
  -- Solve linear system Ax = b
  let x = linearSolve a b
  putStrLn $ "Solution: " ++ show x

-- Principal Component Analysis
pca :: Matrix Double -> Int -> (Matrix Double, Vector Double)
pca dataMatrix numComponents =
  let centered   = centerMatrix dataMatrix
      covariance = trans centered <> centered / fromIntegral (rows centered - 1)
      (vals, vecs) = eigSH covariance
      topVecs    = subMatrix (0,0) (cols covariance, numComponents) vecs
  in (centered <> topVecs, vals)

centerMatrix :: Matrix Double -> Matrix Double
centerMatrix m =
  let means = fromColumns [meanColumn m col | col <- [0..cols m - 1]]
  in m - repmat means (rows m) 1

meanColumn :: Matrix Double -> Int -> Vector Double
meanColumn m col = scalar (mean (toRows m !! col))
```

---

## ขั้นตอนที่ 762: Neural Network from Scratch

```haskell
-- Simple neural network ใน Haskell

import Numeric.LinearAlgebra

-- Activation functions
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

sigmoidDerivative :: Double -> Double
sigmoidDerivative x = sigmoid x * (1 - sigmoid x)

relu :: Double -> Double
relu = max 0

reluDerivative :: Double -> Double
reluDerivative x = if x > 0 then 1 else 0

-- Layer
data Layer = Layer
  { layerWeights :: Matrix Double
  , layerBiases  :: Vector Double
  , layerActivation :: Double -> Double
  , layerActivationDeriv :: Double -> Double
  }

-- Forward pass
forward :: Layer -> Vector Double -> (Vector Double, Vector Double)
forward layer input =
  let z    = layerWeights layer #> input + layerBiases layer
      activated = cmap (layerActivation layer) z
  in (z, activated)

-- Full network forward pass
forwardNetwork :: [Layer] -> Vector Double -> [(Vector Double, Vector Double)]
forwardNetwork layers input = scanl step (undefined, input) layers
  where step (_, x) layer = forward layer x

-- Loss function (MSE)
mseLoss :: Vector Double -> Vector Double -> Double
mseLoss predicted actual = sumElements ((predicted - actual) ^ 2) / fromIntegral (size predicted)

-- Backpropagation (simplified)
backprop :: [Layer] -> [(Vector Double, Vector Double)] -> Vector Double -> [Matrix Double]
backprop layers activations target =
  let output   = snd (last activations)
      delta    = output - target
  in zipWith3 computeGrad layers (init activations) (tail activations)
  where
    computeGrad layer (_, prevAct) (z, _) =
      let delta' = cmap (layerActivationDeriv layer) z
      in outer delta' prevAct
```

---

## ขั้นตอนที่ 763: Natural Language Processing

```haskell
-- NLP utilities

-- Tokenization
tokenize :: Text -> [Token]
tokenize = map mkToken . T.words . normalize
  where
    normalize = T.toLower . T.filter (\c -> isAlpha c || isSpace c)
    mkToken w = Token { tokenText = w, tokenType = classifyWord w }

data Token = Token
  { tokenText :: Text
  , tokenType :: TokenType
  }

data TokenType = Word | Number | Punctuation | URL

-- Stop words removal
removeStopWords :: [Text] -> [Text] -> [Text]
removeStopWords stopWords = filter (`notElem` stopWords)

-- N-grams
ngrams :: Int -> [a] -> [[a]]
ngrams n xs
  | length xs < n = []
  | otherwise     = take n xs : ngrams n (tail xs)

-- Bag of words
bagOfWords :: [[Text]] -> (Map Text Int, [[Int]])
bagOfWords documents =
  let vocab    = Map.fromList (zip (nub (concat documents)) [0..])
      vectors  = map (documentVector vocab) documents
  in (vocab, vectors)

documentVector :: Map Text Int -> [Text] -> [Int]
documentVector vocab words =
  let counts = Map.fromListWith (+) (map (\w -> (w, 1)) words)
  in [ fromMaybe 0 (Map.lookup (vocab Map.! term) counts)
     | term <- Map.keys vocab
     ]

-- Simple text classification (Naive Bayes)
data NaiveBayes = NaiveBayes
  { nbClasses    :: Map Text Double   -- class -> log prior
  , nbWordProbs  :: Map Text (Map Text Double)  -- class -> word -> log prob
  }

classify :: NaiveBayes -> [Text] -> Text
classify nb words =
  fst . maximumBy (comparing snd) $
  [ (cls, logPrior + sum wordScores)
  | (cls, logPrior) <- Map.toList (nbClasses nb)
  , let wordScores = map (\w -> fromMaybe (-20) (Map.lookup w =<< Map.lookup cls (nbWordProbs nb))) words
  ]
```

---

## ขั้นตอนที่ 764: Time Series Forecasting

```haskell
-- Time series forecasting

-- ARIMA components
data ArimaParams = ArimaParams
  { apP :: Int  -- AR order
  , apD :: Int  -- differencing
  , apQ :: Int  -- MA order
  }

-- Simple AR model
fitAr :: Int -> [Double] -> ARModel
fitAr p series =
  let lagMatrix = buildLagMatrix p series
      ys        = drop p series
      xs        = tail lagMatrix  -- remove first incomplete row
      -- OLS: beta = (X^T X)^-1 X^T y
      xMat      = fromLists xs
      yVec      = fromList ys
      beta      = linearSolve (trans xMat <> xMat) (trans xMat #> yVec)
  in ARModel p (toList beta)

buildLagMatrix :: Int -> [Double] -> [[Double]]
buildLagMatrix p series =
  [ take p (drop i series) | i <- [0..length series - p] ]

-- Forecast with AR model
forecastAR :: ARModel -> [Double] -> Int -> [Double]
forecastAR model history steps = go history steps
  where
    go hist 0 = []
    go hist n =
      let recent = take (armP model) (reverse hist)
          next   = sum (zipWith (*) recent (armCoefficients model))
      in next : go (hist ++ [next]) (n - 1)

-- Exponential smoothing
holtwinters :: Double -> Double -> Double -> [Double] -> [Double]
holtwinters alpha beta gamma series = predictions
  where
    -- Initialize
    l0 = head series
    b0 = (series !! 1) - head series
    
    -- Smooth
    (_, _, predictions) = foldl' update (l0, b0, []) (tail series)
    
    update (l, b, ps) y =
      let l' = alpha * y + (1 - alpha) * (l + b)
          b' = beta * (l' - l) + (1 - beta) * b
      in (l', b', ps ++ [l' + b'])
```

---

## ขั้นตอนที่ 765: Anomaly Detection

```haskell
-- Anomaly detection algorithms

-- Statistical anomaly detection
data AnomalyConfig = AnomalyConfig
  { acWindowSize   :: Int
  , acZScoreThreshold :: Double
  , acIqrThreshold :: Double
  }

data Anomaly = Anomaly
  { anIndex  :: Int
  , anValue  :: Double
  , anScore  :: Double
  , anMethod :: Text
  }

-- Z-score method
detectZScoreAnomalies :: AnomalyConfig -> [Double] -> [Anomaly]
detectZScoreAnomalies cfg values =
  let mu     = mean (fromList values)
      sigma  = stdDev (fromList values)
      zScores = map (\v -> abs ((v - mu) / sigma)) values
  in [ Anomaly i v z "zscore"
     | (i, v, z) <- zip3 [0..] values zScores
     , z > acZScoreThreshold cfg
     ]

-- IQR method
detectIqrAnomalies :: [Double] -> Double -> [Anomaly]
detectIqrAnomalies values multiplier =
  let sorted = sort values
      n      = length values
      q1     = sorted !! (n `div` 4)
      q3     = sorted !! (3 * n `div` 4)
      iqr    = q3 - q1
      lower  = q1 - multiplier * iqr
      upper  = q3 + multiplier * iqr
  in [ Anomaly i v (abs (v - (q1 + q3) / 2) / iqr) "iqr"
     | (i, v) <- zip [0..] values
     , v < lower || v > upper
     ]

-- Isolation Forest (simplified)
data IsolationTree
  = ILeaf Int  -- depth at which isolated
  | INode { itFeature :: Int
          , itSplit    :: Double
          , itLeft     :: IsolationTree
          , itRight    :: IsolationTree
          }

anomalyScore :: IsolationTree -> [Double] -> Double
anomalyScore tree point = fromIntegral (pathLength tree point) / fromIntegral maxDepth
  where maxDepth = 10

pathLength :: IsolationTree -> [Double] -> Int
pathLength (ILeaf d) _ = d
pathLength (INode feat split left right) point =
  if point !! feat < split
    then 1 + pathLength left point
    else 1 + pathLength right point
```

---

## ขั้นตอนที่ 766: Embeddings and Similarity Search

```haskell
-- Vector embeddings and similarity search

import Numeric.LinearAlgebra (Vector, dot, norm_2)

-- Embedding operations
type Embedding = Vector Double

-- Cosine similarity
cosineSim :: Embedding -> Embedding -> Double
cosineSim a b = dot a b / (norm_2 a * norm_2 b)

-- Dot product similarity
dotSim :: Embedding -> Embedding -> Double
dotSim = dot

-- Euclidean distance
euclideanDist :: Embedding -> Embedding -> Double
euclideanDist a b = norm_2 (a - b)

-- K-nearest neighbors
knn :: Embedding -> [(Text, Embedding)] -> Int -> [(Text, Double)]
knn query embeddings k =
  take k . sortBy (flip compare `on` snd) $
  [ (label, cosineSim query emb)
  | (label, emb) <- embeddings
  ]

-- HNSW-like index (simplified flat index for small datasets)
data EmbeddingIndex = EmbeddingIndex
  { eiEmbeddings :: Map Text Embedding
  }

searchIndex :: EmbeddingIndex -> Embedding -> Int -> [(Text, Double)]
searchIndex idx query k =
  take k . sortBy (flip compare `on` snd) $
  [ (key, cosineSim query emb)
  | (key, emb) <- Map.toList (eiEmbeddings idx)
  ]

-- Cluster embeddings (k-means)
kmeans :: Int -> [Embedding] -> IO [[Embedding]]
kmeans k embeddings = do
  -- Random initialization
  centers <- randomSample k embeddings
  
  -- Iterate
  let clusters = iterate (updateCenters . assignPoints centers) centers
  return (snd (head (dropWhile (\(a,b) -> not (converged a b)) (zip clusters (tail clusters)))))
  where
    assignPoints centers embs =
      groupBy (\e -> argmax [cosineSim e c | c <- centers]) embs
    
    updateCenters = map centroid
    
    centroid points = foldl1 (+) points / scalar (fromIntegral (length points))
```

---

## ขั้นตอนที่ 767: OpenAI API Integration

```haskell
-- OpenAI API integration

data OpenAIConfig = OpenAIConfig
  { oaApiKey  :: Text
  , oaBaseUrl :: Text
  , oaTimeout :: Int
  }

-- Chat completion
data ChatMessage' = ChatMessage'
  { cmRole'    :: Text  -- "system", "user", "assistant"
  , cmContent' :: Text
  }

data ChatRequest = ChatRequest
  { crModel       :: Text
  , crMessages    :: [ChatMessage']
  , crTemperature :: Double
  , crMaxTokens   :: Maybe Int
  }

data ChatResponse = ChatResponse
  { crChoices     :: [Choice]
  , crUsage       :: Usage
  }

data Choice = Choice
  { choMessage      :: ChatMessage'
  , choFinishReason :: Text
  }

data Usage = Usage
  { usPromptTokens    :: Int
  , usCompletionTokens :: Int
  , usTotalTokens     :: Int
  }

-- API calls
chat :: OpenAIConfig -> ChatRequest -> IO ChatResponse
chat cfg req = do
  let url = oaBaseUrl cfg <> "/chat/completions"
  response <- httpJSONWith
    (setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 (oaApiKey cfg)]
    . setRequestBodyJSON req
    . setRequestMethod "POST")
    (parseRequest_ (T.unpack url))
  return (responseBody response)

-- Simple chat helper
ask :: OpenAIConfig -> Text -> Text -> IO Text
ask cfg systemPrompt userMessage = do
  response <- chat cfg ChatRequest
    { crModel       = "gpt-4"
    , crMessages    = [ ChatMessage' "system" systemPrompt
                      , ChatMessage' "user"   userMessage
                      ]
    , crTemperature = 0.7
    , crMaxTokens   = Just 1000
    }
  return $ cmContent' $ choMessage $ head $ crChoices response
```

---

## ขั้นตอนที่ 768: RAG (Retrieval-Augmented Generation)

```haskell
-- Retrieval-Augmented Generation

data RagPipeline = RagPipeline
  { rpEmbeddingModel :: Text -> IO Embedding
  , rpVectorStore    :: EmbeddingStore
  , rpLlm            :: Text -> IO Text
  }

data Document = Document
  { docId      :: DocId
  , docContent :: Text
  , docMeta    :: Map Text Text
  }

-- Index documents
indexDocuments :: RagPipeline -> [Document] -> IO ()
indexDocuments rag docs =
  forM_ docs $ \doc -> do
    -- Chunk document
    let chunks = chunkDocument 512 128 (docContent doc)  -- 512 token chunks, 128 overlap
    
    -- Embed each chunk
    forM_ (zip [0..] chunks) $ \(i, chunk) -> do
      emb <- rpEmbeddingModel rag chunk
      storeEmbedding (rpVectorStore rag) EmbeddingRecord
        { erDocId   = docId doc
        , erChunkId = i
        , erText    = chunk
        , erVector  = emb
        , erMeta    = docMeta doc
        }

-- Query with RAG
query :: RagPipeline -> Text -> Int -> IO Text
query rag question k = do
  -- Embed question
  qEmb <- rpEmbeddingModel rag question
  
  -- Find relevant chunks
  relevant <- searchSimilar (rpVectorStore rag) qEmb k
  
  -- Build context
  let context = T.unlines (map erText relevant)
  
  -- Generate answer
  let prompt = T.unlines
        [ "Context:"
        , context
        , ""
        , "Question: " <> question
        , ""
        , "Answer based only on the context above:"
        ]
  
  rpLlm rag prompt

-- Chunk text with overlap
chunkDocument :: Int -> Int -> Text -> [Text]
chunkDocument size overlap text =
  let words' = T.words text
      n       = length words'
  in [ T.unwords (take size (drop i words'))
     | i <- [0, size - overlap..n - size]
     ]
```

---

## ขั้นตอนที่ 769: ML Model Serving

```haskell
-- Serve ML models via REST API

data ModelServer = ModelServer
  { msModel   :: Model
  , msMetrics :: ModelMetrics
  }

data Model = Model
  { modelPredict :: [Double] -> IO Prediction
  , modelVersion :: Text
  , modelInputSchema  :: Schema
  , modelOutputSchema :: Schema
  }

data Prediction = Prediction
  { predValue      :: Value
  , predConfidence :: Double
  , predLatency    :: Double
  }

-- Prediction endpoint
type PredictAPI
  = "predict"
  :> ReqBody '[JSON] PredictionRequest
  :> Post '[JSON] PredictionResponse

predictHandler :: ModelServer -> PredictionRequest -> Handler PredictionResponse
predictHandler server req = do
  -- Validate input
  case validateAgainstSchema (modelInputSchema (msModel server)) (prFeatures req) of
    Left errs -> throwError err400 { errBody = encode errs }
    Right _ -> do
      -- Predict
      start      <- liftIO getMonotonicTime
      prediction <- liftIO $ modelPredict (msModel server) (prFeatures req)
      end        <- liftIO getMonotonicTime
      
      let latency = (end - start) * 1000
      
      -- Record metrics
      liftIO $ recordPredictionMetrics (msMetrics server) latency
      
      return PredictionResponse
        { prPrediction  = predValue prediction
        , prConfidence  = predConfidence prediction
        , prLatencyMs   = latency
        , prModelVersion = modelVersion (msModel server)
        }

-- Model versioning and A/B testing
data ModelRouter = ModelRouter
  { mrModels  :: Map Text Model
  , mrWeights :: Map Text Double  -- model name -> traffic weight
  }

routeToModel :: ModelRouter -> IO Model
routeToModel router = do
  r <- randomRIO (0.0, 1.0)
  let (name, _) = head $ dropWhile (\(_, cumWeight) -> r > cumWeight)
        $ zip (Map.keys (mrModels router))
              (scanl1 (+) (Map.elems (mrWeights router)))
  return (mrModels router Map.! name)
```

---

## ขั้นตอนที่ 770: Data Pipelines for ML

```haskell
-- ML data pipelines

-- Feature engineering pipeline
data FeaturePipeline = FeaturePipeline
  { fpSteps :: [TransformStep]
  }

data TransformStep
  = Normalize   { tsFeatures :: [Text] }
  | Standardize { tsFeatures :: [Text] }
  | Encode      { tsFeature :: Text, tsMethod :: EncodingMethod }
  | Impute      { tsFeature :: Text, tsStrategy :: ImputeStrategy }
  | DropNA      { tsFeatures :: [Text] }
  | CreateFeature { tsNewFeature :: Text, tsFormula :: Row -> Double }

data EncodingMethod = OneHot | LabelEncode | TargetEncode
data ImputeStrategy = Mean | Median | Mode | Constant Double

-- Apply pipeline
transform :: FeaturePipeline -> [Row] -> IO [Row]
transform pipeline rows = foldM applyStep rows (fpSteps pipeline)

applyStep :: [Row] -> TransformStep -> IO [Row]
applyStep rows (Normalize features) = do
  let stats = map (computeStats rows) features
  return (map (normalizeRow features stats) rows)

applyStep rows (Encode feature OneHot) = do
  let categories = nub (map (\r -> getField feature r) rows)
  return (map (oneHotEncode feature categories) rows)

applyStep rows (Impute feature Mean) = do
  let values = mapMaybe (\r -> getNumericField feature r) rows
  let meanVal = sum values / fromIntegral (length values)
  return (map (imputeField feature meanVal) rows)

-- Train/test split
splitDataset :: Double -> [a] -> IO ([a], [a])
splitDataset ratio dataset = do
  shuffled <- shuffleIO dataset
  let n = floor (fromIntegral (length shuffled) * ratio)
  return (take n shuffled, drop n shuffled)

-- Cross-validation
crossValidate :: Int -> ([a] -> [a] -> IO Double) -> [a] -> IO Double
crossValidate k eval dataset = do
  let folds = splitIntoFolds k dataset
  scores <- forM [0..k-1] $ \i -> do
    let test  = folds !! i
    let train = concat (take i folds ++ drop (i+1) folds)
    eval train test
  return (sum scores / fromIntegral k)
```

---

## ขั้นตอนที่ 771: Recommendation System

```haskell
-- Matrix factorization recommendation system

-- Latent factor model
data LatentFactorModel = LatentFactorModel
  { lfmUserFactors :: Matrix Double  -- users x factors
  , lfmItemFactors :: Matrix Double  -- items x factors
  , lfmUserBias    :: Vector Double
  , lfmItemBias    :: Vector Double
  , lfmGlobalBias  :: Double
  }

-- Predict rating
predictRating' :: LatentFactorModel -> Int -> Int -> Double
predictRating' model userId itemId =
  lfmGlobalBias model
  + lfmUserBias model ! userId
  + lfmItemBias model ! itemId
  + (lfmUserFactors model ! userId) `dot` (lfmItemFactors model ! itemId)

-- SGD training
trainSgd :: [(Int, Int, Double)] -> Int -> Int -> Int -> Double -> LatentFactorModel
trainSgd ratings numUsers numItems numFactors lr =
  let initialModel = initializeModel numUsers numItems numFactors
  in foldl' updateModel initialModel (cycle ratings)  -- simplified

updateModel :: LatentFactorModel -> (Int, Int, Double) -> LatentFactorModel
updateModel model (u, i, r) =
  let pred  = predictRating' model u i
      err   = r - pred
      uFact = lfmUserFactors model ! u
      iFact = lfmItemFactors model ! i
      lr    = 0.01
      reg   = 0.02
      
      -- Update factors
      newUFact = uFact + scalar lr * (scalar err * iFact - scalar reg * uFact)
      newIFact = iFact + scalar lr * (scalar err * uFact - scalar reg * iFact)
  in model  -- update relevant rows

-- Recommend top-N items
recommend :: LatentFactorModel -> Int -> [Int] -> Int -> [(Int, Double)]
recommend model userId candidateItems n =
  take n . sortBy (flip compare `on` snd) $
  [ (item, predictRating' model userId item) | item <- candidateItems ]
```

---

## ขั้นตอนที่ 772: Feature Store

```haskell
-- Feature store for ML features

data FeatureStore = FeatureStore
  { fsOnline  :: OnlineStore   -- Redis for low-latency
  , fsOffline :: OfflineStore  -- Parquet/S3 for batch
  }

-- Feature definition
data FeatureGroup = FeatureGroup
  { fgName     :: Text
  , fgFeatures :: [FeatureDefinition]
  , fgEntity   :: EntityKey
  , fgTtl      :: Maybe NominalDiffTime
  }

data FeatureDefinition = FeatureDefinition
  { fdName      :: Text
  , fdType      :: FeatureType
  , fdTransform :: Maybe (Value -> Value)
  }

data FeatureType = FtInt | FtFloat | FtText | FtBool | FtVector [Int]

-- Write features
writeFeatures :: FeatureStore -> EntityKey -> Map Text Value -> IO ()
writeFeatures store entity features = do
  -- Write to online store
  writeOnline (fsOnline store) entity features
  -- Write to offline store (async)
  forkIO (writeOffline (fsOffline store) entity features)
  return ()

-- Read features for serving
getOnlineFeatures :: FeatureStore -> [EntityKey] -> [Text] -> IO (Map EntityKey (Map Text Value))
getOnlineFeatures store entities featureNames =
  Map.fromList <$> forConcurrently entities (\entity -> do
    features <- readOnline (fsOnline store) entity featureNames
    return (entity, features))

-- Compute features from raw data
computeUserFeatures :: ConnectionPool -> UserId -> IO (Map Text Value)
computeUserFeatures pool uid = do
  userStats <- getUserStats pool uid
  orderStats <- getOrderStats pool uid
  
  return $ Map.fromList
    [ ("age_days",       toJSON (diffDays today (userCreatedAt userStats)))
    , ("order_count_30d", toJSON (count30d orderStats))
    , ("avg_order_value", toJSON (avgValue orderStats))
    , ("last_seen_days",  toJSON (daysSinceLastSeen userStats))
    ]
```

---

## ขั้นตอนที่ 773: A/B Testing Framework

```haskell
-- A/B testing framework

data Experiment = Experiment
  { expId        :: ExperimentId
  , expName      :: Text
  , expVariants  :: [Variant]
  , expMetrics   :: [MetricDefinition]
  , expStarted   :: UTCTime
  , expStatus    :: ExperimentStatus
  }

data Variant = Variant
  { varId     :: VariantId
  , varName   :: Text
  , varWeight :: Double
  , varConfig :: Map Text Value
  }

data ExperimentStatus = Draft | Running | Paused | Concluded

-- Assign user to variant (deterministic)
assignVariant :: Experiment -> UserId -> VariantId
assignVariant exp uid =
  let hash   = hashUserId uid `mod` 1000
      bucket = fromIntegral hash / 1000.0
      
      (varId, _) = head $ dropWhile (\(_, cumW) -> bucket > cumW)
        $ zip (map varId (expVariants exp))
              (scanl1 (+) (map varWeight (expVariants exp)))
  in varId

-- Record experiment event
recordExperimentEvent :: Experiment -> UserId -> VariantId -> Text -> Double -> IO ()
recordExperimentEvent exp uid vid metric value = insertEvent ExperimentEvent
  { eeExperimentId = expId exp
  , eeUserId       = uid
  , eeVariantId    = vid
  , eeMetric       = metric
  , eeValue        = value
  , eeTimestamp    = getCurrentTime
  }

-- Statistical significance test (two-sample t-test)
tTest :: [Double] -> [Double] -> TTestResult
tTest control treatment =
  let n1  = fromIntegral (length control)
      n2  = fromIntegral (length treatment)
      mu1 = sum control / n1
      mu2 = sum treatment / n2
      s1  = variance' control
      s2  = variance' treatment
      se  = sqrt (s1/n1 + s2/n2)
      t   = (mu2 - mu1) / se
      df  = (s1/n1 + s2/n2)^2 / ((s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1))
  in TTestResult t df (tPValue t df) (mu2 - mu1)
```

---

## ขั้นตอนที่ 774: Model Monitoring

```haskell
-- ML model performance monitoring

data ModelMonitor = ModelMonitor
  { mmModel       :: Text
  , mmMetrics     :: TVar ModelMetrics'
  , mmDriftDetect :: DriftDetector
  , mmAlerts      :: [AlertRule]
  }

data ModelMetrics' = ModelMetrics'
  { mmPredictions  :: Int
  , mmErrors       :: Int
  , mmLatencyP99   :: Double
  , mmLatencyAvg   :: Double
  , mmConfidences  :: [Double]
  }

-- Data drift detection (PSI)
data DriftDetector = DriftDetector
  { ddBaseline    :: Distribution
  , ddWindow      :: TVar [Double]
  , ddWindowSize  :: Int
  }

-- Population Stability Index
computePsi :: [Double] -> [Double] -> Double
computePsi baseline current =
  let baselineBins = histogram' baseline 10
      currentBins  = histogram' current 10
  in sum $ zipWith psiTerm baselineBins currentBins
  where
    psiTerm expected actual =
      let e = max 0.0001 expected
          a = max 0.0001 actual
      in (a - e) * log (a / e)

checkDrift :: DriftDetector -> [Double] -> IO DriftResult
checkDrift detector recent = do
  let psi = computePsi (ddBaseline detector) recent
  return $ case psi of
    p | p < 0.1  -> NoDrift p
    p | p < 0.2  -> MinorDrift p
    p            -> SignificantDrift p

-- Model performance degradation
detectDegradation :: ModelMonitor -> [PredictionLog] -> IO [DegradationAlert]
detectDegradation monitor logs = do
  let recent   = filter isRecent logs
  let baseline = filter isBaseline logs
  
  let recentAcc   = computeAccuracy recent
  let baselineAcc = computeAccuracy baseline
  
  let degradation = baselineAcc - recentAcc
  
  if degradation > 0.05  -- 5% degradation threshold
    then return [ModelDegradation (mmModel monitor) degradation]
    else return []
```

---

## ขั้นตอนที่ 775: Explainable AI

```haskell
-- Explainability for ML models

-- SHAP-like feature importance
data Explanation = Explanation
  { expPrediction  :: Double
  , expBaseValue   :: Double
  , expFeatureContributions :: Map Text Double
  }

-- Permutation importance
permutationImportance :: (Row -> Double) -> [Row] -> [Text] -> IO (Map Text Double)
permutationImportance predict rows features = do
  let baseline = map predict rows
  let baselineError = mse baseline (map rowLabel rows)
  
  importances <- forM features $ \feat -> do
    -- Permute this feature
    permuted <- shuffleFeature rows feat
    let permutedPreds = map predict permuted
    let permutedError = mse permutedPreds (map rowLabel rows)
    return (feat, permutedError - baselineError)
  
  return (Map.fromList importances)

-- LIME: local linear approximation
lime :: (Row -> Double) -> Row -> [Text] -> Int -> IO Explanation
lime predict instance' features numSamples = do
  -- Generate neighborhood samples
  samples <- replicateM numSamples (perturbInstance instance' features)
  
  -- Get predictions for neighborhood
  let predictions = map predict samples
  
  -- Fit linear model to neighborhood
  let coefs = fitLinearModel samples predictions
  
  return Explanation
    { expPrediction  = predict instance'
    , expBaseValue   = mean (fromList predictions)
    , expFeatureContributions = Map.fromList (zip features coefs)
    }

perturbInstance :: Row -> [Text] -> IO Row
perturbInstance instance' features = do
  mask <- replicateM (length features) (randomRIO (0.0, 1.0) >>= \r -> return (r > 0.5))
  return (applyMask instance' features mask)
```

---

## ขั้นตอนที่ 776: ML Pipeline Orchestration

```haskell
-- ML pipeline orchestration

data MlPipeline = MlPipeline
  { mlpName   :: Text
  , mlpStages :: [MlStage]
  }

data MlStage
  = DataIngestion   { diSource :: DataSource }
  | DataValidation  { dvChecks :: [DataCheck] }
  | FeatureEngineering { feSteps :: FeaturePipeline }
  | Training        { trConfig :: TrainingConfig }
  | Evaluation      { evMetrics :: [MetricName] }
  | Registration    { rgModel :: ModelRegistry }
  | Deployment      { dpTarget :: DeploymentTarget }

data TrainingConfig = TrainingConfig
  { tcAlgorithm  :: Text
  , tcHyperParams :: Map Text Value
  , tcDataSplit  :: DataSplit
  , tcEarlyStopping :: Maybe Int
  }

-- Run ML pipeline
runMlPipeline :: MlPipeline -> PipelineInput -> IO PipelineOutput
runMlPipeline pipeline input = do
  stageOutputs <- foldM runMlStage input (mlpStages pipeline)
  return stageOutputs

runMlStage :: PipelineInput -> MlStage -> IO PipelineInput
runMlStage input (DataIngestion src) = do
  data' <- loadData src
  return input { piData = data' }

runMlStage input (Training config) = do
  model <- trainModel config (piData input)
  return input { piModel = Just model }

runMlStage input (Evaluation metrics) = do
  model <- maybe (throwIO NoModelError) return (piModel input)
  results <- evaluateModel model (piTestData input) metrics
  logMetrics results
  return input { piMetrics = results }

runMlStage input (Deployment target) = do
  model <- maybe (throwIO NoModelError) return (piModel input)
  deployModel model target
  return input
```

---

## ขั้นตอนที่ 777: Reinforcement Learning

```haskell
-- Simple RL: Q-Learning

-- Environment interface
class Environment env where
  type State env :: *
  type Action env :: *
  
  reset  :: env -> IO (State env)
  step   :: env -> State env -> Action env -> IO (State env, Double, Bool)
  actions :: env -> [Action env]

-- Q-Learning agent
data QAgent s a = QAgent
  { qaQTable   :: TVar (Map (s, a) Double)
  , qaAlpha    :: Double  -- learning rate
  , qaGamma    :: Double  -- discount factor
  , qaEpsilon  :: Double  -- exploration rate
  }

-- Choose action (epsilon-greedy)
chooseAction :: (Ord s, Ord a) => QAgent s a -> [a] -> s -> IO a
chooseAction agent actions state = do
  r <- randomRIO (0.0, 1.0)
  if r < qaEpsilon agent
    then do
      idx <- randomRIO (0, length actions - 1)
      return (actions !! idx)
    else do
      qTable <- readTVarIO (qaQTable agent)
      let qValues = [ (a, fromMaybe 0 (Map.lookup (state, a) qTable)) | a <- actions ]
      return (fst (maximumBy (comparing snd) qValues))

-- Update Q-value
updateQ :: (Ord s, Ord a) => QAgent s a -> [a] -> s -> a -> Double -> s -> IO ()
updateQ agent actions state action reward nextState = do
  qTable <- readTVarIO (qaQTable agent)
  let currentQ = fromMaybe 0 (Map.lookup (state, action) qTable)
  let maxNextQ = maximum [fromMaybe 0 (Map.lookup (nextState, a) qTable) | a <- actions]
  let newQ     = currentQ + qaAlpha agent * (reward + qaGamma agent * maxNextQ - currentQ)
  atomically $ modifyTVar (qaQTable agent) (Map.insert (state, action) newQ)

-- Training loop
trainAgent :: (Environment env, Ord (State env), Ord (Action env))
           => env -> QAgent (State env) (Action env) -> Int -> IO ()
trainAgent env agent episodes = forM_ [1..episodes] $ \ep -> do
  state0 <- reset env
  let acts = actions env
  go state0 0
  where
    go state step = do
      action <- chooseAction agent (actions env) state
      (nextState, reward, done) <- step env state action
      updateQ agent (actions env) state action reward nextState
      unless done (go nextState (step + 1))
```

---

## ขั้นตอนที่ 778: AI-Powered Search

```haskell
-- AI-powered semantic search

data SemanticSearch = SemanticSearch
  { ssEmbedModel :: Text -> IO Embedding
  , ssVectorDb   :: VectorDatabase
  , ssReranker   :: Maybe (Text -> [SearchResult] -> IO [SearchResult])
  }

-- Hybrid search: semantic + keyword
hybridSearch :: SemanticSearch -> Text -> HybridConfig -> IO [SearchResult]
hybridSearch ss query cfg = do
  -- Semantic search
  qEmb     <- ssEmbedModel ss query
  semanticResults <- vectorSearch (ssVectorDb ss) qEmb (hcTopK cfg)
  
  -- Keyword search (BM25)
  kwResults <- keywordSearch (ssVectorDb ss) query (hcTopK cfg)
  
  -- Merge with RRF (Reciprocal Rank Fusion)
  let merged = rrfMerge semanticResults kwResults (hcSemanticWeight cfg)
  
  -- Rerank if available
  case ssReranker ss of
    Nothing      -> return (take (hcTopK cfg) merged)
    Just reranker -> reranker query (take (hcTopK cfg) merged)

-- RRF score
rrfMerge :: [SearchResult] -> [SearchResult] -> Double -> [SearchResult]
rrfMerge semantic keyword semanticWeight =
  let k = 60  -- RRF constant
      
      semScores = Map.fromList
        [ (srId r, semanticWeight * (1 / fromIntegral (i + k)))
        | (i, r) <- zip [0..] semantic
        ]
      
      kwScores = Map.fromList
        [ (srId r, (1 - semanticWeight) * (1 / fromIntegral (i + k)))
        | (i, r) <- zip [0..] keyword
        ]
      
      combined = Map.unionWith (+) semScores kwScores
      
      allResults = nubBy (\a b -> srId a == srId b) (semantic ++ keyword)
      
  in sortBy (flip compare `on` (\r -> fromMaybe 0 (Map.lookup (srId r) combined))) allResults
```

---

## ขั้นตอนที่ 779: Causal Inference

```haskell
-- Basic causal inference tools

-- Propensity score matching
data PropensityModel = PropensityModel
  { pmPredict :: [Double] -> Double  -- P(treatment | covariates)
  }

computePropensityScores :: [[Double]] -> [Bool] -> PropensityModel
computePropensityScores covariates treatment =
  let model = trainLogisticRegression covariates treatment
  in PropensityModel { pmPredict = predict model }

-- Match treated to control units
matchUnits :: [(Double, Bool, a)] -> [(Double, Bool, a)]
matchUnits units =
  let treated  = filter (\(_, t, _) -> t) units
      control  = filter (\(_, t, _) -> not t) units
  in concat [ [t, findNearest ps control] | t@(ps, _, _) <- treated ]
  where
    findNearest ps = minimumBy (comparing (\(ps', _, _) -> abs (ps - ps')))

-- Average Treatment Effect
computeAte :: [Double] -> [Double] -> Double
computeAte treatmentOutcomes controlOutcomes =
  avg treatmentOutcomes - avg controlOutcomes
  where avg xs = sum xs / fromIntegral (length xs)

-- Difference-in-differences
did :: [Double] -> [Double] -> [Double] -> [Double] -> Double
did treatedBefore treatedAfter controlBefore controlAfter =
  let treatChange   = avg treatedAfter  - avg treatedBefore
      controlChange = avg controlAfter  - avg controlBefore
  in treatChange - controlChange
```

---

## ขั้นตอนที่ 780: โปรเจกต์: AI-Powered API

```haskell
-- Complete AI-powered API platform

data AiPlatform = AiPlatform
  { apEmbeddingService :: Text -> IO Embedding
  , apLlmService       :: ChatRequest -> IO ChatResponse
  , apVectorStore      :: VectorDatabase
  , apFeatureStore     :: FeatureStore
  , apModelRegistry    :: ModelRegistry
  }

-- AI-powered search endpoint
type SearchAPI
  = "search"
  :> ReqBody '[JSON] SearchRequest
  :> Post '[JSON] SearchResponse

searchHandler :: AiPlatform -> SearchRequest -> Handler SearchResponse
searchHandler platform req = do
  -- Embed query
  qEmb <- liftIO $ apEmbeddingService platform (srQuery req)
  
  -- Vector search
  results <- liftIO $ vectorSearch (apVectorStore platform) qEmb 10
  
  -- Generate AI summary
  summary <- if srSummarize req
    then liftIO $ generateSummary platform (srQuery req) results
    else return Nothing
  
  return SearchResponse
    { srsResults = results
    , srsSummary = summary
    , srsTook    = 0  -- timing
    }

generateSummary :: AiPlatform -> Text -> [SearchResult] -> IO Text
generateSummary platform query results = do
  let context = T.unlines (map srContent results)
  let prompt  = "Based on these search results, answer: " <> query
  
  response <- apLlmService platform ChatRequest
    { crModel       = "gpt-4"
    , crMessages    =
        [ ChatMessage' "system" "You are a helpful assistant. Answer based on the provided context."
        , ChatMessage' "user"   (prompt <> "\n\nContext:\n" <> context)
        ]
    , crTemperature = 0.3
    , crMaxTokens   = Just 500
    }
  
  return (cmContent' (choMessage (head (crChoices response))))
```

---

*[← Part 38](part-38.md) | [Part 40 →](part-40.md)*
