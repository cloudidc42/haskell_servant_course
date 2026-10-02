# Part 48: Web3 & Blockchain
## ขั้นตอนที่ 941-960

---

## ขั้นตอนที่ 941: Cryptography Fundamentals

```haskell
-- Cryptography in Haskell

import Crypto.Hash (hash, SHA256(..), Digest)
import Crypto.Sign.Ed25519
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS

-- Hash functions
sha256 :: BS.ByteString -> Digest SHA256
sha256 = hash

sha256Hex :: BS.ByteString -> Text
sha256Hex = T.pack . show . sha256

-- HMAC
import Crypto.MAC.HMAC (hmac, HMAC)
import qualified Crypto.MAC.HMAC as HMAC

hmacSHA256 :: BS.ByteString -> BS.ByteString -> HMAC SHA256
hmacSHA256 key msg = hmac key msg

-- AES encryption
encrypt :: BS.ByteString -> BS.ByteString -> Either String BS.ByteString
encrypt key plaintext = do
  ctx <- case cipherInit key :: Either CryptoError AES256 of
    Left err -> Left (show err)
    Right c  -> Right c
  let iv = nullIV :: IV AES256
  return (ctrCombine ctx iv plaintext)

-- Ed25519 signatures
signMessage :: SecretKey -> BS.ByteString -> Signature
signMessage sk msg = dsign sk msg

verifyMessage :: PublicKey -> BS.ByteString -> Signature -> Bool
verifyMessage pk msg sig = dverify pk msg sig

-- Generate keypair
newKeyPair :: IO (PublicKey, SecretKey)
newKeyPair = do
  seed <- getRandomBytes 32
  return (createKeypairFromSeed_ seed)

-- Merkle tree
data MerkleNode = MerkleLeaf BS.ByteString | MerkleBranch BS.ByteString MerkleNode MerkleNode

merkleRoot :: [BS.ByteString] -> BS.ByteString
merkleRoot [] = ""
merkleRoot [x] = convert (sha256 x)
merkleRoot xs = let mid   = length xs `div` 2
                    left  = merkleRoot (take mid xs)
                    right = merkleRoot (drop mid xs)
                in convert (sha256 (left <> right))

merkleProof :: [BS.ByteString] -> Int -> [BS.ByteString]
merkleProof [] _ = []
merkleProof [_] _ = []
merkleProof xs i = 
  let mid = length xs `div` 2
  in if i < mid
     then merkleRoot (drop mid xs) : merkleProof (take mid xs) i
     else merkleRoot (take mid xs) : merkleProof (drop mid xs) (i - mid)
```

---

## ขั้นตอนที่ 942: Blockchain Data Structures

```haskell
-- Blockchain core data structures

import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

-- Block structure
data Block = Block
  { blkIndex     :: Int
  , blkTimestamp :: POSIXTime
  , blkPrevHash  :: Text
  , blkHash      :: Text
  , blkData      :: [Transaction]
  , blkNonce     :: Int
  , blkDifficulty :: Int
  } deriving (Show)

-- Transaction
data Transaction = Tx
  { txFrom      :: Address
  , txTo        :: Address
  , txAmount    :: Amount
  , txFee       :: Amount
  , txSignature :: Signature
  , txHash      :: TxHash
  } deriving (Show)

type Address = Text
type Amount  = Integer  -- in satoshis/wei
type TxHash  = Text

-- Calculate block hash
blockHash :: Block -> Text
blockHash blk = sha256Hex . BSL.toStrict . encode $ object
  [ "index"     .= blkIndex blk
  , "timestamp" .= (floor (blkTimestamp blk) :: Int)
  , "prevHash"  .= blkPrevHash blk
  , "data"      .= blkData blk
  , "nonce"     .= blkNonce blk
  ]

-- Create genesis block
genesisBlock :: Block
genesisBlock = Block
  { blkIndex      = 0
  , blkTimestamp  = 0
  , blkPrevHash   = "0000000000000000000000000000000000000000000000000000000000000000"
  , blkHash       = ""
  , blkData       = []
  , blkNonce      = 0
  , blkDifficulty = 4
  }

-- Validate block
validateBlock :: Block -> Block -> Bool
validateBlock prevBlk blk =
  blkPrevHash blk == blkHash prevBlk &&
  blkHash blk == blockHash blk &&
  blkIndex blk == blkIndex prevBlk + 1 &&
  T.isPrefixOf (T.replicate (blkDifficulty blk) "0") (blkHash blk)
```

---

## ขั้นตอนที่ 943: Proof of Work

```haskell
-- Proof of Work mining

-- Mine a block (find valid nonce)
mineBlock :: Block -> IO Block
mineBlock blk = do
  timestamp <- getPOSIXTime
  let target = T.replicate (blkDifficulty blk) "0"
  go blk { blkTimestamp = timestamp } 0
  where
    go blk nonce = do
      let blk' = blk { blkNonce = nonce, blkHash = "" }
          hash' = blockHash blk'
      if T.isPrefixOf target hash'
        then return blk' { blkHash = hash' }
        else go blk (nonce + 1)

-- Mining with progress
mineWithProgress :: Block -> IO Block
mineWithProgress blk = do
  startTime <- getCurrentTime
  timestamp <- getPOSIXTime
  let target = T.replicate (blkDifficulty blk) "0"
      go nonce = do
        let blk' = blk { blkTimestamp = timestamp, blkNonce = nonce, blkHash = "" }
            hash' = blockHash blk'
        when (nonce `mod` 100000 == 0) $ do
          elapsed <- diffUTCTime <$> getCurrentTime <*> pure startTime
          putStrLn ("Mining... nonce=" ++ show nonce ++ " elapsed=" ++ show elapsed)
        if T.isPrefixOf target hash'
          then do
            endTime <- getCurrentTime
            let elapsed = diffUTCTime endTime startTime
            putStrLn ("Block mined! nonce=" ++ show nonce ++ " time=" ++ show elapsed)
            return blk' { blkHash = hash' }
          else go (nonce + 1)
  go 0

-- Adjust difficulty based on mining time
adjustDifficulty :: Block -> NominalDiffTime -> Int
adjustDifficulty blk elapsed
  | elapsed < 5  = blkDifficulty blk + 1  -- too fast, increase difficulty
  | elapsed > 30 = max 1 (blkDifficulty blk - 1)  -- too slow, decrease
  | otherwise    = blkDifficulty blk
```

---

## ขั้นตอนที่ 944: UTXO Model

```haskell
-- UTXO (Unspent Transaction Output) model

data UTXO = UTXO
  { utxoTxHash :: TxHash
  , utxoIndex  :: Int
  , utxoAmount :: Amount
  , utxoOwner  :: Address
  } deriving (Show, Eq, Ord)

-- UTXO set
type UTXOSet = Map (TxHash, Int) UTXO

-- Validate and process transaction
processTransaction :: UTXOSet -> Transaction -> Either TxError UTXOSet
processTransaction utxoSet tx = do
  -- Find inputs
  inputs <- forM (txInputs tx) $ \(txHash, idx) ->
    case Map.lookup (txHash, idx) utxoSet of
      Nothing   -> Left (UTXONotFound txHash idx)
      Just utxo -> Right utxo
  
  -- Verify ownership
  forM_ inputs $ \utxo ->
    unless (utxoOwner utxo == txFrom tx) (Left UnauthorizedSpend)
  
  -- Check amounts
  let totalIn  = sum (map utxoAmount inputs)
      totalOut = sum (map sndAmount (txOutputs tx)) + txFee tx
  unless (totalIn >= totalOut) (Left InsufficientFunds)
  
  -- Remove spent UTXOs, add new ones
  let spent   = Map.fromList [((txHash, idx), ()) | (txHash, idx) <- txInputs tx]
      removed = foldr Map.delete utxoSet (Map.keys spent)
      added   = Map.fromList
        [ ((txHash tx, i), UTXO (txHash tx) i amount addr)
        | (i, (addr, amount)) <- zip [0..] (txOutputs tx) ]
  return (Map.union added removed)

-- Coinbase transaction (mining reward)
coinbaseTransaction :: Address -> Amount -> TxHash -> Transaction
coinbaseTransaction miner reward blockHash = Tx
  { txFrom      = "coinbase"
  , txTo        = miner
  , txAmount    = reward
  , txFee       = 0
  , txSignature = ""
  , txHash      = sha256Hex (blockHash <> T.pack (show reward))
  }
```

---

## ขั้นตอนที่ 945: Account-Based Model (Ethereum-like)

```haskell
-- Account-based blockchain model

data AccountState = AccountState
  { acBalance  :: Integer  -- in wei
  , acNonce    :: Int      -- prevents replay attacks
  , acCode     :: Maybe ContractCode
  , acStorage  :: Map Text Text  -- contract storage
  } deriving (Show)

type WorldState = Map Address AccountState

-- EVM-like execution environment
data ExecEnv = ExecEnv
  { envFrom      :: Address
  , envTo        :: Address
  , envValue     :: Integer
  , envData      :: BS.ByteString
  , envGasLimit  :: Int
  , envGasPrice  :: Integer
  }

-- Process Ethereum-like transaction
processEthTx :: WorldState -> EthTransaction -> Either EthError WorldState
processEthTx world tx = do
  -- Get sender account
  sender <- case Map.lookup (ethFrom tx) world of
    Nothing -> Left (AccountNotFound (ethFrom tx))
    Just a  -> Right a
  
  -- Check nonce
  unless (acNonce sender == ethNonce tx) (Left (InvalidNonce (acNonce sender) (ethNonce tx)))
  
  -- Check balance (value + gas cost)
  let gasCost = toInteger (ethGasLimit tx) * ethGasPrice tx
      totalCost = ethValue tx + gasCost
  unless (acBalance sender >= totalCost) (Left InsufficientBalance)
  
  -- Deduct from sender
  let sender' = sender
        { acBalance = acBalance sender - totalCost
        , acNonce   = acNonce sender + 1
        }
  
  -- Credit receiver
  let receiver = fromMaybe (AccountState 0 0 Nothing Map.empty) (Map.lookup (ethTo tx) world)
      receiver' = receiver { acBalance = acBalance receiver + ethValue tx }
  
  return $ Map.insert (ethFrom tx) sender'
         $ Map.insert (ethTo tx)   receiver' world
```

---

## ขั้นตอนที่ 946: Smart Contract VM

```haskell
-- Simple smart contract VM (EVM-like)

data Opcode
  = PUSH Int   -- push value onto stack
  | POP        -- pop from stack
  | ADD | SUB | MUL | DIV | MOD
  | EQ | LT | GT | NOT
  | JUMP Int   -- unconditional jump
  | JUMPI Int  -- conditional jump
  | MLOAD      -- memory load
  | MSTORE     -- memory store
  | SLOAD      -- storage load
  | SSTORE     -- storage store
  | CALL       -- call another contract
  | RETURN     -- return from execution
  | REVERT     -- revert state changes
  | LOG        -- emit event
  deriving (Show)

data VMState = VMState
  { vmStack    :: [Int]
  , vmMemory   :: Map Int Int
  , vmStorage  :: Map Text Int
  , vmPC       :: Int      -- program counter
  , vmGas      :: Int
  , vmLogs     :: [LogEntry]
  }

data VMResult
  = VMSuccess Int      -- result value
  | VMRevert Text      -- revert with message
  | VMOutOfGas

-- Execute one instruction
execOpcode :: [Opcode] -> VMState -> Either VMResult VMState
execOpcode ops vm = case ops !! vmPC vm of
  PUSH n  -> right (push n)
  POP     -> right (pop . snd . pop')
  ADD     -> binaryOp (+)
  SUB     -> binaryOp (-)
  MUL     -> binaryOp (*)
  DIV     -> binaryOp div
  EQ      -> binaryOp (\a b -> if a == b then 1 else 0)
  JUMP n  -> right (setPC n)
  JUMPI n -> let (cond, vm') = pop' vm in if cond /= 0 then right (setPC n) else right id
  RETURN  -> let (v, _) = pop' vm in Left (VMSuccess v)
  REVERT  -> Left (VMRevert "Reverted")
  _       -> right id
  where
    right f = Right (f vm { vmPC = vmPC vm + 1, vmGas = vmGas vm - 1 })
    pop' v  = (head (vmStack v), v { vmStack = tail (vmStack v) })
    push n v = v { vmStack = n : vmStack v }
    pop v    = v { vmStack = tail (vmStack v) }
    setPC n v = v { vmPC = n }
    binaryOp op = let (a, vm')  = pop' vm
                      (b, vm'') = pop' vm'
                  in right (\v -> push (op a b) v) vm''
```

---

## ขั้นตอนที่ 947: P2P Network

```haskell
-- P2P network for blockchain

import Network.Socket
import Control.Concurrent.STM

data Peer = Peer
  { peerAddr :: (String, Int)  -- host, port
  , peerConn :: Maybe Handle
  }

data Node = Node
  { nodePeers   :: TVar [Peer]
  , nodeBlocks  :: TVar Blockchain
  , nodeTxPool  :: TVar [Transaction]
  , nodePort    :: Int
  }

-- Message types
data P2PMessage
  = MsgVersion { msgVersion :: Int, msgHeight :: Int }
  | MsgGetBlocks { msgFrom :: Int }
  | MsgBlocks { msgBlocks :: [Block] }
  | MsgTransaction Transaction
  | MsgPing
  | MsgPong
  deriving (Generic, FromJSON, ToJSON)

-- Start P2P listener
startListener :: Node -> IO ()
startListener node = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet (fromIntegral (nodePort node)) (tupleToHostAddress (127,0,0,1)))
  listen sock 10
  forever $ do
    (conn, addr) <- accept sock
    forkIO (handleConnection node conn addr)

-- Handle incoming connection
handleConnection :: Node -> Socket -> SockAddr -> IO ()
handleConnection node sock addr = do
  handle <- socketToHandle sock ReadWriteMode
  peer   <- newPeer (show addr, 0) (Just handle)
  addPeer node peer
  
  -- Message loop
  let loop = do
        line <- BSL.hGetLine handle
        case decode line of
          Nothing  -> return ()
          Just msg -> do
            handleMessage node peer msg
            loop
  loop `finally` removePeer node peer

handleMessage :: Node -> Peer -> P2PMessage -> IO ()
handleMessage node peer msg = case msg of
  MsgVersion _ height -> do
    ourHeight <- length <$> readTVarIO (nodeBlocks node)
    when (height > ourHeight) $ sendMessage peer (MsgGetBlocks ourHeight)
  
  MsgGetBlocks from -> do
    blocks <- drop from <$> readTVarIO (nodeBlocks node)
    sendMessage peer (MsgBlocks blocks)
  
  MsgBlocks blocks -> do
    ourChain <- readTVarIO (nodeBlocks node)
    let newChain = ourChain ++ blocks
    when (isValidChain newChain) $
      atomically (writeTVar (nodeBlocks node) newChain)
  
  MsgTransaction tx -> do
    atomically (modifyTVar' (nodeTxPool node) (tx:))
    broadcastTx node tx peer
  
  MsgPing -> sendMessage peer MsgPong
  MsgPong -> return ()

-- Broadcast to all peers except sender
broadcastTx :: Node -> Transaction -> Peer -> IO ()
broadcastTx node tx except = do
  peers <- readTVarIO (nodePeers node)
  forM_ (filter (/= except) peers) (`sendMessage` MsgTransaction tx)
```

---

## ขั้นตอนที่ 948: Consensus Mechanisms

```haskell
-- Various consensus mechanisms

-- Proof of Stake
data Validator = Validator
  { valAddress :: Address
  , valStake   :: Integer
  , valPubKey  :: PublicKey
} deriving (Show)

-- Weighted random selection for PoS
selectValidator :: [Validator] -> IO Validator
selectValidator validators = do
  let totalStake = sum (map valStake validators)
      weighted   = [(v, valStake v) | v <- validators]
  r <- randomRIO (0, totalStake - 1)
  return (go r weighted)
  where
    go _ [v]   = fst v
    go r ((v, s):rest)
      | r < s    = v
      | otherwise = go (r - s) rest
    go _ [] = error "empty validators"

-- BFT-style consensus (simplified PBFT)
data PBFTMsg
  = Preprepare { ppView :: Int, ppSeq :: Int, ppBlock :: Block }
  | Prepare    { prView :: Int, prSeq :: Int, prHash :: Text, prFrom :: Address }
  | Commit     { cmView :: Int, cmSeq :: Int, cmHash :: Text, cmFrom :: Address }

data PBFTState = PBFTState
  { pbftView      :: Int
  , pbftSeq       :: Int
  , pbftPrepares  :: Map (Int, Int) (Set Address)
  , pbftCommits   :: Map (Int, Int) (Set Address)
  , pbftCommitted :: Set (Int, Int)
  }

-- 2f+1 messages needed for f faults
quorumSize :: Int -> Int
quorumSize n = 2 * (n `div` 3) + 1

processMsg :: Int -> PBFTState -> PBFTMsg -> (PBFTState, Maybe Block)
processMsg n state msg = case msg of
  Preprepare v s blk ->
    (state, Nothing)  -- broadcast Prepare
    
  Prepare v s h from ->
    let prepares = Map.insertWith Set.union (v, s) (Set.singleton from) (pbftPrepares state)
        count    = Set.size (fromMaybe Set.empty (Map.lookup (v, s) prepares))
    in if count >= quorumSize n
       then (state { pbftPrepares = prepares }, Nothing)  -- broadcast Commit
       else (state { pbftPrepares = prepares }, Nothing)
  
  Commit v s h from ->
    let commits = Map.insertWith Set.union (v, s) (Set.singleton from) (pbftCommits state)
        count   = Set.size (fromMaybe Set.empty (Map.lookup (v, s) commits))
    in if count >= quorumSize n && (v, s) `Set.notMember` pbftCommitted state
       then (state { pbftCommits = commits, pbftCommitted = Set.insert (v, s) (pbftCommitted state) }, Just undefined)
       else (state { pbftCommits = commits }, Nothing)
```

---

## ขั้นตอนที่ 949: Token Standards

```haskell
-- ERC-20 like token implementation

-- Token state
data TokenState = TokenState
  { tksBalances   :: Map Address Integer
  , tksAllowances :: Map (Address, Address) Integer
  , tksTotalSupply :: Integer
  , tksName       :: Text
  , tksSymbol     :: Text
  , tksDecimals   :: Int
}

-- Token contract interface
class Token t where
  balanceOf   :: t -> Address -> Integer
  transfer    :: t -> Address -> Address -> Integer -> Either TokenError t
  approve     :: t -> Address -> Address -> Integer -> Either TokenError t
  transferFrom :: t -> Address -> Address -> Address -> Integer -> Either TokenError t
  allowance   :: t -> Address -> Address -> Integer

-- ERC-20 implementation
instance Token TokenState where
  balanceOf ts addr = fromMaybe 0 (Map.lookup addr (tksBalances ts))
  
  transfer ts from to amount = do
    let fromBal = balanceOf ts from
    unless (fromBal >= amount) (Left InsufficientBalance)
    let ts' = ts
          { tksBalances = Map.insert from (fromBal - amount)
                        $ Map.insertWith (+) to amount
                        $ tksBalances ts }
    return ts'
  
  approve ts owner spender amount = Right ts
    { tksAllowances = Map.insert (owner, spender) amount (tksAllowances ts) }
  
  allowance ts owner spender =
    fromMaybe 0 (Map.lookup (owner, spender) (tksAllowances ts))
  
  transferFrom ts caller from to amount = do
    let allowed = allowance ts from caller
    unless (allowed >= amount) (Left InsufficientAllowance)
    ts' <- transfer ts from to amount
    return ts'
      { tksAllowances = Map.insert (from, caller) (allowed - amount) (tksAllowances ts') }

-- NFT (ERC-721 like)
data NFTState = NFTState
  { nftOwners   :: Map Int Address      -- tokenId -> owner
  , nftApproved :: Map Int Address      -- tokenId -> approved address
  , nftForAll   :: Map (Address, Address) Bool  -- owner -> operator -> approved
  }

nftOwnerOf :: NFTState -> Int -> Maybe Address
nftOwnerOf nft tokenId = Map.lookup tokenId (nftOwners nft)

nftTransfer :: NFTState -> Address -> Address -> Int -> Either NFTError NFTState
nftTransfer nft from to tokenId = do
  owner <- maybe (Left TokenNotFound) Right (nftOwnerOf nft tokenId)
  unless (owner == from) (Left NotOwner)
  return nft { nftOwners = Map.insert tokenId to (nftOwners nft) }
```

---

## ขั้นตอนที่ 950: DeFi Protocols

```haskell
-- DeFi: Automated Market Maker (Uniswap-like AMM)

-- Liquidity pool state
data Pool = Pool
  { poolReserve0 :: Integer  -- token0 amount
  , poolReserve1 :: Integer  -- token1 amount
  , poolLPTokens :: Map Address Integer  -- LP token balances
  , poolFee      :: Double   -- e.g., 0.003 for 0.3%
}

-- Constant product formula: x * y = k
poolK :: Pool -> Integer
poolK pool = poolReserve0 pool * poolReserve1 pool

-- Calculate amount out for swap
getAmountOut :: Pool -> Bool -> Integer -> Integer
getAmountOut pool zeroForOne amountIn =
  let k          = poolK pool
      feeAmount  = floor (fromIntegral amountIn * poolFee pool)
      netIn      = amountIn - feeAmount
      (resIn, resOut) = if zeroForOne
                        then (poolReserve0 pool, poolReserve1 pool)
                        else (poolReserve1 pool, poolReserve0 pool)
      newResIn   = resIn + netIn
      newResOut  = k `div` newResIn
  in resOut - newResOut

-- Execute swap
swap :: Pool -> Address -> Bool -> Integer -> Integer -> Either DeFiError (Pool, Integer)
swap pool user zeroForOne amountIn minAmountOut = do
  let amountOut = getAmountOut pool zeroForOne amountIn
  unless (amountOut >= minAmountOut) (Left Slippage)
  
  let pool' = if zeroForOne
              then pool { poolReserve0 = poolReserve0 pool + amountIn
                        , poolReserve1 = poolReserve1 pool - amountOut }
              else pool { poolReserve1 = poolReserve1 pool + amountIn
                        , poolReserve0 = poolReserve0 pool - amountOut }
  return (pool', amountOut)

-- Add liquidity
addLiquidity :: Pool -> Address -> Integer -> Integer -> Either DeFiError (Pool, Integer)
addLiquidity pool provider amount0 amount1 = do
  let totalLP = sum (Map.elems (poolLPTokens pool))
      lpMinted = if totalLP == 0
                 then floor (sqrt (fromIntegral (amount0 * amount1) :: Double))
                 else min (amount0 * totalLP `div` poolReserve0 pool)
                          (amount1 * totalLP `div` poolReserve1 pool)
  
  let pool' = pool
        { poolReserve0 = poolReserve0 pool + amount0
        , poolReserve1 = poolReserve1 pool + amount1
        , poolLPTokens = Map.insertWith (+) provider lpMinted (poolLPTokens pool)
        }
  return (pool', lpMinted)
```

---

## ขั้นตอนที่ 951: Zero-Knowledge Proofs (Simplified)

```haskell
-- Zero-Knowledge proof concepts

-- Simple ZK proof for knowledge of discrete logarithm
-- Prove knowledge of x such that g^x = h (mod p)

data ZKProof = ZKProof
  { zkCommitment :: Integer  -- r = g^k mod p
  , zkResponse   :: Integer  -- s = k - cx mod q
  }

-- Schnorr protocol
prove :: Integer -> Integer -> Integer -> Integer -> Integer -> IO ZKProof
prove p q g x h = do
  k <- randomRIO (1, q-1)  -- random nonce
  let r  = modPow g k p            -- commitment
      c  = schnorrChallenge p q g h r  -- challenge (hash)
      s  = (k - c * x) `mod` q      -- response
  return (ZKProof r s)

verify :: Integer -> Integer -> Integer -> Integer -> ZKProof -> Bool
verify p q g h (ZKProof r s) =
  let c  = schnorrChallenge p q g h r
      lhs = modPow g s p * modPow h c p `mod` p
  in lhs == r

schnorrChallenge :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
schnorrChallenge p q g h r =
  let bytes = encode (p, q, g, h, r)
  in fromIntegral (fromBSL (BSL.take 8 (sha256Bytes bytes))) `mod` q

-- Pedersen commitment (hiding + binding)
data Commitment = Commitment Integer

commit :: Integer -> Integer -> Integer -> Integer -> Integer -> Commitment
commit p g h value randomness =
  Commitment (modPow g value p * modPow h randomness p `mod` p)

openCommitment :: Integer -> Integer -> Integer -> Commitment -> Integer -> Integer -> Bool
openCommitment p g h (Commitment c) value randomness =
  c == (modPow g value p * modPow h randomness p `mod` p)
```

---

## ขั้นตอนที่ 952: Wallet Implementation

```haskell
-- HD Wallet implementation (BIP-32 inspired)

import Crypto.Hash (RIPEMD160(..), SHA256(..))
import Data.Bits (xor)

-- Key derivation
data HDKey = HDKey
  { hdPrivKey  :: BS.ByteString   -- 32 bytes
  , hdPubKey   :: BS.ByteString   -- 33 bytes (compressed)
  , hdChainCode :: BS.ByteString  -- 32 bytes
  , hdDepth    :: Int
  , hdIndex    :: Int
  }

-- Derive child key
deriveChild :: HDKey -> Int -> HDKey
deriveChild parent index =
  let data' = BS.concat
        [ hdPubKey parent
        , encode32BE index
        ]
      hmacResult = hmacSHA512 (hdChainCode parent) data'
      (il, ir)   = BS.splitAt 32 hmacResult
      childPriv  = (parseBigNum il + parseBigNum (hdPrivKey parent)) `mod` secp256k1N
      childPub   = pointMultiply childPriv secp256k1G
  in HDKey
     { hdPrivKey  = encodeBigNum childPriv
     , hdPubKey   = compressPoint childPub
     , hdChainCode = ir
     , hdDepth    = hdDepth parent + 1
     , hdIndex    = index
     }

-- Derive path (e.g., m/44'/0'/0')
derivePath :: HDKey -> [Int] -> HDKey
derivePath = foldl' deriveChild

-- Bitcoin address from public key
pubKeyToAddress :: BS.ByteString -> Text
pubKeyToAddress pubKey =
  let sha256Hash  = convert (hash pubKey :: Digest SHA256)
      ripemd      = convert (hash sha256Hash :: Digest RIPEMD160)
      withPrefix  = "\x00" <> ripemd
      checksum    = BS.take 4 (doubleHash withPrefix)
      payload     = withPrefix <> checksum
  in base58Check payload

-- Mnemonic seed phrase (BIP-39 inspired)
mnemonicToSeed :: [Text] -> BS.ByteString
mnemonicToSeed words = pbkdf2HMACSHA512 (T.encodeUtf8 (T.unwords words)) "mnemonic" 2048 64
```

---

## ขั้นตอนที่ 953: Block Explorer API

```haskell
-- Block explorer REST API using Servant

type BlockchainAPI
  = "blocks" :> Get '[JSON] [BlockSummary]
  :<|> "blocks" :> Capture "hash" Text :> Get '[JSON] BlockDetail
  :<|> "blocks" :> "height" :> Capture "height" Int :> Get '[JSON] BlockDetail
  :<|> "transactions" :> Capture "txhash" Text :> Get '[JSON] TxDetail
  :<|> "address" :> Capture "address" Text :> Get '[JSON] AddressInfo
  :<|> "mempool" :> Get '[JSON] [TxSummary]
  :<|> "search" :> QueryParam "q" Text :> Get '[JSON] SearchResult

data BlockSummary = BlockSummary
  { bsHash      :: Text
  , bsHeight    :: Int
  , bsTimestamp :: UTCTime
  , bsTxCount   :: Int
  , bsSize      :: Int
} deriving (Generic, ToJSON)

data BlockDetail = BlockDetail
  { bdHeader       :: BlockSummary
  , bdTransactions :: [TxSummary]
  , bdDifficulty   :: Int
  , bdMiner        :: Address
  , bdReward       :: Integer
} deriving (Generic, ToJSON)

data AddressInfo = AddressInfo
  { aiAddress   :: Address
  , aiBalance   :: Integer
  , aiTxCount   :: Int
  , aiFirstSeen :: Maybe UTCTime
  , aiLastSeen  :: Maybe UTCTime
  , aiTxHistory :: [TxSummary]
} deriving (Generic, ToJSON)

-- Handlers
blockchainServer :: BlockchainState -> Server BlockchainAPI
blockchainServer state =
  getBlocks state
  :<|> getBlock state
  :<|> getBlockByHeight state
  :<|> getTransaction state
  :<|> getAddress state
  :<|> getMempool state
  :<|> searchBlockchain state

getBlocks :: BlockchainState -> Handler [BlockSummary]
getBlocks state = do
  blocks <- liftIO (readTVarIO (bsChain state))
  return (map blockToSummary (take 20 (reverse blocks)))
```

---

## ขั้นตอนที่ 954: Oracle & Cross-Chain

```haskell
-- Blockchain oracle for external data

data Oracle = Oracle
  { oracleId     :: Text
  , oracleSource :: Text  -- data source URL
  , oracleMethod :: Text  -- GET, POST, etc.
  , oraclePath   :: [Text]  -- JSON path to extract
}

data OracleReport = OracleReport
  { orValue     :: Value
  , orTimestamp :: UTCTime
  , orSignature :: Signature
  , orOracle    :: Text
}

-- Multi-oracle aggregation
aggregateOracles :: [OracleReport] -> IO (Maybe Value)
aggregateOracles reports = do
  let n    = length reports
      values = mapMaybe (parseNumber . orValue) reports
  if null values
    then return Nothing
    else do
      let sorted  = sort values
          median  = sorted !! (n `div` 2)  -- median aggregation
      return (Just (Number (fromFloatDigits median)))

-- Cross-chain bridge (simplified)
data BridgeEvent
  = LockTokens     Address Integer Text  -- from, amount, targetChain
  | ReleaseTokens  Address Integer       -- to, amount
  | BridgeProof    Text BS.ByteString    -- txHash, merkleProof

-- Bridge validator
validateBridgeProof :: BridgeEvent -> MerkleProof -> SourceChain -> Bool
validateBridgeProof (ReleaseTokens _ _) proof sourceChain =
  let root = scMerkleRoot sourceChain
  in verifyMerkleProof root (bridgeLeaf proof) (merkleProofPath proof)
validateBridgeProof _ _ _ = False

-- Atomic swap
data AtomicSwap = AtomicSwap
  { asHashlock   :: Text     -- hash of secret
  , asTimelock   :: UTCTime  -- expiry
  , asInitiator  :: Address
  , asParticipant :: Address
  , asAmount     :: Integer
}

initiateSwap :: Text -> UTCTime -> Address -> Integer -> IO AtomicSwap
initiateSwap secret expiry participant amount = do
  let hashlock = sha256Hex (T.encodeUtf8 secret)
  initiator <- getMyAddress
  return (AtomicSwap hashlock expiry initiator participant amount)
```

---

## ขั้นตอนที่ 955: IPFS Integration

```haskell
-- IPFS (InterPlanetary File System) integration

import Network.HTTP.Client
import Network.HTTP.Client.TLS

data IPFSClient = IPFSClient
  { ipfsManager :: Manager
  , ipfsApiUrl  :: Text
}

newIPFSClient :: Text -> IO IPFSClient
newIPFSClient apiUrl = do
  mgr <- newManager tlsManagerSettings
  return (IPFSClient mgr apiUrl)

-- Add content to IPFS
ipfsAdd :: IPFSClient -> BS.ByteString -> IO (Either Text CID)
ipfsAdd client content = do
  let url = T.unpack (ipfsApiUrl client) ++ "/api/v0/add"
  request <- parseRequest url
  let req = request
        { method = "POST"
        , requestBody = RequestBodyBS content
        }
  resp <- httpLbs req (ipfsManager client)
  case eitherDecode (responseBody resp) of
    Left err   -> return (Left (T.pack err))
    Right json -> return (Right (json ^. key "Hash" . _String))

-- Get content from IPFS
ipfsGet :: IPFSClient -> CID -> IO (Either Text BS.ByteString)
ipfsGet client cid = do
  let url = T.unpack (ipfsApiUrl client) ++ "/api/v0/cat?arg=" ++ T.unpack cid
  request <- parseRequest url
  resp <- httpLbs request (ipfsManager client)
  return (Right (BSL.toStrict (responseBody resp)))

-- Pin content
ipfsPin :: IPFSClient -> CID -> IO (Either Text ())
ipfsPin client cid = do
  let url = T.unpack (ipfsApiUrl client) ++ "/api/v0/pin/add?arg=" ++ T.unpack cid
  request <- parseRequest url
  resp <- httpLbs (request { method = "POST" }) (ipfsManager client)
  return (if statusCode (responseStatus resp) == 200 then Right () else Left "Pin failed")

-- Store NFT metadata on IPFS
storeNFTMetadata :: IPFSClient -> NFTMetadata -> IO (Either Text CID)
storeNFTMetadata client meta = ipfsAdd client (BSL.toStrict (encode meta))

data NFTMetadata = NFTMetadata
  { nftName        :: Text
  , nftDescription :: Text
  , nftImage       :: Text  -- IPFS CID of image
  , nftAttributes  :: [Attribute]
} deriving (Generic, ToJSON, FromJSON)
```

---

## ขั้นตอนที่ 956: DID (Decentralized Identity)

```haskell
-- Decentralized Identifiers (DID)

data DID = DID
  { didMethod     :: Text   -- "ethr", "key", etc.
  , didIdentifier :: Text   -- method-specific identifier
} deriving (Show, Eq)

-- Parse DID string: "did:ethr:0x1234..."
parseDID :: Text -> Maybe DID
parseDID s = case T.splitOn ":" s of
  ["did", method, identifier] -> Just (DID method identifier)
  _                           -> Nothing

-- DID Document
data DIDDocument = DIDDocument
  { didDocId             :: DID
  , didDocController     :: [DID]
  , didDocVerification   :: [VerificationMethod]
  , didDocAuthentication :: [Text]  -- refs to verification methods
  , didDocService        :: [ServiceEndpoint]
}

data VerificationMethod = VM
  { vmId           :: Text
  , vmType         :: Text   -- "Ed25519VerificationKey2020"
  , vmController   :: DID
  , vmPublicKeyHex :: Text
}

data ServiceEndpoint = SE
  { seId      :: Text
  , seType    :: Text    -- "LinkedDomains", "DIDCommMessaging"
  , seEndpoint :: Text
}

-- Resolve DID document
resolveDID :: DID -> IO (Maybe DIDDocument)
resolveDID did = case didMethod did of
  "key"  -> Just <$> resolveKeyDID did
  "ethr" -> resolveEthrDID did
  _      -> return Nothing

resolveKeyDID :: DID -> IO DIDDocument
resolveKeyDID did = do
  let pubKeyHex = didIdentifier did
      vmId      = "did:key:" <> pubKeyHex <> "#" <> pubKeyHex
  return DIDDocument
    { didDocId           = did
    , didDocController   = [did]
    , didDocVerification = [VM vmId "Ed25519VerificationKey2020" did pubKeyHex]
    , didDocAuthentication = [vmId]
    , didDocService      = []
    }

-- Verifiable Credential
data VC = VC
  { vcContext     :: [Text]
  , vcType        :: [Text]
  , vcIssuer      :: DID
  , vcSubject     :: DID
  , vcIssuanceDate :: UTCTime
  , vcClaims      :: Map Text Value
  , vcProof       :: VCProof
} deriving (Generic, ToJSON, FromJSON)
```

---

## ขั้นตอนที่ 957: Layer 2 Scaling

```haskell
-- Layer 2 scaling solutions

-- State channels
data StateChannel = StateChannel
  { scParties     :: [Address]
  , scBalances    :: Map Address Integer
  , scNonce       :: Int
  , scTimeout     :: UTCTime
  , scSignatures  :: [Signature]
}

-- Off-chain state update
updateChannel :: StateChannel -> Map Address Integer -> [Signature] -> Either ChannelError StateChannel
updateChannel sc newBalances sigs = do
  -- Verify signatures from all parties
  forM_ (scParties sc) $ \party ->
    case find (\s -> verifyMessage (partyPubKey party) (channelMsg sc newBalances) s) sigs of
      Nothing -> Left (MissingSignature party)
      Just _  -> Right ()
  
  -- Check balance conservation
  let oldTotal = sum (Map.elems (scBalances sc))
      newTotal = sum (Map.elems newBalances)
  unless (oldTotal == newTotal) (Left BalanceMismatch)
  
  return sc { scBalances = newBalances, scNonce = scNonce sc + 1, scSignatures = sigs }

-- Optimistic rollup (simplified)
data RollupBatch = RollupBatch
  { rbTxs         :: [Transaction]
  , rbStateRoot   :: Text
  , rbBatchNumber :: Int
  , rbProposer    :: Address
  , rbTimestamp   :: UTCTime
}

data RollupState = RollupState
  { rsCurrentBatch   :: Int
  , rsBatches        :: Map Int RollupBatch
  , rsChallengePeriod :: NominalDiffTime  -- fraud proof window
  , rsFinalizedBatch :: Int
}

-- Submit batch to L1
submitBatch :: RollupState -> RollupBatch -> IO (Either RollupError Text)
submitBatch state batch = do
  -- Submit compressed transaction data to L1
  let batchData = compressTxData (rbTxs batch)
  txHash <- submitToL1 batchData
  return (Right txHash)

-- Challenge batch (fraud proof)
challengeBatch :: RollupState -> Int -> Int -> BS.ByteString -> IO (Either RollupError ())
challengeBatch state batchNum txIndex fraudProof = do
  batch <- maybe (return (Left BatchNotFound)) Right
           (Map.lookup batchNum (rsBatches state))
  verifyFraudProof batch txIndex fraudProof
```

---

## ขั้นตอนที่ 958: DAO Governance

```haskell
-- DAO (Decentralized Autonomous Organization) governance

data Proposal = Proposal
  { propId         :: Int
  , propTitle      :: Text
  , propDescription :: Text
  , propActions    :: [GovernanceAction]
  , propProposer   :: Address
  , propStartBlock :: Int
  , propEndBlock   :: Int
  , propStatus     :: ProposalStatus
}

data ProposalStatus = Pending | Active | Succeeded | Defeated | Executed | Cancelled
  deriving (Show, Eq)

data GovernanceAction
  = TransferFunds Address Integer
  | UpdateParam Text Value
  | UpgradeContract Text Text  -- contract name, new implementation
  | CallContract Address BS.ByteString

data Vote = Vote
  { voteProposalId :: Int
  , voteVoter      :: Address
  , voteSupport    :: VoteSupport
  , voteWeight     :: Integer  -- token balance at snapshot
  , voteReason     :: Maybe Text
  , voteTimestamp  :: UTCTime
}

data VoteSupport = Against | For | Abstain deriving (Show, Eq)

-- DAO state
data DAOState = DAOState
  { daoProposals     :: Map Int Proposal
  , daoVotes         :: Map Int [Vote]  -- proposalId -> votes
  , daoTokenBalances :: Map Address Integer
  , daoQuorum        :: Double    -- required participation %
  , daoThreshold     :: Double    -- required approval %
}

-- Check proposal result
proposalResult :: DAOState -> Int -> Maybe ProposalStatus
proposalResult dao propId = do
  prop  <- Map.lookup propId (daoProposals dao)
  votes <- Map.lookup propId (daoVotes dao)
  let forVotes      = sum [voteWeight v | v <- votes, voteSupport v == For]
      againstVotes  = sum [voteWeight v | v <- votes, voteSupport v == Against]
      totalVotes    = sum (map voteWeight votes)
      totalSupply   = sum (Map.elems (daoTokenBalances dao))
      participation = fromIntegral totalVotes / fromIntegral totalSupply :: Double
      forRatio      = fromIntegral forVotes / fromIntegral totalVotes :: Double
  if participation < daoQuorum dao
    then return Defeated
    else if forRatio >= daoThreshold dao
         then return Succeeded
         else return Defeated
```

---

## ขั้นตอนที่ 959: MEV & Flash Loans

```haskell
-- MEV (Miner Extractable Value) and Flash Loans

-- Flash loan interface
data FlashLoan = FlashLoan
  { flToken    :: Address
  , flAmount   :: Integer
  , flFee      :: Integer  -- fee = amount * fee_rate
  , flBorrower :: Address
}

-- Execute flash loan atomically
executeFlashLoan :: FlashLoan -> (Integer -> IO ()) -> IO ()
executeFlashLoan loan action = do
  -- 1. Lend tokens
  transferTokens (flToken loan) lenderAddress (flBorrower loan) (flAmount loan)
  
  -- 2. Execute borrower's action (atomically)
  action (flAmount loan)
  
  -- 3. Check repayment (atomically reverting if not)
  balance <- tokenBalance (flToken loan) (flBorrower loan)
  let required = flAmount loan + flFee loan
  unless (balance >= required) (rollback "Flash loan not repaid")
  
  -- 4. Collect repayment
  transferTokens (flToken loan) (flBorrower loan) lenderAddress required

-- Arbitrage using flash loans
arbitrage :: FlashLoan -> Pool -> Pool -> IO ()
arbitrage loan pool1 pool2 = executeFlashLoan loan $ \amount -> do
  -- Buy token1 on pool1 (cheaper)
  let amountOut1 = getAmountOut pool1 True amount
  executeSwap pool1 True amount
  
  -- Sell token1 on pool2 (more expensive)
  let amountOut2 = getAmountOut pool2 False amountOut1
  executeSwap pool2 False amountOut1
  
  -- Keep profit, repay loan
  let profit = amountOut2 - amount - flFee loan
  when (profit <= 0) (rollback "No profit")

-- Sandwich attack detection (for MEV protection)
detectSandwich :: [Transaction] -> [[Transaction]]
detectSandwich txs =
  [ [front, victim, back]
  | front  <- txs
  , victim <- txs
  , back   <- txs
  , isBuy  front && isBuy victim && isSell back
  , txFrom front == txFrom back
  , txFrom front /= txFrom victim
  ]
```

---

## ขั้นตอนที่ 960: โปรเจกต์: Full Blockchain Node

```haskell
-- Complete blockchain node implementation

module BlockchainNode where

data NodeConfig = NodeConfig
  { ncPort        :: Int
  , ncDataDir     :: FilePath
  , ncBootPeers   :: [(String, Int)]
  , ncMiningAddr  :: Maybe Address
  , ncDifficulty  :: Int
  , ncNetworkId   :: Int
}

data FullNode = FullNode
  { fnChain       :: TVar Blockchain
  , fnMempool     :: TVar (Map TxHash Transaction)
  , fnUTXOSet     :: TVar UTXOSet
  , fnPeers       :: TVar [Peer]
  , fnStorage     :: Storage
  , fnEventBus    :: EventBus
  , fnConfig      :: NodeConfig
}

-- Initialize and start node
startNode :: NodeConfig -> IO FullNode
startNode cfg = do
  -- Load or create chain
  chain <- loadOrCreateChain (ncDataDir cfg)
  
  -- Initialize state
  chainVar  <- newTVarIO chain
  mempoolVar <- newTVarIO Map.empty
  utxoVar   <- newTVarIO (buildUTXOSet chain)
  peersVar  <- newTVarIO []
  storage   <- openStorage (ncDataDir cfg)
  eventBus  <- newEventBus
  
  let node = FullNode chainVar mempoolVar utxoVar peersVar storage eventBus cfg
  
  -- Start services
  startP2PServer node
  startP2PDiscovery node (ncBootPeers cfg)
  startRPCServer node
  whenJust (ncMiningAddr cfg) (startMiner node)
  startMemPoolMonitor node
  
  logInfo "Node started"
  return node

-- Mining loop
startMiner :: FullNode -> Address -> IO ()
startMiner node minerAddr = forkIO_ $ forever $ do
  chain   <- readTVarIO (fnChain node)
  txs     <- getBestTxs (fnMempool node) 100
  let prevBlock  = last chain
      newBlock   = Block
        { blkIndex      = length chain
        , blkPrevHash   = blkHash prevBlock
        , blkData       = txs
        , blkDifficulty = ncDifficulty (fnConfig node)
        , blkTimestamp  = 0  -- will be set in mineBlock
        , blkNonce      = 0
        , blkHash       = ""
        }
  mined <- mineBlock newBlock
  
  -- Try to add to chain
  atomically $ do
    c <- readTVar (fnChain node)
    when (validateBlock (last c) mined) $ do
      writeTVar (fnChain node) (c ++ [mined])
  
  -- Remove mined txs from mempool
  atomically $ modifyTVar' (fnMempool node)
    (\m -> foldr Map.delete m (map txHash txs))
  
  emit (fnEventBus node) "block.mined" (toJSON mined)
```

---

*[← Part 47](part-47.md) | [Part 49 →](part-49.md)*
