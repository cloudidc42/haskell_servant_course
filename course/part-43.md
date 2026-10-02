# Part 43: Formal Verification & Liquid Haskell
## ขั้นตอนที่ 841-860

---

## ขั้นตอนที่ 841: Liquid Haskell Basics

```haskell
-- Liquid Haskell: types with logical predicates

{-@ LIQUID "--exact-data-cons" @-}

-- Refined types: Int ที่ต้องเป็น positive
{-@ type Positive = { v:Int | v > 0 } @-}
{-@ type NonNeg   = { v:Int | v >= 0 } @-}
{-@ type Between lo hi = { v:Int | lo <= v && v <= hi } @-}

-- Function ที่ verified safe
{-@ safeDiv :: Int -> Positive -> Int @-}
safeDiv :: Int -> Int -> Int
safeDiv x y = x `div` y  -- y guaranteed non-zero by type

-- List length refinement
{-@ type ListN a N = { v:[a] | len v == N } @-}
{-@ type NonEmpty a = { v:[a] | len v > 0 } @-}

{-@ safeHead :: NonEmpty a -> a @-}
safeHead :: [a] -> a
safeHead (x:_) = x
safeHead []    = error "impossible"  -- LH proves this unreachable

-- Bounded index
{-@ safeIndex :: xs:[a] -> { i:Int | 0 <= i && i < len xs } -> a @-}
safeIndex :: [a] -> Int -> a
safeIndex (x:_)  0 = x
safeIndex (_:xs) n = safeIndex xs (n-1)
safeIndex []     _ = error "impossible"
```

---

## ขั้นตอนที่ 842: Refinement Types for Data Structures

```haskell
-- Verified data structures with LH

-- Sorted list invariant
{-@ type Sorted = [Int]<{ \x y -> x <= y }> @-}

{-@ merge :: Sorted -> Sorted -> Sorted @-}
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- BST invariant
{-@ data BST a = Leaf
               | Node { val  :: a
                      , left  :: BST { v:a | v < val }
                      , right :: BST { v:a | v > val } }
@-}
data BST a = Leaf | Node a (BST a) (BST a)

{-@ bstInsert :: Ord a => a -> BST a -> BST a @-}
bstInsert :: Ord a => a -> BST a -> BST a
bstInsert x Leaf = Node x Leaf Leaf
bstInsert x (Node v l r)
  | x < v    = Node v (bstInsert x l) r
  | x > v    = Node v l (bstInsert x r)
  | otherwise = Node v l r

-- Heap invariant
{-@ data MinHeap a = MLeaf
                   | MNode { top      :: a
                           , leftH    :: MinHeap { v:a | top <= v }
                           , rightH   :: MinHeap { v:a | top <= v } }
@-}

-- Size bounds
{-@ measure size @-}
{-@ size :: [a] -> NonNeg @-}
size :: [a] -> Int
size [] = 0
size (_:xs) = 1 + size xs
```

---

## ขั้นตอนที่ 843: Termination Proofs

```haskell
-- Proving termination with LH

-- Structural recursion on list length
{-@ fib :: NonNeg -> NonNeg @-}
{-@ Decrease fib 1 @-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)  -- LH checks n-1 < n and n-2 < n

-- Termination by lexicographic order
{-@ mergeSort :: xs:[a] -> { v:[a] | len v == len xs } / [len xs] @-}
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let n    = length xs `div` 2
      (l, r) = splitAt n xs
  in merge (mergeSort l) (mergeSort r)  -- len l < len xs, len r < len xs

-- Proving correctness of reverse
{-@ reverse' :: xs:[a] -> { v:[a] | len v == len xs } @-}
reverse' :: [a] -> [a]
reverse' = go []
  where
    {-@ go :: acc:[a] -> xs:[a] -> { v:[a] | len v == len acc + len xs } @-}
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs
```

---

## ขั้นตอนที่ 844: Memory Safety Proofs

```haskell
-- Proving memory safety properties

-- No out-of-bounds access
{-@ type Matrix m n a = { v:[[a]] | len v == m && all (\r -> len r == n) v } @-}

{-@ matGet :: m:Nat -> n:Nat -> Matrix m n a
           -> { r:Int | 0 <= r && r < m }
           -> { c:Int | 0 <= c && c < n }
           -> a @-}
matGet :: Int -> Int -> [[a]] -> Int -> Int -> a
matGet _ _ mat r c = (mat !! r) !! c  -- safe because of preconditions

-- Safe pointer-like access
{-@ data SafeArray a = SafeArray { saData :: [a], saSize :: { v:Int | v == len saData } } @-}
data SafeArray a = SafeArray { saData :: [a], saSize :: Int }

{-@ saGet :: sa:SafeArray a -> { i:Int | 0 <= i && i < saSize sa } -> a @-}
saGet :: SafeArray a -> Int -> a
saGet sa i = saData sa !! i

-- Null safety (verified)
{-@ assume getMaybe :: Maybe a -> { b:Bool | b = isJust a } -> a @-}
getMaybe :: Maybe a -> Bool -> a
getMaybe (Just x) True  = x
getMaybe Nothing  False = error "impossible"
getMaybe _        _     = error "impossible"
```

---

## ขั้นตอนที่ 845: Logical Specifications

```haskell
-- Writing logical specifications

-- Specification for sorting
{-@ isSorted :: Ord a => [a] -> Bool @-}
isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)

{-@ isPermutation :: Eq a => [a] -> [a] -> Bool @-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = null (xs \\ ys) && null (ys \\ xs)

-- Verified sort specification
{-@ sortSpec :: (Ord a, Eq a) => xs:[a]
             -> { v:[a] | isSorted v && isPermutation v xs } @-}

-- Financial calculations (verified)
{-@ type Money = { v:Double | v >= 0.0 } @-}
{-@ type Rate  = { v:Double | 0.0 <= v && v <= 1.0 } @-}

{-@ compoundInterest :: Money -> Rate -> Positive -> Money @-}
compoundInterest :: Double -> Double -> Int -> Double
compoundInterest principal rate years =
  principal * (1 + rate) ^ years

-- Verified: result is always >= principal
{-@ compoundNeverLess :: p:Money -> r:Rate -> n:Positive
                      -> { v:Bool | v = compoundInterest p r n >= p } @-}

-- Security properties
{-@ type Sanitized = { v:Text | noXss v } @-}
{-@ measure noXss :: Text -> Bool @-}
{-@ sanitize :: Text -> Sanitized @-}
sanitize :: Text -> Text
sanitize = escapeHtml  -- LH assumes escapeHtml produces Sanitized output
```

---

## ขั้นตอนที่ 846: Dependent Types with Singletons

```haskell
-- Dependent types using singletons library

{-# LANGUAGE DataKinds, GADTs, TypeFamilies, SingI #-}

import Data.Singletons
import Data.Singletons.Prelude

-- Type-level natural numbers
data Nat = Zero | Succ Nat

-- Singleton for Nat
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- Type-safe vector
data Vec (n :: Nat) a where
  VNil  :: Vec 'Zero a
  VCons :: a -> Vec n a -> Vec ('Succ n) a

-- Type-safe operations
vHead :: Vec ('Succ n) a -> a
vHead (VCons x _) = x

vTail :: Vec ('Succ n) a -> Vec n a
vTail (VCons _ xs) = xs

-- Type-safe zip (vectors must have same length)
vZip :: Vec n a -> Vec n b -> Vec n (a, b)
vZip VNil         VNil         = VNil
vZip (VCons x xs) (VCons y ys) = VCons (x, y) (vZip xs ys)

-- Replicate with type-level count
vReplicate :: SNat n -> a -> Vec n a
vReplicate SZero     _ = VNil
vReplicate (SSucc n) x = VCons x (vReplicate n x)

-- Convert to list (lose length info)
toList' :: Vec n a -> [a]
toList' VNil         = []
toList' (VCons x xs) = x : toList' xs

-- Type-level matrix
type Matrix m n a = Vec m (Vec n a)

matTranspose :: Matrix m n a -> Matrix n m a
matTranspose = undefined  -- complex but type-safe
```

---

## ขั้นตอนที่ 847: Verification of Security Properties

```haskell
-- Verifying security properties

-- Information flow types
{-@ type High a = Tagged HIGH a @-}
{-@ type Low  a = Tagged LOW  a @-}

data Level = HIGH | LOW

newtype Tagged (l :: Level) a = Tagged { unTagged :: a }

-- High-to-low flow is forbidden at type level
-- Low operations can use Low values
{-@ processLow :: Low Int -> Low Int @-}
processLow :: Tagged 'LOW Int -> Tagged 'LOW Int
processLow (Tagged n) = Tagged (n * 2)

-- Cannot leak high data to low context
-- This would be a type error:
-- leakHigh :: High Int -> Low Int
-- leakHigh (Tagged n) = Tagged n  -- TYPE ERROR

-- Declassify with explicit policy
{-@ declassify :: High a -> IO (Low a) @-}
declassify :: Tagged 'HIGH a -> IO (Tagged 'LOW a)
declassify (Tagged x) = do
  logDeclassification  -- audit trail required
  return (Tagged x)

-- SQL injection prevention
{-@ type SafeSQL = { v:Text | isSafe v } @-}
{-@ measure isSafe :: Text -> Bool @-}

{-@ parameterized :: SafeSQL -> [Value] -> Query @-}
parameterized :: Text -> [Value] -> Query
parameterized template params = Query template params

-- Only allow safe templates (no user input)
{-@ userQuery :: userId:Int -> SafeSQL @-}
userQuery :: Int -> Text
userQuery _ = "SELECT * FROM users WHERE id = ?"  -- literal is always safe
```

---

## ขั้นตอนที่ 848: Proof by Induction

```haskell
-- Inductive proofs encoded in Haskell types

-- Natural number equality proof
data Eq' a b where
  Refl :: Eq' a a

-- Prove 0 + n = n
zeroAddLeft :: SNat n -> Eq' (Zero :+ n) n
zeroAddLeft SZero     = Refl
zeroAddLeft (SSucc n) = case zeroAddLeft n of
  Refl -> Refl

-- Prove n + 0 = n (requires induction)
addZeroRight :: SNat n -> Eq' (n :+ Zero) n
addZeroRight SZero     = Refl
addZeroRight (SSucc n) = case addZeroRight n of
  Refl -> Refl

-- Prove associativity (n + m) + p = n + (m + p)
addAssoc :: SNat n -> SNat m -> SNat p -> Eq' ((n :+ m) :+ p) (n :+ (m :+ p))
addAssoc SZero     m p = Refl
addAssoc (SSucc n) m p = case addAssoc n m p of
  Refl -> Refl

-- Using proofs for safe operations
type family (:+) (m :: Nat) (n :: Nat) :: Nat where
  'Zero   :+ n = n
  'Succ m :+ n = 'Succ (m :+ n)

-- Append vectors (type-level length addition)
vAppend :: Vec m a -> Vec n a -> Vec (m :+ n) a
vAppend VNil         ys = ys
vAppend (VCons x xs) ys = VCons x (vAppend xs ys)
```

---

## ขั้นตอนที่ 849: Model Checking Concepts

```haskell
-- Model checking with Haskell

-- Kripke structure (transition system)
data KripkeModel s a = KripkeModel
  { kmStates   :: Set s
  , kmInit     :: Set s
  , kmTrans    :: s -> Set s
  , kmLabels   :: s -> Set a  -- atomic propositions true in each state
  }

-- CTL formulas
data CTLFormula a
  = AP     a           -- atomic proposition
  | Not    (CTLFormula a)
  | And    (CTLFormula a) (CTLFormula a)
  | Or     (CTLFormula a) (CTLFormula a)
  | EX     (CTLFormula a)  -- exists next
  | AX     (CTLFormula a)  -- all next
  | EF     (CTLFormula a)  -- exists finally
  | AF     (CTLFormula a)  -- all finally
  | EG     (CTLFormula a)  -- exists globally
  | AG     (CTLFormula a)  -- all globally
  | EU     (CTLFormula a) (CTLFormula a)  -- exists until
  | AU     (CTLFormula a) (CTLFormula a)  -- all until

-- Model check CTL formula
modelCheck :: (Ord s, Ord a) => KripkeModel s a -> CTLFormula a -> Set s
modelCheck m (AP p)    = Set.filter (Set.member p . kmLabels m) (kmStates m)
modelCheck m (Not f)   = kmStates m `Set.difference` modelCheck m f
modelCheck m (And f g) = modelCheck m f `Set.intersection` modelCheck m g
modelCheck m (EX f) =
  let sat = modelCheck m f
  in Set.filter (\s -> not (Set.null (kmTrans m s `Set.intersection` sat))) (kmStates m)
modelCheck m (EG f) = checkEG m (modelCheck m f)
modelCheck m (EU f g) = checkEU m (modelCheck m f) (modelCheck m g)
modelCheck m f = modelCheck m (desugar f)

checkEG :: (Ord s) => KripkeModel s a -> Set s -> Set s
checkEG m sat = fixpoint (\s -> s `Set.intersection` Set.filter (\v -> not (Set.null (kmTrans m v `Set.intersection` s))) (kmStates m)) sat
```

---

## ขั้นตอนที่ 850: Property-Based Specifications

```haskell
-- Writing specifications as properties

-- Algebraic laws
class Monoid a => VerifiedMonoid a where
  -- mempty <> x = x
  leftIdentity  :: a -> Bool
  leftIdentity x = mempty <> x == x
  
  -- x <> mempty = x
  rightIdentity :: a -> Bool
  rightIdentity x = x <> mempty == x
  
  -- (x <> y) <> z = x <> (y <> z)
  associativity :: a -> a -> a -> Bool
  associativity x y z = (x <> y) <> z == x <> (y <> z)

-- Verify with QuickCheck
verifyMonoid :: (VerifiedMonoid a, Arbitrary a, Show a, Eq a) => Proxy a -> IO ()
verifyMonoid proxy = do
  quickCheck (leftIdentity  :: a -> Bool)
  quickCheck (rightIdentity :: a -> Bool)
  quickCheck (associativity :: a -> a -> a -> Bool)

-- Functor laws
class Functor f => VerifiedFunctor f where
  -- fmap id = id
  identityLaw :: Eq (f Int) => f Int -> Bool
  identityLaw fx = fmap id fx == fx
  
  -- fmap (f . g) = fmap f . fmap g
  compositionLaw :: Eq (f Int) => f Int -> (Int -> Int) -> (Int -> Int) -> Bool
  compositionLaw fx f g = fmap (f . g) fx == (fmap f . fmap g) fx

-- Monad laws
monadLaws :: (Monad m, Eq (m Int)) => (Int -> m Int) -> (Int -> m Int) -> m Int -> Int -> Bool
monadLaws f g mx x =
  -- left identity: return a >>= f = f a
  (return x >>= f) == f x &&
  -- right identity: m >>= return = m
  (mx >>= return) == mx &&
  -- associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
  ((mx >>= f) >>= g) == (mx >>= \a -> f a >>= g)
```

---

## ขั้นตอนที่ 851: Type-Safe Contracts

```haskell
-- Design by Contract with types

-- Precondition/postcondition encoding
newtype Requires pre a = Requires { unRequires :: pre -> a }
newtype Ensures  post a = Ensures  { unEnsures  :: a -> post -> Bool }

-- Contract for safe division
{-# ANN safeDiv' "CONTRACT: divisor /= 0 => result * divisor == dividend" #-}
safeDiv' :: forall pre. (pre ~ (Int, Int))
         => Int -> Int -> Maybe Int
safeDiv' x 0 = Nothing  -- precondition violated
safeDiv' x y = Just (x `div` y)

-- Phantom type contracts
data Checked     -- validated
data Unchecked   -- not yet validated

newtype Input t a = Input { unInput :: a }

check :: (a -> Bool) -> Input Unchecked a -> Maybe (Input Checked a)
check predicate (Input x)
  | predicate x = Just (Input x)
  | otherwise   = Nothing

-- Only accept Checked inputs for sensitive operations
sensitiveOperation :: Input Checked Int -> Int
sensitiveOperation (Input x) = x * 2

-- Type-level state
{-# LANGUAGE DataKinds #-}
data ConnectionState = Open | Closed

newtype Connection' (s :: ConnectionState) = Connection' Handle

open  :: IO (Connection' 'Open)
close :: Connection' 'Open -> IO (Connection' 'Closed)
query :: Connection' 'Open -> Text -> IO [[Value]]

-- Cannot query a closed connection (type error)
-- wrongUsage :: IO ()
-- wrongUsage = do
--   conn  <- open
--   conn' <- close conn
--   query conn' "SELECT 1"  -- TYPE ERROR: Connection' 'Closed is not 'Open
```

---

## ขั้นตอนที่ 852: Certified Programs

```haskell
-- Programs with embedded correctness proofs

-- Certified sort (verified correct)
data SortResult a = SortResult
  { srResult :: [a]
  , srProof  :: IsSorted a (srResult)  -- proof that result is sorted
  }

-- Proof that a list is sorted
data IsSorted a :: [a] -> Type where
  SortedNil  :: IsSorted a '[]
  SortedOne  :: IsSorted a '[x]
  SortedCons :: (x <= y) => IsSorted a (y:ys) -> IsSorted a (x:y:ys)

-- Certified insertion (maintains sorted invariant)
certInsert :: (Ord a, Decidable (<=)) => a -> [a] -> IsSorted a xs -> SortResult a
certInsert x [] SortedNil  = SortResult [x] SortedOne
certInsert x [y] SortedOne
  | x <= y    = SortResult [x, y] (SortedCons SortedOne)
  | otherwise = SortResult [y, x] (SortedCons SortedOne)
certInsert x (y:ys) (SortedCons proof)
  | x <= y    = SortResult (x:y:ys) (SortedCons (SortedCons proof))
  | otherwise =
      let SortResult ys' proof' = certInsert x ys proof
      in SortResult (y:ys') (SortedCons proof')

-- Certified Fibonacci with correct values
certFib :: SNat n -> (Nat, ProofFibCorrect n)
certFib SZero             = (0, FibZeroProof)
certFib (SSucc SZero)     = (1, FibOneProof)
certFib (SSucc (SSucc n)) =
  let (fn, pn) = certFib n
      (fn1, pn1) = certFib (SSucc n)
  in (fn + fn1, FibSuccProof pn pn1)
```

---

## ขั้นตอนที่ 853: Invariant Maintenance

```haskell
-- Maintaining invariants in stateful programs

-- Bank account with balance invariant
newtype Balance = Balance { getBalance :: Int } deriving (Show, Eq, Ord)

{-@ type NonNegBalance = { v:Balance | getBalance v >= 0 } @-}

data Account = Account
  { accId      :: AccountId
  , accBalance :: Balance
  }

-- Smart constructors that maintain invariants
deposit :: Int -> Account -> Either String Account
deposit amount acc
  | amount <= 0 = Left "Deposit must be positive"
  | otherwise   = Right acc { accBalance = Balance (getBalance (accBalance acc) + amount) }

withdraw :: Int -> Account -> Either String Account
withdraw amount acc
  | amount <= 0 = Left "Withdrawal must be positive"
  | getBalance (accBalance acc) < amount = Left "Insufficient funds"
  | otherwise   = Right acc { accBalance = Balance (getBalance (accBalance acc) - amount) }

-- Transfer maintains system-wide total balance
transfer :: Int -> Account -> Account -> Either String (Account, Account)
transfer amount from to = do
  from' <- withdraw amount from
  to'   <- deposit  amount to
  return (from', to')

-- Proof: total balance preserved by transfer
-- getBalance (accBalance from') + getBalance (accBalance to')
-- == getBalance (accBalance from)  + getBalance (accBalance to)
```

---

## ขั้นตอนที่ 854: Refinements for APIs

```haskell
-- Refined types in API definitions

-- HTTP status codes with refinements
{-@ type Status2xx = { v:Int | 200 <= v && v < 300 } @-}
{-@ type Status4xx = { v:Int | 400 <= v && v < 500 } @-}
{-@ type Status5xx = { v:Int | 500 <= v && v < 600 } @-}

data Response a
  = Success { resStatus :: Status2xx, resBody :: a }
  | ClientError { resStatus :: Status4xx, resMessage :: Text }
  | ServerError { resStatus :: Status5xx, resMessage :: Text }

-- Email validation
{-@ type ValidEmail = { v:Text | isValidEmail v } @-}
{-@ measure isValidEmail :: Text -> Bool @-}

{-@ validateEmail :: Text -> Maybe ValidEmail @-}
validateEmail :: Text -> Maybe Text
validateEmail email
  | isValidEmailFormat email = Just email
  | otherwise                = Nothing

-- Age-restricted API
{-@ type Adult = { v:Int | v >= 18 } @-}

{-@ restrictedContent :: Adult -> Content @-}
restrictedContent :: Int -> Content
restrictedContent age = fullContent  -- safe because age >= 18 guaranteed

-- API rate limiting
{-@ type RateLimited a = { v:IO a | usesRateLimit v } @-}

-- Pagination with valid bounds
{-@ type PageSize = { v:Int | 1 <= v && v <= 100 } @-}
{-@ type PageNum  = { v:Int | v >= 1 } @-}

{-@ paginate :: PageNum -> PageSize -> [a] -> [a] @-}
paginate :: Int -> Int -> [a] -> [a]
paginate page size xs =
  let offset = (page - 1) * size
  in take size (drop offset xs)
```

---

## ขั้นตอนที่ 855: Abstract Interpretation

```haskell
-- Abstract interpretation for static analysis

-- Abstract domain: sign analysis
data Sign = Negative | Zero | Positive | Unknown

signPlus :: Sign -> Sign -> Sign
signPlus Negative Negative = Negative
signPlus Positive Positive = Positive
signPlus Zero     s        = s
signPlus s        Zero     = s
signPlus _        _        = Unknown

signMul :: Sign -> Sign -> Sign
signMul Negative Negative = Positive
signMul Positive Positive = Positive
signMul Negative Positive = Negative
signMul Positive Negative = Negative
signMul Zero     _        = Zero
signMul _        Zero     = Zero
signMul _        _        = Unknown

-- Abstract evaluation
abstractEval :: Map Text Sign -> Expr -> Sign
abstractEval env (ELit (LInt n))
  | n < 0    = Negative
  | n == 0   = Zero
  | otherwise = Positive
abstractEval env (EVar x) = fromMaybe Unknown (Map.lookup x env)
abstractEval env (EBinOp OpPlus l r) =
  signPlus (abstractEval env l) (abstractEval env r)
abstractEval env (EBinOp OpMul l r) =
  signMul (abstractEval env l) (abstractEval env r)
abstractEval _ _ = Unknown

-- Interval domain
data Interval
  = Empty
  | Range { lo :: Maybe Int, hi :: Maybe Int }  -- Nothing = infinity
  deriving (Show, Eq)

-- Join (least upper bound)
intervalJoin :: Interval -> Interval -> Interval
intervalJoin Empty i = i
intervalJoin i Empty = i
intervalJoin (Range l1 h1) (Range l2 h2) =
  Range (mergeMin l1 l2) (mergeMax h1 h2)
  where
    mergeMin (Just a) (Just b) = Just (min a b)
    mergeMin _ _ = Nothing
    mergeMax (Just a) (Just b) = Just (max a b)
    mergeMax _ _ = Nothing
```

---

## ขั้นตอนที่ 856: Denotational Semantics

```haskell
-- Denotational semantics in Haskell

-- Domain: computable partial values (using Maybe)
type D a = Maybe a  -- Nothing = non-termination

-- Semantic function for expressions
sem :: Expr -> Map Text Int -> D Int
sem (ELit (LInt n)) _   = Just n
sem (EVar x) env        = Map.lookup x env
sem (EBinOp OpPlus l r) env = do
  lv <- sem l env
  rv <- sem r env
  return (lv + rv)
sem (EBinOp OpDiv l r) env = do
  lv <- sem l env
  rv <- sem r env
  if rv == 0 then Nothing  -- division by zero = non-termination
  else Just (lv `div` rv)
sem (EIf cond then' else') env = do
  cv <- sem cond env
  if cv /= 0 then sem then' env else sem else' env

-- Fixed-point for loops
fix :: (D a -> D a) -> D a
fix f = go Nothing
  where go prev =
          let next = f prev
          in if next == prev then next else go next

-- Semantic function for while loops
semWhile :: Expr -> Block -> Map Text Int -> D (Map Text Int)
semWhile cond body = fix $ \continue env ->
  case sem cond env of
    Nothing -> Nothing
    Just cv -> if cv == 0
      then Just env  -- condition false, exit
      else case execBlock body env of
        Nothing   -> Nothing
        Just env' -> continue env'  -- recurse with new env
```

---

## ขั้นตอนที่ 857: Operational Semantics

```haskell
-- Small-step and big-step operational semantics

-- Small-step reduction
data Config = Config Expr (Map Text Value)

-- Single step
smallStep :: Config -> Maybe Config
smallStep (Config (EBinOp op (ELit l) (ELit r)) env) =
  Just (Config (ELit (evalOp op l r)) env)

smallStep (Config (EBinOp op l r) env)
  | isValue l = do
      Config r' env' <- smallStep (Config r env)
      return (Config (EBinOp op l r') env')
  | otherwise = do
      Config l' env' <- smallStep (Config l env)
      return (Config (EBinOp op l' r) env')

smallStep (Config (EVar x) env) =
  case Map.lookup x env of
    Just v  -> Just (Config (valToExpr v) env)
    Nothing -> Nothing  -- stuck

smallStep (Config (EApp (ELit (LFn params body)) args) env)
  | all isLiteral args =
      let bindings = zip params (map extractLit args)
          env'     = Map.fromList bindings `Map.union` env
      in Just (Config body env')

-- Multi-step reduction
multiStep :: Config -> Config
multiStep cfg = case smallStep cfg of
  Nothing   -> cfg  -- stuck or finished
  Just cfg' -> multiStep cfg'

-- Big-step evaluation
bigStep :: Expr -> Map Text Value -> Maybe Value
bigStep (ELit lit) _ = Just (litToValue lit)
bigStep (EVar x) env = Map.lookup x env
bigStep (EBinOp op l r) env = do
  lv <- bigStep l env
  rv <- bigStep r env
  evalBinOp op lv rv
bigStep (EApp f args) env = do
  fv   <- bigStep f env
  argVs <- mapM (\a -> bigStep a env) args
  applyBigStep fv argVs
```

---

## ขั้นตอนที่ 858: Type Soundness

```haskell
-- Type soundness: progress and preservation

-- Type system
data TypeJudgment = TypeJudgment
  { tjEnv  :: TyEnv
  , tjExpr :: Expr
  , tjType :: Ty
  }

-- Progress: well-typed terms are either values or can step
data Progress e
  = IsValue e
  | CanStep e  -- can take a small step

progress :: TypedExpr -> Progress TypedExpr
progress (TLit l)    = IsValue (TLit l)
progress (TApp f args)
  | isValue f && all isValue args = IsValue (reduceApp f args)
  | otherwise                     = CanStep (TApp (step f) args)
progress (TBinOp op l r ty)
  | isValue l && isValue r = CanStep (TLit (evalBinOpTyped op l r ty))
  | isValue l              = CanStep (TBinOp op l (step r) ty)
  | otherwise              = CanStep (TBinOp op (step l) r ty)

-- Preservation: if e : T and e -> e', then e' : T
preservation :: TypedExpr -> Ty -> TypedExpr -> Ty
preservation e t e'
  | typeOf e' == t = t  -- preserved
  | otherwise      = error "Preservation violated!"

-- Subject reduction (should be proof, shown as check)
subjectReduction :: TypedExpr -> Bool
subjectReduction e = case step e of
  Nothing -> True  -- value, nothing to check
  Just e' -> typeOf e == typeOf e'
```

---

## ขั้นตอนที่ 859: Verified Algorithms with Agda-Style Proofs

```haskell
-- Agda-inspired proof techniques in Haskell

-- Natural number proofs
data Nat' = Z | S Nat' deriving (Eq, Ord)

-- Proof of equality
data PropEq a b where
  Refl' :: PropEq a a

-- Proof: S Z + n = S n
addSZ :: SNat n -> PropEq (S Z :+: n) (S n)
addSZ = undefined  -- by induction

-- Proof: n + m = m + n (commutativity)
addComm :: SNat m -> SNat n -> PropEq (m :+: n) (n :+: m)
addComm SZ n = case addZeroRight n of Refl' -> Refl'
addComm (SS m) n = case addComm m n of
  Refl' -> case addSuccRight m n of
    Refl' -> Refl'

-- Coq-style tactics (simulated)
data Tactic a
  = Assumption
  | Apply (a -> a)
  | Rewrite (a -> a)
  | Induction

runTactic :: Tactic a -> a -> a
runTactic Assumption  x = x
runTactic (Apply f)   x = f x
runTactic (Rewrite f) x = f x
runTactic Induction   x = x  -- simplified

-- Certified binary search
{-@ certBinarySearch :: Sorted [a] -> a -> Maybe Int @-}
certBinarySearch :: Ord a => [a] -> a -> Maybe Int
certBinarySearch sorted target = go 0 (length sorted - 1)
  where
    arr = listArray (0, length sorted - 1) sorted
    go lo hi
      | lo > hi = Nothing
      | otherwise =
          let mid = (lo + hi) `div` 2
              v   = arr ! mid
          in case compare target v of
               EQ -> Just mid
               LT -> go lo (mid - 1)
               GT -> go (mid + 1) hi
```

---

## ขั้นตอนที่ 860: โปรเจกต์: Verified Data Pipeline

```haskell
-- Verified data pipeline with correctness proofs

module VerifiedPipeline where

import Data.Typeable (Typeable)

-- Type-safe pipeline stages
data Stage input output = Stage
  { stageName :: Text
  , stageRun  :: input -> IO output
  , stageSpec :: [Property input output]
  }

data Property input output = Property
  { propName :: Text
  , propCheck :: input -> output -> Bool
  }

-- Compose stages with type safety
(>>>) :: Stage a b -> Stage b c -> Stage a c
s1 >>> s2 = Stage
  { stageName = stageName s1 <> " >>> " <> stageName s2
  , stageRun  = \input -> do
      mid <- stageRun s1 input
      stageRun s2 mid
  , stageSpec = []  -- compose proofs if needed
  }

-- Verified parsing stage
parseStage :: (FromJSON a, Typeable a) => Stage ByteString a
parseStage = Stage
  { stageName = "parse"
  , stageRun  = \bs -> case eitherDecodeStrict bs of
      Left err -> throwIO (ParseError err)
      Right v  -> return v
  , stageSpec =
      [ Property "output is always valid JSON" (\_ _ -> True)
      ]
  }

-- Verified validation stage
validateStage :: (a -> Either [Text] a) -> Stage a a
validateStage validator = Stage
  { stageName = "validate"
  , stageRun  = \input -> case validator input of
      Left errs -> throwIO (ValidationError errs)
      Right v   -> return v
  , stageSpec =
      [ Property "validator is idempotent" (\_ out -> isRight (validator out))
      ]
  }

-- Run pipeline with verification
runVerified :: (Eq output, Show output) => Stage input output -> input -> IO output
runVerified stage input = do
  output <- stageRun stage input
  
  -- Check all specs
  let violations = filter (\p -> not (propCheck p input output)) (stageSpec stage)
  unless (null violations) $
    throwIO (VerificationError (map propName violations))
  
  return output

-- Example verified ETL pipeline
userPipeline :: Stage ByteString ProcessedUser
userPipeline =
  parseStage
  >>> validateStage validateUser
  >>> transformStage normalizeUser
  >>> validateStage validateProcessed
```

---

*[← Part 42](part-42.md) | [Part 44 →](part-44.md)*
