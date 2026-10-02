# Part 41: Compiler Design in Haskell
## ขั้นตอนที่ 801-820

---

## ขั้นตอนที่ 801: Lexer Design

```haskell
-- Lexer สำหรับ simple language

module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- Token types
data Token
  = TkInt     Int
  | TkFloat   Double
  | TkStr     Text
  | TkIdent   Text
  | TkKeyword Keyword
  | TkOp      Op
  | TkLParen  | TkRParen
  | TkLBrace  | TkRBrace
  | TkLBrack  | TkRBrack
  | TkComma   | TkSemi
  | TkEOF
  deriving (Show, Eq)

data Keyword
  = KwIf | KwElse | KwWhile | KwFor
  | KwReturn | KwLet | KwFn | KwClass
  | KwTrue | KwFalse | KwNull
  deriving (Show, Eq)

data Op
  = OpPlus | OpMinus | OpMul | OpDiv | OpMod
  | OpEq | OpNeq | OpLt | OpGt | OpLte | OpGte
  | OpAnd | OpOr | OpNot
  | OpAssign | OpArrow
  deriving (Show, Eq)

data LexerState = LexerState
  { lsSource :: Text
  , lsPos    :: Int
  , lsLine   :: Int
  , lsCol    :: Int
  }

type Lexer a = StateT LexerState (Either LexError) a

data LexError = LexError
  { leMessage :: Text
  , leLine    :: Int
  , leCol     :: Int
  }

-- Main tokenizer
tokenize :: Text -> Either LexError [Token]
tokenize src = evalStateT scanAll (LexerState src 0 1 1)
  where
    scanAll = do
      tok <- nextToken
      case tok of
        TkEOF -> return [TkEOF]
        t     -> (t :) <$> scanAll

nextToken :: Lexer Token
nextToken = do
  skipWhitespaceAndComments
  st <- get
  if lsPos st >= T.length (lsSource st)
    then return TkEOF
    else do
      c <- currentChar
      case c of
        '('  -> advance >> return TkLParen
        ')'  -> advance >> return TkRParen
        '{'  -> advance >> return TkLBrace
        '}'  -> advance >> return TkRBrace
        '['  -> advance >> return TkLBrack
        ']'  -> advance >> return TkRBrack
        ','  -> advance >> return TkComma
        ';'  -> advance >> return TkSemi
        '"'  -> lexString
        '-'  -> lexMinusOrArrow
        _ | isDigit c     -> lexNumber
          | isAlpha c     -> lexIdentOrKeyword
          | otherwise     -> lexOperator

-- Lex identifiers/keywords
lexIdentOrKeyword :: Lexer Token
lexIdentOrKeyword = do
  ident <- takeWhile1 isAlphaNum
  return $ case ident of
    "if"     -> TkKeyword KwIf
    "else"   -> TkKeyword KwElse
    "while"  -> TkKeyword KwWhile
    "return" -> TkKeyword KwReturn
    "let"    -> TkKeyword KwLet
    "fn"     -> TkKeyword KwFn
    "true"   -> TkKeyword KwTrue
    "false"  -> TkKeyword KwFalse
    "null"   -> TkKeyword KwNull
    other    -> TkIdent other
```

---

## ขั้นตอนที่ 802: Parser Combinators

```haskell
-- Custom parser combinators

module Parser where

import Control.Applicative
import Data.Text (Text)

-- Parser type
newtype Parser a = Parser
  { runParser :: [Token] -> Either ParseError (a, [Token])
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (\(a, ts) -> (f a, ts)) . p

instance Applicative Parser where
  pure a = Parser (\ts -> Right (a, ts))
  Parser pf <*> Parser pa = Parser $ \ts -> do
    (f, ts')  <- pf ts
    (a, ts'') <- pa ts'
    return (f a, ts'')

instance Alternative Parser where
  empty = Parser (\_ -> Left (ParseError "empty"))
  Parser p1 <|> Parser p2 = Parser $ \ts ->
    case p1 ts of
      Left _    -> p2 ts
      Right res -> Right res

instance Monad Parser where
  return = pure
  Parser pa >>= f = Parser $ \ts -> do
    (a, ts') <- pa ts
    runParser (f a) ts'

-- Basic parsers
satisfy :: (Token -> Bool) -> Parser Token
satisfy pred = Parser $ \case
  (t:ts) | pred t -> Right (t, ts)
  _               -> Left (ParseError "unexpected token")

token :: Token -> Parser Token
token t = satisfy (== t)

keyword :: Keyword -> Parser ()
keyword kw = () <$ satisfy (== TkKeyword kw)

-- Expression parser with precedence climbing
data Expr
  = Lit  Int
  | Var  Text
  | App  Expr [Expr]
  | BinOp Op Expr Expr
  | UnOp  Op Expr
  | If    Expr Expr Expr
  | Lambda [Text] Expr
  deriving (Show)

parseExpr :: Parser Expr
parseExpr = parsePrec 0

parsePrec :: Int -> Parser Expr
parsePrec prec = do
  left <- parseUnary
  parseInfix prec left

parseInfix :: Int -> Expr -> Parser Expr
parseInfix prec left = do
  mOp <- optional (satisfy isInfixOp)
  case mOp of
    Nothing -> return left
    Just (TkOp op)
      | opPrec op >= prec -> do
          right <- parsePrec (opPrec op + 1)
          parseInfix prec (BinOp op left right)
      | otherwise -> return left

opPrec :: Op -> Int
opPrec OpOr  = 1
opPrec OpAnd = 2
opPrec OpEq  = 3; opPrec OpNeq = 3
opPrec OpLt  = 4; opPrec OpGt = 4
opPrec OpPlus = 5; opPrec OpMinus = 5
opPrec OpMul = 6; opPrec OpDiv = 6; opPrec OpMod = 6
opPrec _     = 0
```

---

## ขั้นตอนที่ 803: AST Design

```haskell
-- Abstract Syntax Tree

module AST where

-- Program
data Program = Program
  { progDecls :: [Decl]
  } deriving (Show)

-- Declarations
data Decl
  = DeclFn    FnDecl
  | DeclClass ClassDecl
  | DeclImpl  ImplDecl
  | DeclData  DataDecl
  deriving (Show)

data FnDecl = FnDecl
  { fnName   :: Text
  , fnParams :: [(Text, Maybe Type)]
  , fnReturn :: Maybe Type
  , fnBody   :: Block
  , fnSpan   :: Span
  } deriving (Show)

-- Types
data Type
  = TyName  Text
  | TyApp   Type [Type]
  | TyFn    [Type] Type
  | TyTuple [Type]
  | TyInfer  -- _
  deriving (Show, Eq)

-- Statements
data Stmt
  = SExpr  Expr
  | SLet   Pattern (Maybe Type) Expr
  | SReturn Expr
  | SIf    Expr Block (Maybe Block)
  | SWhile Expr Block
  | SFor   Text Expr Block
  deriving (Show)

-- Expressions
data Expr
  = ELit   Lit
  | EVar   Text
  | EApp   Expr [Expr]
  | EBinOp Op Expr Expr
  | EUnOp  Op Expr
  | ELambda [(Text, Maybe Type)] Expr
  | EBlock  Block
  | EMatch  Expr [(Pattern, Expr)]
  | EField  Expr Text
  | EIndex  Expr Expr
  | EIf     Expr Expr Expr
  deriving (Show)

-- Literals
data Lit
  = LInt    Int
  | LFloat  Double
  | LStr    Text
  | LBool   Bool
  | LNull
  deriving (Show)

-- Patterns
data Pattern
  = PVar   Text
  | PLit   Lit
  | PTuple [Pattern]
  | PList  [Pattern]
  | PCons  Pattern Pattern
  | PConst Text [Pattern]
  | PWild
  deriving (Show)

-- Source spans for error reporting
data Span = Span
  { spanStart :: (Int, Int)
  , spanEnd   :: (Int, Int)
  } deriving (Show)

type Block = [Stmt]
```

---

## ขั้นตอนที่ 804: Type Checker

```haskell
-- Hindley-Milner type inference

module TypeChecker where

import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Monad.Except

-- Types with type variables
data Ty
  = TyVar  TyVar
  | TyCon  Text
  | TyApp  Ty Ty
  | TyArr  Ty Ty
  deriving (Show, Eq)

type TyVar = Int

-- Type scheme (forall a. Ty)
data Scheme = Forall [TyVar] Ty

-- Substitution
type Subst = Map TyVar Ty

-- Type environment
type TyEnv = Map Text Scheme

-- Unification
unify :: Ty -> Ty -> Either TypeError Subst
unify (TyVar u) t                  = bindTyVar u t
unify t         (TyVar u)          = bindTyVar u t
unify (TyCon a) (TyCon b)
  | a == b                         = Right Map.empty
unify (TyApp f1 x1) (TyApp f2 x2) = do
  s1 <- unify f1 f2
  s2 <- unify (apply s1 x1) (apply s1 x2)
  return (compose s2 s1)
unify (TyArr a1 b1) (TyArr a2 b2)  = unify (TyApp (TyApp (TyCon "->") a1) b1)
                                           (TyApp (TyApp (TyCon "->") a2) b2)
unify t1 t2                         = Left (TypeError (UnificationFail t1 t2))

bindTyVar :: TyVar -> Ty -> Either TypeError Subst
bindTyVar u t
  | t == TyVar u   = Right Map.empty
  | u `occursIn` t = Left (TypeError (OccursCheck u t))
  | otherwise      = Right (Map.singleton u t)

occursIn :: TyVar -> Ty -> Bool
occursIn u (TyVar v)     = u == v
occursIn u (TyCon _)     = False
occursIn u (TyApp f x)   = u `occursIn` f || u `occursIn` x
occursIn u (TyArr a b)   = u `occursIn` a || u `occursIn` b

-- Apply substitution
apply :: Subst -> Ty -> Ty
apply s (TyVar u)    = fromMaybe (TyVar u) (Map.lookup u s)
apply s (TyApp f x)  = TyApp (apply s f) (apply s x)
apply s (TyArr a b)  = TyArr (apply s a) (apply s b)
apply _ ty           = ty

-- Compose substitutions
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- Infer type
infer :: TyEnv -> Expr -> IO (Subst, Ty)
infer env (ELit (LInt _))  = return (Map.empty, TyCon "Int")
infer env (ELit (LBool _)) = return (Map.empty, TyCon "Bool")
infer env (EVar x)         = case Map.lookup x env of
  Nothing     -> throwIO (TypeError (UnboundVariable x))
  Just scheme -> do
    ty <- instantiate scheme
    return (Map.empty, ty)
infer env (EApp f args) = do
  (sf, tf) <- infer env f
  retTy    <- freshTyVar
  (sArgs, argTy) <- inferArgs (applyEnv sf env) args
  let funcTy = foldr TyArr retTy argTy
  s <- liftEither (unify (apply sArgs tf) funcTy)
  return (compose s (compose sArgs sf), apply s retTy)
```

---

## ขั้นตอนที่ 805: Code Generation

```haskell
-- Code generation to LLVM IR

module CodeGen where

import LLVM.IRBuilder
import LLVM.AST

-- Generate LLVM IR from typed AST
type CodeGenM = IRBuilderT ModuleBuilder

genExpr :: TypedExpr -> CodeGenM Operand
genExpr (TLit (LInt n)) = return (int32 (fromIntegral n))
genExpr (TLit (LBool b)) = return (bit (if b then 1 else 0))
genExpr (TVar name ty) = do
  ptr <- asks (Map.lookup name . cgEnv)
  case ptr of
    Nothing  -> error ("Unknown variable: " ++ T.unpack name)
    Just ptr -> load ptr 4

genExpr (TBinOp OpPlus a b ty) = do
  av <- genExpr a
  bv <- genExpr b
  add av bv

genExpr (TBinOp OpLt a b ty) = do
  av <- genExpr a
  bv <- genExpr b
  icmp IP.SLT av bv

genExpr (TIf cond then' else' ty) = do
  condVal <- genExpr cond
  thenBB <- freshName "then"
  elseBB  <- freshName "else"
  mergeBB <- freshName "merge"
  
  condBr condVal thenBB elseBB
  
  emitBlockStart thenBB
  thenVal <- genExpr then'
  br mergeBB
  
  emitBlockStart elseBB
  elseVal <- genExpr else'
  br mergeBB
  
  emitBlockStart mergeBB
  phi [(thenVal, thenBB), (elseVal, elseBB)]

-- Generate function
genFunction :: TypedFnDecl -> ModuleBuilder ()
genFunction fn = do
  let paramTypes = map (\(_, ty) -> toLlvmType ty) (tfdParams fn)
  let retType    = toLlvmType (tfdReturn fn)
  
  function (mkName (T.unpack (tfdName fn))) (zip paramTypes (map (\(n,_) -> mkName (T.unpack n)) (tfdParams fn))) retType $ \args -> do
    -- Set up environment with function args
    let env = Map.fromList (zip (map fst (tfdParams fn)) args)
    local (\s -> s { cgEnv = env }) (genBlock (tfdBody fn))
```

---

## ขั้นตอนที่ 806: Optimizations

```haskell
-- Compiler optimizations

-- Constant folding
constantFold :: Expr -> Expr
constantFold (BinOp OpPlus (Lit (LInt a)) (Lit (LInt b))) = Lit (LInt (a + b))
constantFold (BinOp OpMul  (Lit (LInt a)) (Lit (LInt b))) = Lit (LInt (a * b))
constantFold (BinOp OpMul  (Lit (LInt 0)) _)              = Lit (LInt 0)
constantFold (BinOp OpMul  _ (Lit (LInt 0)))              = Lit (LInt 0)
constantFold (BinOp OpPlus (Lit (LInt 0)) e)              = constantFold e
constantFold (BinOp OpPlus e (Lit (LInt 0)))              = constantFold e
constantFold (BinOp op l r) = BinOp op (constantFold l) (constantFold r)
constantFold (App f args)   = App (constantFold f) (map constantFold args)
constantFold (If (Lit (LBool True))  t _) = constantFold t
constantFold (If (Lit (LBool False)) _ e) = constantFold e
constantFold e = e

-- Dead code elimination
deadCodeElim :: [Stmt] -> [Stmt]
deadCodeElim [] = []
deadCodeElim (s:ss) = case s of
  SReturn e -> [SReturn (deadCodeExpr e)]  -- eliminate code after return
  SIf cond t e ->
    let (t', e') = (deadCodeElim t, fmap deadCodeElim e)
    in SIf cond t' e' : deadCodeElim ss
  _ -> s : deadCodeElim ss

-- Inlining small functions
inlineFunctions :: Map Text FnDecl -> Expr -> Expr
inlineFunctions fns (App (Var name) args) =
  case Map.lookup name fns of
    Just fn | isSmallFunction fn ->
      let subst = Map.fromList (zip (map fst (fnParams fn)) args)
      in substituteArgs subst (blockToExpr (fnBody fn))
    _ -> App (Var name) (map (inlineFunctions fns) args)
inlineFunctions fns (BinOp op l r) = BinOp op (inlineFunctions fns l) (inlineFunctions fns r)
inlineFunctions _ e = e

isSmallFunction :: FnDecl -> Bool
isSmallFunction fn = length (fnBody fn) <= 3 && not (isRecursive fn)
```

---

## ขั้นตอนที่ 807: Error Recovery

```haskell
-- Error recovery in parsing

-- Error reporting
data ParseError = ParseError
  { peMessage  :: Text
  , peLoc      :: Span
  , peExpected :: [Text]
  , peGot      :: Maybe Token
  } deriving (Show)

-- Parser with error recovery
data ParseResult a
  = POk    a [ParseError]  -- success with warnings
  | PError [ParseError]    -- failure
  deriving (Show)

-- Synchronize on error (skip tokens until sync point)
synchronize :: [Token] -> Parser ()
synchronize syncTokens = Parser $ \ts ->
  let ts' = dropWhile (not . isSyncToken) ts
  in Right ((), ts')
  where isSyncToken t = t `elem` syncTokens

-- Parse with recovery
parseWithRecovery :: Parser a -> [Token] -> (Maybe a, [ParseError])
parseWithRecovery parser tokens =
  case runParser parser tokens of
    Right (a, _)  -> (Just a, [])
    Left err      ->
      let (remaining, recovered) = tryRecover tokens
      in case parseWithRecovery parser remaining of
           (a, errs) -> (a, err : errs)

-- Error messages
prettyParseError :: Text -> ParseError -> Text
prettyParseError source err =
  let Span (l1, c1) (l2, c2) = peLoc err
      lines   = T.lines source
      line    = lines !! (l1 - 1)
      pointer = T.replicate (c1 - 1) " " <> T.replicate (c2 - c1) "^"
  in T.unlines
       [ T.pack (show l1) <> ":" <> T.pack (show c1) <> ": error: " <> peMessage err
       , line
       , pointer
       , "Expected: " <> T.intercalate ", " (peExpected err)
       ]
```

---

## ขั้นตอนที่ 808: Semantic Analysis

```haskell
-- Semantic analysis pass

module SemanticAnalysis where

-- Name resolution
type Scope = Map Text (Binding Text)

data Binding a
  = BVar   a Type
  | BFn    a [Type] Type
  | BType  a
  | BClass a [Method]
  deriving (Show)

-- Resolve names in AST
resolveNames :: Expr -> StateT Scope (Either NameError) ResolvedExpr
resolveNames (EVar name) = do
  scope <- get
  case Map.lookup name scope of
    Nothing -> throwError (NameError (UnboundName name))
    Just b  -> return (REVar name b)

resolveNames (ELambda params body) = do
  -- Push new scope
  let paramScope = Map.fromList [(n, BVar n t) | (n, Just t) <- params]
  (body', _) <- local (<> paramScope) (resolveNames body)
  return (RELambda params body')

resolveNames (ELet pat ty expr body) = do
  expr' <- resolveNames expr
  -- Bind pattern variables
  let vars = patternVars pat
  body' <- local (Map.union (Map.fromList [(v, BVar v ty') | (v, ty') <- vars])) (resolveNames body)
  return (RELet pat ty expr' body')

-- Check for unused variables
checkUnused :: ResolvedExpr -> Writer [Warning] ()
checkUnused expr = do
  let used = usedVars expr
  let bound = boundVars expr
  mapM_ (\v -> tell [UnusedVariable v]) (bound \\ used)

-- Check exhaustiveness of pattern matches
checkExhaustiveness :: Type -> [Pattern] -> Either ExhaustivenessError ()
checkExhaustiveness ty pats = do
  let missing = computeMissing ty pats
  unless (null missing) (throwError (NonExhaustive missing))
```

---

## ขั้นตอนที่ 809: Interpreter

```haskell
-- Tree-walking interpreter

module Interpreter where

import qualified Data.Map.Strict as Map
import Data.IORef

-- Values
data Value
  = VInt     Int
  | VFloat   Double
  | VBool    Bool
  | VStr     Text
  | VNull
  | VList    [Value]
  | VTuple   [Value]
  | VFn      [Text] Block Env
  | VBuiltin Text ([Value] -> IO Value)
  | VObject  (IORef (Map Text Value))

type Env = Map Text (IORef Value)

-- Evaluate expression
eval :: Env -> Expr -> IO Value
eval env (ELit (LInt n))  = return (VInt n)
eval env (ELit (LBool b)) = return (VBool b)
eval env (ELit (LStr s))  = return (VStr s)
eval env (ELit LNull)     = return VNull

eval env (EVar name) = case Map.lookup name env of
  Nothing  -> throwIO (RuntimeError (UndefinedVar name))
  Just ref -> readIORef ref

eval env (EApp fn args) = do
  fnVal   <- eval env fn
  argVals <- mapM (eval env) args
  apply fnVal argVals

eval env (EIf cond then' else') = do
  condVal <- eval env cond
  case condVal of
    VBool True  -> eval env then'
    VBool False -> eval env else'
    _           -> throwIO (TypeError "Condition must be boolean")

-- Apply function
apply :: Value -> [Value] -> IO Value
apply (VFn params body closureEnv) args
  | length params /= length args =
      throwIO (RuntimeError (ArityMismatch (length params) (length args)))
  | otherwise = do
      argRefs <- mapM newIORef args
      let localEnv = Map.fromList (zip params argRefs) `Map.union` closureEnv
      execBlock localEnv body

apply (VBuiltin _ fn) args = fn args
apply v _ = throwIO (TypeError ("Not a function: " <> showValue v))

-- Execute statements
execBlock :: Env -> Block -> IO Value
execBlock env stmts = go stmts
  where
    go [] = return VNull
    go (SReturn e : _) = eval env e
    go (s : rest) = exec env s >> go rest
```

---

## ขั้นตอนที่ 810: REPL Implementation

```haskell
-- Read-Eval-Print Loop

module Repl where

import System.Console.Haskeline

data ReplState = ReplState
  { rsEnv     :: Env
  , rsHistory :: [Text]
  , rsScope   :: TyEnv
  }

-- Run REPL
runRepl :: IO ()
runRepl = do
  state <- newIORef initialState
  runInputT defaultSettings (loop state)
  where
    loop state = do
      minput <- getInputLine "λ> "
      case minput of
        Nothing    -> return ()
        Just ""    -> loop state
        Just ":q"  -> outputStrLn "Goodbye!"
        Just ":h"  -> printHelp >> loop state
        Just (':':'t':' ':expr) -> typeCheck state expr >> loop state
        Just input -> do
          result <- liftIO (evalInput state input)
          case result of
            Left err  -> outputStrLn ("Error: " ++ T.unpack err) >> loop state
            Right val -> do
              outputStrLn (showValue val)
              loop state

evalInput :: IORef ReplState -> String -> IO (Either Text Value)
evalInput stateRef input = do
  state <- readIORef stateRef
  case tokenize (T.pack input) >>= parseExpr of
    Left err   -> return (Left (prettyParseError (T.pack input) err))
    Right expr ->
      catch
        (do result <- eval (rsEnv state) expr
            return (Right result))
        (\(RuntimeError msg) -> return (Left msg))

-- Multi-line input support
getMultilineInput :: InputT IO (Maybe String)
getMultilineInput = do
  firstLine <- getInputLine "λ> "
  case firstLine of
    Nothing    -> return Nothing
    Just line  ->
      if needsContinuation line
        then do rest <- getContinuation
                return (Just (line ++ rest))
        else return (Just line)

needsContinuation :: String -> Bool
needsContinuation s = length (filter (== '{') s) /= length (filter (== '}') s)
```

---

## ขั้นตอนที่ 811: JIT Compilation Concepts

```haskell
-- JIT compilation ideas (conceptual Haskell representation)

-- IR (Intermediate Representation)
data IR
  = IRConst   Int
  | IRVar     Int        -- register/variable index
  | IRBinOp   IRBinOp IR IR
  | IRLoad    Int        -- load from memory address
  | IRStore   Int IR     -- store to memory
  | IRBranch  IR IR IR   -- conditional branch
  | IRCall    Text [IR]
  | IRPhi     [(IR, BasicBlock)]
  | IRReturn  IR
  deriving (Show)

data IRBinOp = IRAdd | IRSub | IRMul | IRDiv | IRCmp CmpOp
data CmpOp   = CEq | CNe | CLt | CGt | CLe | CGe

-- Basic block
data BasicBlock = BasicBlock
  { bbLabel :: Text
  , bbInstrs :: [IR]
  , bbTerminator :: IR
  }

-- Control flow graph
data CFG = CFG
  { cfgEntry  :: BasicBlock
  , cfgBlocks :: Map Text BasicBlock
  }

-- Simple bytecode VM
data Bytecode
  = PUSH Int
  | POP
  | ADD | SUB | MUL | DIV
  | LOAD Int  -- load from stack slot
  | STORE Int -- store to stack slot
  | JMP  Int  -- jump to offset
  | JIF  Int  -- conditional jump
  | CALL Text
  | RET
  deriving (Show)

-- Run bytecode
runBytecode :: [Bytecode] -> [Int] -> IO [Int]
runBytecode program initialStack = go 0 initialStack
  where
    instrArr = listArray (0, length program - 1) program
    
    go pc stack
      | pc >= length program = return stack
      | otherwise = case instrArr ! pc of
          PUSH n -> go (pc+1) (n : stack)
          POP    -> go (pc+1) (tail stack)
          ADD    -> let (b:a:rest) = stack in go (pc+1) ((a+b):rest)
          JMP n  -> go n stack
          JIF n  -> let (c:rest) = stack
                    in go (if c /= 0 then n else pc+1) rest
          RET    -> return stack
          _      -> go (pc+1) stack
```

---

## ขั้นตอนที่ 812: Language Extensions

```haskell
-- Macro system / language extensions

-- Template for DSL generation
data MacroDef = MacroDef
  { mdName    :: Text
  , mdParams  :: [Text]
  , mdBody    :: Expr
  }

-- Expand macros
expandMacro :: Map Text MacroDef -> Expr -> Expr
expandMacro macros (App (Var name) args) =
  case Map.lookup name macros of
    Just macro ->
      let subst = Map.fromList (zip (mdParams macro) args)
      in expandMacro macros (substituteExpr subst (mdBody macro))
    Nothing -> App (Var name) (map (expandMacro macros) args)
expandMacro macros (BinOp op l r) =
  BinOp op (expandMacro macros l) (expandMacro macros r)
expandMacro _ e = e

-- Quasi-quotation (like Template Haskell)
data Quoted
  = QExpr Expr
  | QSplice Expr    -- unquote with $()
  | QList  [Quoted]

expandQuote :: Quoted -> Expr
expandQuote (QExpr e)    = App (Var "mkExpr") [quoteLiteral e]
expandQuote (QSplice e)  = e
expandQuote (QList qs)   = App (Var "mkList") [App (Var "list") (map expandQuote qs)]

-- Syntax extensions
data SyntaxExt
  = DoNotation    [DoStatement]
  | ListComp      Expr [Guard]
  | ArrowNotation

desugareDoNotation :: [DoStatement] -> Expr
desugareDoNotation [DSReturn e] = App (Var "return") [e]
desugareDoNotation (DSBind pat e : rest) =
  App (App (Var ">>=") [e]) [Lambda [patToVar pat] (desugareDoNotation rest)]
desugareDoNotation (DSExpr e : rest) =
  App (App (Var ">>") [e]) [desugareDoNotation rest]
desugareDoNotation (DSLet pat e : rest) =
  App (Lambda [patToVar pat] (desugareDoNotation rest)) [e]
```

---

## ขั้นตอนที่ 813: Garbage Collection Concepts

```haskell
-- GC concepts (conceptual implementation)

-- Simple mark-and-sweep GC
data HeapObject = HeapObject
  { hoData    :: Value
  , hoMarked  :: IORef Bool
  , hoRefs    :: [IORef HeapObject]
  }

data GCState = GCState
  { gcHeap   :: IORef [HeapObject]
  , gcRoots  :: IORef [IORef HeapObject]
  }

-- Mark phase: mark all reachable objects
markPhase :: GCState -> IO ()
markPhase gc = do
  roots <- readIORef (gcRoots gc)
  mapM_ markReachable roots

markReachable :: IORef HeapObject -> IO ()
markReachable ref = do
  obj <- readIORef ref
  already <- readIORef (hoMarked obj)
  unless already $ do
    writeIORef (hoMarked obj) True
    mapM_ markReachable (hoRefs obj)

-- Sweep phase: collect unmarked objects
sweepPhase :: GCState -> IO Int
sweepPhase gc = do
  heap <- readIORef (gcHeap gc)
  (alive, dead) <- partitionM (isMarked) heap
  
  -- Unmark alive objects for next cycle
  mapM_ unmark alive
  
  -- Return dead objects to free list
  writeIORef (gcHeap gc) alive
  return (length dead)

isMarked :: HeapObject -> IO Bool
isMarked = readIORef . hoMarked

unmark :: HeapObject -> IO ()
unmark obj = writeIORef (hoMarked obj) False

-- Generational GC concepts
data GenerationalGC = GenerationalGC
  { ggcGen0     :: IORef [HeapObject]  -- nursery (most allocations)
  , ggcGen1     :: IORef [HeapObject]  -- older objects
  , ggcGen2     :: IORef [HeapObject]  -- long-lived objects
  , ggcAllocCnt :: IORef Int
  }
```

---

## ขั้นตอนที่ 814: Linker and Loader

```haskell
-- Linker concepts

data ObjectFile = ObjectFile
  { ofName      :: FilePath
  , ofSymbols   :: [(Text, Symbol)]
  , ofRelocsToApply :: [Relocation]
  , ofSections  :: Map Text ByteString
  }

data Symbol = Symbol
  { symName     :: Text
  , symOffset   :: Int
  , symSection  :: Text
  , symExternal :: Bool
  }

data Relocation = Relocation
  { relOffset  :: Int
  , relSymbol  :: Text
  , relType    :: RelocType
  }

data RelocType = RelAbsolute | RelRelative | RelPLT | RelGOT

-- Link multiple object files
link :: [ObjectFile] -> Either LinkError ExecutableImage
link objs = do
  -- Collect all symbols
  let allSymbols = Map.fromList (concatMap ofSymbols objs)
  
  -- Check for undefined symbols
  let externalRefs = concatMap (map relSymbol . ofRelocsToApply) objs
  let undefined'   = filter (\s -> Map.notMember s allSymbols) externalRefs
  unless (null undefined') (Left (UndefinedSymbols undefined'))
  
  -- Assign addresses
  let (layout, baseAddr) = layoutSections objs
  
  -- Apply relocations
  sections <- mapM (applyRelocs allSymbols layout) (concatMap ofRelocsToApply objs)
  
  return ExecutableImage
    { eiCode    = Map.lookup ".text" layout
    , eiData    = Map.lookup ".data" layout
    , eiEntryPoint = baseAddr
    }
```

---

## ขั้นตอนที่ 815: Profiling and Analysis

```haskell
-- Compiler analysis tools

-- Control flow analysis
computeDominators :: CFG -> Map BasicBlock (Set BasicBlock)
computeDominators cfg =
  let n      = cfgBlocks cfg
      entry  = cfgEntry cfg
      allBBs = Set.fromList (Map.elems n)
      
      initDoms = Map.insert entry (Set.singleton entry)
                $ Map.fromList [(b, allBBs) | b <- Map.elems n, b /= entry]
      
  in fixpoint updateDoms initDoms
  where
    updateDoms doms = Map.mapWithKey (\bb _ ->
      Set.insert bb $
      Set.foldl' Set.intersection Set.empty
        (Set.fromList [fromMaybe Set.empty (Map.lookup pred doms) | pred <- predecessors cfg bb])
      ) doms

-- Live variable analysis
liveVariables :: CFG -> Map BasicBlock (Set Text, Set Text)
liveVariables cfg = fixpoint updateLive initialLive
  where
    initialLive = Map.map (\bb -> (computeUseDef bb)) (cfgBlocks cfg)
    
    computeUseDef bb =
      let instrs = bbInstrs bb
      in foldl' addInstr (Set.empty, Set.empty) instrs
    
    updateLive liveMap = Map.mapWithKey (\bb (use, def) ->
      let successors = cfgSuccessors cfg bb
          liveOut    = Set.unions [fst (liveMap Map.! s) | s <- successors]
          liveIn     = use `Set.union` (liveOut `Set.difference` def)
      in (liveIn, def)) liveMap

-- Static analysis: detect potential null dereferences
nullabilityAnalysis :: Program -> [Warning]
nullabilityAnalysis prog = concatMap checkDecl (progDecls prog)
  where
    checkDecl (DeclFn fn) =
      let nullable  = Map.fromList [(p, isNullable ty) | (p, Just ty) <- fnParams fn]
          warnings  = runWriter (checkBlock nullable (fnBody fn))
      in execWriter warnings
    
    isNullable (TyName "Nullable") = True
    isNullable (TyApp (TyName "Maybe") _) = True
    isNullable _ = False
```

---

## ขั้นตอนที่ 816: Bytecode Optimization

```haskell
-- Bytecode-level optimizations

-- Peephole optimization
peepholeOptimize :: [Bytecode] -> [Bytecode]
peepholeOptimize [] = []
peepholeOptimize (PUSH a : PUSH b : ADD : rest) =
  PUSH (a + b) : peepholeOptimize rest  -- constant fold
peepholeOptimize (PUSH 0 : ADD : rest) =
  peepholeOptimize rest                  -- identity elimination
peepholeOptimize (PUSH 1 : MUL : rest) =
  peepholeOptimize rest
peepholeOptimize (STORE n : LOAD n' : rest) | n == n' =
  STORE n : peepholeOptimize rest         -- store-load peephole
peepholeOptimize (i : rest) =
  i : peepholeOptimize rest

-- Register allocation (linear scan)
data RegInterval = RegInterval
  { riVar   :: Int
  , riStart :: Int
  , riEnd   :: Int
  , riReg   :: Maybe Register
  }

linearScanAlloc :: [RegInterval] -> Map Int Register
linearScanAlloc intervals =
  let sorted   = sortBy (comparing riStart) intervals
      numRegs  = 8  -- number of available registers
      freeRegs = Set.fromList [0..numRegs-1]
  in Map.fromList $ snd $ foldl' (allocate) (freeRegs, []) sorted
  where
    allocate (free, allocs) interval
      | Set.null free =
          -- Spill longest-lived interval
          let spilled = minimumBy (comparing riEnd) (map fst allocs)
          in undefined  -- spill to memory
      | otherwise =
          let reg = Set.findMin free
          in (Set.delete reg free, (riVar interval, reg) : allocs)
```

---

## ขั้นตอนที่ 817: Debug Information

```haskell
-- Debug info generation (DWARF-like)

data DebugInfo = DebugInfo
  { diCompileUnit :: CompileUnit
  , diSubprograms :: [Subprogram]
  , diTypes       :: [DIType]
  , diVariables   :: [DIVariable]
  }

data CompileUnit = CompileUnit
  { cuFile     :: FilePath
  , cuLanguage :: Text
  , cuProducer :: Text
  }

data Subprogram = Subprogram
  { spName      :: Text
  , spFile      :: FilePath
  , spLine      :: Int
  , spType      :: DIType
  , spVariables :: [DIVariable]
  }

data DIType
  = DIBasicType { dtName :: Text, dtSize :: Int }
  | DIPointerType { dtBase :: DIType }
  | DICompositeType { dtName :: Text, dtMembers :: [(Text, DIType)] }

data DIVariable = DIVariable
  { dvName     :: Text
  , dvType     :: DIType
  , dvLocation :: Location
  }

data Location
  = InRegister Register
  | InMemory   { locBase :: Register, locOffset :: Int }

-- Generate debug info from typed AST
generateDebugInfo :: Program -> TypedProgram -> DebugInfo
generateDebugInfo prog typed = DebugInfo
  { diCompileUnit = CompileUnit
      { cuFile     = "input.lang"
      , cuLanguage = "MyLanguage"
      , cuProducer = "MyCompiler v1.0"
      }
  , diSubprograms = map genSubprogram (zip (progDecls prog) (tpDecls typed))
  , diTypes       = collectTypes typed
  , diVariables   = collectVariables typed
  }
```

---

## ขั้นตอนที่ 818: Source Maps

```haskell
-- Source maps for debugging

data SourceMap = SourceMap
  { smVersion  :: Int
  , smFile     :: FilePath
  , smSources  :: [FilePath]
  , smMappings :: Text  -- VLQ encoded
  }

-- Source location
data SrcLoc = SrcLoc
  { slFile   :: Int   -- index into sources
  , slLine   :: Int
  , slColumn :: Int
  }

-- Target location
data DstLoc = DstLoc
  { dlLine   :: Int
  , dlColumn :: Int
  }

-- Build source map
buildSourceMap :: [(DstLoc, SrcLoc)] -> SourceMap
buildSourceMap mappings =
  let sortedMappings = sortBy (comparing fst) mappings
      encoded        = encodeVLQ sortedMappings
  in SourceMap
       { smVersion  = 3
       , smFile     = "output.js"
       , smSources  = []
       , smMappings = encoded
       }

-- VLQ encoding (base64)
encodeVLQ :: [(DstLoc, SrcLoc)] -> Text
encodeVLQ = T.concat . intersperse ";" . map encodeGroup . groupByLine
  where
    encodeGroup group = T.concat . intersperse "," $ map encodeMapping group
    
    encodeMapping (DstLoc _ col, SrcLoc file line srcCol) =
      encodeBase64VLQ [col, file, line, srcCol]

-- Decode source map
lookupSourceLocation :: SourceMap -> DstLoc -> Maybe SrcLoc
lookupSourceLocation sm dst = do
  let mappings = decodeMappings (smMappings sm)
  findNearest dst mappings
```

---

## ขั้นตอนที่ 819: Language Server Protocol

```haskell
-- Language Server Protocol implementation

module Lsp where

import Data.Aeson (ToJSON, FromJSON)

-- LSP message types
data LspRequest = LspRequest
  { lspId     :: Maybe Int
  , lspMethod :: Text
  , lspParams :: Value
  } deriving (Show, FromJSON, ToJSON)

data LspResponse = LspResponse
  { lspResId     :: Maybe Int
  , lspResult    :: Maybe Value
  , lspError     :: Maybe LspError
  } deriving (Show, ToJSON)

data LspError = LspError
  { lspErrorCode    :: Int
  , lspErrorMessage :: Text
  } deriving (Show, ToJSON)

-- Document sync
data TextDocumentItem = TextDocumentItem
  { tdiUri        :: Text
  , tdiLanguageId :: Text
  , tdiVersion    :: Int
  , tdiText       :: Text
  }

-- Handle LSP request
handleRequest :: LspState -> LspRequest -> IO LspResponse
handleRequest state req = case lspMethod req of
  "initialize"              -> handleInitialize state req
  "textDocument/completion" -> handleCompletion state req
  "textDocument/hover"      -> handleHover state req
  "textDocument/definition" -> handleDefinition state req
  "textDocument/diagnostic" -> handleDiagnostics state req
  _                         -> return (methodNotFound req)

-- Completion provider
handleCompletion :: LspState -> LspRequest -> IO LspResponse
handleCompletion state req = do
  let params = parseParams req :: CompletionParams
  let doc    = lsDocuments state Map.! cpTextDocument params
  let pos    = cpPosition params
  
  completions <- getCompletionsAtPosition doc pos state
  return (success (lspId req) (toJSON completions))

-- Hover info
handleHover :: LspState -> LspRequest -> IO LspResponse
handleHover state req = do
  let params = parseParams req :: HoverParams
  let doc    = lsDocuments state Map.! hpTextDocument params
  
  case getTypeAtPosition doc (hpPosition params) state of
    Nothing -> return (success (lspId req) Null)
    Just ty -> return (success (lspId req) (toJSON (Hover (T.pack (showType ty)))))
```

---

## ขั้นตอนที่ 820: โปรเจกต์: Mini Language Compiler

```haskell
-- Complete mini-language compiler pipeline

module Main where

-- Compiler pipeline
data CompilerConfig = CompilerConfig
  { ccOptimize   :: Bool
  , ccDebugInfo  :: Bool
  , ccTarget     :: CompileTarget
  , ccOutputFile :: FilePath
  }

data CompileTarget = TargetBytecode | TargetLLVM | TargetJS

compile :: CompilerConfig -> FilePath -> IO ()
compile cfg srcFile = do
  -- Read source
  src <- T.readFile srcFile
  
  -- Lexing
  tokens <- case tokenize src of
    Left err  -> die ("Lex error: " ++ T.unpack (prettyLexError err))
    Right tks -> return tks
  
  -- Parsing
  ast <- case parseProgram tokens of
    Left err  -> die ("Parse error: " ++ T.unpack (prettyParseError src err))
    Right ast -> return ast
  
  -- Semantic analysis
  (resolvedAst, warnings) <- case resolveNames ast of
    Left err   -> die ("Name error: " ++ T.unpack (prettyNameError err))
    Right (r, w) -> mapM_ (putStrLn . ("Warning: " ++) . T.unpack . prettyWarning) w >> return (r, w)
  
  -- Type checking
  (typedAst, typeEnv) <- case inferTypes resolvedAst of
    Left err   -> die ("Type error: " ++ T.unpack (prettyTypeError err))
    Right r    -> return r
  
  -- Optimization (if enabled)
  let optimized = if ccOptimize cfg
        then optimize typedAst
        else typedAst
  
  -- Code generation
  case ccTarget cfg of
    TargetBytecode -> do
      bytecode <- generateBytecode optimized
      when (ccDebugInfo cfg) (generateDebugInfo typedAst)
      writeBytecode (ccOutputFile cfg) bytecode
    
    TargetLLVM -> do
      llvmIr <- generateLLVM optimized
      writeLLVM (ccOutputFile cfg) llvmIr
    
    TargetJS -> do
      js <- generateJS optimized
      writeJS (ccOutputFile cfg) js
  
  putStrLn ("Compiled " ++ srcFile ++ " -> " ++ ccOutputFile cfg)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing  -> printUsage
    Just cfg -> compile cfg (cfgInput cfg)
```

---

*[← Part 40](part-40.md) | [Part 42 →](part-42.md)*
