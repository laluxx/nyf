{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--- CORE TYPES AND INFRASTRUCTURE

module Ops.LintOps.Core where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)
import Data.List (nub, sortOn, partition)
import qualified AST

--- Lint Issue Types

data LintIssue = LintIssue
  { issueFile :: FilePath
  , issueLine :: Maybe Int
  , issueColumn :: Maybe Int
  , issueLevel :: LintLevel
  , issueCategory :: IssueCategory
  , issueCode :: String
  , issueMessage :: String
  , issueSuggestion :: Maybe String
  , issueAutoFix :: Maybe AutoFix
  } deriving (Show, Eq)

data LintLevel = Error | Warning | Info
  deriving (Show, Eq, Ord)

data IssueCategory
  = Correctness     -- Bugs that will cause runtime errors
  | Safety          -- Memory safety, resource management
  | Security        -- Security vulnerabilities
  | Performance     -- Performance issues
  | Maintainability -- Code quality and maintainability
  | Style           -- Code style and conventions
  | Documentation   -- Missing or poor documentation
  | Complexity      -- High complexity warnings
  deriving (Show, Eq, Ord)

data AutoFix = AutoFix
  { fixDescription :: String
  , fixOldText :: T.Text
  , fixNewText :: T.Text
  } deriving (Show, Eq)

--- Analysis Context

data AnalysisContext = AnalysisContext
  { ctxFilePath :: FilePath
  , ctxFileContent :: T.Text
  , ctxDecls :: [AST.Decl]
  , ctxConfig :: LintConfig
  , ctxTypeEnv :: TypeEnvironment
  , ctxSymbolTable :: SymbolTable
  } deriving (Show, Eq)

data LintConfig = LintConfig
  { cfgMaxLineLength :: Int
  , cfgMaxComplexity :: Int
  , cfgMaxFunctionLength :: Int
  , cfgMaxParameters :: Int
  , cfgMaxNestingDepth :: Int
  , cfgEnabledChecks :: Set.Set String
  , cfgDisabledChecks :: Set.Set String
  , cfgTreatWarningsAsErrors :: Bool
  , cfgIgnorePatterns :: [T.Text]
  } deriving (Show, Eq)

defaultConfig :: LintConfig
defaultConfig = LintConfig
  { cfgMaxLineLength = 100
  , cfgMaxComplexity = 10
  , cfgMaxFunctionLength = 50
  , cfgMaxParameters = 6
  , cfgMaxNestingDepth = 4
  , cfgEnabledChecks = Set.empty
  , cfgDisabledChecks = Set.empty
  , cfgTreatWarningsAsErrors = False
  , cfgIgnorePatterns = []
  }

--- Type System

data InferredType
  = TInt | TFloat | TStr | TBool | TUnit
  | TList InferredType
  | TTuple [InferredType]
  | TDict InferredType InferredType
  | TFn [InferredType] InferredType
  | TStruct T.Text [(T.Text, InferredType)]
  | TUnknown
  | TAny
  | TNullable InferredType
  | TUnion [InferredType]
  | TVar Int  -- Type variable for unification
  deriving (Show, Eq)

type TypeEnvironment = Map.Map T.Text TypeScheme

data TypeScheme = TypeScheme
  { schemeVars :: [Int]  -- Quantified type variables
  , schemeType :: InferredType
  } deriving (Show, Eq)

--- Symbol Table

data SymbolTable = SymbolTable
  { symGlobals :: Map.Map T.Text SymbolInfo
  , symFunctions :: Map.Map T.Text FunctionInfo
  , symTypes :: Map.Map T.Text TypeInfo
  } deriving (Show, Eq)

data SymbolInfo = SymbolInfo
  { symName :: T.Text
  , symType :: InferredType
  , symMutable :: Bool
  , symDefined :: Int   -- Line number
  , symUsages :: [Int]  -- Line numbers where used
  } deriving (Show, Eq)

data FunctionInfo = FunctionInfo
  { fnName :: T.Text
  , fnParams :: [AST.Param]
  , fnReturnType :: Maybe InferredType
  , fnBody :: [AST.Stmt]
  , fnCalls :: [T.Text]     -- Functions this calls
  , fnCalledBy :: [T.Text]  -- Functions that call this
  , fnComplexity :: Int
  } deriving (Show, Eq)

data TypeInfo = TypeInfo
  { typeName :: T.Text
  , typeDefinition :: InferredType
  } deriving (Show, Eq)

--- Control Flow Graph

data CFG = CFG
  { cfgNodes :: Map.Map Int CFGNode
  , cfgEntry :: Int
  , cfgExit :: Int
  } deriving (Show, Eq)

data CFGNode = CFGNode
  { nodeId :: Int
  , nodeStmts :: [AST.Stmt]
  , nodeSuccessors :: [Int]
  , nodePredecessors :: [Int]
  , nodeType :: NodeType
  } deriving (Show, Eq)

data NodeType
  = EntryNode
  | ExitNode
  | BasicBlock
  | BranchNode
  | LoopHeader
  | LoopExit
  deriving (Show, Eq)

--- Data Flow Facts

data DataFlowFacts = DataFlowFacts
  { dfReachingDefs :: Map.Map Int (Set.Set Definition)
  , dfLiveVars :: Map.Map Int (Set.Set T.Text)
  , dfAvailableExprs :: Map.Map Int (Set.Set AST.Expr)
  , dfConstantValues :: Map.Map T.Text ConstantValue
  } deriving (Show, Eq)

data Definition = Definition
  { defVar :: T.Text
  , defNode :: Int
  , defValue :: Maybe AST.Expr
  } deriving (Show, Eq)

instance Ord Definition where
  compare (Definition v1 n1 _) (Definition v2 n2 _) =
    compare (v1, n1) (v2, n2) -- Compare only var name and node, ignore expr

data ConstantValue
  = ConstInt Integer
  | ConstFloat Double
  | ConstStr T.Text
  | ConstBool Bool
  | NotConstant
  deriving (Show, Eq, Ord)

--- Lint Check Interface

class LintCheck a where
  checkName :: a -> String
  checkCategory :: a -> IssueCategory
  runCheck :: a -> AnalysisContext -> [LintIssue]
  isEnabled :: a -> LintConfig -> Bool
  isEnabled check cfg =
    let name = checkName check
    in not (name `Set.member` cfgDisabledChecks cfg) &&
       (Set.null (cfgEnabledChecks cfg) || name `Set.member` cfgEnabledChecks cfg)

--- Helper Functions

mkIssue :: FilePath -> T.Text -> Maybe T.Text -> LintLevel -> IssueCategory
        -> String -> String -> Maybe String -> LintIssue
mkIssue path content pattern level category code msg suggestion =
  let lineNum = case pattern of
        Just p -> findLineNumber content p
        Nothing -> Nothing
  in LintIssue
    { issueFile = path
    , issueLine = lineNum
    , issueColumn = Just 1
    , issueLevel = level
    , issueCategory = category
    , issueCode = code
    , issueMessage = msg
    , issueSuggestion = suggestion
    , issueAutoFix = Nothing
    }

findLineNumber :: T.Text -> T.Text -> Maybe Int
findLineNumber content pattern =
  let lines' = T.lines content
      matches = [(n, line) | (n, line) <- zip [1..] lines', pattern `T.isInfixOf` line]
  in case matches of
    ((n, _):_) -> Just n
    [] -> Nothing

--- Type Utilities

isNumericType :: InferredType -> Bool
isNumericType TInt = True
isNumericType TFloat = True
isNumericType _ = False

typesCompatible :: InferredType -> InferredType -> Bool
typesCompatible TAny _ = True
typesCompatible _ TAny = True
typesCompatible TUnknown _ = True
typesCompatible _ TUnknown = True
typesCompatible (TNullable t1) t2 = typesCompatible t1 t2
typesCompatible t1 (TNullable t2) = typesCompatible t1 t2
typesCompatible (TList t1) (TList t2) = typesCompatible t1 t2
typesCompatible (TTuple ts1) (TTuple ts2) =
  length ts1 == length ts2 && and (zipWith typesCompatible ts1 ts2)
typesCompatible t1 t2 = t1 == t2

unifyTypes :: InferredType -> InferredType -> Maybe InferredType
unifyTypes TAny t = Just t
unifyTypes t TAny = Just t
unifyTypes TUnknown t = Just t
unifyTypes t TUnknown = Just t
unifyTypes (TList t1) (TList t2) = TList <$> unifyTypes t1 t2
unifyTypes (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = TTuple <$> sequence (zipWith unifyTypes ts1 ts2)
  | otherwise = Nothing
unifyTypes t1 t2
  | t1 == t2 = Just t1
  | otherwise = Nothing

-- Build initial analysis context
buildContext :: FilePath -> T.Text -> [AST.Decl] -> LintConfig -> AnalysisContext
buildContext path content decls config =
  let typeEnv = buildTypeEnvironment decls
      symTable = buildSymbolTable decls
  in AnalysisContext
    { ctxFilePath = path
    , ctxFileContent = content
    , ctxDecls = decls
    , ctxConfig = config
    , ctxTypeEnv = typeEnv
    , ctxSymbolTable = symTable
    }

-- Build type environment from declarations
buildTypeEnvironment :: [AST.Decl] -> TypeEnvironment
buildTypeEnvironment decls = Map.fromList $ concatMap extractTypes decls
  where
    extractTypes :: AST.Decl -> [(T.Text, TypeScheme)]
    extractTypes (AST.DefineDecl names expr) =
      let inferredType = inferExprType Map.empty expr
      in [(name, TypeScheme [] inferredType) | name <- names]
    extractTypes (AST.FnDecl name params retType _ _) =
      let paramTypes = map (maybe TUnknown (const TUnknown) . AST.paramType) params
          returnType = maybe TUnknown (const TUnknown) retType
          fnType = TFn paramTypes returnType
      in [(name, TypeScheme [] fnType)]
    extractTypes _ = []

-- Build symbol table
buildSymbolTable :: [AST.Decl] -> SymbolTable
buildSymbolTable decls =
  SymbolTable
    { symGlobals = Map.fromList $ concatMap extractGlobals decls
    , symFunctions = Map.fromList $ mapMaybe extractFunction decls
    , symTypes = Map.empty
    }
  where
    extractGlobals :: AST.Decl -> [(T.Text, SymbolInfo)]
    extractGlobals (AST.DefineDecl names _) =
      [(name, SymbolInfo name TUnknown False 0 []) | name <- names]
    extractGlobals _ = []

    extractFunction :: AST.Decl -> Maybe (T.Text, FunctionInfo)
    extractFunction (AST.FnDecl name params retType _ stmts) =
      Just (name, FunctionInfo
        { fnName = name
        , fnParams = params
        , fnReturnType = Nothing
        , fnBody = stmts
        , fnCalls = []
        , fnCalledBy = []
        , fnComplexity = 0
        })
    extractFunction _ = Nothing

-- Infer expression type
inferExprType :: TypeEnvironment -> AST.Expr -> InferredType
inferExprType _ (AST.IntLit _) = TInt
inferExprType _ (AST.FloatLit _) = TFloat
inferExprType _ (AST.StrLit _) = TStr
inferExprType _ (AST.BoolLit _) = TBool
inferExprType env (AST.ListLit exprs) =
  case exprs of
    [] -> TList TUnknown
    (e:_) -> TList (inferExprType env e)
inferExprType env (AST.Var name) =
  maybe TUnknown schemeType (Map.lookup name env)
inferExprType env (AST.BinOp op l r) =
  let lt = inferExprType env l
      rt = inferExprType env r
  in case op of
    AST.Add -> if isNumericType lt then lt else TUnknown
    AST.Sub -> if isNumericType lt then lt else TUnknown
    AST.Mul -> if isNumericType lt then lt else TUnknown
    AST.Div -> if isNumericType lt then lt else TUnknown
    AST.Eq -> TBool
    AST.Neq -> TBool
    AST.Lt -> TBool
    AST.Gt -> TBool
    AST.Le -> TBool
    AST.Ge -> TBool
    _ -> TUnknown
inferExprType _ _ = TUnknown
