{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Correctness where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified AST
import Ops.LintOps.Core

--- DIVISION BY ZERO CHECK

data DivisionByZeroCheck = DivisionByZeroCheck

instance LintCheck DivisionByZeroCheck where
  checkName _ = "division-by-zero"
  checkCategory _ = Correctness
  runCheck _ ctx = checkDivisionByZero ctx

checkDivisionByZero :: AnalysisContext -> [LintIssue]
checkDivisionByZero ctx =
  concatMap (checkDeclDivByZero ctx) (ctxDecls ctx)

checkDeclDivByZero :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclDivByZero ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtDivByZero ctx name) stmts
checkDeclDivByZero _ _ = []

checkStmtDivByZero :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtDivByZero ctx fnName stmt =
  case stmt of
    AST.ExprStmt expr -> checkExprDivByZero ctx fnName expr
    AST.VarDecl _ expr -> checkExprDivByZero ctx fnName expr
    AST.Assign _ expr -> checkExprDivByZero ctx fnName expr
    AST.CompoundAssign _ _ expr -> checkExprDivByZero ctx fnName expr
    AST.IfStmt cond thn els ->
      checkExprDivByZero ctx fnName cond ++
      concatMap (checkStmtDivByZero ctx fnName) thn ++
      maybe [] (concatMap (checkStmtDivByZero ctx fnName)) els
    AST.WhileStmt cond body ->
      checkExprDivByZero ctx fnName cond ++
      concatMap (checkStmtDivByZero ctx fnName) body
    AST.ForStmt _ expr body ->
      checkExprDivByZero ctx fnName expr ++
      concatMap (checkStmtDivByZero ctx fnName) body
    AST.ReturnStmt (Just expr) -> checkExprDivByZero ctx fnName expr
    AST.BlockStmt stmts -> concatMap (checkStmtDivByZero ctx fnName) stmts
    _ -> []

checkExprDivByZero :: AnalysisContext -> T.Text -> AST.Expr -> [LintIssue]
checkExprDivByZero ctx fnName expr =
  case expr of
    AST.BinOp AST.Div left right | isZero right ->
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("/" <> T.pack (show right)))
        Error Correctness "E001"
        ("Division by zero in function '" ++ T.unpack fnName ++ "'")
        (Just "Check the divisor value")]

    AST.BinOp AST.Mod left right | isZero right ->
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("%" <> T.pack (show right)))
        Error Correctness "E002"
        ("Modulo by zero in function '" ++ T.unpack fnName ++ "'")
        (Just "Check the divisor value")]

    AST.BinOp _ left right ->
      checkExprDivByZero ctx fnName left ++ checkExprDivByZero ctx fnName right

    AST.LogicalOp _ left right ->
      checkExprDivByZero ctx fnName left ++ checkExprDivByZero ctx fnName right

    AST.UnOp _ e -> checkExprDivByZero ctx fnName e

    AST.Call _ args ->
      concatMap (checkExprDivByZero ctx fnName . AST.argValue) args

    AST.MemberCall obj _ args ->
      checkExprDivByZero ctx fnName obj ++
      concatMap (checkExprDivByZero ctx fnName . AST.argValue) args

    AST.Index base start end step ->
      checkExprDivByZero ctx fnName base ++
      maybe [] (checkExprDivByZero ctx fnName) start ++
      maybe [] (checkExprDivByZero ctx fnName) end ++
      maybe [] (checkExprDivByZero ctx fnName) step

    AST.ListLit exprs -> concatMap (checkExprDivByZero ctx fnName) exprs
    AST.TupleLit exprs -> concatMap (checkExprDivByZero ctx fnName) exprs
    AST.DictLit pairs ->
      concatMap (\(k, v) -> checkExprDivByZero ctx fnName k ++ checkExprDivByZero ctx fnName v) pairs

    _ -> []

isZero :: AST.Expr -> Bool
isZero (AST.IntLit 0) = True
isZero (AST.FloatLit 0.0) = True
isZero _ = False

--- ARRAY BOUNDS CHECK

data ArrayBoundsCheck = ArrayBoundsCheck

instance LintCheck ArrayBoundsCheck where
  checkName _ = "array-bounds"
  checkCategory _ = Correctness
  runCheck _ ctx = checkArrayBounds ctx

checkArrayBounds :: AnalysisContext -> [LintIssue]
checkArrayBounds ctx =
  concatMap (checkDeclArrayBounds ctx) (ctxDecls ctx)

checkDeclArrayBounds :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclArrayBounds ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtArrayBounds ctx name) stmts
checkDeclArrayBounds _ _ = []

checkStmtArrayBounds :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtArrayBounds ctx fnName stmt =
  case stmt of
    AST.ExprStmt expr -> checkExprArrayBounds ctx fnName expr
    AST.VarDecl _ expr -> checkExprArrayBounds ctx fnName expr
    AST.Assign _ expr -> checkExprArrayBounds ctx fnName expr
    AST.IfStmt _ thn els ->
      concatMap (checkStmtArrayBounds ctx fnName) thn ++
      maybe [] (concatMap (checkStmtArrayBounds ctx fnName)) els
    AST.WhileStmt _ body -> concatMap (checkStmtArrayBounds ctx fnName) body
    AST.ForStmt _ _ body -> concatMap (checkStmtArrayBounds ctx fnName) body
    AST.BlockStmt stmts -> concatMap (checkStmtArrayBounds ctx fnName) stmts
    _ -> []

checkExprArrayBounds :: AnalysisContext -> T.Text -> AST.Expr -> [LintIssue]
checkExprArrayBounds ctx fnName expr =
  case expr of
    -- Check list literal indexing
    AST.Index (AST.ListLit items) (Just (AST.IntLit idx)) Nothing Nothing ->
      if idx < 0 || idx >= fromIntegral (length items)
      then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just (T.pack $ show idx))
             Warning Correctness "E004"
             ("Array index " ++ show idx ++ " out of bounds in function '" ++
              T.unpack fnName ++ "' (array length: " ++ show (length items) ++ ")")
             (Just "Check array index is within valid range")]
      else []

    -- Negative index check
    AST.Index _ (Just (AST.IntLit idx)) Nothing Nothing | idx < 0 ->
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just (T.pack $ show idx))
        Warning Correctness "E004"
        ("Negative array index " ++ show idx ++ " in function '" ++ T.unpack fnName ++ "'")
        (Just "Use non-negative indices or check for negative values")]

    AST.Index base start end step ->
      checkExprArrayBounds ctx fnName base ++
      maybe [] (checkExprArrayBounds ctx fnName) start ++
      maybe [] (checkExprArrayBounds ctx fnName) end ++
      maybe [] (checkExprArrayBounds ctx fnName) step

    AST.BinOp _ left right ->
      checkExprArrayBounds ctx fnName left ++ checkExprArrayBounds ctx fnName right

    AST.Call _ args ->
      concatMap (checkExprArrayBounds ctx fnName . AST.argValue) args

    AST.ListLit exprs -> concatMap (checkExprArrayBounds ctx fnName) exprs

    _ -> []

--- OFF-BY-ONE CHECK

data OffByOneCheck = OffByOneCheck

instance LintCheck OffByOneCheck where
  checkName _ = "off-by-one"
  checkCategory _ = Correctness
  runCheck _ ctx = checkOffByOne ctx

checkOffByOne :: AnalysisContext -> [LintIssue]
checkOffByOne ctx =
  concatMap (checkDeclOffByOne ctx) (ctxDecls ctx)

checkDeclOffByOne :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclOffByOne ctx (AST.FnDecl name _ _ _ stmts) =
  checkStmtsOffByOne ctx name stmts
checkDeclOffByOne _ _ = []

checkStmtsOffByOne :: AnalysisContext -> T.Text -> [AST.Stmt] -> [LintIssue]
checkStmtsOffByOne ctx fnName stmts =
  concatMap (checkStmtOffByOne ctx fnName) stmts

checkStmtOffByOne :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtOffByOne ctx fnName stmt =
  case stmt of
    -- Check for loop with suspicious condition: i <= length
    AST.WhileStmt (AST.BinOp AST.Le (AST.Var i) lenExpr) body ->
      if isSizeExpression lenExpr
      then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just i)
             Warning Correctness "E005"
             ("Potential off-by-one error in function '" ++ T.unpack fnName ++
              "': loop condition uses '<=' with size expression")
             (Just "Consider using '<' instead of '<=' when comparing with length/size")]
      else checkStmtsOffByOne ctx fnName body

    -- Check for loop with suspicious upper bound
    AST.WhileStmt (AST.BinOp AST.Lt (AST.Var _) (AST.BinOp AST.Add lenExpr (AST.IntLit 1))) body
      | isSizeExpression lenExpr ->
        [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) Nothing
          Warning Correctness "E005"
          ("Potential off-by-one error in function '" ++ T.unpack fnName ++
           "': loop upper bound is 'length + 1'")
          (Just "Verify the loop bounds are correct")]

    AST.IfStmt _ thn els ->
      checkStmtsOffByOne ctx fnName thn ++
      maybe [] (checkStmtsOffByOne ctx fnName) els

    AST.WhileStmt _ body -> checkStmtsOffByOne ctx fnName body
    AST.ForStmt _ _ body -> checkStmtsOffByOne ctx fnName body
    AST.BlockStmt stmts -> checkStmtsOffByOne ctx fnName stmts

    _ -> []

isSizeExpression :: AST.Expr -> Bool
isSizeExpression expr =
  case expr of
    AST.Call (AST.Var fn) _ -> fn `elem` ["len", "length", "size", "list_len"]
    AST.MemberCall _ method _ -> method `elem` ["len", "length", "size"]
    _ -> False

--- INFINITE LOOP CHECK

data InfiniteLoopCheck = InfiniteLoopCheck

instance LintCheck InfiniteLoopCheck where
  checkName _ = "infinite-loop"
  checkCategory _ = Correctness
  runCheck _ ctx = checkInfiniteLoop ctx

checkInfiniteLoop :: AnalysisContext -> [LintIssue]
checkInfiniteLoop ctx =
  concatMap (checkDeclInfiniteLoop ctx) (ctxDecls ctx)

checkDeclInfiniteLoop :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclInfiniteLoop ctx (AST.FnDecl name _ _ _ stmts) =
  checkStmtsInfiniteLoop ctx name stmts
checkDeclInfiniteLoop _ _ = []

checkStmtsInfiniteLoop :: AnalysisContext -> T.Text -> [AST.Stmt] -> [LintIssue]
checkStmtsInfiniteLoop ctx fnName stmts =
  concatMap (checkStmtInfiniteLoop ctx fnName) stmts

checkStmtInfiniteLoop :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtInfiniteLoop ctx fnName stmt =
  case stmt of
    -- while true with no break
    AST.WhileStmt (AST.BoolLit True) body ->
      if not (hasBreakOrReturn body)
      then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just "while")
             Warning Correctness "E006"
             ("Potential infinite loop in function '" ++ T.unpack fnName ++
              "': 'while true' without break or return")
             (Just "Add a break or return statement, or use a proper condition")]
      else checkStmtsInfiniteLoop ctx fnName body

    -- while 1 with no break
    AST.WhileStmt (AST.IntLit n) body | n /= 0 ->
      if not (hasBreakOrReturn body)
      then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just "while")
             Warning Correctness "E006"
             ("Potential infinite loop in function '" ++ T.unpack fnName ++
              "': while loop with constant true condition")
             (Just "Add a break or return statement, or use a proper condition")]
      else checkStmtsInfiniteLoop ctx fnName body

    AST.WhileStmt _ body -> checkStmtsInfiniteLoop ctx fnName body
    AST.ForStmt _ _ body -> checkStmtsInfiniteLoop ctx fnName body
    AST.IfStmt _ thn els ->
      checkStmtsInfiniteLoop ctx fnName thn ++
      maybe [] (checkStmtsInfiniteLoop ctx fnName) els
    AST.BlockStmt stmts -> checkStmtsInfiniteLoop ctx fnName stmts

    _ -> []

hasBreakOrReturn :: [AST.Stmt] -> Bool
hasBreakOrReturn = any containsBreakOrReturn
  where
    containsBreakOrReturn stmt =
      case stmt of
        AST.BreakStmt -> True
        AST.ReturnStmt _ -> True
        AST.IfStmt _ thn els ->
          hasBreakOrReturn thn || maybe False hasBreakOrReturn els
        AST.WhileStmt _ body -> hasBreakOrReturn body
        AST.ForStmt _ _ body -> hasBreakOrReturn body
        AST.BlockStmt stmts -> hasBreakOrReturn stmts
        _ -> False

--- NULL ACCESS CHECK

data NullAccessCheck = NullAccessCheck

instance LintCheck NullAccessCheck where
  checkName _ = "null-access"
  checkCategory _ = Correctness
  runCheck _ ctx = checkNullAccess ctx

checkNullAccess :: AnalysisContext -> [LintIssue]
checkNullAccess ctx = [] -- TODO: Implement proper null checking

--- UNINITIALIZED VARIABLE CHECK

data UninitializedVarCheck = UninitializedVarCheck

instance LintCheck UninitializedVarCheck where
  checkName _ = "uninitialized-var"
  checkCategory _ = Correctness
  runCheck _ ctx = checkUninitializedVars ctx

checkUninitializedVars :: AnalysisContext -> [LintIssue]
checkUninitializedVars ctx =
  concatMap (checkDeclUninit ctx) (ctxDecls ctx)

checkDeclUninit :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclUninit ctx (AST.FnDecl name params _ _ stmts) =
  let paramNames = Set.fromList [AST.paramName p | p <- params]
      -- Also collect all constants defined in the module
      constants = collectConstants (ctxDecls ctx)
      initializedSet = Set.union paramNames constants
      issues = checkStmtsUninit ctx name initializedSet stmts
  in issues
checkDeclUninit _ _ = []

-- Collect all constant names from define declarations
collectConstants :: [AST.Decl] -> Set.Set T.Text
collectConstants decls = Set.fromList $ concatMap getConstNames decls
  where
    getConstNames (AST.DefineDecl names _) = names
    getConstNames _ = []

checkStmtsUninit :: AnalysisContext -> T.Text -> Set.Set T.Text -> [AST.Stmt] -> [LintIssue]
checkStmtsUninit ctx fnName initialized stmts =
  let (issues, _finalInit) = foldl (checkStmtUninit ctx fnName) ([], initialized) stmts
  in issues

checkStmtUninit :: AnalysisContext -> T.Text -> ([LintIssue], Set.Set T.Text) -> AST.Stmt -> ([LintIssue], Set.Set T.Text)
checkStmtUninit ctx fnName (accIssues, initialized) stmt =
  case stmt of
    -- Variable declarations add to initialized set
    AST.VarDecl names expr ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
          newInit = foldl (flip Set.insert) initialized names
      in (accIssues ++ exprIssues, newInit)

    -- Assignments add to initialized set
    AST.Assign (AST.Var varName) expr ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
          newInit = Set.insert varName initialized
      in (accIssues ++ exprIssues, newInit)

    AST.Assign lhs expr ->
      let lhsIssues = checkExprUninit ctx fnName initialized lhs
          exprIssues = checkExprUninit ctx fnName initialized expr
      in (accIssues ++ lhsIssues ++ exprIssues, initialized)

    -- Compound assignments
    AST.CompoundAssign (AST.Var varName) _ expr ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
          newInit = Set.insert varName initialized
      in (accIssues ++ exprIssues, newInit)

    -- Expression statements
    AST.ExprStmt expr ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
      in (accIssues ++ exprIssues, initialized)

    -- If statements
    AST.IfStmt cond thenStmts elseStmts ->
      let condIssues = checkExprUninit ctx fnName initialized cond
          thenIssues = checkStmtsUninit ctx fnName initialized thenStmts
          elseIssues = maybe [] (checkStmtsUninit ctx fnName initialized) elseStmts
      in (accIssues ++ condIssues ++ thenIssues ++ elseIssues, initialized)

    -- While loops
    AST.WhileStmt cond body ->
      let condIssues = checkExprUninit ctx fnName initialized cond
          bodyIssues = checkStmtsUninit ctx fnName initialized body
      in (accIssues ++ condIssues ++ bodyIssues, initialized)

    -- For loops
    AST.ForStmt varName expr body ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
          newInit = Set.insert varName initialized
          bodyIssues = checkStmtsUninit ctx fnName newInit body
      in (accIssues ++ exprIssues ++ bodyIssues, initialized)

    -- Return statements
    AST.ReturnStmt (Just expr) ->
      let exprIssues = checkExprUninit ctx fnName initialized expr
      in (accIssues ++ exprIssues, initialized)

    AST.BlockStmt stmts ->
      let (blockIssues, newInit) = foldl (checkStmtUninit ctx fnName) ([], initialized) stmts
      in (accIssues ++ blockIssues, newInit)

    _ -> (accIssues, initialized)

checkExprUninit :: AnalysisContext -> T.Text -> Set.Set T.Text -> AST.Expr -> [LintIssue]
checkExprUninit ctx fnName initialized expr =
  case expr of
    -- Variable reference - check if initialized
    AST.Var varName ->
      -- Don't check if:
      -- 1. Variable is initialized
      -- 2. It's a known function
      -- 3. The name contains operators (probably a parsing artifact)
      if Set.member varName initialized ||
         isKnownFunction varName ||
         hasOperatorChars varName
      then []
      else [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just varName)
             Error Correctness "E003"
             ("Variable '" ++ T.unpack varName ++ "' in function '" ++
              T.unpack fnName ++ "' may be used before initialization")
             (Just "Initialize the variable with 'def' before using it")]

    -- Function calls - don't check the function name itself
    AST.Call callExpr args ->
      let argIssues = concatMap (checkExprUninit ctx fnName initialized . AST.argValue) args
      in case callExpr of
           AST.Var _fnName -> argIssues
           _ -> checkExprUninit ctx fnName initialized callExpr ++ argIssues

    -- Member calls
    AST.MemberCall objExpr _member args ->
      let objIssues = checkExprUninit ctx fnName initialized objExpr
          argIssues = concatMap (checkExprUninit ctx fnName initialized . AST.argValue) args
      in objIssues ++ argIssues

    -- Binary operations
    AST.BinOp _ left right ->
      checkExprUninit ctx fnName initialized left ++
      checkExprUninit ctx fnName initialized right

    -- Logical operations
    AST.LogicalOp _ left right ->
      checkExprUninit ctx fnName initialized left ++
      checkExprUninit ctx fnName initialized right

    -- Unary operations
    AST.UnOp _ expr' ->
      checkExprUninit ctx fnName initialized expr'

    -- Index operations
    AST.Index baseExpr start end step ->
      let baseIssues = checkExprUninit ctx fnName initialized baseExpr
          startIssues = maybe [] (checkExprUninit ctx fnName initialized) start
          endIssues = maybe [] (checkExprUninit ctx fnName initialized) end
          stepIssues = maybe [] (checkExprUninit ctx fnName initialized) step
      in baseIssues ++ startIssues ++ endIssues ++ stepIssues

    -- List literals
    AST.ListLit exprs ->
      concatMap (checkExprUninit ctx fnName initialized) exprs

    -- Tuple literals
    AST.TupleLit exprs ->
      concatMap (checkExprUninit ctx fnName initialized) exprs

    -- Dict literals
    AST.DictLit pairs ->
      concatMap (\(k, v) -> checkExprUninit ctx fnName initialized k ++
                            checkExprUninit ctx fnName initialized v) pairs

    -- Literals don't use variables
    _ -> []

-- Known runtime/standard library functions
isKnownFunction :: T.Text -> Bool
isKnownFunction name =
  T.isPrefixOf "rt_" name ||
  T.isPrefixOf "sys_" name ||
  name `elem` knownFunctions

knownFunctions :: [T.Text]
knownFunctions =
  [ "fork", "execve", "waitpid", "list", "get", "append", "set"
  , "list_len", "load32", "load64", "store32", "store64"
  , "malloc", "free", "dup2", "kill", "exit"
  , "print", "println", "len", "range", "map", "filter", "reduce"
  , "open", "close", "read", "write", "seek"
  , "sleep", "time", "clock"
  , "args"  -- Variadic argument name
  , "panic", "getitem", "setitem", "concat", "slice", "str_len"
  ]

-- Check if a variable name contains operator characters
-- (which indicates it's probably a parsing artifact we should ignore)
hasOperatorChars :: T.Text -> Bool
hasOperatorChars name = T.any (`elem` operatorChars) name
  where
    operatorChars = ['*', '+', '-', '/', '%', '<', '>', '=', '!', '&', '|', '^']
