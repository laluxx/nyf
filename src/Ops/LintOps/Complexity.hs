{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Complexity where

import qualified Data.Text as T
import qualified AST
import Ops.LintOps.Core

-- ============================================================================
-- CYCLOMATIC COMPLEXITY CHECK
-- ============================================================================

data CyclomaticComplexityCheck = CyclomaticComplexityCheck

instance LintCheck CyclomaticComplexityCheck where
  checkName _ = "cyclomatic-complexity"
  checkCategory _ = Complexity
  runCheck _ ctx = checkCyclomaticComplexity ctx

checkCyclomaticComplexity :: AnalysisContext -> [LintIssue]
checkCyclomaticComplexity ctx =
  concatMap (checkDeclComplexity ctx) (ctxDecls ctx)

checkDeclComplexity :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclComplexity ctx (AST.FnDecl name _ _ _ stmts) =
  let complexity = calculateComplexity stmts
      maxComplexity = cfgMaxComplexity (ctxConfig ctx)
  in if complexity > maxComplexity
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Warning Complexity "C001"
            ("Function '" ++ T.unpack name ++ "' has cyclomatic complexity of " ++
             show complexity ++ " (max: " ++ show maxComplexity ++ ")")
            (Just "Consider breaking this function into smaller functions")]
     else []
checkDeclComplexity _ _ = []

-- Calculate cyclomatic complexity: M = E - N + 2P
-- Simplified: count decision points + 1
calculateComplexity :: [AST.Stmt] -> Int
calculateComplexity stmts = 1 + sum (map countDecisionPoints stmts)

countDecisionPoints :: AST.Stmt -> Int
countDecisionPoints stmt = case stmt of
  AST.IfStmt _ thn els ->
    1 + sum (map countDecisionPoints thn) +
    maybe 0 (sum . map countDecisionPoints) els

  AST.WhileStmt _ body ->
    1 + sum (map countDecisionPoints body)

  AST.ForStmt _ _ body ->
    1 + sum (map countDecisionPoints body)

  AST.MatchStmt _ arms defaultCase ->
    length arms +
    sum (map (sum . map countDecisionPoints . AST.matchConseq) arms) +
    maybe 0 (sum . map countDecisionPoints) defaultCase

  AST.TryStmt tryBody _ catchBody ->
    1 + sum (map countDecisionPoints tryBody) +
    sum (map countDecisionPoints catchBody)

  AST.BlockStmt stmts' ->
    sum (map countDecisionPoints stmts')

  -- Also count logical operators in expressions as decision points
  AST.ExprStmt expr -> countExprDecisionPoints expr
  AST.VarDecl _ expr -> countExprDecisionPoints expr
  AST.Assign _ expr -> countExprDecisionPoints expr

  _ -> 0

countExprDecisionPoints :: AST.Expr -> Int
countExprDecisionPoints expr = case expr of
  AST.LogicalOp AST.And _ _ -> 1
  AST.LogicalOp AST.Or _ _ -> 1
  AST.BinOp _ left right ->
    countExprDecisionPoints left + countExprDecisionPoints right
  AST.UnOp _ e -> countExprDecisionPoints e
  AST.Call _ args ->
    sum (map (countExprDecisionPoints . AST.argValue) args)
  _ -> 0

-- ============================================================================
-- FUNCTION LENGTH CHECK
-- ============================================================================

data FunctionLengthCheck = FunctionLengthCheck

instance LintCheck FunctionLengthCheck where
  checkName _ = "function-length"
  checkCategory _ = Complexity
  runCheck _ ctx = checkFunctionLength ctx

checkFunctionLength :: AnalysisContext -> [LintIssue]
checkFunctionLength ctx =
  concatMap (checkDeclLength ctx) (ctxDecls ctx)

checkDeclLength :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclLength ctx (AST.FnDecl name _ _ _ stmts) =
  let lineCount = countStatementLines stmts
      maxLength = cfgMaxFunctionLength (ctxConfig ctx)
  in if lineCount > maxLength
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Warning Complexity "C002"
            ("Function '" ++ T.unpack name ++ "' is too long (" ++
             show lineCount ++ " statements, max: " ++ show maxLength ++ ")")
            (Just "Consider breaking this function into smaller functions")]
     else []
checkDeclLength _ _ = []

countStatementLines :: [AST.Stmt] -> Int
countStatementLines = sum . map countStmtLines

countStmtLines :: AST.Stmt -> Int
countStmtLines stmt = case stmt of
  AST.IfStmt _ thn els ->
    1 + countStatementLines thn + maybe 0 countStatementLines els
  AST.WhileStmt _ body ->
    1 + countStatementLines body
  AST.ForStmt _ _ body ->
    1 + countStatementLines body
  AST.BlockStmt stmts ->
    countStatementLines stmts
  AST.MatchStmt _ arms defaultCase ->
    1 + sum (map (countStatementLines . AST.matchConseq) arms) +
    maybe 0 countStatementLines defaultCase
  AST.TryStmt tryBody _ catchBody ->
    1 + countStatementLines tryBody + countStatementLines catchBody
  _ -> 1

-- ============================================================================
-- PARAMETER COUNT CHECK
-- ============================================================================

data ParameterCountCheck = ParameterCountCheck

instance LintCheck ParameterCountCheck where
  checkName _ = "parameter-count"
  checkCategory _ = Complexity
  runCheck _ ctx = checkParameterCount ctx

checkParameterCount :: AnalysisContext -> [LintIssue]
checkParameterCount ctx =
  concatMap (checkDeclParamCount ctx) (ctxDecls ctx)

checkDeclParamCount :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclParamCount ctx (AST.FnDecl name params _ _ _) =
  let paramCount = length params
      maxParams = cfgMaxParameters (ctxConfig ctx)
  in if paramCount > maxParams
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Warning Complexity "C003"
            ("Function '" ++ T.unpack name ++ "' has too many parameters (" ++
             show paramCount ++ ", max: " ++ show maxParams ++ ")")
            (Just "Consider using a parameter object or breaking up the function")]
     else []
checkDeclParamCount _ _ = []

-- ============================================================================
-- COGNITIVE COMPLEXITY CHECK
-- ============================================================================

data CognitiveComplexityCheck = CognitiveComplexityCheck

instance LintCheck CognitiveComplexityCheck where
  checkName _ = "cognitive-complexity"
  checkCategory _ = Complexity
  runCheck _ ctx = checkCognitiveComplexity ctx

checkCognitiveComplexity :: AnalysisContext -> [LintIssue]
checkCognitiveComplexity ctx =
  concatMap (checkDeclCognitiveComplexity ctx) (ctxDecls ctx)

checkDeclCognitiveComplexity :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclCognitiveComplexity ctx (AST.FnDecl name _ _ _ stmts) =
  let complexity = calculateCognitiveComplexity stmts 0
      -- Use same max as cyclomatic for now
      maxComplexity = cfgMaxComplexity (ctxConfig ctx)
  in if complexity > maxComplexity + 5  -- Cognitive is typically higher
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Info Complexity "C004"
            ("Function '" ++ T.unpack name ++ "' has high cognitive complexity (" ++
             show complexity ++ ")")
            (Just "Simplify control flow and reduce nesting")]
     else []
checkDeclCognitiveComplexity _ _ = []

-- Cognitive complexity considers nesting depth and control flow breaks
calculateCognitiveComplexity :: [AST.Stmt] -> Int -> Int
calculateCognitiveComplexity stmts nestLevel =
  sum (map (calcStmtCognitiveComplexity nestLevel) stmts)

calcStmtCognitiveComplexity :: Int -> AST.Stmt -> Int
calcStmtCognitiveComplexity nestLevel stmt = case stmt of
  AST.IfStmt _ thn els ->
    let baseScore = 1 + nestLevel  -- if adds 1 + nesting penalty
        thnScore = calculateCognitiveComplexity thn (nestLevel + 1)
        elsScore = maybe 0 (\e -> 1 + calculateCognitiveComplexity e (nestLevel + 1)) els
    in baseScore + thnScore + elsScore

  AST.WhileStmt _ body ->
    1 + nestLevel + calculateCognitiveComplexity body (nestLevel + 1)

  AST.ForStmt _ _ body ->
    1 + nestLevel + calculateCognitiveComplexity body (nestLevel + 1)

  AST.MatchStmt _ arms defaultCase ->
    let armScore = sum (map (\arm ->
          1 + calculateCognitiveComplexity (AST.matchConseq arm) (nestLevel + 1)) arms)
        defaultScore = maybe 0 (\d ->
          1 + calculateCognitiveComplexity d (nestLevel + 1)) defaultCase
    in armScore + defaultScore

  AST.TryStmt tryBody _ catchBody ->
    1 + nestLevel +
    calculateCognitiveComplexity tryBody (nestLevel + 1) +
    calculateCognitiveComplexity catchBody (nestLevel + 1)

  AST.BreakStmt -> 1  -- Break in control flow
  AST.ContinueStmt -> 1  -- Continue in control flow

  AST.BlockStmt stmts' ->
    calculateCognitiveComplexity stmts' nestLevel

  _ -> 0

-- ============================================================================
-- EXCESSIVE NESTING CHECK
-- ============================================================================

data ExcessiveNestingCheck = ExcessiveNestingCheck

instance LintCheck ExcessiveNestingCheck where
  checkName _ = "excessive-nesting"
  checkCategory _ = Complexity
  runCheck _ ctx = checkExcessiveNesting ctx

checkExcessiveNesting :: AnalysisContext -> [LintIssue]
checkExcessiveNesting ctx =
  concatMap (checkDeclNesting ctx) (ctxDecls ctx)

checkDeclNesting :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclNesting ctx (AST.FnDecl name _ _ _ stmts) =
  let maxDepth = findMaxNestingDepth stmts 0
      maxAllowed = cfgMaxNestingDepth (ctxConfig ctx)
  in if maxDepth > maxAllowed
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Warning Complexity "C005"
            ("Function '" ++ T.unpack name ++ "' has excessive nesting (" ++
             show maxDepth ++ " levels, max: " ++ show maxAllowed ++ ")")
            (Just "Reduce nesting by extracting functions or using early returns")]
     else []
checkDeclNesting _ _ = []

findMaxNestingDepth :: [AST.Stmt] -> Int -> Int
findMaxNestingDepth stmts currentDepth =
  maximum (currentDepth : map (calcStmtDepth currentDepth) stmts)

calcStmtDepth :: Int -> AST.Stmt -> Int
calcStmtDepth currentDepth stmt = case stmt of
  AST.IfStmt _ thn els ->
    let newDepth = currentDepth + 1
        thnDepth = findMaxNestingDepth thn newDepth
        elsDepth = maybe currentDepth (findMaxNestingDepth `flip` newDepth) els
    in max thnDepth elsDepth

  AST.WhileStmt _ body ->
    findMaxNestingDepth body (currentDepth + 1)

  AST.ForStmt _ _ body ->
    findMaxNestingDepth body (currentDepth + 1)

  AST.BlockStmt stmts' ->
    findMaxNestingDepth stmts' currentDepth

  AST.MatchStmt _ arms defaultCase ->
    let armDepths = map (\arm ->
          findMaxNestingDepth (AST.matchConseq arm) (currentDepth + 1)) arms
        defaultDepth = maybe currentDepth (findMaxNestingDepth `flip` (currentDepth + 1)) defaultCase
    in maximum (currentDepth : defaultDepth : armDepths)

  AST.TryStmt tryBody _ catchBody ->
    max (findMaxNestingDepth tryBody (currentDepth + 1))
        (findMaxNestingDepth catchBody (currentDepth + 1))

  _ -> currentDepth
