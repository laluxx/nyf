{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.DeadCode where

import qualified Data.Text as T
import qualified AST
import Ops.LintOps.Core

-- ============================================================================
-- DEAD CODE DETECTION
-- ============================================================================

data DeadCodeCheck = DeadCodeCheck

instance LintCheck DeadCodeCheck where
  checkName _ = "dead-code"
  checkCategory _ = Correctness
  runCheck _ ctx = checkDeadCode ctx

checkDeadCode :: AnalysisContext -> [LintIssue]
checkDeadCode ctx =
  concatMap (checkDeclDeadCode ctx) (ctxDecls ctx)

checkDeclDeadCode :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclDeadCode ctx (AST.FnDecl name _ _ _ stmts) =
  checkStmtsDeadCode ctx name stmts False
checkDeclDeadCode _ _ = []

-- Check for dead code after returns, breaks, continues
checkStmtsDeadCode :: AnalysisContext -> T.Text -> [AST.Stmt] -> Bool -> [LintIssue]
checkStmtsDeadCode _ _ [] _ = []
checkStmtsDeadCode ctx fnName (stmt:rest) alreadyReturned =
  if alreadyReturned
  then -- This statement is dead code
    [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) Nothing
      Warning Correctness "E007"
      ("Dead code in function '" ++ T.unpack fnName ++
       "': unreachable statement after return/break/continue")
      (Just "Remove unreachable code")] ++
    checkStmtsDeadCode ctx fnName rest True
  else
    let stmtIssues = checkStmtDeadCode ctx fnName stmt
        doesReturn = stmtAlwaysReturns stmt
    in stmtIssues ++ checkStmtsDeadCode ctx fnName rest doesReturn

checkStmtDeadCode :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtDeadCode ctx fnName stmt =
  case stmt of
    AST.IfStmt _ thn els ->
      let thnIssues = checkStmtsDeadCode ctx fnName thn False
          elsIssues = maybe [] (\e -> checkStmtsDeadCode ctx fnName e False) els
      in thnIssues ++ elsIssues

    AST.WhileStmt _ body ->
      checkStmtsDeadCode ctx fnName body False

    AST.ForStmt _ _ body ->
      checkStmtsDeadCode ctx fnName body False

    AST.BlockStmt stmts ->
      checkStmtsDeadCode ctx fnName stmts False

    AST.TryStmt tryBody _ catchBody ->
      checkStmtsDeadCode ctx fnName tryBody False ++
      checkStmtsDeadCode ctx fnName catchBody False

    AST.MatchStmt _ arms defaultCase ->
      concatMap (\arm -> checkStmtsDeadCode ctx fnName (AST.matchConseq arm) False) arms ++
      maybe [] (\d -> checkStmtsDeadCode ctx fnName d False) defaultCase

    _ -> []

-- Check if a statement always returns/breaks/continues
stmtAlwaysReturns :: AST.Stmt -> Bool
stmtAlwaysReturns stmt =
  case stmt of
    AST.ReturnStmt _ -> True
    AST.BreakStmt -> True
    AST.ContinueStmt -> True
    AST.IfStmt _ thn (Just els) ->
      -- If both branches return, the if statement returns
      stmtsAlwaysReturn thn && stmtsAlwaysReturn els
    AST.BlockStmt stmts -> stmtsAlwaysReturn stmts
    _ -> False

stmtsAlwaysReturn :: [AST.Stmt] -> Bool
stmtsAlwaysReturn [] = False
stmtsAlwaysReturn stmts = any stmtAlwaysReturns stmts

-- ============================================================================
-- UNREACHABLE CODE IN LOOPS
-- ============================================================================

data UnreachableLoopCheck = UnreachableLoopCheck

instance LintCheck UnreachableLoopCheck where
  checkName _ = "unreachable-loop"
  checkCategory _ = Correctness
  runCheck _ ctx = checkUnreachableLoop ctx

checkUnreachableLoop :: AnalysisContext -> [LintIssue]
checkUnreachableLoop ctx =
  concatMap (checkDeclUnreachableLoop ctx) (ctxDecls ctx)

checkDeclUnreachableLoop :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclUnreachableLoop ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtUnreachableLoop ctx name) stmts
checkDeclUnreachableLoop _ _ = []

checkStmtUnreachableLoop :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtUnreachableLoop ctx fnName stmt =
  case stmt of
    -- Loop with condition that's always false
    AST.WhileStmt (AST.BoolLit False) _ ->
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just "while")
        Warning Correctness "E008"
        ("Unreachable loop in function '" ++ T.unpack fnName ++
         "': condition is always false")
        (Just "Remove the loop or fix the condition")]

    AST.WhileStmt (AST.IntLit 0) _ ->
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just "while")
        Warning Correctness "E008"
        ("Unreachable loop in function '" ++ T.unpack fnName ++
         "': condition is always false")
        (Just "Remove the loop or fix the condition")]

    AST.WhileStmt _ body ->
      concatMap (checkStmtUnreachableLoop ctx fnName) body

    AST.ForStmt _ _ body ->
      concatMap (checkStmtUnreachableLoop ctx fnName) body

    AST.IfStmt _ thn els ->
      concatMap (checkStmtUnreachableLoop ctx fnName) thn ++
      maybe [] (concatMap (checkStmtUnreachableLoop ctx fnName)) els

    AST.BlockStmt stmts ->
      concatMap (checkStmtUnreachableLoop ctx fnName) stmts

    _ -> []

-- ============================================================================
-- REDUNDANT CONDITION CHECK
-- ============================================================================

data RedundantConditionCheck = RedundantConditionCheck

instance LintCheck RedundantConditionCheck where
  checkName _ = "redundant-condition"
  checkCategory _ = Maintainability
  runCheck _ ctx = checkRedundantCondition ctx

checkRedundantCondition :: AnalysisContext -> [LintIssue]
checkRedundantCondition ctx =
  concatMap (checkDeclRedundantCondition ctx) (ctxDecls ctx)

checkDeclRedundantCondition :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclRedundantCondition ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtRedundantCondition ctx name) stmts
checkDeclRedundantCondition _ _ = []

checkStmtRedundantCondition :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtRedundantCondition ctx fnName stmt =
  case stmt of
    -- Nested if with same condition
    AST.IfStmt cond1 thn els ->
      let nestedIssues = checkForNestedSameCondition ctx fnName cond1 thn
          thnIssues = concatMap (checkStmtRedundantCondition ctx fnName) thn
          elsIssues = maybe [] (concatMap (checkStmtRedundantCondition ctx fnName)) els
      in nestedIssues ++ thnIssues ++ elsIssues

    AST.WhileStmt _ body ->
      concatMap (checkStmtRedundantCondition ctx fnName) body

    AST.ForStmt _ _ body ->
      concatMap (checkStmtRedundantCondition ctx fnName) body

    AST.BlockStmt stmts ->
      concatMap (checkStmtRedundantCondition ctx fnName) stmts

    _ -> []

checkForNestedSameCondition :: AnalysisContext -> T.Text -> AST.Expr -> [AST.Stmt] -> [LintIssue]
checkForNestedSameCondition ctx fnName outerCond stmts =
  concatMap checkStmt stmts
  where
    checkStmt (AST.IfStmt innerCond _ _)
      | exprsEqual outerCond innerCond =
        [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) Nothing
          Warning Maintainability "M001"
          ("Redundant nested condition in function '" ++ T.unpack fnName ++
           "': inner if has same condition as outer if")
          (Just "Remove the redundant nested if")]
    checkStmt _ = []

-- Simple structural equality for expressions
exprsEqual :: AST.Expr -> AST.Expr -> Bool
exprsEqual (AST.Var v1) (AST.Var v2) = v1 == v2
exprsEqual (AST.IntLit i1) (AST.IntLit i2) = i1 == i2
exprsEqual (AST.BoolLit b1) (AST.BoolLit b2) = b1 == b2
exprsEqual (AST.BinOp op1 l1 r1) (AST.BinOp op2 l2 r2) =
  op1 == op2 && exprsEqual l1 l2 && exprsEqual r1 r2
exprsEqual _ _ = False
