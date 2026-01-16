{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Performance where

import qualified Data.Text as T
import qualified AST
import Ops.LintOps.Core

-- Nested Loop Check
data NestedLoopCheck = NestedLoopCheck

instance LintCheck NestedLoopCheck where
  checkName _ = "nested-loops"
  checkCategory _ = Performance
  runCheck _ ctx = checkNestedLoops ctx

checkNestedLoops :: AnalysisContext -> [LintIssue]
checkNestedLoops ctx =
  concatMap (checkDeclNestedLoops ctx) (ctxDecls ctx)

checkDeclNestedLoops :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclNestedLoops ctx (AST.FnDecl name _ _ _ stmts) =
  let depth = findMaxLoopNesting stmts
  in if depth > cfgMaxNestingDepth (ctxConfig ctx)
     then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
            Warning Performance "P001"
            ("Function '" ++ T.unpack name ++ "' has deeply nested loops (depth " ++
             show depth ++ ") - consider refactoring for better performance")
            (Just "Extract inner loops to separate functions or use different algorithms")]
     else []
checkDeclNestedLoops _ _ = []

findMaxLoopNesting :: [AST.Stmt] -> Int
findMaxLoopNesting stmts = maximum (0 : map countNesting stmts)
  where
    countNesting (AST.WhileStmt _ body) = 1 + findMaxLoopNesting body
    countNesting (AST.ForStmt _ _ body) = 1 + findMaxLoopNesting body
    countNesting (AST.IfStmt _ thn els) =
      max (findMaxLoopNesting thn) (maybe 0 findMaxLoopNesting els)
    countNesting _ = 0

-- Inefficient String Concatenation Check
data InefficiientStringConcatCheck = InefficiientStringConcatCheck

instance LintCheck InefficiientStringConcatCheck where
  checkName _ = "inefficient-string-concat"
  checkCategory _ = Performance
  runCheck _ ctx = checkStringConcat ctx

checkStringConcat :: AnalysisContext -> [LintIssue]
checkStringConcat ctx = [] -- TODO: Implement

-- Constant in Loop Check
data ConstantInLoopCheck = ConstantInLoopCheck

instance LintCheck ConstantInLoopCheck where
  checkName _ = "constant-in-loop"
  checkCategory _ = Performance
  runCheck _ ctx = checkConstantInLoop ctx

checkConstantInLoop :: AnalysisContext -> [LintIssue]
checkConstantInLoop ctx = [] -- TODO: Implement
