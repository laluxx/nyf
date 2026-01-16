{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Security where

import qualified Data.Text as T
import qualified AST
import Ops.LintOps.Core

-- SQL Injection Check
data SqlInjectionCheck = SqlInjectionCheck

instance LintCheck SqlInjectionCheck where
  checkName _ = "sql-injection"
  checkCategory _ = Security
  runCheck _ ctx = checkSqlInjection ctx

checkSqlInjection :: AnalysisContext -> [LintIssue]
checkSqlInjection ctx =
  concatMap (checkDeclSqlInjection ctx) (ctxDecls ctx)

checkDeclSqlInjection :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclSqlInjection ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtSqlInjection ctx name) stmts
checkDeclSqlInjection _ _ = []

checkStmtSqlInjection :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtSqlInjection ctx fnName stmt =
  case stmt of
    AST.ExprStmt expr -> checkExprSqlInjection ctx fnName expr
    AST.VarDecl _ expr -> checkExprSqlInjection ctx fnName expr
    _ -> []

checkExprSqlInjection :: AnalysisContext -> T.Text -> AST.Expr -> [LintIssue]
checkExprSqlInjection ctx fnName expr =
  case expr of
    -- Check for string concatenation in SQL queries
    AST.Call (AST.Var fn) args | fn `elem` ["query", "execute", "exec"] ->
      let hasStringConcat = any (isStringConcatArg . AST.argValue) args
      in if hasStringConcat
         then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just fn)
                Error Security "S001"
                ("Potential SQL injection in function '" ++ T.unpack fnName ++
                 "' - avoid string concatenation in SQL queries")
                (Just "Use parameterized queries instead")]
         else []
    _ -> []
  where
    isStringConcatArg (AST.BinOp AST.Add (AST.StrLit _) _) = True
    isStringConcatArg (AST.BinOp AST.Add _ (AST.StrLit _)) = True
    isStringConcatArg _ = False

-- Path Traversal Check
data PathTraversalCheck = PathTraversalCheck

instance LintCheck PathTraversalCheck where
  checkName _ = "path-traversal"
  checkCategory _ = Security
  runCheck _ ctx = checkPathTraversal ctx

checkPathTraversal :: AnalysisContext -> [LintIssue]
checkPathTraversal ctx =
  concatMap (checkDeclPathTraversal ctx) (ctxDecls ctx)

checkDeclPathTraversal :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclPathTraversal ctx (AST.FnDecl name _ _ _ stmts) =
  concatMap (checkStmtPathTraversal ctx name) stmts
checkDeclPathTraversal _ _ = []

checkStmtPathTraversal :: AnalysisContext -> T.Text -> AST.Stmt -> [LintIssue]
checkStmtPathTraversal ctx fnName stmt =
  case stmt of
    AST.ExprStmt expr -> checkExprPathTraversal ctx fnName expr
    _ -> []

checkExprPathTraversal :: AnalysisContext -> T.Text -> AST.Expr -> [LintIssue]
checkExprPathTraversal ctx fnName expr =
  case expr of
    AST.Call (AST.Var fn) _args | fn `elem` ["open", "read_file", "readFile"] ->
      -- Check if path contains user input without validation
      [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just fn)
        Warning Security "S002"
        ("Potential path traversal in function '" ++ T.unpack fnName ++
         "' - validate file paths from user input")
        (Just "Sanitize paths and check for '..' segments")]
    _ -> []

-- Hardcoded Secret Check
data HardcodedSecretCheck = HardcodedSecretCheck

instance LintCheck HardcodedSecretCheck where
  checkName _ = "hardcoded-secret"
  checkCategory _ = Security
  runCheck _ ctx = checkHardcodedSecrets ctx

checkHardcodedSecrets :: AnalysisContext -> [LintIssue]
checkHardcodedSecrets ctx =
  let content = ctxFileContent ctx
      lines' = T.lines content
      suspiciousPatterns = ["password", "secret", "api_key", "apikey", "token", "key"]
  in concatMap (checkLineForSecret ctx suspiciousPatterns) (zip [1..] lines')

checkLineForSecret :: AnalysisContext -> [T.Text] -> (Int, T.Text) -> [LintIssue]
checkLineForSecret ctx patterns (lineNum, line) =
  let lowerLine = T.toLower line
      hasPattern = any (`T.isInfixOf` lowerLine) patterns
      hasAssignment = "=" `T.isInfixOf` line
      hasQuotedString = "\"" `T.isInfixOf` line || "'" `T.isInfixOf` line
  in if hasPattern && hasAssignment && hasQuotedString
     then [LintIssue (ctxFilePath ctx) (Just lineNum) (Just 1)
            Warning Security "S003"
            "Potential hardcoded secret detected"
            (Just "Use environment variables or a secrets manager")
            Nothing]
     else []

-- Insecure Random Check
data InsecureRandomCheck = InsecureRandomCheck

instance LintCheck InsecureRandomCheck where
  checkName _ = "insecure-random"
  checkCategory _ = Security
  runCheck _ ctx = checkInsecureRandom ctx

checkInsecureRandom :: AnalysisContext -> [LintIssue]
checkInsecureRandom ctx = [] -- TODO: Implement
