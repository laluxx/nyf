{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Style where

import qualified Data.Text as T
import Data.Char (isUpper, isLower, isDigit)
import qualified AST
import Ops.LintOps.Core

-- Line Length Check
data LineLengthCheck = LineLengthCheck

instance LintCheck LineLengthCheck where
  checkName _ = "line-length"
  checkCategory _ = Style
  runCheck _ ctx = checkLineLength ctx

checkLineLength :: AnalysisContext -> [LintIssue]
checkLineLength ctx =
  let lines' = T.lines (ctxFileContent ctx)
      maxLen = cfgMaxLineLength (ctxConfig ctx)
      longLines = [(n, line) | (n, line) <- zip [1..] lines',
                                T.length line > maxLen,
                                not (T.isPrefixOf ";;" line)]
  in [LintIssue (ctxFilePath ctx) (Just n) (Just 1)
       Info Style "ST001"
       ("Line exceeds " ++ show maxLen ++ " characters (" ++
        show (T.length ln) ++ " chars)")
       Nothing Nothing
     | (n, ln) <- longLines]

-- Naming Convention Check
data NamingConventionCheck = NamingConventionCheck

instance LintCheck NamingConventionCheck where
  checkName _ = "naming-convention"
  checkCategory _ = Style
  runCheck _ ctx = checkNamingConventions ctx

checkNamingConventions :: AnalysisContext -> [LintIssue]
checkNamingConventions ctx =
  let fnIssues = checkFunctionNames ctx
      constIssues = checkConstantNames ctx
  in fnIssues ++ constIssues

checkFunctionNames :: AnalysisContext -> [LintIssue]
checkFunctionNames ctx =
  let functions = [name | AST.FnDecl name _ _ _ _ <- ctxDecls ctx]
  in [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("fn " <> name))
       Info Style "ST002"
       ("Function '" ++ T.unpack name ++ "' should use snake_case naming")
       Nothing
     | name <- functions, not (isSnakeCase name), name /= "main"]

checkConstantNames :: AnalysisContext -> [LintIssue]
checkConstantNames ctx =
  let constants = [name | AST.DefineDecl names _ <- ctxDecls ctx, name <- names]
  in [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("define " <> name))
       Info Style "ST003"
       ("Constant '" ++ T.unpack name ++ "' should use UPPER_CASE naming")
       Nothing
     | name <- constants, not (isUpperCase name)]

isSnakeCase :: T.Text -> Bool
isSnakeCase t = T.all (\c -> isLower c || isDigit c || c == '_') t

isUpperCase :: T.Text -> Bool
isUpperCase t = T.all (\c -> isUpper c || isDigit c || c == '_') t

-- Trailing Whitespace Check
data TrailingWhitespaceCheck = TrailingWhitespaceCheck

instance LintCheck TrailingWhitespaceCheck where
  checkName _ = "trailing-whitespace"
  checkCategory _ = Style
  runCheck _ ctx = checkTrailingWhitespace ctx

checkTrailingWhitespace :: AnalysisContext -> [LintIssue]
checkTrailingWhitespace ctx =
  let lines' = T.lines (ctxFileContent ctx)
      trailingWS = [(n, line) | (n, line) <- zip [1..] lines',
                                 not (T.null line),
                                 T.last line == ' ' || T.last line == '\t']
  in [LintIssue (ctxFilePath ctx) (Just n) (Just (T.length line))
       Info Style "ST004"
       "Line has trailing whitespace"
       (Just "Remove trailing spaces")
       Nothing
     | (n, line) <- trailingWS]

-- Import Order Check
data ImportOrderCheck = ImportOrderCheck

instance LintCheck ImportOrderCheck where
  checkName _ = "import-order"
  checkCategory _ = Style
  runCheck _ ctx = checkImportOrder ctx

checkImportOrder :: AnalysisContext -> [LintIssue]
checkImportOrder ctx = [] -- TODO: Implement
