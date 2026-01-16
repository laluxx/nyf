{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Documentation where

import qualified Data.Text as T
import Data.Char (isUpper)
import qualified AST
import Ops.LintOps.Core

-- Missing Docstring Check
data MissingDocstringCheck = MissingDocstringCheck

instance LintCheck MissingDocstringCheck where
  checkName _ = "missing-docstring"
  checkCategory _ = Documentation
  runCheck _ ctx = checkMissingDocstrings ctx

checkMissingDocstrings :: AnalysisContext -> [LintIssue]
checkMissingDocstrings ctx =
  let functions = [(name, doc) | AST.FnDecl name _ _ doc _ <- ctxDecls ctx]
      missingDocs = [name | (name, Nothing) <- functions, name /= "main"]
  in [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("fn " <> name))
       Info Documentation "D001"
       ("Function '" ++ T.unpack name ++ "' is missing a docstring")
       (Just "Add a docstring describing what the function does")
     | name <- missingDocs]

-- Docstring Quality Check
data DocstringQualityCheck = DocstringQualityCheck

instance LintCheck DocstringQualityCheck where
  checkName _ = "docstring-quality"
  checkCategory _ = Documentation
  runCheck _ ctx = checkDocstringQuality ctx

checkDocstringQuality :: AnalysisContext -> [LintIssue]
checkDocstringQuality ctx =
  concatMap (checkFunctionDocstring ctx) (ctxDecls ctx)

checkFunctionDocstring :: AnalysisContext -> AST.Decl -> [LintIssue]
checkFunctionDocstring ctx (AST.FnDecl name params _ (Just doc) _) =
  let issues = []
      -- Check if docstring ends with period
      periodIssue = if not (T.null doc) && not (T.isSuffixOf "." doc)
                    then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("fn " <> name))
                           Warning Documentation "D002"
                           ("Docstring for '" ++ T.unpack name ++ "' should end with a period")
                           Nothing]
                    else []

      -- Check if first character is uppercase
      capIssue = if not (T.null doc) && not (isUpper (T.head doc))
                 then [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just ("fn " <> name))
                        Warning Documentation "D003"
                        ("Docstring for '" ++ T.unpack name ++ "' should start with uppercase")
                        Nothing]
                 else []
  in issues ++ periodIssue ++ capIssue
checkFunctionDocstring _ _ = []
