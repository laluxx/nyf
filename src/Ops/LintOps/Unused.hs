{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Unused where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (nub)
import qualified AST
import Ops.LintOps.Core

-- ============================================================================
-- UNUSED VARIABLE CHECK
-- ============================================================================

data UnusedVariableCheck = UnusedVariableCheck

instance LintCheck UnusedVariableCheck where
  checkName _ = "unused-variable"
  checkCategory _ = Maintainability
  runCheck _ ctx = checkUnusedVariables ctx

checkUnusedVariables :: AnalysisContext -> [LintIssue]
checkUnusedVariables ctx =
  concatMap (checkDeclUnusedVars ctx) (ctxDecls ctx)

checkDeclUnusedVars :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclUnusedVars ctx (AST.FnDecl name params _ _ stmts) =
  let defined = collectDefinedVars stmts
      used = collectUsedVars stmts
      unused = Set.difference defined used
      -- Filter out common "intentionally unused" patterns
      actuallyUnused = Set.filter (not . isIntentionallyUnused) unused
  in [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just var)
       Info Maintainability "M002"
       ("Variable '" ++ T.unpack var ++ "' in function '" ++
        T.unpack name ++ "' is defined but never used")
       (Just "Remove unused variable or prefix with '_' if intentional")
     | var <- Set.toList actuallyUnused]
checkDeclUnusedVars _ _ = []

-- Check if variable name indicates it's intentionally unused
isIntentionallyUnused :: T.Text -> Bool
isIntentionallyUnused name = T.isPrefixOf "_" name

-- Collect all variables defined in statements
collectDefinedVars :: [AST.Stmt] -> Set.Set T.Text
collectDefinedVars stmts = Set.fromList $ concatMap collectDefined stmts
  where
    collectDefined stmt = case stmt of
      AST.VarDecl names _ -> names
      AST.Assign (AST.Var name) _ -> [name]
      AST.ForStmt var _ body -> var : concatMap collectDefined body
      AST.IfStmt _ thn els ->
        concatMap collectDefined thn ++
        maybe [] (concatMap collectDefined) els
      AST.WhileStmt _ body -> concatMap collectDefined body
      AST.BlockStmt stmts' -> concatMap collectDefined stmts'
      AST.TryStmt tryBody mVar catchBody ->
        concatMap collectDefined tryBody ++
        maybe [] (:[]) mVar ++
        concatMap collectDefined catchBody
      _ -> []

-- Collect all variables used in statements
collectUsedVars :: [AST.Stmt] -> Set.Set T.Text
collectUsedVars stmts = Set.fromList $ concatMap collectUsed stmts
  where
    collectUsed stmt = case stmt of
      AST.ExprStmt expr -> collectUsedInExpr expr
      AST.VarDecl _ expr -> collectUsedInExpr expr
      AST.Assign lhs rhs -> collectUsedInExpr lhs ++ collectUsedInExpr rhs
      AST.CompoundAssign lhs _ rhs -> collectUsedInExpr lhs ++ collectUsedInExpr rhs
      AST.IfStmt cond thn els ->
        collectUsedInExpr cond ++
        concatMap collectUsed thn ++
        maybe [] (concatMap collectUsed) els
      AST.WhileStmt cond body ->
        collectUsedInExpr cond ++ concatMap collectUsed body
      AST.ForStmt _ expr body ->
        collectUsedInExpr expr ++ concatMap collectUsed body
      AST.ReturnStmt (Just expr) -> collectUsedInExpr expr
      AST.BlockStmt stmts' -> concatMap collectUsed stmts'
      AST.TryStmt tryBody _ catchBody ->
        concatMap collectUsed tryBody ++ concatMap collectUsed catchBody
      AST.MatchStmt expr arms defaultCase ->
        collectUsedInExpr expr ++
        concatMap (concatMap collectUsed . AST.matchConseq) arms ++
        maybe [] (concatMap collectUsed) defaultCase
      _ -> []

collectUsedInExpr :: AST.Expr -> [T.Text]
collectUsedInExpr expr = case expr of
  AST.Var name -> [name]
  AST.Call callExpr args ->
    collectUsedInExpr callExpr ++
    concatMap (collectUsedInExpr . AST.argValue) args
  AST.MemberCall obj _ args ->
    collectUsedInExpr obj ++
    concatMap (collectUsedInExpr . AST.argValue) args
  AST.Index base start end step ->
    collectUsedInExpr base ++
    maybe [] collectUsedInExpr start ++
    maybe [] collectUsedInExpr end ++
    maybe [] collectUsedInExpr step
  AST.BinOp _ left right ->
    collectUsedInExpr left ++ collectUsedInExpr right
  AST.LogicalOp _ left right ->
    collectUsedInExpr left ++ collectUsedInExpr right
  AST.UnOp _ e -> collectUsedInExpr e
  AST.ListLit exprs -> concatMap collectUsedInExpr exprs
  AST.TupleLit exprs -> concatMap collectUsedInExpr exprs
  AST.DictLit pairs ->
    concatMap (\(k, v) -> collectUsedInExpr k ++ collectUsedInExpr v) pairs
  AST.Lambda params _ body _ ->
    let paramNames = map AST.paramName params
        bodyVars = concatMap collectUsed body
    in filter (`notElem` paramNames) bodyVars
  AST.FnExpr params _ body _ ->
    let paramNames = map AST.paramName params
        bodyVars = concatMap collectUsed body
    in filter (`notElem` paramNames) bodyVars
  _ -> []
  where
    collectUsed stmt = collectUsedInExpr =<< getExprsFromStmt stmt
    getExprsFromStmt (AST.ExprStmt e) = [e]
    getExprsFromStmt (AST.ReturnStmt (Just e)) = [e]
    getExprsFromStmt _ = []

-- ============================================================================
-- UNUSED PARAMETER CHECK
-- ============================================================================

data UnusedParameterCheck = UnusedParameterCheck

instance LintCheck UnusedParameterCheck where
  checkName _ = "unused-parameter"
  checkCategory _ = Maintainability
  runCheck _ ctx = checkUnusedParameters ctx

checkUnusedParameters :: AnalysisContext -> [LintIssue]
checkUnusedParameters ctx =
  concatMap (checkDeclUnusedParams ctx) (ctxDecls ctx)

checkDeclUnusedParams :: AnalysisContext -> AST.Decl -> [LintIssue]
checkDeclUnusedParams ctx (AST.FnDecl name params _ _ stmts) =
  let paramNames = Set.fromList $ map AST.paramName params
      used = collectUsedVars stmts
      unused = Set.difference paramNames used
      actuallyUnused = Set.filter (not . isIntentionallyUnused) unused
  in [mkIssue (ctxFilePath ctx) (ctxFileContent ctx) (Just name)
       Info Maintainability "M003"
       ("Parameter '" ++ T.unpack param ++ "' in function '" ++
        T.unpack name ++ "' is never used")
       (Just "Remove unused parameter or prefix with '_' if intentional")
     | param <- Set.toList actuallyUnused]
checkDeclUnusedParams _ _ = []

-- ============================================================================
-- UNUSED IMPORT CHECK
-- ============================================================================

data UnusedImportCheck = UnusedImportCheck

instance LintCheck UnusedImportCheck where
  checkName _ = "unused-import"
  checkCategory _ = Maintainability
  runCheck _ ctx = checkUnusedImports ctx

checkUnusedImports :: AnalysisContext -> [LintIssue]
checkUnusedImports ctx =
  let imports = collectImports (ctxDecls ctx)
      usedModules = collectUsedModules (ctxDecls ctx)
      unusedImports = [(imp, findImportLine imp (ctxDecls ctx))
                      | imp <- imports, not (isModuleUsed imp usedModules)]
  in [LintIssue (ctxFilePath ctx) line (Just 1)
       Info Maintainability "M004"
       ("Import '" ++ T.unpack imp ++ "' is never used")
       (Just "Remove unused import")
       Nothing
     | (imp, line) <- unusedImports]

-- Find the line number where an import is declared
findImportLine :: T.Text -> [AST.Decl] -> Maybe Int
findImportLine targetImport decls = go decls 1
  where
    go [] _ = Nothing
    go (AST.UseDecl name _ : rest) lineNum
      | name == targetImport = Just lineNum
      | otherwise = go rest (lineNum + 1)
    go (_ : rest) lineNum = go rest (lineNum + 1)

collectImports :: [AST.Decl] -> [T.Text]
collectImports decls = [name | AST.UseDecl name _ <- decls]

-- Simplified check: assume module is used if any function from it is called
-- In a real implementation, you'd track which functions come from which modules
collectUsedModules :: [AST.Decl] -> Set.Set T.Text
collectUsedModules decls =
  Set.fromList $ concatMap getModulesFromDecl decls
  where
    getModulesFromDecl (AST.FnDecl _ _ _ _ stmts) =
      concatMap getModulesFromStmt stmts
    getModulesFromDecl _ = []

    getModulesFromStmt stmt = case stmt of
      AST.ExprStmt expr -> getModulesFromExpr expr
      AST.IfStmt _ thn els ->
        concatMap getModulesFromStmt thn ++
        maybe [] (concatMap getModulesFromStmt) els
      AST.WhileStmt _ body -> concatMap getModulesFromStmt body
      AST.ForStmt _ _ body -> concatMap getModulesFromStmt body
      _ -> []

    getModulesFromExpr expr = case expr of
      AST.Call (AST.Var name) _ ->
        -- Check if function name suggests it's from a module
        if T.any (== '.') name
        then [T.takeWhile (/= '.') name]
        else []
      _ -> []

isModuleUsed :: T.Text -> Set.Set T.Text -> Bool
isModuleUsed imp usedModules =
  -- Check if the import path matches any used module
  let parts = T.splitOn "." imp
      lastPart = if null parts then imp else last parts
  in Set.member lastPart usedModules || Set.member imp usedModules

-- ============================================================================
-- UNUSED FUNCTION CHECK
-- ============================================================================

data UnusedFunctionCheck = UnusedFunctionCheck

instance LintCheck UnusedFunctionCheck where
  checkName _ = "unused-function"
  checkCategory _ = Maintainability
  runCheck _ ctx = checkUnusedFunctions ctx

checkUnusedFunctions :: AnalysisContext -> [LintIssue]
checkUnusedFunctions ctx =
  let functions = collectFunctionsWithLines (ctxDecls ctx)
      called = collectCalledFunctions (ctxDecls ctx)
      -- Don't report 'main' as unused
      unused = [(fn, line) | (fn, line) <- functions, fn /= "main" && fn `notElem` called]
  in [LintIssue (ctxFilePath ctx) line (Just 1)
       Info Maintainability "M005"
       ("Function '" ++ T.unpack fn ++ "' is defined but never called")
       (Just "Remove unused function or mark as exported")
       Nothing
     | (fn, line) <- unused]

collectFunctionsWithLines :: [AST.Decl] -> [(T.Text, Maybe Int)]
collectFunctionsWithLines decls = go decls 1
  where
    go [] _ = []
    go (AST.FnDecl name _ _ _ _ : rest) lineNum =
      (name, Just lineNum) : go rest (lineNum + 1)
    go (_ : rest) lineNum = go rest (lineNum + 1)

collectCalledFunctions :: [AST.Decl] -> [T.Text]
collectCalledFunctions decls =
  nub $ concatMap getCallsFromDecl decls
  where
    getCallsFromDecl (AST.FnDecl _ _ _ _ stmts) =
      concatMap getCallsFromStmt stmts
    getCallsFromDecl _ = []

    getCallsFromStmt stmt = case stmt of
      AST.ExprStmt expr -> getCallsFromExpr expr
      AST.VarDecl _ expr -> getCallsFromExpr expr
      AST.Assign _ expr -> getCallsFromExpr expr
      AST.IfStmt cond thn els ->
        getCallsFromExpr cond ++
        concatMap getCallsFromStmt thn ++
        maybe [] (concatMap getCallsFromStmt) els
      AST.WhileStmt cond body ->
        getCallsFromExpr cond ++ concatMap getCallsFromStmt body
      AST.ForStmt _ expr body ->
        getCallsFromExpr expr ++ concatMap getCallsFromStmt body
      AST.ReturnStmt (Just expr) -> getCallsFromExpr expr
      AST.BlockStmt stmts -> concatMap getCallsFromStmt stmts
      _ -> []

    getCallsFromExpr expr = case expr of
      AST.Call (AST.Var name) args ->
        name : concatMap (getCallsFromExpr . AST.argValue) args
      AST.MemberCall obj _ args ->
        getCallsFromExpr obj ++
        concatMap (getCallsFromExpr . AST.argValue) args
      AST.BinOp _ left right ->
        getCallsFromExpr left ++ getCallsFromExpr right
      AST.UnOp _ e -> getCallsFromExpr e
      AST.ListLit exprs -> concatMap getCallsFromExpr exprs
      _ -> []
