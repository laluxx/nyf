{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ops.LintOps.CFG where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified AST
import Ops.LintOps.Core

--- CONTROL FLOW GRAPH CONSTRUCTION

-- Build CFG for a function
buildCFG :: [AST.Stmt] -> CFG
buildCFG stmts =
  let (nodes, nextId) = buildNodes stmts 1 2
      entryNode = CFGNode 0 [] [1] [] EntryNode
      exitNode = CFGNode nextId [] [] [] ExitNode
      allNodes = Map.fromList $ (0, entryNode) : (nextId, exitNode) : nodes
  in CFG
    { cfgNodes = allNodes
    , cfgEntry = 0
    , cfgExit = nextId
    }

-- Build nodes from statements
buildNodes :: [AST.Stmt] -> Int -> Int -> ([(Int, CFGNode)], Int)
buildNodes [] currentId nextId = ([], currentId)
buildNodes (stmt:rest) currentId nextId =
  case stmt of
    AST.IfStmt cond thenBranch elseBranch ->
      let (thenNodes, thenExitId) = buildNodes thenBranch nextId (nextId + 100)
          (elseNodes, elseExitId) = case elseBranch of
                Just branch -> buildNodes branch (thenExitId + 1) (thenExitId + 101)
                Nothing -> ([], thenExitId)

          -- Branch node
          branchNode = CFGNode currentId [stmt]
            [nextId, if elseBranch == Nothing then elseExitId + 1 else thenExitId + 1]
            [] BranchNode

          -- Merge node
          mergeId = elseExitId + 1
          mergePreds = if elseBranch == Nothing
                       then [currentId, thenExitId]
                       else [thenExitId, elseExitId]
          mergeNode = CFGNode mergeId [] [mergeId + 1] mergePreds BasicBlock

          -- Build rest
          (restNodes, finalId) = buildNodes rest (mergeId + 1) (mergeId + 100)

          allNodes = (currentId, branchNode) : thenNodes ++ elseNodes ++
                     [(mergeId, mergeNode)] ++ restNodes
      in (allNodes, finalId)

    AST.WhileStmt cond body ->
      let (bodyNodes, bodyExitId) = buildNodes body nextId (nextId + 100)

          -- Loop header
          loopHeader = CFGNode currentId [stmt]
            [nextId, bodyExitId + 1]  -- To body or exit
            [currentId, bodyExitId]   -- From previous and loop back
            LoopHeader

          -- Loop exit
          loopExit = CFGNode (bodyExitId + 1) [] [bodyExitId + 2] [currentId] LoopExit

          -- Build rest
          (restNodes, finalId) = buildNodes rest (bodyExitId + 2) (bodyExitId + 100)

          -- Update body exit to loop back
          allNodes = (currentId, loopHeader) : bodyNodes ++
                     [(bodyExitId + 1, loopExit)] ++ restNodes
      in (allNodes, finalId)

    AST.ForStmt var range body ->
      -- Similar to while loop
      let (bodyNodes, bodyExitId) = buildNodes body nextId (nextId + 100)

          loopHeader = CFGNode currentId [stmt]
            [nextId, bodyExitId + 1]
            [currentId, bodyExitId]
            LoopHeader

          loopExit = CFGNode (bodyExitId + 1) [] [bodyExitId + 2] [currentId] LoopExit

          (restNodes, finalId) = buildNodes rest (bodyExitId + 2) (bodyExitId + 100)

          allNodes = (currentId, loopHeader) : bodyNodes ++
                     [(bodyExitId + 1, loopExit)] ++ restNodes
      in (allNodes, finalId)

    AST.ReturnStmt _ ->
      -- Return goes directly to exit
      let returnNode = CFGNode currentId [stmt] [] [] BasicBlock
          (restNodes, finalId) = buildNodes rest (currentId + 1) nextId
      in ((currentId, returnNode) : restNodes, finalId)

    AST.BreakStmt ->
      -- Break handled by parent loop
      let breakNode = CFGNode currentId [stmt] [] [] BasicBlock
          (restNodes, finalId) = buildNodes rest (currentId + 1) nextId
      in ((currentId, breakNode) : restNodes, finalId)

    AST.ContinueStmt ->
      -- Continue handled by parent loop
      let continueNode = CFGNode currentId [stmt] [] [] BasicBlock
          (restNodes, finalId) = buildNodes rest (currentId + 1) nextId
      in ((currentId, continueNode) : restNodes, finalId)

    _ ->
      -- Basic statement
      let basicNode = CFGNode currentId [stmt] [currentId + 1] [] BasicBlock
          (restNodes, finalId) = buildNodes rest (currentId + 1) nextId
      in ((currentId, basicNode) : restNodes, finalId)

--- DATA FLOW ANALYSIS

-- Compute reaching definitions
computeReachingDefinitions :: CFG -> Map.Map Int (Set.Set Definition)
computeReachingDefinitions cfg =
  let initialFacts = Map.fromList [(nodeId node, Set.empty) | node <- Map.elems (cfgNodes cfg)]
  in iterateDataFlow cfg initialFacts gen kill transfer Set.union
  where
    gen :: CFGNode -> Set.Set Definition
    gen node = Set.fromList [Definition var (nodeId node) (Just expr)
                            | AST.VarDecl vars expr <- nodeStmts node, var <- vars]

    kill :: CFGNode -> Set.Set Definition -> Set.Set Definition
    kill node defs =
      let definedVars = [var | AST.VarDecl vars _ <- nodeStmts node, var <- vars]
      in Set.filter (\def -> defVar def `notElem` definedVars) defs

    transfer :: CFGNode -> Set.Set Definition -> Set.Set Definition
    transfer node inSet = Set.union (gen node) (kill node inSet)

-- Compute live variables (backward analysis)
computeLiveVariables :: CFG -> Map.Map Int (Set.Set T.Text)
computeLiveVariables cfg =
  let initialFacts = Map.fromList [(nodeId node, Set.empty) | node <- Map.elems (cfgNodes cfg)]
  in iterateBackward cfg initialFacts use def transfer Set.union
  where
    use :: CFGNode -> Set.Set T.Text
    use node = Set.fromList $ concatMap extractUsedVars (nodeStmts node)

    def :: CFGNode -> Set.Set T.Text -> Set.Set T.Text
    def node vars =
      let definedVars = Set.fromList [var | AST.VarDecl vars _ <- nodeStmts node, var <- vars]
      in Set.difference vars definedVars

    transfer :: CFGNode -> Set.Set T.Text -> Set.Set T.Text
    transfer node outSet = Set.union (use node) (def node outSet)

    extractUsedVars :: AST.Stmt -> [T.Text]
    extractUsedVars stmt = case stmt of
      AST.ExprStmt expr -> extractVarsFromExpr expr
      AST.Assign _ expr -> extractVarsFromExpr expr
      AST.VarDecl _ expr -> extractVarsFromExpr expr
      AST.IfStmt cond _ _ -> extractVarsFromExpr cond
      AST.WhileStmt cond _ -> extractVarsFromExpr cond
      AST.ReturnStmt (Just expr) -> extractVarsFromExpr expr
      _ -> []

-- Extract variables from expression
extractVarsFromExpr :: AST.Expr -> [T.Text]
extractVarsFromExpr (AST.Var v) = [v]
extractVarsFromExpr (AST.BinOp _ l r) =
  extractVarsFromExpr l ++ extractVarsFromExpr r
extractVarsFromExpr (AST.UnOp _ e) = extractVarsFromExpr e
extractVarsFromExpr (AST.Call fn args) =
  extractVarsFromExpr fn ++ concatMap (extractVarsFromExpr . AST.argValue) args
extractVarsFromExpr (AST.MemberCall obj _ args) =
  extractVarsFromExpr obj ++ concatMap (extractVarsFromExpr . AST.argValue) args
extractVarsFromExpr (AST.Index arr maybeIdx _ _) =
  let arrVars = extractVarsFromExpr arr
      idxVars = case maybeIdx of
                  Just idx -> extractVarsFromExpr idx
                  Nothing -> []
  in arrVars ++ idxVars
extractVarsFromExpr _ = []

-- Generic forward data flow iteration
iterateDataFlow :: (Ord a) => CFG -> Map.Map Int a -> (CFGNode -> a) -> (CFGNode -> a -> a)
        -> (CFGNode -> a -> a) -> (a -> a -> a) -> Map.Map Int a
iterateDataFlow cfg initialFacts gen kill transfer meet =
  let nodes = Map.elems (cfgNodes cfg)
      workList = Set.fromList (map nodeId nodes)
  in iterateHelper cfg initialFacts transfer meet workList

iterateHelper :: (Ord a) => CFG -> Map.Map Int a -> (CFGNode -> a -> a)
              -> (a -> a -> a) -> Set.Set Int -> Map.Map Int a
iterateHelper cfg facts transfer meet workList
  | Set.null workList = facts
  | otherwise =
      let nodeId' = Set.findMin workList
          remainingWork = Set.delete nodeId' workList
          Just node = Map.lookup nodeId' (cfgNodes cfg)

          -- Compute IN fact by meeting predecessor OUT facts
          predFacts = [Map.findWithDefault (error "Missing fact") pred facts
                      | pred <- nodePredecessors node]
          inFact = if null predFacts
                   then Map.findWithDefault (error "Missing initial") nodeId' facts
                   else foldr1 meet predFacts

          -- Compute OUT fact
          outFact = transfer node inFact
          oldOutFact = Map.findWithDefault outFact nodeId' facts

          -- If changed, add successors to worklist
          newFacts = Map.insert nodeId' outFact facts
          newWork = if outFact /= oldOutFact
                    then foldr Set.insert remainingWork (nodeSuccessors node)
                    else remainingWork
      in iterateHelper cfg newFacts transfer meet newWork

-- Generic backward data flow iteration
iterateBackward :: (Ord a) => CFG -> Map.Map Int a -> (CFGNode -> a)
                -> (CFGNode -> a -> a) -> (CFGNode -> a -> a)
                -> (a -> a -> a) -> Map.Map Int a
iterateBackward cfg initialFacts use def transfer meet =
  let nodes = Map.elems (cfgNodes cfg)
      workList = Set.fromList (map nodeId nodes)
  in iterateBackwardHelper cfg initialFacts transfer meet workList

iterateBackwardHelper :: (Ord a) => CFG -> Map.Map Int a -> (CFGNode -> a -> a)
                      -> (a -> a -> a) -> Set.Set Int -> Map.Map Int a
iterateBackwardHelper cfg facts transfer meet workList
  | Set.null workList = facts
  | otherwise =
      let nodeId' = Set.findMin workList
          remainingWork = Set.delete nodeId' workList
          Just node = Map.lookup nodeId' (cfgNodes cfg)

          -- Compute OUT fact by meeting successor IN facts
          succFacts = [Map.findWithDefault (error "Missing fact") succ facts
                      | succ <- nodeSuccessors node]
          outFact = if null succFacts
                    then Map.findWithDefault (error "Missing initial") nodeId' facts
                    else foldr1 meet succFacts

          -- Compute IN fact
          inFact = transfer node outFact
          oldInFact = Map.findWithDefault inFact nodeId' facts

          -- If changed, add predecessors to worklist
          newFacts = Map.insert nodeId' inFact facts
          newWork = if inFact /= oldInFact
                    then foldr Set.insert remainingWork (nodePredecessors node)
                    else remainingWork
      in iterateBackwardHelper cfg newFacts transfer meet newWork

--- CONSTANT PROPAGATION

type ConstantFacts = Map.Map T.Text ConstantValue

computeConstants :: CFG -> Map.Map Int ConstantFacts
computeConstants cfg =
  let initialFacts = Map.fromList [(nodeId node, Map.empty) | node <- Map.elems (cfgNodes cfg)]
  in iterateDataFlow cfg initialFacts gen kill transfer meetConstants
  where
    gen :: CFGNode -> ConstantFacts
    gen node = Map.fromList $ concatMap extractConstants (nodeStmts node)

    kill :: CFGNode -> ConstantFacts -> ConstantFacts
    kill node facts =
      let definedVars = [var | AST.VarDecl vars _ <- nodeStmts node, var <- vars]
      in foldr Map.delete facts definedVars

    transfer :: CFGNode -> ConstantFacts -> ConstantFacts
    transfer node inFacts = Map.union (gen node) (kill node inFacts)

    extractConstants :: AST.Stmt -> [(T.Text, ConstantValue)]
    extractConstants (AST.VarDecl [var] expr) =
      case evalConstExpr Map.empty expr of
        Just val -> [(var, val)]
        Nothing -> [(var, NotConstant)]
    extractConstants _ = []

    meetConstants :: ConstantFacts -> ConstantFacts -> ConstantFacts
    meetConstants = Map.unionWith meetConstantValue

    meetConstantValue :: ConstantValue -> ConstantValue -> ConstantValue
    meetConstantValue v1 v2
      | v1 == v2 = v1
      | otherwise = NotConstant

-- Evaluate constant expression
evalConstExpr :: ConstantFacts -> AST.Expr -> Maybe ConstantValue
evalConstExpr _ (AST.IntLit n) = Just (ConstInt n)
evalConstExpr _ (AST.FloatLit f) = Just (ConstFloat f)
evalConstExpr _ (AST.StrLit s) = Just (ConstStr s)
evalConstExpr _ (AST.BoolLit b) = Just (ConstBool b)
evalConstExpr facts (AST.Var v) = Map.lookup v facts
evalConstExpr facts (AST.BinOp op l r) = do
  lval <- evalConstExpr facts l
  rval <- evalConstExpr facts r
  case (op, lval, rval) of
    (AST.Add, ConstInt a, ConstInt b) -> Just (ConstInt (a + b))
    (AST.Sub, ConstInt a, ConstInt b) -> Just (ConstInt (a - b))
    (AST.Mul, ConstInt a, ConstInt b) -> Just (ConstInt (a * b))
    (AST.Div, ConstInt a, ConstInt b) | b /= 0 -> Just (ConstInt (a `div` b))
    _ -> Nothing
evalConstExpr _ _ = Nothing
