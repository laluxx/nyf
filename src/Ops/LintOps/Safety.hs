{-# LANGUAGE OverloadedStrings #-}

module Ops.LintOps.Safety where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified AST
import Ops.LintOps.Core
import Ops.LintOps.CFG

-- Resource Leak Check
data ResourceLeakCheck = ResourceLeakCheck

instance LintCheck ResourceLeakCheck where
  checkName _ = "resource-leak"
  checkCategory _ = Safety
  runCheck _ ctx = checkResourceLeaks ctx

checkResourceLeaks :: AnalysisContext -> [LintIssue]
checkResourceLeaks ctx = [] -- TODO: Implement

-- Double Release Check
data DoubleReleaseCheck = DoubleReleaseCheck

instance LintCheck DoubleReleaseCheck where
  checkName _ = "double-release"
  checkCategory _ = Safety
  runCheck _ ctx = checkDoubleRelease ctx

checkDoubleRelease :: AnalysisContext -> [LintIssue]
checkDoubleRelease ctx = [] -- TODO: Implement

-- Use After Free Check
data UseAfterFreeCheck = UseAfterFreeCheck

instance LintCheck UseAfterFreeCheck where
  checkName _ = "use-after-free"
  checkCategory _ = Safety
  runCheck _ ctx = checkUseAfterFree ctx

checkUseAfterFree :: AnalysisContext -> [LintIssue]
checkUseAfterFree ctx = [] -- TODO: Implement
