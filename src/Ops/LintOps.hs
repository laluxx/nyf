{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ops.LintOps where


import System.Directory (getDirectoryContents, doesDirectoryExist, getCurrentDirectory, doesFileExist, canonicalizePath)
import System.FilePath ((</>), takeExtension, takeFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (sortOn, nub, partition)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Parser
import qualified AST

-- Import all modules
import Ops.LintOps.Core
import Ops.LintOps.CFG
import Ops.LintOps.Correctness
import qualified Ops.LintOps.Safety as Safety
import qualified Ops.LintOps.Security as Security
import qualified Ops.LintOps.Performance as Performance
import qualified Ops.LintOps.Style as Style
import qualified Ops.LintOps.Documentation as Documentation
import qualified Ops.LintOps.DeadCode as DeadCode
import qualified Ops.LintOps.Unused as Unused
import qualified Ops.LintOps.Complexity as Complexity

--- CHECK REGISTRY

-- Existential wrapper for lint checks
data AnyCheck = forall a. LintCheck a => AnyCheck a

allChecks :: [AnyCheck]
allChecks =
  -- Correctness checks
  [ AnyCheck DivisionByZeroCheck
  , AnyCheck ArrayBoundsCheck
  , AnyCheck OffByOneCheck
  , AnyCheck InfiniteLoopCheck
  , AnyCheck NullAccessCheck
  , AnyCheck UninitializedVarCheck

  -- Dead code checks
  , AnyCheck DeadCode.DeadCodeCheck
  , AnyCheck DeadCode.UnreachableLoopCheck

  -- Safety checks
  , AnyCheck Safety.ResourceLeakCheck
  , AnyCheck Safety.DoubleReleaseCheck
  , AnyCheck Safety.UseAfterFreeCheck

  -- Security checks
  , AnyCheck Security.SqlInjectionCheck
  , AnyCheck Security.PathTraversalCheck
  , AnyCheck Security.HardcodedSecretCheck
  , AnyCheck Security.InsecureRandomCheck

  -- Performance checks
  , AnyCheck Performance.NestedLoopCheck
  , AnyCheck Performance.InefficiientStringConcatCheck
  , AnyCheck Performance.ConstantInLoopCheck

  -- Style checks
  , AnyCheck Style.LineLengthCheck
  , AnyCheck Style.NamingConventionCheck
  , AnyCheck Style.TrailingWhitespaceCheck
  , AnyCheck Style.ImportOrderCheck

  -- Documentation checks
  , AnyCheck Documentation.MissingDocstringCheck
  , AnyCheck Documentation.DocstringQualityCheck

  -- Maintainability checks
  , AnyCheck DeadCode.RedundantConditionCheck
  , AnyCheck Unused.UnusedVariableCheck
  , AnyCheck Unused.UnusedParameterCheck
  , AnyCheck Unused.UnusedImportCheck
  , AnyCheck Unused.UnusedFunctionCheck

  -- Complexity checks
  , AnyCheck Complexity.CyclomaticComplexityCheck
  , AnyCheck Complexity.FunctionLengthCheck
  , AnyCheck Complexity.ParameterCountCheck
  , AnyCheck Complexity.CognitiveComplexityCheck
  , AnyCheck Complexity.ExcessiveNestingCheck
  ]

-- Run a check if enabled
runCheckIfEnabled :: AnyCheck -> AnalysisContext -> [LintIssue]
runCheckIfEnabled (AnyCheck check) ctx =
  if isEnabled check (ctxConfig ctx)
  then runCheck check ctx
  else []

--- MAIN ENTRY POINTS

lintPath :: FilePath -> IO ()
lintPath path = lintPathWithConfig path defaultConfig

lintPathWithConfig :: FilePath -> LintConfig -> IO ()
lintPathWithConfig path config = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile && takeExtension path == ".ny"
    then void (lintFileWithConfig path config)
    else if isDir
      then lintDirectoryWithConfig path config
      else do
        putStrLn ""
        putStrLn $ "\x1b[1;31mError:\x1b[0m Invalid path: " ++ path
        putStrLn "Expected a .ny file or directory"
        putStrLn ""

lintDirectory :: FilePath -> IO ()
lintDirectory dir = lintDirectoryWithConfig dir defaultConfig

lintDirectoryWithConfig :: FilePath -> LintConfig -> IO ()
lintDirectoryWithConfig dir config = do
  nyFiles <- findNyFiles dir

  -- Filter out ignored patterns
  let filteredFiles = filter (not . shouldIgnore config) nyFiles

  if null filteredFiles
    then do
      putStrLn ""
      putStrLn $ "\x1b[1;33mWarning:\x1b[0m No .ny files found in: " ++ dir
      putStrLn ""
    else do
      putStrLn ""
      putStrLn $ "\x1b[1;94mLinting\x1b[0m " ++ show (length filteredFiles) ++
                 " files in: " ++ dir
      putStrLn ""
      allIssues <- concat <$> mapM (\f -> lintFileQuietWithConfig f config) filteredFiles
      displayLintSummary allIssues

lintFile :: FilePath -> IO [LintIssue]
lintFile path = lintFileWithConfig path defaultConfig

lintFileWithConfig :: FilePath -> LintConfig -> IO [LintIssue]
lintFileWithConfig path config = do
  content <- TIO.readFile path

  case parse Parser.file path content of
    Left parseErr -> do
      putStrLn $ formatEmacsError path 1 1 "error" "Parse" "File failed to parse"
      putStrLn $ indent 4 (errorBundlePretty parseErr)
      return [LintIssue path (Just 1) (Just 1) Error Correctness "E000"
              "File failed to parse" Nothing Nothing]

    Right decls -> do
      let ctx = buildContext path content decls config
          allIssues = performAllChecks ctx

      if null allIssues
        then do
          putStrLn $ "\x1b[1;32m✓\x1b[0m " ++ path ++ " \x1b[2m(no issues)\x1b[0m"
          return []
        else do
          putStrLn $ "\x1b[1;33m⚠\x1b[0m " ++ path ++ " \x1b[2m(" ++
                     show (length allIssues) ++ " issues)\x1b[0m"
          mapM_ (putStrLn . formatIssueEmacs) allIssues
          return allIssues

lintFileQuiet :: FilePath -> IO [LintIssue]
lintFileQuiet path = lintFileQuietWithConfig path defaultConfig

lintFileQuietWithConfig :: FilePath -> LintConfig -> IO [LintIssue]
lintFileQuietWithConfig path config = do
  content <- TIO.readFile path

  case parse Parser.file path content of
    Left parseErr -> do
      putStrLn $ formatEmacsError path 1 1 "error" "Parse" "File failed to parse"
      putStrLn $ indent 4 (errorBundlePretty parseErr)
      return [LintIssue path (Just 1) (Just 1) Error Correctness "E000"
              "File failed to parse" Nothing Nothing]

    Right decls -> do
      let ctx = buildContext path content decls config
          allIssues = performAllChecks ctx

      if null allIssues
        then do
          putStrLn $ "\x1b[1;32m✓\x1b[0m " ++ path ++ " \x1b[2m(no issues)\x1b[0m"
          return []
        else do
          putStrLn $ "\x1b[1;33m⚠\x1b[0m " ++ path ++ " \x1b[2m(" ++
                     show (length allIssues) ++ " issues)\x1b[0m"
          return allIssues

--- PERFORM ALL CHECKS

performAllChecks :: AnalysisContext -> [LintIssue]
performAllChecks ctx =
  let issues = concatMap (`runCheckIfEnabled` ctx) allChecks
      sorted = sortIssues issues
  in applyFilters ctx sorted

-- Sort issues by severity, then by file/line
sortIssues :: [LintIssue] -> [LintIssue]
sortIssues = sortOn (\issue ->
  (issueLevel issue, issueFile issue, fromMaybe 0 (issueLine issue)))

-- Apply any additional filters
applyFilters :: AnalysisContext -> [LintIssue] -> [LintIssue]
applyFilters ctx issues =
  let config = ctxConfig ctx
      filtered = if cfgTreatWarningsAsErrors config
                 then map promoteWarning issues
                 else issues
  in filtered
  where
    promoteWarning issue =
      if issueLevel issue == Warning
      then issue { issueLevel = Error }
      else issue

-- ============================================================================
-- CONFIGURATION LOADING
-- ============================================================================

loadConfig :: FilePath -> IO LintConfig
loadConfig configPath = do
  exists <- doesFileExist configPath
  if exists
    then parseConfigFile configPath
    else return defaultConfig

parseConfigFile :: FilePath -> IO LintConfig
parseConfigFile path = do
  content <- TIO.readFile path
  let lines' = T.lines content
  return $ parseConfigLines lines' defaultConfig

parseConfigLines :: [T.Text] -> LintConfig -> LintConfig
parseConfigLines [] config = config
parseConfigLines (line:rest) config =
  let trimmed = T.strip line
  in if T.null trimmed || T.isPrefixOf "#" trimmed
     then parseConfigLines rest config
     else case T.splitOn ":" trimmed of
       [key, value] ->
         let key' = T.strip key
             value' = T.strip value
             updated = updateConfig key' value' config
         in parseConfigLines rest updated
       _ -> parseConfigLines rest config

updateConfig :: T.Text -> T.Text -> LintConfig -> LintConfig
updateConfig "max-line-length" value config =
  case readMaybeInt value of
    Just n -> config { cfgMaxLineLength = n }
    Nothing -> config
updateConfig "max-complexity" value config =
  case readMaybeInt value of
    Just n -> config { cfgMaxComplexity = n }
    Nothing -> config
updateConfig "max-function-length" value config =
  case readMaybeInt value of
    Just n -> config { cfgMaxFunctionLength = n }
    Nothing -> config
updateConfig "max-parameters" value config =
  case readMaybeInt value of
    Just n -> config { cfgMaxParameters = n }
    Nothing -> config
updateConfig "max-nesting-depth" value config =
  case readMaybeInt value of
    Just n -> config { cfgMaxNestingDepth = n }
    Nothing -> config
updateConfig "treat-warnings-as-errors" value config =
  config { cfgTreatWarningsAsErrors = value `elem` ["true", "yes", "1"] }
updateConfig "enable" value config =
  config { cfgEnabledChecks = Set.insert (T.unpack value) (cfgEnabledChecks config) }
updateConfig "disable" value config =
  config { cfgDisabledChecks = Set.insert (T.unpack value) (cfgDisabledChecks config) }
updateConfig "ignore" value config =
  config { cfgIgnorePatterns = value : cfgIgnorePatterns config }
updateConfig _ _ config = config

readMaybeInt :: T.Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing

--- HELPER FUNCTIONS

shouldIgnore :: LintConfig -> FilePath -> Bool
shouldIgnore config path =
  let pathText = T.pack path
      patterns = cfgIgnorePatterns config
  in any (`T.isInfixOf` pathText) patterns

findNyFiles :: FilePath -> IO [FilePath]
findNyFiles dir = do
  absDir <- canonicalizePath dir  -- Make the directory path absolute
  contents <- getDirectoryContents absDir
  let validEntries = filter (`notElem` [".", ".."]) contents
  results <- mapM (processEntry absDir) validEntries
  return (concat results)
  where
    processEntry baseDir entry = do
      let fullPath = baseDir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then findNyFiles fullPath
        else if takeExtension entry == ".ny"
          then return [fullPath]
          else return []

-- findNyFiles :: FilePath -> IO [FilePath]
-- findNyFiles dir = do
--   contents <- getDirectoryContents dir
--   let validEntries = filter (`notElem` [".", ".."]) contents
--   results <- mapM processEntry validEntries
--   return (concat results)
--   where
--     processEntry entry = do
--       let fullPath = dir </> entry
--       isDir <- doesDirectoryExist fullPath
--       if isDir
--         then findNyFiles fullPath
--         else if takeExtension entry == ".ny"
--           then return [fullPath]
--           else return []

void :: IO a -> IO ()
void action = action >> return ()

unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action

indent :: Int -> String -> String
indent n s = unlines $ map ((replicate n ' ') ++) (lines s)

--- FORMATTING AND DISPLAY

formatEmacsError :: FilePath -> Int -> Int -> String -> String -> String -> String
formatEmacsError file line col level category msg =
  file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++
  level ++ ": [" ++ category ++ "] " ++ msg

formatIssueEmacs :: LintIssue -> String
formatIssueEmacs issue =
  let line = fromMaybe 1 (issueLine issue)
      col = fromMaybe 1 (issueColumn issue)
      level = case issueLevel issue of
        Error -> "error"
        Warning -> "warning"
        Info -> "info"
      category = show (issueCategory issue)
  in file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++
     level ++ ": [" ++ issueCode issue ++ "] " ++ issueMessage issue ++
     case issueSuggestion issue of
       Just suggestion -> "\n    → " ++ suggestion
       Nothing -> ""
  where file = issueFile issue

displayLintSummary :: [LintIssue] -> IO ()
displayLintSummary issues = do
  let errors = filter ((== Error) . issueLevel) issues
      warnings = filter ((== Warning) . issueLevel) issues
      infos = filter ((== Info) . issueLevel) issues
      fileGroups = groupByFile issues
      byCategory = groupByCategory issues

  putStrLn ""

  unless (null issues) $ do
    mapM_ displayFileIssues fileGroups
    putStrLn ""

    putStrLn "\x1b[1mIssues by Category:\x1b[0m"
    mapM_ displayCategoryCount byCategory
    putStrLn ""

  displaySeverityCounts errors warnings infos

  putStrLn ""

  if null issues
    then putStrLn "\x1b[1;32m✓ All files passed linting!\x1b[0m"
    else do
      putStrLn $ "\x1b[1;33m⚠ Found " ++ show (length issues) ++ " total issues\x1b[0m"
      when (not $ null errors) $
        putStrLn "\x1b[1;31m✗ Build failed due to errors\x1b[0m"

  putStrLn ""

displaySeverityCounts :: [LintIssue] -> [LintIssue] -> [LintIssue] -> IO ()
displaySeverityCounts errors warnings infos = do
  unless (null errors) $
    putStrLn $ "\x1b[1;31m✗ Errors:\x1b[0m " ++ show (length errors)
  unless (null warnings) $
    putStrLn $ "\x1b[1;33m⚠ Warnings:\x1b[0m " ++ show (length warnings)
  unless (null infos) $
    putStrLn $ "\x1b[1;36mℹ Info:\x1b[0m " ++ show (length infos)

groupByFile :: [LintIssue] -> [(FilePath, [LintIssue])]
groupByFile issues =
  let files = nub $ map issueFile issues
  in [(file, filter ((== file) . issueFile) issues) | file <- files]

groupByCategory :: [LintIssue] -> [(IssueCategory, [LintIssue])]
groupByCategory issues =
  let categories = nub $ map issueCategory issues
  in [(cat, filter ((== cat) . issueCategory) issues) | cat <- categories]

displayFileIssues :: (FilePath, [LintIssue]) -> IO ()
displayFileIssues (file, issues) = do
  putStrLn $ file ++ ":"
  mapM_ (putStrLn . formatIssueEmacs) issues

displayCategoryCount :: (IssueCategory, [LintIssue]) -> IO ()
displayCategoryCount (category, issues) =
  putStrLn $ "  " ++ categoryIcon category ++ " " ++
             show category ++ ": " ++ show (length issues)

categoryIcon :: IssueCategory -> String
categoryIcon Correctness = "\x1b[1;31m✗\x1b[0m"
categoryIcon Safety = "\x1b[1;33m⚠\x1b[0m"
categoryIcon Security = "\x1b[1;35m🔒\x1b[0m"
categoryIcon Performance = "\x1b[1;33m⚡\x1b[0m"
categoryIcon Maintainability = "\x1b[1;36m🔧\x1b[0m"
categoryIcon Style = "\x1b[1;94m✨\x1b[0m"
categoryIcon Documentation = "\x1b[1;92m📝\x1b[0m"
categoryIcon Complexity = "\x1b[1;93m📊\x1b[0m"

when :: Bool -> IO () -> IO ()
when True action = action
when False _ = return ()

--- JSON OUTPUT (for CI/CD integration)

outputJson :: [LintIssue] -> IO ()
outputJson issues = do
  putStrLn "{"
  putStrLn "  \"issues\": ["
  mapM_ outputIssueJson (zip issues [1..])
  putStrLn "  ]"
  putStrLn "}"

outputIssueJson :: (LintIssue, Int) -> IO ()
outputIssueJson (issue, idx) = do
  let comma = if idx == 1 then "" else ","
  putStrLn $ comma ++ "    {"
  putStrLn $ "      \"file\": \"" ++ escapeJson (issueFile issue) ++ "\","
  putStrLn $ "      \"line\": " ++ show (fromMaybe 0 (issueLine issue)) ++ ","
  putStrLn $ "      \"column\": " ++ show (fromMaybe 0 (issueColumn issue)) ++ ","
  putStrLn $ "      \"level\": \"" ++ show (issueLevel issue) ++ "\","
  putStrLn $ "      \"category\": \"" ++ show (issueCategory issue) ++ "\","
  putStrLn $ "      \"code\": \"" ++ issueCode issue ++ "\","
  putStrLn $ "      \"message\": \"" ++ escapeJson (issueMessage issue) ++ "\""
  putStrLn "    }"

escapeJson :: String -> String
escapeJson = concatMap escape
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape c = [c]

-- Add these functions to Ops/LintOps.hs

-- ============================================================================
-- VALIDATE PATH (Quick syntax validation only)
-- ============================================================================

validatePath :: FilePath -> IO ()
validatePath path = validatePathWithConfig path defaultConfig

validatePathWithConfig :: FilePath -> LintConfig -> IO ()
validatePathWithConfig path config = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile && takeExtension path == ".ny"
    then void (validateFileWithConfig path config)
    else if isDir
      then validateDirectoryWithConfig path config
      else do
        putStrLn ""
        putStrLn $ "\x1b[1;31mError:\x1b[0m Invalid path: " ++ path
        putStrLn "Expected a .ny file or directory"
        putStrLn ""

validateFileWithConfig :: FilePath -> LintConfig -> IO Bool
validateFileWithConfig path _config = do
  content <- TIO.readFile path

  case parse Parser.file path content of
    Left parseErr -> do
      putStrLn $ "\x1b[1;31m✗\x1b[0m " ++ path ++ " \x1b[2m(parse error)\x1b[0m"
      putStrLn $ indent 4 (errorBundlePretty parseErr)
      return False

    Right _decls -> do
      putStrLn $ "\x1b[1;32m✓\x1b[0m " ++ path ++ " \x1b[2m(valid syntax)\x1b[0m"
      return True

validateDirectoryWithConfig :: FilePath -> LintConfig -> IO ()
validateDirectoryWithConfig dir config = do
  nyFiles <- findNyFiles dir

  let filteredFiles = filter (not . shouldIgnore config) nyFiles

  if null filteredFiles
    then do
      putStrLn ""
      putStrLn $ "\x1b[1;33mWarning:\x1b[0m No .ny files found in: " ++ dir
      putStrLn ""
    else do
      putStrLn ""
      putStrLn $ "\x1b[1;94mValidating\x1b[0m " ++ show (length filteredFiles) ++
                 " files in: " ++ dir
      putStrLn ""
      results <- mapM (\f -> validateFileWithConfig f config) filteredFiles
      let validCount = length (filter id results)
          invalidCount = length (filter not results)
      putStrLn ""
      if invalidCount == 0
        then putStrLn $ "\x1b[1;32m✓ All " ++ show validCount ++ " files have valid syntax!\x1b[0m"
        else putStrLn $ "\x1b[1;31m✗ " ++ show invalidCount ++ " files have syntax errors\x1b[0m"
      putStrLn ""

-- ============================================================================
-- STATS PATH (Show statistics about code)
-- ============================================================================

statsPath :: FilePath -> IO ()
statsPath path = statsPathWithConfig path defaultConfig

statsPathWithConfig :: FilePath -> LintConfig -> IO ()
statsPathWithConfig path config = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path

  if isFile && takeExtension path == ".ny"
    then statsFile path
    else if isDir
      then statsDirectory path config
      else do
        putStrLn ""
        putStrLn $ "\x1b[1;31mError:\x1b[0m Invalid path: " ++ path
        putStrLn "Expected a .ny file or directory"
        putStrLn ""

statsFile :: FilePath -> IO ()
statsFile path = do
  content <- TIO.readFile path

  case parse Parser.file path content of
    Left parseErr -> do
      putStrLn ""
      putStrLn $ "\x1b[1;31mError:\x1b[0m Failed to parse " ++ path
      putStrLn $ indent 4 (errorBundlePretty parseErr)
      putStrLn ""

    Right decls -> do
      let stats = computeStats content decls
      displayFileStats path stats

statsDirectory :: FilePath -> LintConfig -> IO ()
statsDirectory dir config = do
  nyFiles <- findNyFiles dir
  let filteredFiles = filter (not . shouldIgnore config) nyFiles

  if null filteredFiles
    then do
      putStrLn ""
      putStrLn $ "\x1b[1;33mWarning:\x1b[0m No .ny files found in: " ++ dir
      putStrLn ""
    else do
      allStats <- catMaybes <$> mapM statsFileQuiet filteredFiles
      displayDirectoryStats dir allStats

statsFileQuiet :: FilePath -> IO (Maybe FileStats)
statsFileQuiet path = do
  content <- TIO.readFile path
  case parse Parser.file path content of
    Left _ -> return Nothing
    Right decls -> return $ Just (computeStats content decls)

data FileStats = FileStats
  { statLines :: Int
  , statFunctions :: Int
  , statComments :: Int
  , statBlankLines :: Int
  } deriving (Show)

computeStats :: T.Text -> [AST.Decl] -> FileStats
computeStats content decls =
  let lines' = T.lines content
      totalLines = length lines'
      blankLines = length $ filter (T.null . T.strip) lines'
      functions = length [() | AST.FnDecl {} <- decls]
      comments = length [() | AST.CommentDecl {} <- decls]
  in FileStats totalLines functions comments blankLines

displayFileStats :: FilePath -> FileStats -> IO ()
displayFileStats path stats = do
  putStrLn ""
  putStrLn $ "\x1b[1mStatistics for:\x1b[0m " ++ path
  putStrLn ""
  putStrLn $ "  Lines of code:   " ++ show (statLines stats)
  putStrLn $ "  Functions:       " ++ show (statFunctions stats)
  putStrLn $ "  Comments:        " ++ show (statComments stats)
  putStrLn $ "  Blank lines:     " ++ show (statBlankLines stats)
  putStrLn ""

displayDirectoryStats :: FilePath -> [FileStats] -> IO ()
displayDirectoryStats dir allStats = do
  let totalLines = sum $ map statLines allStats
      totalFunctions = sum $ map statFunctions allStats
      totalComments = sum $ map statComments allStats
      totalBlankLines = sum $ map statBlankLines allStats
      fileCount = length allStats

  putStrLn ""
  putStrLn $ "\x1b[1mStatistics for:\x1b[0m " ++ dir
  putStrLn ""
  putStrLn $ "  Files:           " ++ show fileCount
  putStrLn $ "  Total lines:     " ++ show totalLines
  putStrLn $ "  Functions:       " ++ show totalFunctions
  putStrLn $ "  Comments:        " ++ show totalComments
  putStrLn $ "  Blank lines:     " ++ show totalBlankLines
  putStrLn ""
  putStrLn $ "  Avg lines/file:  " ++ show (if fileCount > 0 then totalLines `div` fileCount else 0)
  putStrLn ""
