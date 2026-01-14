{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Directory
    ( doesDirectoryExist,
      doesFileExist,
      listDirectory,
      createDirectory,
      getModificationTime,
      getCurrentDirectory,
      setCurrentDirectory,
      removeFile,
      removeDirectoryRecursive )
import System.FilePath ( (</>), takeExtension, takeDirectory, splitDirectories )
import System.IO (stderr, hPutStrLn)
import System.Process ( rawSystem, readProcess )
import Control.Monad (filterM, when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_nyf (getDataDir, version)
import Data.Version (showVersion)
import Control.Exception (catch, SomeException)
import Data.List ( isInfixOf, dropWhileEnd, isSuffixOf, intercalate, isPrefixOf )
import Data.Char (isSpace)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import Parser (file)
import Formatter (format)
import FormatConfig (loadFormatConfig, findFormatYaml)
import Templates
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Test

--- ANSI COLOR CODES

white :: String
white = "\ESC[97m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

red :: String
red = "\ESC[31m"

blue :: String
blue = "\ESC[34m"

cyan :: String
cyan = "\ESC[36m"

gray :: String
gray = "\ESC[90m"

reset :: String
reset = "\ESC[0m"

bold :: String
bold = "\ESC[1m"

dim :: String
dim = "\ESC[2m"


--- MAIN ENTRY POINT

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      printUsage

    ["--version"] ->
      printVersion

    ["-v"] ->
      printVersion

    ["version"] ->
      printVersion

    ["help"] ->
      printUsage

    ["help", cmd] ->
      printCommandHelp cmd

    ["-h"] ->
      printUsage

    ["--help"] ->
      printUsage

    ["test"] ->
      runTests

    ["test", pattern] ->
      runTestsWithPattern pattern

    ["new", projectName] ->
      createNewProject projectName

    ["init"] ->
      initProject

    ["run"] ->
      runProject

    ["run", filepath] ->
      runSpecificFile filepath

    ["build"] ->
      buildProject

    ["clean"] ->
      cleanProject

    ["watch"] ->
      watchProject

    ["format", path] ->
      formatPath path False False Nothing

    ["format", "--check", path] ->
      formatPath path True False Nothing

    ["format", "--diff", path] ->
      formatPath path False True Nothing

    ["format", "--verify", path] ->
      formatPath path False False (Just True)

    ["format", "--config", configPath, path] ->
      formatPath path False False (Just False) -- Will implement config loading

    ["lint", path] ->
      lintPath path

    ["validate", path] ->
      validatePath path

    ["stats", path] ->
      statsPath path

    ["ast", path] ->
      astPath path False

    ["ast", "pretty", path] ->
      astPath path True

    ["ast", "delete", path] ->
      astDeletePath path

    ["deps", filepath] ->
      depsFile filepath

    ["docs", path] ->
      docsPath path

    ["config"] ->
      showConfig

    ["completion", shell] ->
      generateCompletion shell

    [path] ->
      lintPath path

    _ ->
      printUsage

--- VERSION

printVersion :: IO ()
printVersion = do
  putStrLn $ bold ++ "nyf" ++ reset ++ " version " ++ showVersion version
  putStrLn "Nytrix Formal Functional Formatter"

--- HELP SYSTEM

printUsage :: IO ()
printUsage = do
  putStrLn $ bold ++ "nyf" ++ reset ++ " - Nytrix Formal Functional Formatter"
  putStrLn ""
  putStrLn $ dim ++ "A highly configurable code formatter and build tool for Nytrix" ++ reset
  putStrLn ""
  putStrLn $ bold ++ "USAGE:" ++ reset
  putStrLn "  nyf <command> [options]"
  putStrLn ""
  putStrLn $ bold ++ "PROJECT COMMANDS:" ++ reset
  putStrLn $ "  " ++ cyan ++ "new" ++ reset ++ " <name>              Create a new Nytrix project"
  putStrLn $ "  " ++ cyan ++ "init" ++ reset ++ "                    Initialize project in current directory"
  putStrLn $ "  " ++ cyan ++ "run" ++ reset ++ " [file]              Compile and run (defaults to src/main.ny)"
  putStrLn $ "  " ++ cyan ++ "build" ++ reset ++ "                   Compile project without running"
  putStrLn $ "  " ++ cyan ++ "clean" ++ reset ++ "                   Remove build artifacts"
  putStrLn $ "  " ++ cyan ++ "watch" ++ reset ++ "                   Watch for changes and auto-rebuild"
  putStrLn ""
  putStrLn $ bold ++ "CODE QUALITY:" ++ reset
  putStrLn $ "  " ++ cyan ++ "lint" ++ reset ++ " <path>              Parse and check syntax"
  putStrLn $ "  " ++ cyan ++ "format" ++ reset ++ " <path>            Format code in place"
  putStrLn $ "  " ++ cyan ++ "format --check" ++ reset ++ " <path>   Check if formatting is needed (exit 1 if yes)"
  putStrLn $ "  " ++ cyan ++ "format --diff" ++ reset ++ " <path>    Show formatting changes without writing"
  putStrLn $ "  " ++ cyan ++ "format --verify" ++ reset ++ " <path>  Format and verify result parses"
  putStrLn $ "  " ++ cyan ++ "validate" ++ reset ++ " <path>          Comprehensive validation (parse + lint)"
  putStrLn $ "  " ++ cyan ++ "stats" ++ reset ++ " <path>             Show code statistics"
  putStrLn ""
  putStrLn $ bold ++ "ANALYSIS:" ++ reset
  putStrLn $ "  " ++ cyan ++ "ast" ++ reset ++ " <path>               Generate AST JSON"
  putStrLn $ "  " ++ cyan ++ "ast --pretty" ++ reset ++ " <path>     Generate pretty-printed AST JSON"
  putStrLn $ "  " ++ cyan ++ "ast delete" ++ reset ++ " <path>       Delete AST JSON files"
  putStrLn $ "  " ++ cyan ++ "deps" ++ reset ++ " <file>              Show dependency tree"
  putStrLn ""
  putStrLn $ bold ++ "TESTING & DOCS:" ++ reset
  putStrLn $ "  " ++ cyan ++ "test" ++ reset ++ " [pattern]           Run tests (optionally matching pattern)"
  putStrLn $ "  " ++ cyan ++ "docs" ++ reset ++ " <path>              Generate documentation"
  putStrLn ""
  putStrLn $ bold ++ "UTILITIES:" ++ reset
  putStrLn $ "  " ++ cyan ++ "config" ++ reset ++ "                   Show current configuration"
  putStrLn $ "  " ++ cyan ++ "completion" ++ reset ++ " <shell>      Generate shell completions (bash/zsh/fish)"
  putStrLn $ "  " ++ cyan ++ "version" ++ reset ++ "                  Show version information"
  putStrLn $ "  " ++ cyan ++ "help" ++ reset ++ " [command]           Show help for a command"
  putStrLn ""
  putStrLn $ bold ++ "EXAMPLES:" ++ reset
  putStrLn $ "  nyf new my-project       " ++ dim ++ "# Create new project" ++ reset
  putStrLn $ "  nyf run                  " ++ dim ++ "# Build and run main.ny" ++ reset
  putStrLn $ "  nyf format src/          " ++ dim ++ "# Format all files in src/" ++ reset
  putStrLn $ "  nyf lint .               " ++ dim ++ "# Check all .ny files" ++ reset
  putStrLn $ "  nyf test                 " ++ dim ++ "# Run test suite" ++ reset
  putStrLn ""
  putStrLn $ "For more information: " ++ cyan ++ "nyf help <command>" ++ reset

printCommandHelp :: String -> IO ()
printCommandHelp cmd = case cmd of
  "new" -> do
    putStrLn $ bold ++ "nyf new" ++ reset ++ " - Create a new Nytrix project"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "  nyf new <project-name>"
    putStrLn ""
    putStrLn "Creates a new directory with:"
    putStrLn "  • src/main.ny - Main source file"
    putStrLn "  • format.yaml - Formatting configuration"
    putStrLn "  • package.yaml - Project metadata"
    putStrLn "  • README.md - Project documentation"
    putStrLn "  • .gitignore - Git ignore rules"

  "init" -> do
    putStrLn $ bold ++ "nyf init" ++ reset ++ " - Initialize project in current directory"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "  nyf init"
    putStrLn ""
    putStrLn "Creates project files in the current directory."

  "format" -> do
    putStrLn $ bold ++ "nyf format" ++ reset ++ " - Format Nytrix code"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "  nyf format <path>              Format in place"
    putStrLn "  nyf format --check <path>      Check only (CI mode)"
    putStrLn "  nyf format --diff <path>       Show diff without writing"
    putStrLn "  nyf format --verify <path>     Format and verify"
    putStrLn ""
    putStrLn "The path can be a file or directory (recursive)."

  "lint" -> do
    putStrLn $ bold ++ "nyf lint" ++ reset ++ " - Parse and check syntax"
    putStrLn ""
    putStrLn "USAGE:"
    putStrLn "  nyf lint <path>"
    putStrLn ""
    putStrLn "Parses files and reports syntax errors without modifying them."

  _ -> do
    putStrLn $ "No detailed help available for: " ++ cmd
    putStrLn "Run 'nyf help' for available commands."

--- TEST RUNNER

runTests :: IO ()
runTests = runTestsWithPattern ""

runTestsWithPattern :: String -> IO ()
runTestsWithPattern pattern = do
  -- First try local tests/ directory (for development)
  localTestsExist <- doesDirectoryExist "tests"

  if localTestsExist
    then do
      if null pattern
        then putStrLn "Running all tests from local tests/ directory..."
        else putStrLn $ "Running tests matching '" ++ pattern ++ "' from local tests/ directory..."
      Test.runAllTests -- TODO: Add pattern matching to Test module
    else do
      -- Fall back to installed data files
      dataDir <- getDataDir
      let testsDir = dataDir </> "tests"
      testsExist <- doesDirectoryExist testsDir

      if testsExist
        then do
          putStrLn $ "Running tests from " ++ testsDir ++ "..."
          currentDir <- getCurrentDirectory
          setCurrentDirectory dataDir
          Test.runAllTests
          setCurrentDirectory currentDir
        else do
          putStrLn $ red ++ "Error: No tests found" ++ reset
          putStrLn "  - Local tests/ directory not found"
          putStrLn $ "  - Installed tests not found at: " ++ testsDir
          exitFailure

--- PROJECT SCAFFOLDING

createNewProject :: String -> IO ()
createNewProject projectName = do
  -- Validate project name
  when (null projectName) $ do
    putStrLn $ red ++ "Error: Project name cannot be empty" ++ reset
    exitFailure

  when (elem '/' projectName || elem '\\' projectName) $ do
    putStrLn $ red ++ "Error: Project name cannot contain path separators" ++ reset
    exitFailure

  -- Check if directory already exists
  exists <- doesDirectoryExist projectName
  when exists $ do
    putStrLn $ red ++ "Error: Directory '" ++ projectName ++ "' already exists" ++ reset
    exitFailure

  putStrLn $ bold ++ "Creating new Nytrix project: " ++ projectName ++ reset
  putStrLn ""

  -- Get git info
  mGitUser <- getGitRemote
  mUserName <- getGitUserName
  mUserEmail <- getGitUserEmail

  let gitUser = maybe "username" id mGitUser
      userName = maybe "Your Name" id mUserName
      userEmail = maybe "your.email@example.com" id mUserEmail

  -- Create directory structure
  createDirectory projectName
  createDirectory (projectName </> "src")
  createDirectory (projectName </> "build")
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created directory: " ++ projectName
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created directory: " ++ projectName </> "src"
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created directory: " ++ projectName </> "build"

  -- Create files
  let mainPath = projectName </> "src" </> "main.ny"
  TIO.writeFile mainPath (mainTemplate projectName)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ mainPath

  let configPath = projectName </> "format.yaml"
  TIO.writeFile configPath formatYamlTemplate
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ configPath

  let packagePath = projectName </> "package.yaml"
  TIO.writeFile packagePath (packageYamlTemplate projectName gitUser userName userEmail)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ packagePath

  let readmePath = projectName </> "README.md"
  TIO.writeFile readmePath (readmeTemplate projectName)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ readmePath

  let gitignorePath = projectName </> ".gitignore"
  TIO.writeFile gitignorePath gitignoreTemplate
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ gitignorePath

  putStrLn ""
  putStrLn $ green ++ bold ++ "✨ Project created successfully!" ++ reset
  putStrLn ""
  putStrLn $ bold ++ "Next steps:" ++ reset
  putStrLn $ "  " ++ cyan ++ "cd " ++ projectName ++ reset
  putStrLn $ "  " ++ cyan ++ "nyf run" ++ reset ++ "              # Compile and run"
  putStrLn $ "  " ++ cyan ++ "nyf format src/" ++ reset ++ "      # Format your code"
  putStrLn ""

initProject :: IO ()
initProject = do
  putStrLn $ bold ++ "Initializing Nytrix project in current directory..." ++ reset
  putStrLn ""

  -- Get current directory name as project name
  currentDir <- getCurrentDirectory
  let projectName = last $ splitDirectories currentDir

  -- Check if src/ already exists
  srcExists <- doesDirectoryExist "src"
  when srcExists $ do
    putStrLn $ red ++ "Error: src/ directory already exists" ++ reset
    putStrLn "This directory appears to already be a Nytrix project."
    exitFailure

  -- Get git info
  mGitUser <- getGitRemote
  mUserName <- getGitUserName
  mUserEmail <- getGitUserEmail

  let gitUser = maybe "username" id mGitUser
      userName = maybe "Your Name" id mUserName
      userEmail = maybe "your.email@example.com" id mUserEmail

  -- Create directory structure
  createDirectory "src"
  createDirectory "build"
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created directory: src"
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created directory: build"

  -- Create files
  let mainPath = "src" </> "main.ny"
  TIO.writeFile mainPath (mainTemplate projectName)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ mainPath

  let configPath = "format.yaml"
  TIO.writeFile configPath formatYamlTemplate
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ configPath

  let packagePath = "package.yaml"
  TIO.writeFile packagePath (packageYamlTemplate projectName gitUser userName userEmail)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ packagePath

  let readmePath = "README.md"
  TIO.writeFile readmePath (readmeTemplate projectName)
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ readmePath

  let gitignorePath = ".gitignore"
  TIO.writeFile gitignorePath gitignoreTemplate
  putStrLn $ green ++ "  ✓ " ++ reset ++ "Created file: " ++ gitignorePath

  putStrLn ""
  putStrLn $ green ++ bold ++ "✨ Project initialized successfully!" ++ reset
  putStrLn ""

--- BUILD AND RUN

buildProject :: IO ()
buildProject = do
  let mainPath = "src" </> "main.ny"
  mainExists <- doesFileExist mainPath

  unless mainExists $ do
    putStrLn $ red ++ "Error: src/main.ny not found" ++ reset
    putStrLn "Run this command from your project root directory"
    exitFailure

  let buildDir = "build"
  let executable = buildDir </> "main"

  buildExists <- doesDirectoryExist buildDir
  unless buildExists $ createDirectory buildDir

  putStrLn $ dim ++ "Compiling src/main.ny..." ++ reset
  exitCode <- rawSystem "nytrix" ["-o", executable, mainPath]
  case exitCode of
    ExitSuccess -> putStrLn $ green ++ "✓ Build successful" ++ reset
    ExitFailure _ -> do
      putStrLn $ red ++ "✗ Build failed" ++ reset
      exitFailure

cleanProject :: IO ()
cleanProject = do
  buildExists <- doesDirectoryExist "build"
  if buildExists
    then do
      removeDirectoryRecursive "build"
      putStrLn $ green ++ "✓ Removed build/ directory" ++ reset
    else
      putStrLn $ dim ++ "Nothing to clean (build/ does not exist)" ++ reset

watchProject :: IO ()
watchProject = do
  putStrLn $ yellow ++ "Watch mode not yet implemented" ++ reset
  putStrLn "This will watch for file changes and auto-rebuild."
  exitFailure

runProject :: IO ()
runProject = do
  let mainPath = "src" </> "main.ny"
  mainExists <- doesFileExist mainPath

  unless mainExists $ do
    putStrLn $ red ++ "Error: src/main.ny not found" ++ reset
    putStrLn "Run this command from your project root directory"
    exitFailure

  let buildDir = "build"
  let executable = buildDir </> "main"

  buildExists <- doesDirectoryExist buildDir
  unless buildExists $ createDirectory buildDir

  exeExists <- doesFileExist executable

  needsCompile <- if exeExists
    then do
      mainModTime <- getModificationTime mainPath
      exeModTime <- getModificationTime executable
      return $ mainModTime > exeModTime
    else return True

  when needsCompile $ do
    putStrLn $ dim ++ "Compiling src/main.ny..." ++ reset
    exitCode <- rawSystem "nytrix" ["-o", executable, mainPath]
    case exitCode of
      ExitSuccess -> putStrLn $ green ++ "✓ Compiled successfully" ++ reset
      ExitFailure _ -> do
        putStrLn $ red ++ "✗ Compilation failed" ++ reset
        exitFailure

  putStrLn $ dim ++ "Running..." ++ reset
  putStrLn ""
  exitCode <- rawSystem executable []
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> exitWith (ExitFailure code)

runSpecificFile :: FilePath -> IO ()
runSpecificFile filepath = do
  fileExists <- doesFileExist filepath
  unless fileExists $ do
    putStrLn $ red ++ "Error: File not found: " ++ filepath ++ reset
    exitFailure

  let buildDir = "build"
  let executable = buildDir </> "temp_run"

  buildExists <- doesDirectoryExist buildDir
  unless buildExists $ createDirectory buildDir

  putStrLn $ dim ++ "Compiling " ++ filepath ++ "..." ++ reset
  exitCode <- rawSystem "nytrix" ["-o", executable, filepath]
  case exitCode of
    ExitSuccess -> putStrLn $ green ++ "✓ Compiled successfully" ++ reset
    ExitFailure _ -> do
      putStrLn $ red ++ "✗ Compilation failed" ++ reset
      exitFailure

  putStrLn $ dim ++ "Running..." ++ reset
  putStrLn ""
  exitCode' <- rawSystem executable []
  case exitCode' of
    ExitSuccess -> return ()
    ExitFailure code -> exitWith (ExitFailure code)

--- GIT USER INFORMATION

getGitUserName :: IO (Maybe String)
getGitUserName = catch
  (Just . trim <$> readProcess "git" ["config", "user.name"] "")
  (\(_::SomeException) -> return Nothing)

getGitUserEmail :: IO (Maybe String)
getGitUserEmail = catch
  (Just . trim <$> readProcess "git" ["config", "user.email"] "")
  (\(_::SomeException) -> return Nothing)

getGitRemote :: IO (Maybe String)
getGitRemote = catch
  (Just . extractGithubUsername . trim <$> readProcess "git" ["config", "remote.origin.url"] "")
  (\(_::SomeException) -> return Nothing)

extractGithubUsername :: String -> String
extractGithubUsername url
  | "github.com:" `isInfixOf` url =
      let afterColon = drop 1 $ dropWhile (/= ':') url
          username = takeWhile (/= '/') afterColon
      in username
  | "github.com/" `isInfixOf` url =
      let gitStr = "github.com/" :: String
          afterGithub = drop (length gitStr) $ dropWhile (/= 'm') url
          username = takeWhile (/= '/') afterGithub
      in if null username then "username" else username
  | otherwise = "username"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

--- FORMAT OPERATIONS

formatPath :: FilePath -> Bool -> Bool -> Maybe Bool -> IO ()
formatPath path checkOnly showDiff verify = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then formatDirectoryRecursive path checkOnly showDiff verify
    else if isFile
      then formatSingleFile path checkOnly showDiff verify
      else do
        putStrLn $ red ++ "Error: Not a file or directory: " ++ path ++ reset
        exitFailure

formatDirectoryRecursive :: FilePath -> Bool -> Bool -> Maybe Bool -> IO ()
formatDirectoryRecursive dir checkOnly showDiff verify = do
  unless showDiff $ putStrLn $ dim ++ "Formatting directory: " ++ dir ++ reset
  contents <- listDirectory dir

  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (\f -> formatSingleFile (dir </> f) checkOnly showDiff verify) nyFiles

  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (\d -> formatDirectoryRecursive (dir </> d) checkOnly showDiff verify) subdirs

formatSingleFile :: FilePath -> Bool -> Bool -> Maybe Bool -> IO ()
formatSingleFile filepath checkOnly showDiff verify = do
  unless showDiff $ putStrLn $ dim ++ "Formatting: " ++ formatFilePath filepath ++ reset

  input <- TIO.readFile filepath

  let dir = takeDirectory filepath
  mConfigPath <- findFormatYaml dir
  cfg <- loadFormatConfig mConfigPath

  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ red ++ "Parse error in " ++ filepath ++ ":" ++ reset
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right ast -> do
      let formatted = format cfg ast

      if checkOnly
        then
          if formatted == input
            then putStrLn $ green ++ "✓" ++ reset ++ " Already formatted: " ++ formatFilePath filepath
            else do
              putStrLn $ yellow ++ "✗" ++ reset ++ " Needs formatting: " ++ formatFilePath filepath
              exitFailure
        else if showDiff
          then do
            putStrLn $ bold ++ "--- " ++ filepath ++ reset
            putStrLn $ bold ++ "+++ " ++ filepath ++ " (formatted)" ++ reset
            -- Simple diff (could be enhanced)
            when (formatted /= input) $ do
              putStrLn $ red ++ "- Original" ++ reset
              putStrLn $ green ++ "+ Formatted" ++ reset
          else do
            TIO.writeFile filepath formatted
            putStrLn $ green ++ "✓" ++ reset ++ " Formatted: " ++ formatFilePath filepath

            -- Verify if requested
            when (verify == Just True) $ do
              verifyInput <- TIO.readFile filepath
              case parse file filepath verifyInput of
                Left verifyErr -> do
                  putStrLn $ red ++ "✗ Verification failed - formatted code doesn't parse!" ++ reset
                  TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty verifyErr
                  exitFailure
                Right _ ->
                  putStrLn $ green ++ "  ✓ Verified: formatted code parses correctly" ++ reset

--- LINT OPERATIONS

lintPath :: FilePath -> IO ()
lintPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then lintDirectoryRecursive path
    else if isFile
      then lintSingleFile path
      else do
        putStrLn $ red ++ "Error: Not a file or directory: " ++ path ++ reset
        exitFailure

lintDirectoryRecursive :: FilePath -> IO ()
lintDirectoryRecursive dir = do
  putStrLn $ dim ++ "Linting directory: " ++ dir ++ reset
  contents <- listDirectory dir

  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (lintSingleFile . (dir </>)) nyFiles

  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (lintDirectoryRecursive . (dir </>)) subdirs

lintSingleFile :: FilePath -> IO ()
lintSingleFile filepath = do
  input <- TIO.readFile filepath

  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ red ++ "Parse error in " ++ filepath ++ ":" ++ reset
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right _ast -> do
      putStrLn $ green ++ "✓" ++ reset ++ " Parsed successfully: " ++ formatFilePath filepath

--- VALIDATE OPERATIONS

validatePath :: FilePath -> IO ()
validatePath path = do
  putStrLn $ dim ++ "Validating: " ++ path ++ reset
  lintPath path
  putStrLn $ green ++ "✓ Validation complete" ++ reset

--- STATS OPERATIONS

statsPath :: FilePath -> IO ()
statsPath path = do
  isFile <- doesFileExist path

  if isFile
    then statsFile path
    else do
      putStrLn $ yellow ++ "Stats for directories not yet implemented" ++ reset
      putStrLn "Please specify a single .ny file"
      exitFailure

statsFile :: FilePath -> IO ()
statsFile filepath = do
  input <- TIO.readFile filepath
  let lines' = T.lines input
      lineCount = length lines'
      nonEmptyLines = length $ filter (not . T.null . T.strip) lines'

  putStrLn $ bold ++ "Statistics for: " ++ filepath ++ reset
  putStrLn $ "  Lines: " ++ show lineCount
  putStrLn $ "  Non-empty lines: " ++ show nonEmptyLines
  putStrLn $ dim ++ "  (More stats coming soon)" ++ reset

--- AST OPERATIONS

astPath :: FilePath -> Bool -> IO ()
astPath path pretty = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then do
      count <- astDirectoryRecursive path pretty
      putStrLn ""
      putStrLn $ green ++ "Generated " ++ show count ++ " AST JSON file(s)" ++ reset
    else if isFile
      then do
        astSingleFile path pretty
        putStrLn ""
        putStrLn $ green ++ "Generated 1 AST JSON file" ++ reset
      else do
        putStrLn $ red ++ "Error: Not a file or directory: " ++ path ++ reset
        exitFailure

astDirectoryRecursive :: FilePath -> Bool -> IO Int
astDirectoryRecursive dir pretty = do
  putStrLn $ dim ++ "Generating AST JSON for directory: " ++ dir ++ reset
  contents <- listDirectory dir

  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (\f -> astSingleFile (dir </> f) pretty) nyFiles
  let currentCount = length nyFiles

  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  subcounts <- mapM (\d -> astDirectoryRecursive (dir </> d) pretty) subdirs

  return $ currentCount + sum subcounts

astSingleFile :: FilePath -> Bool -> IO ()
astSingleFile filepath pretty = do
  putStrLn $ dim ++ "Generating AST: " ++ formatFilePath filepath ++ reset

  input <- TIO.readFile filepath

  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ red ++ "Parse error in " ++ filepath ++ ":" ++ reset
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right ast -> do
      let jsonPath = take (length filepath - 3) filepath ++ ".json"

      if pretty
        then do
          -- Pretty print JSON with indentation
          let jsonData = Aeson.encode ast
          let prettyJson = BLC.unlines $ map BLC.pack $ lines $ prettyPrintJSON $ BLC.unpack jsonData
          BL.writeFile jsonPath prettyJson
        else do
          let jsonData = Aeson.encode ast
          BL.writeFile jsonPath jsonData

      putStrLn $ green ++ "✓" ++ reset ++ " Generated: " ++ formatFilePathJSON jsonPath

-- Simple JSON pretty printer (basic indentation)
prettyPrintJSON :: String -> String
prettyPrintJSON json = unlines $ prettyLines 0 json
  where
    prettyLines :: Int -> String -> [String]
    prettyLines _ [] = []
    prettyLines indent ('{':rest) =
      (replicate indent ' ' ++ "{") : prettyLines (indent + 2) rest
    prettyLines indent ('}':rest) =
      let newIndent = max 0 (indent - 2)
      in (replicate newIndent ' ' ++ "}") : prettyLines newIndent rest
    prettyLines indent ('[':rest) =
      (replicate indent ' ' ++ "[") : prettyLines (indent + 2) rest
    prettyLines indent (']':rest) =
      let newIndent = max 0 (indent - 2)
      in (replicate newIndent ' ' ++ "]") : prettyLines newIndent rest
    prettyLines indent (',':rest) =
      ("," : prettyLines indent rest)
    prettyLines indent (c:rest) =
      case prettyLines indent rest of
        [] -> [[c]]
        (l:ls) -> (c:l) : ls

astDeletePath :: FilePath -> IO ()
astDeletePath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then do
      count <- astDeleteDirectoryRecursive path
      putStrLn ""
      putStrLn $ green ++ "Deleted " ++ show count ++ " AST JSON file(s)" ++ reset
    else if isFile
      then do
        deleted <- astDeleteSingleFile path
        putStrLn ""
        if deleted
          then putStrLn $ green ++ "Deleted 1 AST JSON file" ++ reset
          else putStrLn $ dim ++ "No AST JSON file to delete" ++ reset
      else do
        putStrLn $ red ++ "Error: Not a file or directory: " ++ path ++ reset
        exitFailure

astDeleteDirectoryRecursive :: FilePath -> IO Int
astDeleteDirectoryRecursive dir = do
  putStrLn $ dim ++ "Deleting AST JSON for directory: " ++ dir ++ reset
  contents <- listDirectory dir

  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  results <- mapM (astDeleteSingleFile . (dir </>)) nyFiles
  let currentCount = length $ filter id results

  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  subcounts <- mapM (astDeleteDirectoryRecursive . (dir </>)) subdirs

  return $ currentCount + sum subcounts

astDeleteSingleFile :: FilePath -> IO Bool
astDeleteSingleFile filepath = do
  let nyPath = if ".ny" `isSuffixOf` filepath
                 then filepath
                 else filepath ++ ".ny"

  nyExists <- doesFileExist nyPath

  if not nyExists
    then do
      putStrLn $ yellow ++ "Warning: Skipping - no corresponding .ny file found for: " ++ filepath ++ reset
      return False
    else do
      let jsonPath = if ".ny" `isSuffixOf` filepath
                       then take (length filepath - 3) filepath ++ ".json"
                       else filepath ++ ".json"

      jsonExists <- doesFileExist jsonPath

      if jsonExists
        then do
          removeFile jsonPath
          putStrLn $ green ++ "✓" ++ reset ++ " Deleted: " ++ formatFilePathJSON jsonPath
          return True
        else do
          putStrLn $ dim ++ "Note: JSON file does not exist: " ++ jsonPath ++ reset
          return False

--- DEPS OPERATIONS

depsFile :: FilePath -> IO ()
depsFile filepath = do
  putStrLn $ yellow ++ "Dependency analysis not yet implemented" ++ reset
  putStrLn $ "This will show the dependency tree for: " ++ filepath
  exitFailure

--- DOCS OPERATIONS

docsPath :: FilePath -> IO ()
docsPath path = do
  putStrLn $ yellow ++ "Documentation generation not yet implemented" ++ reset
  putStrLn $ "This will generate docs for: " ++ path
  exitFailure

--- CONFIG OPERATIONS

showConfig :: IO ()
showConfig = do
  mConfigPath <- findFormatYaml "."
  case mConfigPath of
    Nothing -> do
      putStrLn $ dim ++ "No format.yaml found, using defaults" ++ reset
    Just configPath -> do
      putStrLn $ bold ++ "Using configuration from: " ++ configPath ++ reset
      cfg <- loadFormatConfig mConfigPath
      putStrLn ""
      putStrLn $ dim ++ "(Configuration details would be displayed here)" ++ reset

--- COMPLETION OPERATIONS

generateCompletion :: String -> IO ()
generateCompletion shell = case shell of
  "bash" -> do
    putStrLn $ yellow ++ "Bash completion not yet implemented" ++ reset
    exitFailure
  "zsh" -> do
    putStrLn $ yellow ++ "Zsh completion not yet implemented" ++ reset
    exitFailure
  "fish" -> do
    putStrLn $ yellow ++ "Fish completion not yet implemented" ++ reset
    exitFailure
  _ -> do
    putStrLn $ red ++ "Error: Unknown shell: " ++ shell ++ reset
    putStrLn "Supported shells: bash, zsh, fish"
    exitFailure

--- HELPER FUNCTIONS

formatFilePath :: FilePath -> String
formatFilePath path =
  let withoutExt = if ".ny" `isSuffixOf` path then take (length path - 3) path else path
      parts = splitPath withoutExt
      dottedPath = intercalate "." parts
      segments = splitOn' '.' dottedPath
  in colorizeSegments segments
  where
    splitPath :: FilePath -> [String]
    splitPath p = filter (not . null) $ map cleanSep $ splitDirectories p
      where
        cleanSep = filter (\c -> c /= '/' && c /= '\\')

    splitOn' :: Char -> String -> [String]
    splitOn' _ [] = []
    splitOn' delim str = case break (== delim) str of
      (part, []) -> [part]
      (part, _:rest) -> part : splitOn' delim rest

    colorizeSegments :: [String] -> String
    colorizeSegments [] = ""
    colorizeSegments [x] = white ++ x ++ reset
    colorizeSegments (x:xs) =
      yellow ++ x ++ reset ++ gray ++ "." ++ reset ++ colorizeSegments xs

formatFilePathJSON :: FilePath -> String
formatFilePathJSON path =
  let withoutExt = if ".json" `isSuffixOf` path then take (length path - 5) path else path
      parts = splitPath withoutExt
      dottedPath = intercalate "." parts
      segments = splitOn' '.' dottedPath
  in colorizeSegments segments
  where
    splitPath :: FilePath -> [String]
    splitPath p = filter (not . null) $ map cleanSep $ splitDirectories p
      where
        cleanSep = filter (\c -> c /= '/' && c /= '\\')

    splitOn' :: Char -> String -> [String]
    splitOn' _ [] = []
    splitOn' delim str = case break (== delim) str of
      (part, []) -> [part]
      (part, _:rest) -> part : splitOn' delim rest

    colorizeSegments :: [String] -> String
    colorizeSegments [] = ""
    colorizeSegments [x] = white ++ x ++ reset
    colorizeSegments (x:xs) =
      yellow ++ x ++ reset ++ gray ++ "." ++ reset ++ colorizeSegments xs
