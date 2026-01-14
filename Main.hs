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
      setCurrentDirectory )
import System.FilePath ( (</>), takeExtension, takeDirectory, splitDirectories )
import System.IO (stderr)
import System.Process ( rawSystem, readProcess )
import Control.Monad (filterM, when, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_nyf (getDataDir)
import Control.Exception (catch, SomeException)
import Data.List ( isInfixOf, dropWhileEnd, isSuffixOf, intercalate )
import Data.Char (isSpace)

import Parser (file)
import Formatter (format)
import FormatConfig (loadFormatConfig, findFormatYaml)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Test

--- ANSI COLOR CODES

white :: String
white = "\ESC[97m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

gray :: String
gray = "\ESC[90m"

reset :: String
reset = "\ESC[0m"

bold :: String
bold = "\ESC[1m"


--- MAIN ENTRY POINT

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] ->
      runTests

    ["new", projectName] ->
      createNewProject projectName

    ["run"] ->
      runProject

    ["format", path] ->
      formatPath path

    ["check", path] ->
      checkPath path

    [path] ->
      checkPath path

    _ ->
      printUsage

--- TEST RUNNER

runTests :: IO ()
runTests = do
  -- First try local tests/ directory (for development)
  localTestsExist <- doesDirectoryExist "tests"

  if localTestsExist
    then do
      putStrLn "Running tests from local tests/ directory..."
      Test.runAllTests
    else do
      -- Fall back to installed data files
      dataDir <- getDataDir
      let testsDir = dataDir </> "tests"
      testsExist <- doesDirectoryExist testsDir

      if testsExist
        then do
          putStrLn $ "Running tests from " ++ testsDir ++ "..."
          -- Change to the data directory so Test module can find files
          currentDir <- getCurrentDirectory
          setCurrentDirectory dataDir
          Test.runAllTests
          setCurrentDirectory currentDir
        else do
          putStrLn "Error: No tests found"
          putStrLn "  - Local tests/ directory not found"
          putStrLn $ "  - Installed tests not found at: " ++ testsDir
          putStrLn ""
          putStrLn "If you're developing nyf, run 'nyf test' from the project root."
          putStrLn "If you installed nyf, the test files may not have been installed."
          exitFailure

--- PROJECT SCAFFOLDING

-- | Create a new Nytrix project with standard structure
createNewProject :: String -> IO ()
createNewProject projectName = do
  -- Validate project name
  when (null projectName) $ do
    putStrLn "Error: Project name cannot be empty"
    exitFailure

  when (elem '/' projectName || elem '\\' projectName) $ do
    putStrLn "Error: Project name cannot contain path separators"
    exitFailure

  -- Check if directory already exists
  exists <- doesDirectoryExist projectName
  when exists $ do
    putStrLn $ "Error: Directory '" ++ projectName ++ "' already exists"
    exitFailure

  putStrLn $ "Creating new Nytrix project: " ++ projectName
  putStrLn ""

  -- Create directory structure
  createDirectory projectName
  createDirectory (projectName </> "src")
  createDirectory (projectName </> "build")
  putStrLn $ "✓ Created directory: " ++ projectName
  putStrLn $ "✓ Created directory: " ++ projectName </> "src"
  putStrLn $ "✓ Created directory: " ++ projectName </> "build"

  -- Create main.ny
  let mainPath = projectName </> "src" </> "main.ny"
  TIO.writeFile mainPath (mainTemplate projectName)
  putStrLn $ "✓ Created file: " ++ mainPath

  -- Create format.yaml
  let configPath = projectName </> "format.yaml"
  TIO.writeFile configPath formatYamlTemplate
  putStrLn $ "✓ Created file: " ++ configPath

  -- Create README.md
  let readmePath = projectName </> "README.md"
  TIO.writeFile readmePath (readmeTemplate projectName)
  putStrLn $ "✓ Created file: " ++ readmePath

  -- Create .gitignore
  let gitignorePath = projectName </> ".gitignore"
  TIO.writeFile gitignorePath gitignoreTemplate
  putStrLn $ "✓ Created file: " ++ gitignorePath

  putStrLn ""
  putStrLn $ green ++ "Project created successfully!" ++ reset
  putStrLn ""
  putStrLn "Next steps:"
  putStrLn $ "  cd " ++ projectName
  putStrLn   "  nyf run              # Compile and run"
  putStrLn   "  nyf format src/      # Format your code"
  putStrLn ""

--- RUN PROJECT

-- | Find main.ny and compile/run it with nytrix
runProject :: IO ()
runProject = do
  -- Look for src/main.ny
  let mainPath = "src" </> "main.ny"
  mainExists <- doesFileExist mainPath

  unless mainExists $ do
    putStrLn "Error: src/main.ny not found"
    putStrLn "Run this command from your project root directory"
    exitFailure

  -- Check if already compiled
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
    putStrLn "Compiling src/main.ny..."
    -- Use nytrix to compile: nytrix -o build/main src/main.ny
    exitCode <- rawSystem "nytrix" ["-o", executable, mainPath]
    case exitCode of
      ExitSuccess -> putStrLn $ green ++ "✓ Compiled successfully" ++ reset
      ExitFailure _ -> do
        putStrLn "Compilation failed"
        exitFailure

  -- Run the executable
  putStrLn "Running..."
  putStrLn ""
  exitCode <- rawSystem executable []
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> exitWith (ExitFailure code)

--- PROJECT TEMPLATES

mainTemplate :: String -> T.Text
mainTemplate projectName = T.unlines
  [ ";;; Main Entry Point -- This is the main file for your Nytrix project"
  , ";; Author: NAME"
  , ";; Maintainer: NAME"
  , ";; Version: 0.0.1"
  , ";; Keywords: main " <> T.pack projectName
  , ";; URL: TODO"
  , ";;; Commentary:"
  , ";; TODO Write me!"
  , ";;; Code:"
  , ""
  , "use std.io"
  , ""
  , "fn main() {"
  , "    \"Program entry point\""
  , "    print(\"Hello, " <> T.pack projectName <> "!\")"
  , "}"
  ]

-- Get git user name
getGitUserName :: IO (Maybe String)
getGitUserName = catch
  (Just . trim <$> readProcess "git" ["config", "user.name"] "")
  (\(_::SomeException) -> return Nothing)

-- Get git user email
getGitUserEmail :: IO (Maybe String)
getGitUserEmail = catch
  (Just . trim <$> readProcess "git" ["config", "user.email"] "")
  (\(_::SomeException) -> return Nothing)

-- Get git remote origin
getGitRemote :: IO (Maybe String)
getGitRemote = catch
  (Just . extractGithubUsername . trim <$> readProcess "git" ["config", "remote.origin.url"] "")
  (\(_::SomeException) -> return Nothing)

-- Extract username from git URL
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

-- Updated template with actual user info
packageYamlTemplate :: String -> String -> String -> String -> T.Text
packageYamlTemplate projectName gitUser userName userEmail = T.unlines
  [ "name:                " <> T.pack projectName
  , "version:             0.1.0.0"
  , "github:              \"" <> T.pack gitUser <> "/" <> T.pack projectName <> "\""
  , "license:             MIT"
  , "author:              \"" <> T.pack userName <> "\""
  , "maintainer:          \"" <> T.pack userEmail <> "\""
  , "copyright:           \"2026 " <> T.pack userName <> "\""
  , ""
  , "description:         A Nytrix project"
  , ""
  , "dependencies:"
  , "- nytrix-std >= 0.1.0"
  , ""
  , "nytrix-options:"
  , "- -Wall"
  , "- -Wextra"
  , ""
  , "executables:"
  , "  " <> T.pack projectName <> ":"
  , "    main:                main.ny"
  , "    source-dirs:         src"
  ]

formatYamlTemplate :: T.Text
formatYamlTemplate = T.unlines
  [ ""
  , "### Nyf - CONFIGURATION"
  , ""
  , "### INDENTATION"
  , ""
  , "indentSize: 4                     # Spaces per indentation level"
  , "useTabs: false                    # Use tabs instead of spaces"
  , ""
  , "### LINE LENGTH"
  , ""
  , "maxLineLength: 80                 # Maximum line length before wrapping"
  , "wrapLongExpressions: false        # Automatically wrap expressions exceeding maxLineLength"
  , "wrapAfterOperator: true           # When wrapping: true = wrap after +, false = wrap before +"
  , "binaryOpWrapIndent: 4             # Extra indentation for wrapped binary operations"
  , "chainedCallWrapStyle: sameline    # 'sameline' or 'newline' for .foo().bar() chaining"
  , ""
  , "### BLANK LINES"
  , ""
  , "newlinesBetweenTopLevel: 1        # Blank lines between top-level declarations"
  , "newlinesBetweenFunctions: 2       # Blank lines between function definitions"
  , "newlinesInsideFunction: 0         # Blank lines between statements in function bodies"
  , "newlinesBeforeComment: 0          # Blank lines before standalone comments"
  , "newlinesAfterComment: 0           # Blank lines after standalone comments"
  , ""
  , "### Blank line management"
  , "preserveBlankLines: false         # Keep original blank lines (up to max)"
  , "maxConsecutiveBlankLines: 1       # Collapse N+ blank lines to this many (1 = no double blanks)"
  , "removeBlankLineAtStart: true      # Remove blank lines at file start"
  , "removeBlankLineAtEnd: true        # Remove blank lines at file end"
  , ""
  , "### SPACING: PARAMETERS & ARGUMENTS"
  , ""
  , "spaceAfterComma: 1                # Spaces after commas: (a, b) vs (a,b)"
  , "spaceBeforeComma: 0               # Spaces before commas: (a ,b) vs (a,b)"
  , "spaceInsideParens: 0              # Spaces inside parentheses: ( x ) vs (x)"
  , "spaceInsideBrackets: 0            # Spaces inside brackets: [ x ] vs [x]"
  , "spaceInsideBraces: 0              # Spaces inside braces: { x } vs {x}"
  , ""
  , "### SPACING: OPERATORS"
  , ""
  , "spaceAroundAssign: 1              # Spaces around = in assignments: a = b vs a=b"
  , "spaceAroundBinOp: 1               # Spaces around binary operators: a + b vs a+b"
  , "spaceAroundCompare: 1             # Spaces around comparisons: a == b vs a==b"
  , "spaceAfterUnaryOp: 0              # Spaces after unary operators: ! a vs !a"
  , "spaceAroundColon: 1               # Spaces around colons: a: Int vs a:Int"
  , "spaceAroundArrow: 1               # Spaces around arrows: -> in functions/matches"
  , ""
  , "### ALIGNMENT"
  , ""
  , "#### Assignment alignment (consecutive = signs)"
  , ""
  , "alignAssignments: false           # Vertically align = in consecutive assignments"
  , "alignAssignmentThreshold: 2       # Minimum consecutive lines needed to trigger alignment"
  , ""
  , "#### Comparison alignment (consecutive == != < > etc.)"
  , "alignComparisons: false           # Vertically align comparison operators"
  , "alignComparisonThreshold: 3       # Minimum consecutive lines needed"
  , "alignComparisonOps:               # Which operators to align"
  , "  - \"==\""
  , "  - \"!=\""
  , "  - \"<\""
  , "  - \">\""
  , "  - \"<=\""
  , "  - \">=\""
  , ""
  , "#### Function argument alignment"
  , "alignFunctionArgs: false          # Vertically align function arguments"
  , "oneArgPerLineThreshold: 5         # If more than N args, force one per line"
  , "alignAssertMessages: false        # Special: align second arg in assert() calls"
  , ""
  , "### FUNCTION DEFINITIONS"
  , ""
  , "spaceBeforeFnParen: 0             # Space between function name and (: fn foo () vs fn foo()"
  , "spaceBeforeFnBrace: 1             # Space before opening brace: fn() { vs fn(){"
  , "fnBraceOnNewLine: false           # Place { on new line: fn()\\n{ vs fn() {"
  , "spaceAfterFnKeyword: 1            # Space after 'fn': fn foo vs fn  foo"
  , "fnDocstringStyle: indented        # 'indented' or 'flush' - docstring indentation"
  , ""
  , "### CONTROL STRUCTURES (if, while, for, match)"
  , ""
  , "### Spacing before condition/expression"
  , ""
  , "spaceBeforeIfParen: 1             # if () vs if()"
  , "spaceBeforeWhileParen: 1          # while () vs while()"
  , "spaceBeforeForParen: 1            # for () vs for()"
  , "spaceBeforeMatchParen: 1          # match () vs match()"
  , ""
  , "### Brace placement"
  , "ifBraceOnNewLine: false           # if ()\\n{ vs if () {"
  , "whileBraceOnNewLine: false        # while ()\\n{ vs while () {"
  , "forBraceOnNewLine: false          # for ()\\n{ vs for () {"
  , "matchBraceOnNewLine: false        # match ()\\n{ vs match () {"
  , ""
  , "### Spacing after keyword"
  , ""
  , "spaceAfterIfKeyword: 1            # if vs if"
  , "spaceAfterWhileKeyword: 1         # while vs while"
  , "spaceAfterForKeyword: 1           # for vs for"
  , "spaceAfterMatchKeyword: 1         # match vs match"
  , ""
  , "### MATCH STATEMENTS"
  , ""
  , "spaceBeforeMatchArrow: 1          # Space before ->: pattern -> vs pattern->"
  , "spaceAfterMatchArrow: 1           # Space after ->: -> expr vs ->expr"
  , "matchArmIndent: 0                 # Extra indentation for match arms (in spaces)"
  , "matchPatternOnNewLine: false      # Force each pattern on its own line"
  , ""
  , "### COMMENTS"
  , ""
  , "### Basic comment formatting"
  , "spaceAfterCommentDelimiter: 1     # Spaces after ;: ; comment vs ;comment"
  , "commentDelimiterStyle: single     # 'single', 'double', or 'none' (number of spaces)"
  , "preserveCommentIndent: false      # Keep original comment indentation"
  , "preserveCommentFormatting: false  # Don't reformat text inside comments"
  , ""
  , "### Inline comment alignment"
  , ""
  , "alignInlineComments: false        # Align ; comments to fixed column"
  , "inlineCommentColumn: 40           # Column to align to (if enabled)"
  , ""
  , "### Block comments"
  , ""
  , "blockCommentStyle: horizontal     # 'horizontal' (*) or 'vertical' alignment"
  , ""
  , "### ORG-MODE FEATURES"
  , ""
  , "orgModeEnabled: true              # Enable all org-mode features below"
  , ""
  , "### Heading style"
  , ""
  , "headingStyle: semicolons          # 'semicolons' (;;; Title) or 'hash' (###  Title)"
  , ""
  , "### Checkbox formatting"
  , ""
  , "checkboxStyle: upper              # 'upper' ([X]) or 'lower' ([x]) for checked boxes"
  , ""
  , "### Automatic updates"
  , ""
  , "autoUpdateCookies: true           # Auto-update progress cookies [1/3]"
  , "autoAddClosedTimestamp: true      # Add CLOSED: timestamp when marking DONE"
  , "closedTimestampFormat: \"[%Y-%m-%d %a %H:%M]\"  # Timestamp format string"
  , ""
  , "### LISTS & COLLECTIONS"
  , ""
  , "### General collection formatting"
  , ""
  , "trailingCommaInLists: false       # Allow trailing comma: [1, 2,] vs [1, 2]"
  , "oneElementPerLine: false          # Force one element per line always"
  , "maxElementsPerLine: 5             # Before wrapping to multiple lines"
  , "listWrapThreshold: 60             # Wrap if total width exceeds this"
  , ""
  , "### Dictionary formatting"
  , ""
  , "dictColonStyle: after             # 'none' (a:1), 'after' (a: 1), 'both' (a : 1)"
  , ""
  , "### USE STATEMENTS"
  , ""
  , "### Spacing in use statements"
  , ""
  , "spaceAroundAs: 1                  # Spaces around 'as': use foo as bar vs use foo as bar"
  , "dotSeparatorSpacing: 0            # Spaces around dots: use foo.bar vs use foo . bar"
  , ""
  , "### Use statement organization"
  , ""
  , "sortUseStatements: false          # Sort use statements alphabetically"
  , "groupUseByPrefix: false           # Group by first component (std.*, user.*)"
  , "blankLinesBetweenUseGroups: 1     # Blank lines between groups (if grouping enabled)"
  , ""
  , ""
  , "### DEFINE/ASSIGNMENTS"
  , ""
  , "defineStyle: space                # 'space' (define x y =) or 'comma' (define x, y =)"
  , ""
  , "### LAMBDA & FUNCTION EXPRESSIONS"
  , ""
  , "lambdaStyle: keyword              # 'keyword' (lambda) or 'symbol' (λ)"
  , "spaceAfterLambdaKeyword: 0        # Space after 'lambda'/'λ'"
  , "lambdaBraceOnNewLine: false       # Place lambda body brace on new line"
  , ""
  , "### LAYOUT DECLARATIONS"
  , ""
  , "layoutFieldsOnNewLine: true       # Each field on its own line"
  , "layoutFieldAlignment: false       # Align field types vertically"
  , "layoutTrailingComma: false        # Trailing comma after last field"
  , ""
  , "### BLOCKS & STATEMENTS"
  , ""
  , "### Compact blocks"
  , "compactSmallBlocks: false         # Format small blocks on one line: { return x }"
  , "maxCompactBlockLength: 1          # Max statements in a compact block"
  , "blockStatementSpacing: 0          # Extra blank lines between statements in blocks"
  , ""
  , "### Empty blocks"
  , "emptyBlockStyle: compact          # 'compact' ({}), 'spaced' ({ }), or 'newline' ({\\n})"
  , ""
  , "### STRING LITERALS"
  , ""
  , "stringQuoteStyle: double          # 'double' (\") or 'single' (')"
  , "fstringSpacing: 0                 # Spaces in f-string braces: f\"{x}\" vs f\"{ x }\""
  , "preferTripleQuotes: false         # Use \"\"\" for multi-line strings"
  , ""
  , "### NUMERIC LITERALS"
  , ""
  , "integerGrouping: false            # Use underscores: 1_000_000 vs 1000000"
  , "integerGroupSize: 3               # Group every N digits (usually 3)"
  , "hexUpperCase: true                # 0xFF vs 0xff"
  , "floatScientificThreshold: 1000000 # Use scientific notation for numbers > this"
  , ""
  , "### RETURN STATEMENTS"
  , ""
  , "spaceAfterReturn: 1               # Space after 'return': return x vs return  x"
  , "returnOnSeparateLine: false       # Force return on its own line always"
  , ""
  , "### TRY-CATCH"
  , ""
  , "spaceAfterCatch: 0                # Space after 'catch': catch e vs catch  e"
  , "catchVarStyle: spaced             # 'spaced' (catch e), 'parens' (catch(e)), 'spaced_parens' (catch (e))"
  , ""
  , "### DEFER"
  , ""
  , "deferBraceOnNewLine: false        # Place defer { on new line"
  , "spaceAfterDefer: 1                # Space after 'defer' keyword"
  , ""
  , "### GOTO & LABELS"
  , ""
  , "labelColonSpacing: 0              # Space before colon: label: vs label :"
  , "spaceAfterGoto: 1                 # Space after 'goto' keyword"
  , ""
  , "### TYPE ANNOTATIONS"
  , ""
  , "spaceBeforeTypeColon: 0           # Space before :: x : Int vs x: Int"
  , "spaceAfterTypeColon: 1            # Space after :: x: Int vs x:Int"
  , ""
  , "### INDEXING & SLICING"
  , ""
  , "spaceAroundSliceColon: 0          # Spaces in slices: a[1 : 2] vs a[1:2]"
  , "spaceInsideIndexBrackets: 0       # Spaces in indexing: a[ 0 ] vs a[0]"
  , ""
  , "### ASM & EMBED"
  , ""
  , "asmArgumentSpacing: 1             # Spacing in asm() arguments"
  , "embedArgumentStyle: quoted        # 'quoted' (embed(\"path\")) or 'unquoted' (embed(path))"
  , ""
  , "### COMPTIME"
  , ""
  , "comptimeBraceOnNewLine: false     # Place comptime { on new line"
  , "spaceAfterComptime: 1             # Space after 'comptime' keyword"
  , ""
  , "### GENERAL CLEANUP"
  , ""
  , "trimTrailingWhitespace: true      # Remove trailing spaces from lines"
  , "ensureNewlineAtEOF: true          # Ensure file ends with newline"
  , "normalizeWhitespace: true         # Normalize multiple spaces to single space"
  , "collapseAdjacentSpaces: true      # Multiple spaces → single space (where allowed)"
  , ""
  , "###  STANDARD PRESETS "
  , ""
  , "# You can pick one with ~nyf standard COMPACT~"
  , ""
  , "####  COMPACT"
  , "## indentSize: 2"
  , "## spaceAroundBinOp: 0"
  , "## spaceAroundAssign: 0"
  , "## maxLineLength: 120"
  , "## newlinesBetweenFunctions: 1"
  , ""
  , "####  VERBOSE"
  , "## indentSize: 4"
  , "## spaceAroundBinOp: 1"
  , "## spaceAroundAssign: 1"
  , "## fnBraceOnNewLine: true"
  , "## ifBraceOnNewLine: true"
  , ""
  , "####  ALIGNED STYLE"
  , "## alignAssignments: true"
  , "## alignComparisons: true"
  , "## alignFunctionArgs: true"
  , "## layoutFieldAlignment: true"
  ]

readmeTemplate :: String -> T.Text
readmeTemplate projectName = T.unlines
  [ "# " <> T.pack projectName
  , ""
  , "A project in Nytrix."
  , ""
  , "## Project Structure"
  , ""
  , "```"
  , projectName'
  , "├── src/"
  , "│   └── main.ny        # Main entry point"
  , "├── format.yaml        # Formatter configuration"
  , "└── README.md          # This file"
  , "```"
  , ""
  , "## Getting Started"
  , ""
  , "### Format your code"
  , ""
  , "```bash"
  , "nyf format src/"
  , "```"
  , ""
  , "### Check syntax"
  , ""
  , "```bash"
  , "nyf check src/"
  , "```"
  , ""
  , "### Run your program"
  , ""
  , "```bash"
  , "nytrix run src/main.ny"
  , "```"
  , ""
  , "## Configuration"
  , ""
  , "Edit `format.yaml` to customize code formatting to your preferences."
  , "Over 100+ options are available to control every aspect of formatting."
  , ""
  , "## Learn More"
  , ""
  , "- [Nytrix Documentation](https://github.com/your-repo/nytrix)"
  , "- [Formatter Options](https://github.com/your-repo/nyf)"
  ]
  where
    projectName' = T.pack projectName

gitignoreTemplate :: T.Text
gitignoreTemplate = T.unlines
  [ "# Nytrix build artifacts"
  , "build/"
  , "*.o"
  , "*.hi"
  , "*.exe"
  ]

--- COMMAND IMPLEMENTATIONS

-- | Format a file or directory (writes changes to disk)
formatPath :: FilePath -> IO ()
formatPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then formatDirectoryRecursive path
    else if isFile
      then formatSingleFile path
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

-- | Check/parse a file or directory (no changes to disk)
checkPath :: FilePath -> IO ()
checkPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then checkDirectoryRecursive path
    else if isFile
      then checkSingleFile path
      else do
        putStrLn $ "Error: Not a file or directory: " ++ path
        exitFailure

--- FORMAT OPERATIONS (write to disk)

-- | Recursively format all .ny files in a directory
formatDirectoryRecursive :: FilePath -> IO ()
formatDirectoryRecursive dir = do
  putStrLn $ "Formatting directory: " ++ dir
  contents <- listDirectory dir

  -- Format all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (formatSingleFile . (dir </>)) nyFiles

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (formatDirectoryRecursive . (dir </>)) subdirs

-- | Format a single file in place
formatSingleFile :: FilePath -> IO ()
formatSingleFile filepath = do
  putStrLn $ "Formatting: " ++ formatFilePath filepath

  -- Read input
  input <- TIO.readFile filepath

  -- Load config from file's directory (searches upward)
  let dir = takeDirectory filepath
  mConfigPath <- findFormatYaml dir
  cfg <- loadFormatConfig mConfigPath

  -- Parse
  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ "Parse error in " ++ filepath ++ ":"
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right ast -> do
      -- Format
      let formatted = format cfg ast

      -- Write back
      TIO.writeFile filepath formatted
      putStrLn $ green ++ "✓" ++ reset ++ " Formatted: " ++ formatFilePath filepath

-- Helper function to format file paths with colors
formatFilePath :: FilePath -> String
formatFilePath path =
  let -- Remove .ny extension
      withoutExt = if ".ny" `isSuffixOf` path then take (length path - 3) path else path
      -- Split by directory separator and convert to dot notation
      parts = splitPath withoutExt
      -- Join with dots
      dottedPath = intercalate "." parts
      -- Split by dots to colorize
      segments = splitOn' '.' dottedPath
  in colorizeSegments segments
  where
    -- Split path into components and clean them up
    splitPath :: FilePath -> [String]
    splitPath p = filter (not . null) $ map cleanSep $ splitDirectories p
      where
        cleanSep = filter (\c -> c /= '/' && c /= '\\')

    -- Split string by character
    splitOn' :: Char -> String -> [String]
    splitOn' _ [] = []
    splitOn' delim str = case break (== delim) str of
      (part, []) -> [part]
      (part, _:rest) -> part : splitOn' delim rest

    -- Colorize the segments
    colorizeSegments :: [String] -> String
    colorizeSegments [] = ""
    colorizeSegments [x] = white ++ x ++ reset  -- Last segment is white
    colorizeSegments (x:xs) =
      yellow ++ x ++ reset ++ gray ++ "." ++ reset ++ colorizeSegments xs


--- CHECK OPERATIONS (read-only, no changes to disk)

-- | Recursively check all .ny files in a directory
checkDirectoryRecursive :: FilePath -> IO ()
checkDirectoryRecursive dir = do
  putStrLn $ "Checking directory: " ++ dir
  contents <- listDirectory dir

  -- Check all .ny files in current directory
  let nyFiles = filter (\f -> takeExtension f == ".ny") contents
  mapM_ (checkSingleFile . (dir </>)) nyFiles

  -- Recurse into subdirectories
  subdirs <- filterM (\f -> doesDirectoryExist (dir </> f)) contents
  mapM_ (checkDirectoryRecursive . (dir </>)) subdirs

-- | Check/parse a single file (no formatting)
checkSingleFile :: FilePath -> IO ()
checkSingleFile filepath = do
  input <- TIO.readFile filepath

  case parse file filepath input of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack $ "Parse error in " ++ filepath ++ ":"
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty err
      exitFailure

    Right _ast -> do
      putStrLn $ "✓ Parsed successfully: " ++ filepath

--- HELP

printUsage :: IO ()
printUsage = do
  putStrLn "Nytrix Formal Functional Formatter (nyf)"
  putStrLn ""
  putStrLn "A highly configurable code formatter for Nytrix [100+ options]."
  putStrLn ""
  putStrLn "USAGE:"
  putStrLn "  nyf new <name>       Create a new Nytrix project"
  putStrLn "  nyf run              Compile and run src/main.ny"
  putStrLn "  nyf <file>           Parse and check <file>"
  putStrLn "  nyf <dir>            Parse and check all .ny files in <dir> recursively"
  putStrLn "  nyf check <path>     Same as above (explicit check command)"
  putStrLn "  nyf format <file>    Format <file> in place"
  putStrLn "  nyf format <dir>     Recursively format all .ny files in <dir>"
  putStrLn "  nyf test             Run all tests in tests/ directory"
  putStrLn ""
  putStrLn "EXAMPLES:"
  putStrLn "  nyf new my-project         # Create new project"
  putStrLn "  cd my-project              # Enter project directory"
  putStrLn "  nyf run                    # Compile and run"
  putStrLn "  nyf format src/            # Format all files in src/"
  putStrLn ""
  exitFailure
