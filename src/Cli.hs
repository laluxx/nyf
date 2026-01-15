{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( Command(..)
  , CommandInfo(..)
  , FormatOptions(..)
  , AstOptions(..)
  , commands
  , parseCommand
  , getCommandInfo
  , expandCommand
  ) where

import Data.List (find, isPrefixOf)
import Data.Char (toLower)

-- | Represents all available CLI commands
data Command
  = Version
  | Help (Maybe String)
  | Test (Maybe String)
  | New String
  | Init
  | Run (Maybe FilePath)
  | Build
  | Clean
  | Watch
  | Format FormatOptions FilePath
  | Lint FilePath
  | Validate FilePath
  | Stats FilePath
  | Ast AstOptions FilePath
  | Deps FilePath
  | Docs FilePath
  | DocsWatch FilePath
  | Config
  | Completion String
  deriving (Show, Eq)

data FormatOptions = FormatOptions
  { formatCheck :: Bool
  , formatDiff :: Bool
  , formatVerify :: Maybe Bool
  , formatConfig :: Maybe FilePath
  } deriving (Show, Eq)

data AstOptions = AstOptions
  { astPretty :: Bool
  , astDelete :: Bool
  } deriving (Show, Eq)

-- | Command metadata for help system
data CommandInfo = CommandInfo
  { cmdName :: String
  , cmdCategory :: String
  , cmdShortDesc :: String
  , cmdLongDesc :: String
  , cmdUsage :: [String]
  , cmdExamples :: [(String, String)]
  , cmdOptions :: [(String, String)]
  } deriving (Show)

-- | All available commands with their metadata
commands :: [CommandInfo]
commands =
  [ CommandInfo
      { cmdName = "new"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Create a new Nytrix project"
      , cmdLongDesc = "Creates a new directory with a complete project structure including src/main.ny, format.yaml, package.yaml, README.md, and .gitignore"
      , cmdUsage = ["n n <project-name>", "n new <project-name>"]
      , cmdExamples =
          [ ("n n my-app", "Create a new project called 'my-app'")
          , ("n new game-engine", "Create a game engine project")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "init"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Initialize project in current directory"
      , cmdLongDesc = "Creates project files in the current directory without creating a new subdirectory. Useful for initializing an existing repository."
      , cmdUsage = ["n i", "n init"]
      , cmdExamples =
          [ ("cd my-project && n i", "Initialize in existing directory")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "run"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Compile and run (defaults to src/main.ny)"
      , cmdLongDesc = "Compiles the specified file (or src/main.ny by default) and runs the resulting executable. Automatically recompiles if source is newer than the executable."
      , cmdUsage =
          [ "n r"
          , "n r <file>"
          , "n run <file>"
          ]
      , cmdExamples =
          [ ("n r", "Run the main project")
          , ("n r examples/demo.ny", "Run a specific file")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "build"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Compile project without running"
      , cmdLongDesc = "Compiles src/main.ny and places the executable in build/ directory without running it."
      , cmdUsage = ["n b", "n build"]
      , cmdExamples =
          [ ("n b", "Build the project")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "clean"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Remove build artifacts"
      , cmdLongDesc = "Removes the build/ directory and all compiled artifacts."
      , cmdUsage = ["n c", "n clean"]
      , cmdExamples =
          [ ("n c", "Clean build artifacts")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "watch"
      , cmdCategory = "PROJECT"
      , cmdShortDesc = "Watch for changes and auto-rebuild"
      , cmdLongDesc = "Monitors source files for changes and automatically rebuilds the project when changes are detected."
      , cmdUsage = ["n w", "n watch"]
      , cmdExamples =
          [ ("n w", "Start watch mode")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "lint"
      , cmdCategory = "CODE QUALITY"
      , cmdShortDesc = "Parse and check syntax"
      , cmdLongDesc = "Parses Nytrix files and reports syntax errors without modifying them. Recursively processes directories."
      , cmdUsage = ["n l <path>", "n lint <path>"]
      , cmdExamples =
          [ ("n l src/", "Check all files in src/")
          , ("n l main.ny", "Check a single file")
          , ("n l .", "Check current directory")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "format"
      , cmdCategory = "CODE QUALITY"
      , cmdShortDesc = "Format code"
      , cmdLongDesc = "Formats Nytrix code according to style rules defined in format.yaml. Can format in-place, check formatting, or show diffs."
      , cmdUsage =
          [ "n f <path>"
          , "n f --check <path>"
          , "n f --diff <path>"
          , "n format <path>"
          ]
      , cmdExamples =
          [ ("n f src/", "Format all files in src/")
          , ("n f --check .", "Check if files need formatting (CI mode)")
          , ("n f --diff main.ny", "Show what would change")
          ]
      , cmdOptions =
          [ ("--check", "Check formatting without modifying files (exit 1 if changes needed)")
          , ("--diff", "Show formatting differences without writing")
          , ("--verify", "Format and verify the result parses correctly")
          , ("--config <path>", "Use specific format.yaml configuration")
          ]
      }
  , CommandInfo
      { cmdName = "validate"
      , cmdCategory = "CODE QUALITY"
      , cmdShortDesc = "Comprehensive validation (parse + lint)"
      , cmdLongDesc = "Performs comprehensive validation including parsing and linting checks."
      , cmdUsage = ["n va <path>", "n validate <path>"]
      , cmdExamples =
          [ ("n va src/", "Validate all source files")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "stats"
      , cmdCategory = "CODE QUALITY"
      , cmdShortDesc = "Show code statistics"
      , cmdLongDesc = "Displays statistics about code including line counts, complexity metrics, and more. Defaults to current directory and automatically finds project root."
      , cmdUsage =
          [ "n s"
          , "n s <path>"
          , "n stats <path>"
          ]
      , cmdExamples =
          [ ("n s", "Show stats for current project")
          , ("n s src/", "Show stats starting from src/")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "ast"
      , cmdCategory = "ANALYSIS"
      , cmdShortDesc = "Generate AST JSON"
      , cmdLongDesc = "Parses Nytrix code and generates Abstract Syntax Tree in JSON format. Can output pretty-printed JSON or delete existing AST files."
      , cmdUsage =
          [ "n a <path>"
          , "n a p <path>  (pretty)"
          , "n a d <path>  (delete)"
          , "n ast <path>"
          ]
      , cmdExamples =
          [ ("n a src/", "Generate AST JSON for all files")
          , ("n a p main.ny", "Generate pretty-printed AST")
          , ("n a d src/", "Delete all AST JSON files")
          ]
      , cmdOptions =
          [ ("p, pretty", "Pretty-print JSON with indentation")
          , ("d, delete", "Delete generated AST JSON files")
          ]
      }
  , CommandInfo
      { cmdName = "deps"
      , cmdCategory = "ANALYSIS"
      , cmdShortDesc = "Show dependency tree"
      , cmdLongDesc = "Analyzes and displays the dependency tree for a Nytrix file, showing all imports and their relationships."
      , cmdUsage = ["n d <file>", "n deps <file>"]
      , cmdExamples =
          [ ("n d main.ny", "Show dependencies of main.ny")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "test"
      , cmdCategory = "TESTING & DOCS"
      , cmdShortDesc = "Run tests"
      , cmdLongDesc = "Runs the test suite. Optionally filter tests by pattern."
      , cmdUsage =
          [ "n t"
          , "n t <pattern>"
          , "n test <pattern>"
          ]
      , cmdExamples =
          [ ("n t", "Run all tests")
          , ("n t parser", "Run tests matching 'parser'")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "docs"
      , cmdCategory = "TESTING & DOCS"
      , cmdShortDesc = "Generate documentation"
      , cmdLongDesc = "Generates documentation from docstrings and special annotations in the code (e.g., @fun, @param). Use 'docs watch' to start a live-reloading documentation server."
      , cmdUsage =
          [ "n do <path>"
          , "n docs <path>"
          , "n docs watch"
          , "n docs watch <path>"
          ]
      , cmdExamples =
          [ ("n do src/", "Generate docs for src/")
          , ("n do .", "Generate docs for project")
          , ("n docs watch", "Watch current directory with live reload")
          , ("n docs watch src/", "Watch src/ with live reload")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "config"
      , cmdCategory = "UTILITIES"
      , cmdShortDesc = "Show current configuration"
      , cmdLongDesc = "Displays the current format.yaml configuration and other project settings."
      , cmdUsage = ["n co", "n config"]
      , cmdExamples =
          [ ("n co", "Show configuration")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "completion"
      , cmdCategory = "UTILITIES"
      , cmdShortDesc = "Generate shell completions"
      , cmdLongDesc = "Generates shell completion scripts for bash, zsh, or fish."
      , cmdUsage = ["n comp <shell>", "n completion <shell>"]
      , cmdExamples =
          [ ("n comp bash > /etc/bash_completion.d/n", "Install bash completions")
          , ("n comp zsh > ~/.zsh/completions/_n", "Install zsh completions")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "version"
      , cmdCategory = "UTILITIES"
      , cmdShortDesc = "Show version information"
      , cmdLongDesc = "Displays the current version of n and build information."
      , cmdUsage = ["n v", "n version", "n --version", "n -v"]
      , cmdExamples =
          [ ("n v", "Show version")
          ]
      , cmdOptions = []
      }
  , CommandInfo
      { cmdName = "help"
      , cmdCategory = "UTILITIES"
      , cmdShortDesc = "Show help"
      , cmdLongDesc = "Displays help information. Use 'n h <command>' for detailed help on a specific command."
      , cmdUsage =
          [ "n h"
          , "n h <command>"
          , "n help <command>"
          ]
      , cmdExamples =
          [ ("n h", "Show general help")
          , ("n h format", "Show detailed help for format command")
          ]
      , cmdOptions = []
      }
  ]

-- | Smart command expansion - expands abbreviated commands to full commands
-- Examples: "n" -> "new", "t" -> "test", "a d" -> "ast delete"
expandCommand :: String -> [String]
expandCommand abbrev =
  let matches = filter (isPrefixOf (map toLower abbrev) . map toLower . cmdName) commands
  in case matches of
    [match] -> [cmdName match]  -- Unique match
    _ -> [abbrev]  -- No match or ambiguous, return as-is

-- | Expand a shell argument against valid shell options
expandShell :: String -> String
expandShell abbrev =
  let shells = ["bash", "zsh", "fish"]
      matches = filter (isPrefixOf (map toLower abbrev) . map toLower) shells
  in case matches of
    [match] -> match  -- Unique match
    _ -> abbrev  -- No match or ambiguous, return as-is

-- | Expand all arguments in a list with context awareness
expandArgs :: [String] -> [String]
expandArgs [] = []
expandArgs args = expandArgsWithContext Nothing args
  where
    expandArgsWithContext :: Maybe String -> [String] -> [String]
    expandArgsWithContext _ [] = []
    expandArgsWithContext ctx (arg:rest)
      | arg `elem` ["--version", "-v", "--help", "-h"] = arg : rest
      | arg `elem` ["--check", "--diff", "--verify", "--config"] = arg : rest
      -- If we're in completion context, don't expand - let ConfigOps handle it
      | ctx == Just "completion" = arg : rest
      | otherwise =
          case expandCommand arg of
            [expanded] | expanded /= arg ->
              -- Set context for next arg if this is "completion"
              let newCtx = if expanded == "completion" then Just "completion" else Nothing
              in expanded : expandArgsWithContext newCtx rest
            _ -> arg : expandArgsWithContext ctx rest

-- | Parse command line arguments into a Command with smart expansion
parseCommand :: [String] -> Maybe Command
parseCommand args =
  let expanded = expandArgs args
  in parseCommandExpanded expanded

-- | Parse already-expanded command arguments
parseCommandExpanded :: [String] -> Maybe Command
parseCommandExpanded args = case args of
  [] -> Nothing
  ["--version"] -> Just Version
  ["-v"] -> Just Version
  ["version"] -> Just Version
  ["help"] -> Just (Help Nothing)
  ["help", cmd] -> Just (Help (Just cmd))
  ["-h"] -> Just (Help Nothing)
  ["--help"] -> Just (Help Nothing)
  ["test"] -> Just (Test Nothing)
  ["test", pattern] -> Just (Test (Just pattern))
  ["new", name] -> Just (New name)
  ["init"] -> Just Init
  ["run"] -> Just (Run Nothing)
  ["run", file] -> Just (Run (Just file))
  ["build"] -> Just Build
  ["clean"] -> Just Clean
  ["watch"] -> Just Watch
  ["format", path] -> Just (Format defaultFormatOpts path)
  ["format", "--check", path] -> Just (Format (defaultFormatOpts { formatCheck = True }) path)
  ["format", "--diff", path] -> Just (Format (defaultFormatOpts { formatDiff = True }) path)
  ["format", "--verify", path] -> Just (Format (defaultFormatOpts { formatVerify = Just True }) path)
  ["format", "--config", cfg, path] -> Just (Format (defaultFormatOpts { formatConfig = Just cfg }) path)
  ["stats"] -> Just (Stats "")
  ["stats", path] -> Just (Stats path)
  ["lint", path] -> Just (Lint path)
  ["validate", path] -> Just (Validate path)
  ["ast", path] -> Just (Ast defaultAstOpts path)
  ["ast", "pretty", path] -> Just (Ast (defaultAstOpts { astPretty = True }) path)
  ["ast", "delete", path] -> Just (Ast (defaultAstOpts { astDelete = True }) path)
  ["deps", file] -> Just (Deps file)
  ["docs", "watch"] -> Just (DocsWatch ".")
  ["docs", "watch", path] -> Just (DocsWatch path)
  ["docs", path] -> Just (Docs path)
  ["config"] -> Just Config
  ["completion"] -> Nothing  -- Must specify shell
  ["completion", shell] -> Just (Completion shell)
  _ -> Nothing

defaultFormatOpts :: FormatOptions
defaultFormatOpts = FormatOptions False False Nothing Nothing

defaultAstOpts :: AstOptions
defaultAstOpts = AstOptions False False

-- | Get command info by name
getCommandInfo :: String -> Maybe CommandInfo
getCommandInfo name = find (\cmd -> cmdName cmd == name) commands
