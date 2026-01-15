{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs, getProgName)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure)
import Cli
import qualified HelpSystem
import qualified Ops.ProjectOps as ProjectOps
import qualified Ops.FormatOps as FormatOps
import qualified Ops.LintOps as LintOps
import qualified Ops.AstOps as AstOps
import qualified Ops.DocsOps as DocsOps
import qualified Ops.TestOps as TestOps
import qualified Ops.ConfigOps as ConfigOps
import qualified DocWatch

--- TODO FLAGS [0/2]
-- [ ] validate flag will only validate the file as fast as possible.
--     Without any sort of extra analysis it will only validate correct syntax.
-- [ ] eval "code" flag will eval some code and do extra analysis on the code
--     Before compiling it and pretty printing the output.

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> dwimBehavior progName
    ["completion"] -> showCompletionUsage  -- Special case: completion needs shell arg
    _  -> case parseCommand args of
            Just cmd -> executeCommand cmd
            Nothing -> do
              putStrLn ""
              putStrLn $ "\x1b[1;31mError:\x1b[0m Unknown command or invalid arguments: " ++ unwords args
              putStrLn ""
              putStrLn $ "Run '\x1b[32m" ++ progName ++ " help\x1b[0m' to see available commands"
              putStrLn ""
              suggestSimilarCommands args
              exitFailure

-- | DWIM behavior: Do What I Mean when no arguments provided
dwimBehavior :: String -> IO ()
dwimBehavior progName = do
  currentDir <- getCurrentDirectory
  maybeProjectRoot <- LintOps.findProjectRoot currentDir
  case maybeProjectRoot of
    Just _ -> LintOps.statsPath ""  -- In a project, show stats
    Nothing -> HelpSystem.printUsage  -- Not in a project, show help

-- | Suggest similar commands if user made a typo
suggestSimilarCommands :: [String] -> IO ()
suggestSimilarCommands [] = return ()
suggestSimilarCommands (arg:_) = do
  let allCommands = map cmdName commands
      prefixMatches = filter (isPrefixMatch arg) allCommands
      similarMatches = filter (isSimilar arg) allCommands
      suggestions = if null prefixMatches then similarMatches else prefixMatches

  unless (null suggestions) $ do
    if length suggestions == 1
      then putStrLn $ "Did you mean '\x1b[36m" ++ head suggestions ++ "\x1b[0m'?"
      else do
        putStrLn "Did you mean one of these?"
        mapM_ (\s -> putStrLn $ "  \x1b[36m" ++ s ++ "\x1b[0m") (take 5 suggestions)
    putStrLn ""
  where
    isPrefixMatch :: String -> String -> Bool
    isPrefixMatch abbr full =
      let lowerAbbr = map toLower abbr
          lowerFull = map toLower full
      in lowerAbbr `isPrefixOf` lowerFull && lowerAbbr /= lowerFull

    -- Check if two strings are similar (Levenshtein distance-like)
    isSimilar :: String -> String -> Bool
    isSimilar input cmd =
      let lowerInput = map toLower input
          lowerCmd = map toLower cmd
          distance = levenshtein lowerInput lowerCmd
      in distance <= 2 && distance > 0  -- Allow up to 2 character differences

    -- Simple Levenshtein distance implementation
    levenshtein :: String -> String -> Int
    levenshtein s1 s2 = last $ foldl transform [0..length s1] s2
      where
        transform ns@(n:ns1) c = scanl calc (n+1) (zip3 s1 ns ns1)
          where
            calc z (c1, x, y) = minimum [y+1, z+1, x + (if c1 == c then 0 else 1)]
        transform [] _ = []

    toLower :: Char -> Char
    toLower c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
              | otherwise = c

    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Execute a parsed command
executeCommand :: Command -> IO ()
executeCommand cmd = case cmd of
  Version -> HelpSystem.printVersion
  Help Nothing -> HelpSystem.printUsage
  Help (Just cmdNameArg) -> HelpSystem.printCommandHelp cmdNameArg
  Test pattern -> TestOps.runTests pattern
  New name -> do
    -- Smart behavior: if directory exists, show stats; otherwise create project
    dirExists <- doesDirectoryExist name
    if dirExists
      then do
        putStrLn ""
        putStrLn $ "\x1b[36mi\x1b[0m  Project \x1b[1m" ++ name ++ "\x1b[0m already exists, showing stats:"
        putStrLn ""
        LintOps.statsPath name
      else ProjectOps.createNewProject name
  Init -> ProjectOps.initProject
  Run mFile -> ProjectOps.runProject mFile
  Build -> ProjectOps.buildProject
  Clean -> ProjectOps.cleanProject
  Watch -> ProjectOps.watchProject
  Format opts path -> FormatOps.formatPath path opts
  Lint path -> LintOps.lintPath path
  Validate path -> LintOps.validatePath path
  Stats path -> LintOps.statsPath path
  Ast opts path -> AstOps.astPath path opts
  Deps file -> AstOps.depsFile file
  Docs path -> DocsOps.generateDocsForPath path
  DocsWatch path -> DocWatch.watchAndServe path
  Config -> ConfigOps.showConfig
  Completion shell -> ConfigOps.generateCompletion shell

-- Helper to show completion usage
showCompletionUsage :: IO ()
showCompletionUsage = do
  putStrLn ""
  putStrLn "\x1b[1;31mError:\x1b[0m Missing shell argument"
  putStrLn ""
  putStrLn "Usage: \x1b[32mnyf completion\x1b[0m \x1b[36m<shell>\x1b[0m"
  putStrLn ""
  putStrLn "Available shells:"
  putStrLn "  \x1b[36mbash\x1b[0m  - Bash completion"
  putStrLn "  \x1b[36mzsh\x1b[0m   - Zsh completion"
  putStrLn "  \x1b[36mfish\x1b[0m  - Fish completion"
  putStrLn ""
  putStrLn "Example:"
  putStrLn "  \x1b[2mnyf completion bash > /etc/bash_completion.d/nyf\x1b[0m"
  putStrLn ""

-- Conditional execution helper
unless :: Bool -> IO () -> IO ()
unless True _ = return ()
unless False action = action
