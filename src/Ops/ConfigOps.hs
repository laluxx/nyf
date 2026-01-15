{-# LANGUAGE OverloadedStrings #-}
module Ops.ConfigOps
  ( showConfig
  , generateCompletion
  ) where

import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Cli (commands, cmdName)
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- | Show current configuration
showConfig :: IO ()
showConfig = do
  currentDir <- getCurrentDirectory
  let formatPath = currentDir </> "format.yaml"
  formatExists <- doesFileExist formatPath

  putStrLn ""
  putStrLn "\x1b[1;94mConfiguration\x1b[0m"
  putStrLn ""

  if formatExists
    then do
      content <- TIO.readFile formatPath
      putStrLn "\x1b[1mformat.yaml:\x1b[0m"
      TIO.putStrLn content
    else do
      putStrLn "\x1b[2mNo format.yaml found in current directory\x1b[0m"

  putStrLn ""

-- | Generate shell completion scripts with smart prefix matching
generateCompletion :: String -> IO ()
generateCompletion shell =
  let lowerShell = map toLower shell
      shells = [("bash", bashCompletion), ("zsh", zshCompletion), ("fish", fishCompletion)]
      matches = filter (\(name, _) -> lowerShell `isPrefixOf` name) shells
  in case matches of
    [(_, completion)] -> putStrLn completion  -- Unique match
    [] -> do
      putStrLn $ "Error: Unknown shell '" ++ shell ++ "'"
      putStrLn "Supported shells: bash, zsh, fish"
    _ -> do
      putStrLn $ "Error: Ambiguous shell '" ++ shell ++ "'"
      putStrLn $ "Matches: " ++ unwords (map fst matches)
      putStrLn "Supported shells: bash, zsh, fish"

-- | Bash completion script
bashCompletion :: String
bashCompletion = unlines
  [ "# Bash completion for nyf/n"
  , "# Install: source this file or copy to /etc/bash_completion.d/"
  , ""
  , "_nyf_completions() {"
  , "    local cur prev opts"
  , "    COMPREPLY=()"
  , "    cur=\"${COMP_WORDS[COMP_CWORD]}\""
  , "    prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
  , ""
  , "    # All available commands"
  , "    local commands=\"" ++ unwords (map cmdName commands) ++ "\""
  , ""
  , "    # Subcommands for specific commands"
  , "    case \"${prev}\" in"
  , "        ast|a)"
  , "            opts=\"pretty delete p d\""
  , "            COMPREPLY=( $(compgen -W \"${opts}\" -- ${cur}) )"
  , "            return 0"
  , "            ;;"
  , "        format|f)"
  , "            opts=\"--check --diff --verify --config\""
  , "            COMPREPLY=( $(compgen -W \"${opts}\" -f -- ${cur}) )"
  , "            return 0"
  , "            ;;"
  , "        help|h)"
  , "            COMPREPLY=( $(compgen -W \"${commands}\" -- ${cur}) )"
  , "            return 0"
  , "            ;;"
  , "        completion|comp)"
  , "            COMPREPLY=( $(compgen -W \"bash zsh fish\" -- ${cur}) )"
  , "            return 0"
  , "            ;;"
  , "    esac"
  , ""
  , "    # Complete with commands"
  , "    COMPREPLY=( $(compgen -W \"${commands}\" -- ${cur}) )"
  , "    "
  , "    # Also add file completion"
  , "    if [[ ${#COMPREPLY[@]} -eq 0 ]]; then"
  , "        COMPREPLY=( $(compgen -f -- ${cur}) )"
  , "    fi"
  , "}"
  , ""
  , "complete -F _nyf_completions nyf"
  , "complete -F _nyf_completions n"
  ]

-- | Zsh completion script
zshCompletion :: String
zshCompletion = unlines $
  [ "#compdef nyf n"
  , "# Zsh completion for nyf/n"
  , "# Install: Copy to a directory in $fpath (e.g., ~/.zsh/completions/_nyf)"
  , ""
  , "_nyf() {"
  , "    local -a commands"
  , "    commands=("
  ] ++ map (\cmd -> "        '" ++ cmd ++ ":'") (map cmdName commands) ++
  [ "    )"
  , ""
  , "    local -a ast_opts"
  , "    ast_opts=("
  , "        'pretty:Pretty-print JSON'"
  , "        'p:Pretty-print JSON (short)'"
  , "        'delete:Delete AST files'"
  , "        'd:Delete AST files (short)'"
  , "    )"
  , ""
  , "    local -a format_opts"
  , "    format_opts=("
  , "        '--check:Check formatting without modifying'"
  , "        '--diff:Show formatting differences'"
  , "        '--verify:Format and verify parse'"
  , "        '--config:Use specific config file'"
  , "    )"
  , ""
  , "    _arguments -C \\"
  , "        '1: :->command' \\"
  , "        '*::arg:->args'"
  , ""
  , "    case $state in"
  , "        command)"
  , "            _describe 'command' commands"
  , "            ;;"
  , "        args)"
  , "            case $words[1] in"
  , "                ast|a)"
  , "                    _describe 'ast option' ast_opts"
  , "                    _files"
  , "                    ;;"
  , "                format|f)"
  , "                    _describe 'format option' format_opts"
  , "                    _files"
  , "                    ;;"
  , "                help|h)"
  , "                    _describe 'command' commands"
  , "                    ;;"
  , "                completion|comp)"
  , "                    _values 'shell' bash zsh fish"
  , "                    ;;"
  , "                *)"
  , "                    _files"
  , "                    ;;"
  , "            esac"
  , "            ;;"
  , "    esac"
  , "}"
  , ""
  , "_nyf \"$@\""
  ]

-- | Fish completion script
fishCompletion :: String
fishCompletion = unlines $
  [ "# Fish completion for nyf/n"
  , "# Install: Copy to ~/.config/fish/completions/nyf.fish"
  , ""
  , "# Disable file completion by default"
  , "complete -c nyf -f"
  , "complete -c n -f"
  , ""
  , "# Main commands"
  ] ++
  map (\cmd -> "complete -c nyf -n '__fish_use_subcommand' -a '" ++ cmd ++ "'") (map cmdName commands) ++
  map (\cmd -> "complete -c n -n '__fish_use_subcommand' -a '" ++ cmd ++ "'") (map cmdName commands) ++
  [ ""
  , "# ast subcommands"
  , "complete -c nyf -n '__fish_seen_subcommand_from ast a' -a 'pretty p delete d'"
  , "complete -c n -n '__fish_seen_subcommand_from ast a' -a 'pretty p delete d'"
  , ""
  , "# format options"
  , "complete -c nyf -n '__fish_seen_subcommand_from format f' -l check -d 'Check formatting'"
  , "complete -c n -n '__fish_seen_subcommand_from format f' -l check -d 'Check formatting'"
  , "complete -c nyf -n '__fish_seen_subcommand_from format f' -l diff -d 'Show differences'"
  , "complete -c n -n '__fish_seen_subcommand_from format f' -l diff -d 'Show differences'"
  , "complete -c nyf -n '__fish_seen_subcommand_from format f' -l verify -d 'Verify parse'"
  , "complete -c n -n '__fish_seen_subcommand_from format f' -l verify -d 'Verify parse'"
  , "complete -c nyf -n '__fish_seen_subcommand_from format f' -l config -d 'Config file'"
  , "complete -c n -n '__fish_seen_subcommand_from format f' -l config -d 'Config file'"
  , ""
  , "# help subcommands"
  , "complete -c nyf -n '__fish_seen_subcommand_from help h' -a '" ++ unwords (map cmdName commands) ++ "'"
  , "complete -c n -n '__fish_seen_subcommand_from help h' -a '" ++ unwords (map cmdName commands) ++ "'"
  , ""
  , "# completion shells"
  , "complete -c nyf -n '__fish_seen_subcommand_from completion comp' -a 'bash zsh fish'"
  , "complete -c n -n '__fish_seen_subcommand_from completion comp' -a 'bash zsh fish'"
  , ""
  , "# File completion for relevant commands"
  , "complete -c nyf -n '__fish_seen_subcommand_from format f lint l validate va stats s ast a deps d docs do run r' -F"
  , "complete -c n -n '__fish_seen_subcommand_from format f lint l validate va stats s ast a deps d docs do run r' -F"
  ]
