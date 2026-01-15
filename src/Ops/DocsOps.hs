{-# LANGUAGE OverloadedStrings #-}
module Ops.DocsOps
  ( generateDocsForPath
  ) where

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension, takeBaseName)
import System.Exit (exitFailure)
import Control.Monad (filterM, forM_, unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (parse, errorBundlePretty)
import Parser (file)
import Docs
import qualified Colors as C

-- | Generate documentation for a path (file or directory)
generateDocsForPath :: FilePath -> IO ()
generateDocsForPath path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path

  if isDir
    then generateDocsForDirectory path
    else if isFile
      then generateDocsForFile path
      else do
        putStrLn $ C.error' $ "Not a file or directory: " ++ path
        exitFailure

-- | Generate docs for a single file
generateDocsForFile :: FilePath -> IO ()
generateDocsForFile filepath = do
  putStrLn $ C.info $ "Generating documentation for: " ++ filepath

  input <- TIO.readFile filepath
  case parse file filepath input of
    Left err -> do
      putStrLn $ C.error' $ "Parse error in " ++ filepath
      putStrLn $ errorBundlePretty err
      exitFailure
    Right ast -> do
      let docs = extractDocs ast
      if null docs
        then putStrLn $ C.warning "No documentation found in file"
        else do
          htmlContent <- generateDocs docs  -- Changed: use <- instead of let
          let outputPath = takeBaseName filepath ++ ".html"
          TIO.writeFile outputPath htmlContent
          putStrLn $ C.success $ "Documentation generated: " ++ outputPath
          putStrLn $ C.dim ++ "  Found " ++ show (length docs) ++ " documented item(s)" ++ C.reset

-- | Generate docs for a directory
generateDocsForDirectory :: FilePath -> IO ()
generateDocsForDirectory dir = do
  putStrLn $ C.info $ "Generating documentation for directory: " ++ dir

  -- Create docs output directory
  let docsDir = dir </> "docs"
  createDirectoryIfMissing True docsDir

  -- Find all .ny files
  nyFiles <- findNyFiles dir

  if null nyFiles
    then do
      putStrLn $ C.warning "No .ny files found"
      exitFailure
    else do
      putStrLn $ C.dim ++ "  Found " ++ show (length nyFiles) ++ " file(s)" ++ C.reset

      -- Process each file
      allDocs <- concat <$> mapM processFileForDocs nyFiles

      if null allDocs
        then putStrLn $ C.warning "No documentation found in any files"
        else do
          -- Generate index page
          let indexPath = docsDir </> "index.html"
          htmlContent <- generateDocs allDocs  -- Changed: use <- instead of let
          TIO.writeFile indexPath htmlContent

          putStrLn ""
          putStrLn $ C.success $ "Documentation generated in: " ++ docsDir
          putStrLn $ C.dim ++ "  Total documented items: " ++ show (length allDocs) ++ C.reset
          putStrLn $ C.dim ++ "  Open " ++ indexPath ++ " in your browser" ++ C.reset

-- | Find all .ny files recursively
findNyFiles :: FilePath -> IO [FilePath]
findNyFiles dir = do
  contents <- listDirectory dir
  let fullPaths = map (dir </>) contents

  files <- filterM doesFileExist fullPaths
  let nyFiles = filter (\f -> takeExtension f == ".ny") files

  subdirs <- filterM doesDirectoryExist fullPaths
  subNyFiles <- concat <$> mapM findNyFiles subdirs

  return (nyFiles ++ subNyFiles)

-- | Process a file and extract documentation
processFileForDocs :: FilePath -> IO [(T.Text, DocBlock)]
processFileForDocs filepath = do
  input <- TIO.readFile filepath
  case parse file filepath input of
    Left err -> do
      putStrLn $ C.warning $ "Skipping " ++ filepath ++ " (parse error)"
      return []
    Right ast -> do
      let docs = extractDocs ast
      unless (null docs) $
        putStrLn $ C.dim ++ "  " ++ filepath ++ ": " ++ show (length docs) ++ " item(s)" ++ C.reset
      return docs
