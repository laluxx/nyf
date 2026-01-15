{-# LANGUAGE OverloadedStrings #-}
module Docs
  ( DocAnnotation(..)
  , DocBlock(..)
  , DocElement(..)
  , extractDocs
  , generateDocs
  , parseDocAnnotations
  , parseRichComment
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory)
import Paths_nyf (getDataDir)
import Control.Exception (try, SomeException)
import Data.Function ((&))
import Data.List (find)
import AST

-- | Documentation annotation types (Doxygen-style)
data DocAnnotation
  = FunAnnotation Text
  | ParamAnnotation Text Text
  | ReturnAnnotation Text
  | ExampleAnnotation Text
  | DescAnnotation Text
  | AuthorAnnotation Text
  | VersionAnnotation Text
  | SinceAnnotation Text
  | DeprecatedAnnotation Text
  | SeeAnnotation Text
  | ThrowsAnnotation Text
  | TodoAnnotation Text
  | ModuleAnnotation Text
  deriving (Show, Eq)

-- | Rich comment elements
data DocElement
  = PlainText Text
  | Link Text Text
  | SymbolRef Text
  | Mention Text
  | TodoItem Bool Int Int Text
  | ChecklistItem Bool Text
  | CodeBlock Text
  | Header Int Text
  deriving (Show, Eq)

-- | A documentation block with its annotations and rich content
data DocBlock = DocBlock
  { docSummary :: Text
  , docAnnotations :: [DocAnnotation]
  , docElements :: [DocElement]
  , docLocation :: (Int, Int)
  } deriving (Show, Eq)

-- | Extract documentation from declarations
extractDocs :: [Decl] -> [(Text, DocBlock)]
extractDocs = concatMap extractFromDecl
  where
    extractFromDecl :: Decl -> [(Text, DocBlock)]
    extractFromDecl (FnDecl name _ _ (Just docStr) _) =
      case parseDocString docStr of
        Just docBlock -> [(name, docBlock)]
        Nothing -> []
    extractFromDecl (CommentDecl comment) =
      case extractDocFromComment comment of
        Just (name, docBlock) -> [(name, docBlock)]
        Nothing -> []
    extractFromDecl _ = []

-- | Extract function name from annotations
findFunName :: [DocAnnotation] -> Maybe Text
findFunName [] = Nothing
findFunName (FunAnnotation name : _) = Just name
findFunName (_ : rest) = findFunName rest

-- | Extract module name from annotations
findModuleName :: [DocAnnotation] -> Maybe Text
findModuleName [] = Nothing
findModuleName (ModuleAnnotation name : _) = Just name
findModuleName (_ : rest) = findModuleName rest

-- | Extract documentation from a comment
extractDocFromComment :: Comment -> Maybe (Text, DocBlock)
extractDocFromComment (BlockComment content _) =
  case parseDocString content of
    Just docBlock ->
      case findFunName (docAnnotations docBlock) of
        Just name -> Just (name, docBlock)
        Nothing   ->
          case findModuleName (docAnnotations docBlock) of
            Just modName -> Just (modName, docBlock)
            Nothing -> Just ("(module)", docBlock)
    Nothing -> Nothing
extractDocFromComment _ = Nothing

-- | Parse a docstring into a DocBlock
parseDocString :: Text -> Maybe DocBlock
parseDocString text =
  let trimmedText = T.strip text
  in if T.null trimmedText
       then Nothing
       else
         let lines' = T.lines trimmedText
             annotations = parseDocAnnotations text
             elements = parseRichComment text
             (summaryLines, _) = break (T.null . T.strip) lines'
             summary = T.strip $ T.unlines summaryLines
         in Just $ DocBlock
                { docSummary = if T.null summary then "(no summary)" else summary
                , docAnnotations = annotations
                , docElements = elements
                , docLocation = (0, 0)
                }

-- | Check if a line contains an annotation
isAnnotationLine :: Text -> Bool
isAnnotationLine line =
  let trimmed = T.strip line
  in "@" `T.isPrefixOf` trimmed

-- | Parse an annotation line
parseAnnotationLine :: Text -> DocAnnotation
parseAnnotationLine line =
  let trimmed = T.strip line
      (tag, rest) = T.break (== ' ') (T.drop 1 trimmed)
      content = T.strip rest
  in case T.toLower tag of
       "fun"        -> FunAnnotation content
       "param" ->
         let (paramName, paramDesc) = T.break (== ' ') content
         in ParamAnnotation paramName (T.strip paramDesc)
       "return"     -> ReturnAnnotation     content
       "example"    -> ExampleAnnotation    content
       "desc"       -> DescAnnotation       content
       "author"     -> AuthorAnnotation     content
       "version"    -> VersionAnnotation    content
       "since"      -> SinceAnnotation      content
       "deprecated" -> DeprecatedAnnotation content
       "see"        -> SeeAnnotation        content
       "throws"     -> ThrowsAnnotation     content
       "todo"       -> TodoAnnotation       content
       "module"     -> ModuleAnnotation     content
       _            -> DescAnnotation       line

-- | Parse documentation annotations from text
parseDocAnnotations :: Text -> [DocAnnotation]
parseDocAnnotations text =
  let lines' = T.lines text
      annotLines = filter isAnnotationLine lines'
  in map parseAnnotationLine annotLines

-- | Parse rich comment syntax
parseRichComment :: Text -> [DocElement]
parseRichComment text =
  let lines' = T.lines text
  in concatMap parseCommentLine lines'

-- | Parse a single line for rich comment elements
parseCommentLine :: Text -> [DocElement]
parseCommentLine line
  | "TODO" `T.isInfixOf` line && "[" `T.isInfixOf` line =
      case parseTodoProgress line of
        Just todo -> [todo]
        Nothing -> [PlainText line]
  | "- [" `T.isPrefixOf` T.strip line =
      case parseChecklistItem line of
        Just item -> [item]
        Nothing -> [PlainText line]
  | otherwise = parseInlineElements line

-- | Parse TODO with progress
parseTodoProgress :: Text -> Maybe DocElement
parseTodoProgress line = do
  let afterTodo = T.dropWhile (/= '[') line
  if T.null afterTodo then Nothing
  else do
    let progressPart = T.takeWhile (/= ']') (T.drop 1 afterTodo)
        parts = T.splitOn "/" progressPart
    case parts of
      [doneStr, totalStr] -> do
        done <- readMaybeInt (T.strip doneStr)
        total <- readMaybeInt (T.strip totalStr)
        let textPart = T.strip $ T.takeWhile (/= '[') line
        return $ TodoItem False done total textPart
      _ -> Nothing

-- | Parse checklist item
parseChecklistItem :: Text -> Maybe DocElement
parseChecklistItem line = do
  let trimmed = T.strip line
  if not ("- [" `T.isPrefixOf` trimmed) then Nothing
  else do
    let rest = T.drop 3 trimmed
        checked = case T.uncons rest of
          Just ('X', _) -> True
          Just ('x', _) -> True
          _ -> False
        textPart = T.strip $ T.drop 2 rest
    return $ ChecklistItem checked textPart

-- | Parse inline elements
parseInlineElements :: Text -> [DocElement]
parseInlineElements text
  | T.null text = []
  | "[[" `T.isPrefixOf` text =
      case parseLink text of
        Just (link, rest) -> link : parseInlineElements rest
        Nothing -> PlainText (T.take 1 text) : parseInlineElements (T.drop 1 text)
  | "@" `T.isPrefixOf` text && not (T.null $ T.drop 1 text) =
      case parseMention text of
        Just (mention, rest) -> mention : parseInlineElements rest
        Nothing -> PlainText (T.take 1 text) : parseInlineElements (T.drop 1 text)
  | "`" `T.isPrefixOf` text =
      case parseSymbolRef text of
        Just (symbol, rest) -> symbol : parseInlineElements rest
        Nothing -> PlainText (T.take 1 text) : parseInlineElements (T.drop 1 text)
  | otherwise =
      let (plain, rest) = T.break (`elem` ['[', '@', '`']) text
      in if T.null plain
         then [PlainText text]
         else PlainText plain : parseInlineElements rest

-- | Parse link
parseLink :: Text -> Maybe (DocElement, Text)
parseLink text = do
  let afterOpen = T.drop 2 text
      urlPart = T.takeWhile (/= ']') afterOpen
      afterUrl = T.drop (T.length urlPart + 1) afterOpen
  if not ("[" `T.isPrefixOf` afterUrl) then Nothing
  else do
    let textPart = T.takeWhile (/= ']') (T.drop 1 afterUrl)
        rest = T.drop (T.length textPart + 2) (T.drop 1 afterUrl)
    return (Link urlPart textPart, rest)

-- | Parse mention
parseMention :: Text -> Maybe (DocElement, Text)
parseMention text = do
  let name = T.takeWhile isUsernameChar (T.drop 1 text)
  if T.null name then Nothing
  else Just (Mention name, T.drop (T.length name + 1) text)
  where
    isUsernameChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

-- | Parse symbol reference
parseSymbolRef :: Text -> Maybe (DocElement, Text)
parseSymbolRef text = do
  let symbol = T.takeWhile (/= '`') (T.drop 1 text)
  if T.null symbol then Nothing
  else Just (SymbolRef symbol, T.drop (T.length symbol + 2) text)

-- | Helper to read Int
readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Load template file
loadTemplateFile :: FilePath -> Text -> IO Text
loadTemplateFile filename defaultContent = do
  dataDir <- getDataDir
  let installedPath = dataDir </> "etc" </> "docs" </> filename
  installedExists <- doesFileExist installedPath

  if installedExists
    then TIO.readFile installedPath
    else do
      currentDir <- getCurrentDirectory
      let sourcePath = currentDir </> "etc" </> "docs" </> filename
      sourceExists <- doesFileExist sourcePath

      if sourceExists
        then TIO.readFile sourcePath
        else do
          let relativePath = "etc/docs/" ++ filename
          relativeExists <- doesFileExist relativePath
          if relativeExists
            then TIO.readFile relativePath
            else return defaultContent

-- | Count TODOs in a doc block
countTodos :: DocBlock -> Int
countTodos docBlock = length $ filter isTodoAnnotation (docAnnotations docBlock)
  where
    isTodoAnnotation (TodoAnnotation _) = True
    isTodoAnnotation _ = False

-- | Generate HTML documentation
generateDocs :: [(Text, DocBlock)] -> IO Text
generateDocs docs = do
  putStrLn $ "Generating docs for " ++ show (length docs) ++ " items..."

  -- Load HTML template
  htmlTemplate <- loadTemplateFile "index.html" defaultHtmlTemplate

  -- Generate function documentation
  let functionDocs = if null docs
                     then "<div class=\"function-container\"><div class=\"function\"><h2>No documentation found</h2><p>Add doc comments to your .ny files to see them here.</p></div></div>"
                     else T.unlines $ map generateFunctionDoc docs

  -- Generate functions array for JavaScript
  let jsArray = generateFunctionsArray docs

  putStrLn $ "Generated " ++ show (T.length functionDocs) ++ " chars of function docs"

  -- Replace placeholders
  let finalHtml = htmlTemplate
        & replacePlaceholder "{{FUNCTION_DOCS}}" functionDocs
        & replacePlaceholder "const functions = [];" jsArray

  -- Debug output
  putStrLn "Replacements made:"
  putStrLn $ "  - Functions: " ++ show (length docs)
  putStrLn $ "  - Function docs length: " ++ show (T.length functionDocs)
  putStrLn $ "  - JS array length: " ++ show (T.length jsArray)

  return finalHtml

-- | Generate the JavaScript functions array
generateFunctionsArray :: [(Text, DocBlock)] -> Text
generateFunctionsArray docs =
  let entries = map generateJSEntry docs
      joined = T.intercalate ",\n      " entries
  in "const functions = [\n      " <> joined <> "\n    ];"
  where
    generateJSEntry (name, docBlock) =
      "{ id: '" <> escapeJS name <> "', name: '" <> escapeJS name <>
      "', summary: '" <> escapeJS (docSummary docBlock) <> "' }"

-- | Replace placeholder in text
replacePlaceholder :: Text -> Text -> Text -> Text
replacePlaceholder placeholder value = T.replace placeholder value

-- | Default HTML template (load from file in production, this is fallback)
defaultHtmlTemplate :: Text
defaultHtmlTemplate = ""  -- Will be loaded from index.html file

-- | Generate function documentation HTML matching your template structure
generateFunctionDoc :: (Text, DocBlock) -> Text
generateFunctionDoc (name, docBlock) =
  let todoCount = countTodos docBlock
      todoBadge = if todoCount > 0
                  then "<span class=\"todo-badge\">" <> T.pack (show todoCount) <>
                       " TODO" <> (if todoCount > 1 then "s" else "") <> "</span>"
                  else ""
  in T.unlines
    [ "      <div class=\"function-container\">"
    , "        <div class=\"function\" id=\"" <> escapeHtml name <> "\">"
    , "          <div class=\"lambda-icon\">λ</div>"
    , "          <h2><code>" <> escapeHtml name <> "</code>" <> todoBadge <> "</h2>"
    , "          <div class=\"summary\">" <> escapeHtml (docSummary docBlock) <> "</div>"
    , T.unlines $ map generateAnnotationDoc (docAnnotations docBlock)
    , "        </div>"
    , "      </div>"
    ]

-- | Generate annotation documentation HTML matching your template structure
generateAnnotationDoc :: DocAnnotation -> Text
generateAnnotationDoc ann = case ann of
  FunAnnotation name ->
    "          <div class=\"annotation\"><span class=\"tag\">Function:</span> <code>" <> escapeHtml name <> "</code></div>"

  ParamAnnotation name desc ->
    "          <div class=\"annotation\"><span class=\"tag tag-param\">Parameter:</span> <code>" <>
    escapeHtml name <> "</code> – " <> escapeHtml desc <> "</div>"

  ReturnAnnotation desc ->
    "          <div class=\"annotation\"><span class=\"tag tag-return\">Returns:</span> " <>
    escapeHtml desc <> "</div>"

  ExampleAnnotation code ->
    "          <div class=\"annotation\"><span class=\"tag tag-example\">Example:</span><pre><code>" <>
    escapeHtml code <> "</code></pre></div>"

  DescAnnotation desc ->
    "          <div class=\"annotation\">" <> escapeHtml desc <> "</div>"

  AuthorAnnotation author ->
    "          <div class=\"annotation\"><span class=\"tag tag-author\">Author:</span> " <>
    escapeHtml author <> "</div>"

  VersionAnnotation ver ->
    "          <div class=\"annotation\"><span class=\"tag\">Version:</span> " <> escapeHtml ver <> "</div>"

  SinceAnnotation ver ->
    "          <div class=\"annotation\"><span class=\"tag\">Since:</span> " <> escapeHtml ver <> "</div>"

  DeprecatedAnnotation reason ->
    "          <div class=\"annotation\" style=\"background: rgba(255, 193, 7, 0.1); border-left-color: #ffc107;\"><span class=\"tag tag-deprecated\">⚠ Deprecated:</span> " <>
    escapeHtml reason <> "</div>"

  SeeAnnotation ref ->
    "          <div class=\"annotation\"><span class=\"tag\">See also:</span> " <> escapeHtml ref <> "</div>"

  ThrowsAnnotation exc ->
    "          <div class=\"annotation\"><span class=\"tag\">Throws:</span> " <> escapeHtml exc <> "</div>"

  TodoAnnotation task ->
    "          <div class=\"annotation\" style=\"background: rgba(79, 172, 254, 0.1); border-left-color: #4facfe;\"><span class=\"tag\">TODO:</span> " <>
    escapeHtml task <> "</div>"

  ModuleAnnotation name ->
    "          <div class=\"annotation\"><span class=\"tag\">Module:</span> <code>" <> escapeHtml name <> "</code></div>"

-- | Escape HTML
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;"
           . T.replace "<" "&lt;"
           . T.replace ">" "&gt;"
           . T.replace "\"" "&quot;"
           . T.replace "'" "&#39;"

-- | Escape JavaScript strings
escapeJS :: Text -> Text
escapeJS = T.replace "\\" "\\\\"
         . T.replace "'" "\\'"
         . T.replace "\"" "\\\""
         . T.replace "\n" "\\n"
         . T.replace "\r" "\\r"
