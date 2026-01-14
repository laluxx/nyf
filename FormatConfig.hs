{-# LANGUAGE OverloadedStrings #-}
module FormatConfig where

import Data.Text (Text)
import Data.Yaml
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))

-- | Complete formatting configuration
data FormatConfig = FormatConfig
  { indentSize :: Int
  , useTabs :: Bool
  , maxLineLength :: Int
  , wrapLongExpressions :: Bool
  , wrapAfterOperator :: Bool
  , binaryOpWrapIndent :: Int
  , chainedCallWrapStyle :: ChainedCallStyle
  , newlinesBetweenTopLevel :: Int
  , newlinesBetweenFunctions :: Int
  , newlinesInsideFunction :: Int
  , newlinesBeforeComment :: Int
  , newlinesAfterComment :: Int
  , preserveBlankLines :: Bool
  , maxConsecutiveBlankLines :: Int
  , removeBlankLineAtStart :: Bool
  , removeBlankLineAtEnd :: Bool
  , spaceAfterComma :: Int
  , spaceBeforeComma :: Int
  , spaceInsideParens :: Int
  , spaceInsideBrackets :: Int
  , spaceInsideBraces :: Int
  , spaceAroundAssign :: Int
  , spaceAroundBinOp :: Int
  , spaceAroundCompare :: Int
  , spaceAfterUnaryOp :: Int
  , spaceAroundColon :: Int
  , spaceAroundArrow :: Int
  , alignAssignments :: Bool
  , alignAssignmentThreshold :: Int
  , alignComparisons :: Bool
  , alignComparisonThreshold :: Int
  , alignComparisonOps :: [Text]
  , alignFunctionArgs :: Bool
  , oneArgPerLineThreshold :: Int
  , alignAssertMessages :: Bool
  , spaceBeforeFnParen :: Int
  , spaceBeforeFnBrace :: Int
  , fnBraceOnNewLine :: Bool
  , spaceAfterFnKeyword :: Int
  , fnDocstringStyle :: DocstringStyle
  , spaceBeforeIfParen :: Int
  , spaceBeforeWhileParen :: Int
  , spaceBeforeForParen :: Int
  , spaceBeforeMatchParen :: Int
  , ifBraceOnNewLine :: Bool
  , whileBraceOnNewLine :: Bool
  , forBraceOnNewLine :: Bool
  , matchBraceOnNewLine :: Bool
  , spaceAfterIfKeyword :: Int
  , spaceAfterWhileKeyword :: Int
  , spaceAfterForKeyword :: Int
  , spaceAfterMatchKeyword :: Int
  , spaceBeforeMatchArrow :: Int
  , spaceAfterMatchArrow :: Int
  , matchArmIndent :: Int
  , matchPatternOnNewLine :: Bool
  , alignInlineComments :: Bool
  , inlineCommentColumn :: Int
  , spaceAfterCommentDelimiter :: Int
  , preserveCommentIndent :: Bool
  , preserveCommentFormatting :: Bool
  , commentDelimiterStyle :: CommentDelimiterStyle
  , blockCommentStyle :: BlockCommentStyle
  , orgModeEnabled :: Bool
  , headingStyle :: HeadingStyle
  , checkboxStyle :: CheckboxStyle
  , autoUpdateCookies :: Bool
  , autoAddClosedTimestamp :: Bool
  , closedTimestampFormat :: Text
  , trailingCommaInLists :: Bool
  , oneElementPerLine :: Bool
  , maxElementsPerLine :: Int
  , listWrapThreshold :: Int
  , dictColonStyle :: DictColonStyle
  , spaceAroundAs :: Int
  , dotSeparatorSpacing :: Int
  , sortUseStatements :: Bool
  , groupUseByPrefix :: Bool
  , blankLinesBetweenUseGroups :: Int
  , defineStyle :: DefineStyle
  , lambdaStyle :: LambdaStyle
  , spaceAfterLambdaKeyword :: Int
  , lambdaBraceOnNewLine :: Bool
  , layoutFieldsOnNewLine :: Bool
  , layoutFieldAlignment :: Bool
  , layoutTrailingComma :: Bool
  , compactSmallBlocks :: Bool
  , maxCompactBlockLength :: Int
  , blockStatementSpacing :: Int
  , emptyBlockStyle :: EmptyBlockStyle
  , stringQuoteStyle :: QuoteStyle
  , fstringSpacing :: Int
  , preferTripleQuotes :: Bool
  , integerGrouping :: Bool
  , integerGroupSize :: Int
  , hexUpperCase :: Bool
  , floatScientificThreshold :: Int
  , spaceAfterReturn :: Int
  , returnOnSeparateLine :: Bool
  , spaceAfterCatch :: Int
  , catchVarStyle :: CatchVarStyle
  , deferBraceOnNewLine :: Bool
  , spaceAfterDefer :: Int
  , labelColonSpacing :: Int
  , spaceAfterGoto :: Int
  , spaceBeforeTypeColon :: Int
  , spaceAfterTypeColon :: Int
  , spaceAroundSliceColon :: Int
  , spaceInsideIndexBrackets :: Int
  , asmArgumentSpacing :: Int
  , embedArgumentStyle :: EmbedStyle
  , comptimeBraceOnNewLine :: Bool
  , spaceAfterComptime :: Int
  , ensureNewlineAtEOF :: Bool
  , trimTrailingWhitespace :: Bool
  , normalizeWhitespace :: Bool
  , collapseAdjacentSpaces :: Bool
  } deriving (Show, Eq)

-- Enums
data DocstringStyle = DocstringIndented | DocstringFlush deriving (Show, Eq, Read)
data CommentDelimiterStyle = CommentSingleSpace | CommentDoubleSpace | CommentNoSpace deriving (Show, Eq, Read)
data BlockCommentStyle = BlockHorizontal | BlockVertical deriving (Show, Eq, Read)
data HeadingStyle = HeadingSemicolons | HeadingHash deriving (Show, Eq, Read)
data CheckboxStyle = CheckboxUpperX | CheckboxLowerX deriving (Show, Eq, Read)
data DictColonStyle = DictColonNoSpace | DictColonAfter | DictColonBoth deriving (Show, Eq, Read)
data DefineStyle = DefineSpaceSeparated | DefineCommaSeparated deriving (Show, Eq, Read)
data LambdaStyle = LambdaKeyword | LambdaSymbol deriving (Show, Eq, Read)
data EmptyBlockStyle = EmptyBlockCompact | EmptyBlockSpaced | EmptyBlockNewline deriving (Show, Eq, Read)
data QuoteStyle = QuoteDouble | QuoteSingle deriving (Show, Eq, Read)
data CatchVarStyle = CatchVarSpaced | CatchVarParens | CatchVarSpacedParens deriving (Show, Eq, Read)
data EmbedStyle = EmbedQuoted | EmbedUnquoted deriving (Show, Eq, Read)
data ChainedCallStyle = ChainedSameLine | ChainedNewLine deriving (Show, Eq, Read)

-- Default config
defaultConfig :: FormatConfig
defaultConfig = FormatConfig 4 False 80 False True 4 ChainedSameLine 1 2 0 0 0 False 1 True True 1 0 0 0 0 1 1 1 0 1 1 False 2 False 3 ["==","!="] False 5 False 0 1 False 1 DocstringIndented 1 1 1 1 False False False False 1 1 1 1 1 1 0 False False 40 1 False False CommentSingleSpace BlockHorizontal True HeadingSemicolons CheckboxUpperX True True "[%Y-%m-%d %a %H:%M]" False False 5 60 DictColonAfter 1 0 False False 1 DefineSpaceSeparated LambdaKeyword 0 False True False False False 1 0 EmptyBlockCompact QuoteDouble 0 False False 3 True 1000000 1 False 0 CatchVarSpaced False 1 0 1 0 1 0 0 1 EmbedQuoted False 1 True True True True

-- JSON instances for enums
instance FromJSON DocstringStyle where
  parseJSON = withText "DocstringStyle" $ \t -> case t of
    "indented" -> return DocstringIndented
    "flush" -> return DocstringFlush
    _ -> fail "Invalid DocstringStyle"

instance ToJSON DocstringStyle where
  toJSON DocstringIndented = "indented"
  toJSON DocstringFlush = "flush"

instance FromJSON CommentDelimiterStyle where
  parseJSON = withText "CommentDelimiterStyle" $ \t -> case t of
    "single" -> return CommentSingleSpace
    "double" -> return CommentDoubleSpace
    "none" -> return CommentNoSpace
    _ -> fail "Invalid CommentDelimiterStyle"

instance ToJSON CommentDelimiterStyle where
  toJSON CommentSingleSpace = "single"
  toJSON CommentDoubleSpace = "double"
  toJSON CommentNoSpace = "none"

instance FromJSON BlockCommentStyle where
  parseJSON = withText "BlockCommentStyle" $ \t -> case t of
    "horizontal" -> return BlockHorizontal
    "vertical" -> return BlockVertical
    _ -> fail "Invalid BlockCommentStyle"

instance ToJSON BlockCommentStyle where
  toJSON BlockHorizontal = "horizontal"
  toJSON BlockVertical = "vertical"

instance FromJSON HeadingStyle where
  parseJSON = withText "HeadingStyle" $ \t -> case t of
    "semicolons" -> return HeadingSemicolons
    "hash" -> return HeadingHash
    _ -> fail "Invalid HeadingStyle"

instance ToJSON HeadingStyle where
  toJSON HeadingSemicolons = "semicolons"
  toJSON HeadingHash = "hash"

instance FromJSON CheckboxStyle where
  parseJSON = withText "CheckboxStyle" $ \t -> case t of
    "upper" -> return CheckboxUpperX
    "lower" -> return CheckboxLowerX
    _ -> fail "Invalid CheckboxStyle"

instance ToJSON CheckboxStyle where
  toJSON CheckboxUpperX = "upper"
  toJSON CheckboxLowerX = "lower"

instance FromJSON DictColonStyle where
  parseJSON = withText "DictColonStyle" $ \t -> case t of
    "none" -> return DictColonNoSpace
    "after" -> return DictColonAfter
    "both" -> return DictColonBoth
    _ -> fail "Invalid DictColonStyle"

instance ToJSON DictColonStyle where
  toJSON DictColonNoSpace = "none"
  toJSON DictColonAfter = "after"
  toJSON DictColonBoth = "both"

instance FromJSON DefineStyle where
  parseJSON = withText "DefineStyle" $ \t -> case t of
    "space" -> return DefineSpaceSeparated
    "comma" -> return DefineCommaSeparated
    _ -> fail "Invalid DefineStyle"

instance ToJSON DefineStyle where
  toJSON DefineSpaceSeparated = "space"
  toJSON DefineCommaSeparated = "comma"

instance FromJSON LambdaStyle where
  parseJSON = withText "LambdaStyle" $ \t -> case t of
    "keyword" -> return LambdaKeyword
    "symbol" -> return LambdaSymbol
    _ -> fail "Invalid LambdaStyle"

instance ToJSON LambdaStyle where
  toJSON LambdaKeyword = "keyword"
  toJSON LambdaSymbol = "symbol"

instance FromJSON EmptyBlockStyle where
  parseJSON = withText "EmptyBlockStyle" $ \t -> case t of
    "compact" -> return EmptyBlockCompact
    "spaced" -> return EmptyBlockSpaced
    "newline" -> return EmptyBlockNewline
    _ -> fail "Invalid EmptyBlockStyle"

instance ToJSON EmptyBlockStyle where
  toJSON EmptyBlockCompact = "compact"
  toJSON EmptyBlockSpaced = "spaced"
  toJSON EmptyBlockNewline = "newline"

instance FromJSON QuoteStyle where
  parseJSON = withText "QuoteStyle" $ \t -> case t of
    "double" -> return QuoteDouble
    "single" -> return QuoteSingle
    _ -> fail "Invalid QuoteStyle"

instance ToJSON QuoteStyle where
  toJSON QuoteDouble = "double"
  toJSON QuoteSingle = "single"

instance FromJSON CatchVarStyle where
  parseJSON = withText "CatchVarStyle" $ \t -> case t of
    "spaced" -> return CatchVarSpaced
    "parens" -> return CatchVarParens
    "spaced_parens" -> return CatchVarSpacedParens
    _ -> fail "Invalid CatchVarStyle"

instance ToJSON CatchVarStyle where
  toJSON CatchVarSpaced = "spaced"
  toJSON CatchVarParens = "parens"
  toJSON CatchVarSpacedParens = "spaced_parens"

instance FromJSON EmbedStyle where
  parseJSON = withText "EmbedStyle" $ \t -> case t of
    "quoted" -> return EmbedQuoted
    "unquoted" -> return EmbedUnquoted
    _ -> fail "Invalid EmbedStyle"

instance ToJSON EmbedStyle where
  toJSON EmbedQuoted = "quoted"
  toJSON EmbedUnquoted = "unquoted"

instance FromJSON ChainedCallStyle where
  parseJSON = withText "ChainedCallStyle" $ \t -> case t of
    "sameline" -> return ChainedSameLine
    "newline" -> return ChainedNewLine
    _ -> fail "Invalid ChainedCallStyle"

instance ToJSON ChainedCallStyle where
  toJSON ChainedSameLine = "sameline"
  toJSON ChainedNewLine = "newline"

-- Main FormatConfig FromJSON
instance FromJSON FormatConfig where
  parseJSON = withObject "FormatConfig" $ \v -> FormatConfig
    <$> v .:? "indentSize" .!= indentSize defaultConfig
    <*> v .:? "useTabs" .!= useTabs defaultConfig
    <*> v .:? "maxLineLength" .!= maxLineLength defaultConfig
    <*> v .:? "wrapLongExpressions" .!= wrapLongExpressions defaultConfig
    <*> v .:? "wrapAfterOperator" .!= wrapAfterOperator defaultConfig
    <*> v .:? "binaryOpWrapIndent" .!= binaryOpWrapIndent defaultConfig
    <*> v .:? "chainedCallWrapStyle" .!= chainedCallWrapStyle defaultConfig
    <*> v .:? "newlinesBetweenTopLevel" .!= newlinesBetweenTopLevel defaultConfig
    <*> v .:? "newlinesBetweenFunctions" .!= newlinesBetweenFunctions defaultConfig
    <*> v .:? "newlinesInsideFunction" .!= newlinesInsideFunction defaultConfig
    <*> v .:? "newlinesBeforeComment" .!= newlinesBeforeComment defaultConfig
    <*> v .:? "newlinesAfterComment" .!= newlinesAfterComment defaultConfig
    <*> v .:? "preserveBlankLines" .!= preserveBlankLines defaultConfig
    <*> v .:? "maxConsecutiveBlankLines" .!= maxConsecutiveBlankLines defaultConfig
    <*> v .:? "removeBlankLineAtStart" .!= removeBlankLineAtStart defaultConfig
    <*> v .:? "removeBlankLineAtEnd" .!= removeBlankLineAtEnd defaultConfig
    <*> v .:? "spaceAfterComma" .!= spaceAfterComma defaultConfig
    <*> v .:? "spaceBeforeComma" .!= spaceBeforeComma defaultConfig
    <*> v .:? "spaceInsideParens" .!= spaceInsideParens defaultConfig
    <*> v .:? "spaceInsideBrackets" .!= spaceInsideBrackets defaultConfig
    <*> v .:? "spaceInsideBraces" .!= spaceInsideBraces defaultConfig
    <*> v .:? "spaceAroundAssign" .!= spaceAroundAssign defaultConfig
    <*> v .:? "spaceAroundBinOp" .!= spaceAroundBinOp defaultConfig
    <*> v .:? "spaceAroundCompare" .!= spaceAroundCompare defaultConfig
    <*> v .:? "spaceAfterUnaryOp" .!= spaceAfterUnaryOp defaultConfig
    <*> v .:? "spaceAroundColon" .!= spaceAroundColon defaultConfig
    <*> v .:? "spaceAroundArrow" .!= spaceAroundArrow defaultConfig
    <*> v .:? "alignAssignments" .!= alignAssignments defaultConfig
    <*> v .:? "alignAssignmentThreshold" .!= alignAssignmentThreshold defaultConfig
    <*> v .:? "alignComparisons" .!= alignComparisons defaultConfig
    <*> v .:? "alignComparisonThreshold" .!= alignComparisonThreshold defaultConfig
    <*> v .:? "alignComparisonOps" .!= alignComparisonOps defaultConfig
    <*> v .:? "alignFunctionArgs" .!= alignFunctionArgs defaultConfig
    <*> v .:? "oneArgPerLineThreshold" .!= oneArgPerLineThreshold defaultConfig
    <*> v .:? "alignAssertMessages" .!= alignAssertMessages defaultConfig
    <*> v .:? "spaceBeforeFnParen" .!= spaceBeforeFnParen defaultConfig
    <*> v .:? "spaceBeforeFnBrace" .!= spaceBeforeFnBrace defaultConfig
    <*> v .:? "fnBraceOnNewLine" .!= fnBraceOnNewLine defaultConfig
    <*> v .:? "spaceAfterFnKeyword" .!= spaceAfterFnKeyword defaultConfig
    <*> v .:? "fnDocstringStyle" .!= fnDocstringStyle defaultConfig
    <*> v .:? "spaceBeforeIfParen" .!= spaceBeforeIfParen defaultConfig
    <*> v .:? "spaceBeforeWhileParen" .!= spaceBeforeWhileParen defaultConfig
    <*> v .:? "spaceBeforeForParen" .!= spaceBeforeForParen defaultConfig
    <*> v .:? "spaceBeforeMatchParen" .!= spaceBeforeMatchParen defaultConfig
    <*> v .:? "ifBraceOnNewLine" .!= ifBraceOnNewLine defaultConfig
    <*> v .:? "whileBraceOnNewLine" .!= whileBraceOnNewLine defaultConfig
    <*> v .:? "forBraceOnNewLine" .!= forBraceOnNewLine defaultConfig
    <*> v .:? "matchBraceOnNewLine" .!= matchBraceOnNewLine defaultConfig
    <*> v .:? "spaceAfterIfKeyword" .!= spaceAfterIfKeyword defaultConfig
    <*> v .:? "spaceAfterWhileKeyword" .!= spaceAfterWhileKeyword defaultConfig
    <*> v .:? "spaceAfterForKeyword" .!= spaceAfterForKeyword defaultConfig
    <*> v .:? "spaceAfterMatchKeyword" .!= spaceAfterMatchKeyword defaultConfig
    <*> v .:? "spaceBeforeMatchArrow" .!= spaceBeforeMatchArrow defaultConfig
    <*> v .:? "spaceAfterMatchArrow" .!= spaceAfterMatchArrow defaultConfig
    <*> v .:? "matchArmIndent" .!= matchArmIndent defaultConfig
    <*> v .:? "matchPatternOnNewLine" .!= matchPatternOnNewLine defaultConfig
    <*> v .:? "alignInlineComments" .!= alignInlineComments defaultConfig
    <*> v .:? "inlineCommentColumn" .!= inlineCommentColumn defaultConfig
    <*> v .:? "spaceAfterCommentDelimiter" .!= spaceAfterCommentDelimiter defaultConfig
    <*> v .:? "preserveCommentIndent" .!= preserveCommentIndent defaultConfig
    <*> v .:? "preserveCommentFormatting" .!= preserveCommentFormatting defaultConfig
    <*> v .:? "commentDelimiterStyle" .!= commentDelimiterStyle defaultConfig
    <*> v .:? "blockCommentStyle" .!= blockCommentStyle defaultConfig
    <*> v .:? "orgModeEnabled" .!= orgModeEnabled defaultConfig
    <*> v .:? "headingStyle" .!= headingStyle defaultConfig
    <*> v .:? "checkboxStyle" .!= checkboxStyle defaultConfig
    <*> v .:? "autoUpdateCookies" .!= autoUpdateCookies defaultConfig
    <*> v .:? "autoAddClosedTimestamp" .!= autoAddClosedTimestamp defaultConfig
    <*> v .:? "closedTimestampFormat" .!= closedTimestampFormat defaultConfig
    <*> v .:? "trailingCommaInLists" .!= trailingCommaInLists defaultConfig
    <*> v .:? "oneElementPerLine" .!= oneElementPerLine defaultConfig
    <*> v .:? "maxElementsPerLine" .!= maxElementsPerLine defaultConfig
    <*> v .:? "listWrapThreshold" .!= listWrapThreshold defaultConfig
    <*> v .:? "dictColonStyle" .!= dictColonStyle defaultConfig
    <*> v .:? "spaceAroundAs" .!= spaceAroundAs defaultConfig
    <*> v .:? "dotSeparatorSpacing" .!= dotSeparatorSpacing defaultConfig
    <*> v .:? "sortUseStatements" .!= sortUseStatements defaultConfig
    <*> v .:? "groupUseByPrefix" .!= groupUseByPrefix defaultConfig
    <*> v .:? "blankLinesBetweenUseGroups" .!= blankLinesBetweenUseGroups defaultConfig
    <*> v .:? "defineStyle" .!= defineStyle defaultConfig
    <*> v .:? "lambdaStyle" .!= lambdaStyle defaultConfig
    <*> v .:? "spaceAfterLambdaKeyword" .!= spaceAfterLambdaKeyword defaultConfig
    <*> v .:? "lambdaBraceOnNewLine" .!= lambdaBraceOnNewLine defaultConfig
    <*> v .:? "layoutFieldsOnNewLine" .!= layoutFieldsOnNewLine defaultConfig
    <*> v .:? "layoutFieldAlignment" .!= layoutFieldAlignment defaultConfig
    <*> v .:? "layoutTrailingComma" .!= layoutTrailingComma defaultConfig
    <*> v .:? "compactSmallBlocks" .!= compactSmallBlocks defaultConfig
    <*> v .:? "maxCompactBlockLength" .!= maxCompactBlockLength defaultConfig
    <*> v .:? "blockStatementSpacing" .!= blockStatementSpacing defaultConfig
    <*> v .:? "emptyBlockStyle" .!= emptyBlockStyle defaultConfig
    <*> v .:? "stringQuoteStyle" .!= stringQuoteStyle defaultConfig
    <*> v .:? "fstringSpacing" .!= fstringSpacing defaultConfig
    <*> v .:? "preferTripleQuotes" .!= preferTripleQuotes defaultConfig
    <*> v .:? "integerGrouping" .!= integerGrouping defaultConfig
    <*> v .:? "integerGroupSize" .!= integerGroupSize defaultConfig
    <*> v .:? "hexUpperCase" .!= hexUpperCase defaultConfig
    <*> v .:? "floatScientificThreshold" .!= floatScientificThreshold defaultConfig
    <*> v .:? "spaceAfterReturn" .!= spaceAfterReturn defaultConfig
    <*> v .:? "returnOnSeparateLine" .!= returnOnSeparateLine defaultConfig
    <*> v .:? "spaceAfterCatch" .!= spaceAfterCatch defaultConfig
    <*> v .:? "catchVarStyle" .!= catchVarStyle defaultConfig
    <*> v .:? "deferBraceOnNewLine" .!= deferBraceOnNewLine defaultConfig
    <*> v .:? "spaceAfterDefer" .!= spaceAfterDefer defaultConfig
    <*> v .:? "labelColonSpacing" .!= labelColonSpacing defaultConfig
    <*> v .:? "spaceAfterGoto" .!= spaceAfterGoto defaultConfig
    <*> v .:? "spaceBeforeTypeColon" .!= spaceBeforeTypeColon defaultConfig
    <*> v .:? "spaceAfterTypeColon" .!= spaceAfterTypeColon defaultConfig
    <*> v .:? "spaceAroundSliceColon" .!= spaceAroundSliceColon defaultConfig
    <*> v .:? "spaceInsideIndexBrackets" .!= spaceInsideIndexBrackets defaultConfig
    <*> v .:? "asmArgumentSpacing" .!= asmArgumentSpacing defaultConfig
    <*> v .:? "embedArgumentStyle" .!= embedArgumentStyle defaultConfig
    <*> v .:? "comptimeBraceOnNewLine" .!= comptimeBraceOnNewLine defaultConfig
    <*> v .:? "spaceAfterComptime" .!= spaceAfterComptime defaultConfig
    <*> v .:? "ensureNewlineAtEOF" .!= ensureNewlineAtEOF defaultConfig
    <*> v .:? "trimTrailingWhitespace" .!= trimTrailingWhitespace defaultConfig
    <*> v .:? "normalizeWhitespace" .!= normalizeWhitespace defaultConfig
    <*> v .:? "collapseAdjacentSpaces" .!= collapseAdjacentSpaces defaultConfig

instance ToJSON FormatConfig where
  toJSON cfg = object
    [ "indentSize" .= indentSize cfg
    , "useTabs" .= useTabs cfg
    , "maxLineLength" .= maxLineLength cfg
    ]

findFormatYaml :: FilePath -> IO (Maybe FilePath)
findFormatYaml startPath = search startPath
  where
    search dir = do
      let configPath = dir </> "format.yaml"
      exists <- doesFileExist configPath
      if exists then return (Just configPath)
      else do
        let parent = takeDirectory dir
        if parent == dir then return Nothing else search parent

loadFormatConfig :: Maybe FilePath -> IO FormatConfig
loadFormatConfig Nothing = do
  cwd <- getCurrentDirectory
  mPath <- findFormatYaml cwd
  case mPath of
    Nothing -> return defaultConfig
    Just path -> loadFromPath path
loadFormatConfig (Just path) = loadFromPath path

loadFromPath :: FilePath -> IO FormatConfig
loadFromPath path = do
  result <- decodeFileEither path
  case result of
    Left err -> do
      putStrLn $ "Error parsing format.yaml: " ++ show err
      return defaultConfig
    Right cfg -> return cfg
