{-# LANGUAGE OverloadedStrings #-}
module Formatter (format, formatFile) where

import Data.Text (Text)
import qualified Data.Text as T
import AST
import Data.List (groupBy, sortBy)
import FormatConfig
import Text.Megaparsec (parse, errorBundlePretty)
import Parser (file)

--- PUBLIC API

formatFile :: FormatConfig -> Text -> Text
formatFile cfg source =
  case parseFile source of
    Left err ->
      -- Return original source with error comment at top
      ";; Parse error: " <> T.pack err <> "\n" <> source
    Right decls -> format cfg decls

format :: FormatConfig -> [Decl] -> Text
format cfg decls =
  let (useDecls, otherDecls) = partitionUseDecls decls
      sortedUses = if sortUseStatements cfg then sortUseDecls cfg useDecls else useDecls
      groupedUses = if groupUseByPrefix cfg then groupUseDecls cfg sortedUses else [sortedUses]
      updatedDecls = if orgModeEnabled cfg then updateOrgModeFeatures cfg otherDecls else otherDecls
      formattedUses = formatUseGroups cfg groupedUses
      formattedOthers = formatDeclarations cfg updatedDecls
      allFormatted = formattedUses ++ formattedOthers
      combined = T.intercalate (T.replicate (newlinesBetweenTopLevel cfg) "\n") allFormatted
  in postProcess cfg combined

--- PARSING

parseFile :: Text -> Either String [Decl]
parseFile source =
  case parse file "<formatter>" source of
    Left parseErr -> Left (errorBundlePretty parseErr)
    Right decls -> Right decls

--- USE STATEMENT HANDLING

partitionUseDecls :: [Decl] -> ([Decl], [Decl])
partitionUseDecls = foldr partition ([], [])
  where partition d@(UseDecl _ _) (uses, others) = (d:uses, others)
        partition d (uses, others) = (uses, d:others)

sortUseDecls :: FormatConfig -> [Decl] -> [Decl]
sortUseDecls _cfg = sortBy compareUse
  where compareUse (UseDecl p1 _) (UseDecl p2 _) = compare p1 p2
        compareUse _ _ = EQ

groupUseDecls :: FormatConfig -> [Decl] -> [[Decl]]
groupUseDecls _cfg = groupBy samePrefix
  where samePrefix (UseDecl p1 _) (UseDecl p2 _) = getPrefix p1 == getPrefix p2
        samePrefix _ _ = False
        getPrefix = T.takeWhile (/= '.')

formatUseGroups :: FormatConfig -> [[Decl]] -> [Text]
formatUseGroups cfg groups =
  let formatted = map formatUseGroup groups
      spacing = T.replicate (blankLinesBetweenUseGroups cfg) "\n"
  in [T.intercalate spacing formatted]
  where formatUseGroup g = T.intercalate "\n" $ map (formatDecl cfg 0) g

--- ORG-MODE FEATURES

updateOrgModeFeatures :: FormatConfig -> [Decl] -> [Decl]
updateOrgModeFeatures cfg decls =
  let withCookies = if autoUpdateCookies cfg then updateCookies decls else decls
      withTimestamps = if autoAddClosedTimestamp cfg then addClosedTimestamps cfg withCookies else withCookies
  in withTimestamps

updateCookies :: [Decl] -> [Decl]
updateCookies = map updateDecl
  where
    updateDecl (CommentDecl (LineComment (HeadingComment lvl state txt) orig ind)) =
      let (before, after) = T.breakOn "[" txt
          updated = if "[" `T.isInfixOf` txt
                    then before <> updateCookie txt
                    else txt
      in CommentDecl (LineComment (HeadingComment lvl state updated) orig ind)
    updateDecl d = d

    updateCookie txt =
      let (before, rest) = T.breakOn "[" txt
          (_, after) = T.breakOn "]" rest
      in if T.null rest then txt else "[0/0]" <> T.drop 1 after

addClosedTimestamps :: FormatConfig -> [Decl] -> [Decl]
addClosedTimestamps _cfg = id

--- DECLARATION FORMATTING

formatDeclarations :: FormatConfig -> [Decl] -> [Text]
formatDeclarations cfg decls =
  let grouped = if alignAssignments cfg || alignComparisons cfg
                then groupForAlignment cfg decls else map (:[]) decls
      formatted = concatMap (formatGroup cfg) grouped
  in addFunctionSpacing cfg formatted

groupForAlignment :: FormatConfig -> [Decl] -> [[Decl]]
groupForAlignment cfg = groupBy shouldGroup
  where shouldGroup d1 d2 = (alignAssignments cfg && isAssign d1 && isAssign d2)
                          || (alignComparisons cfg && isComp d1 && isComp d2)
        isAssign (DefineDecl _ _) = True
        isAssign (AssignDecl _ _) = True
        isAssign _ = False
        isComp (ExprDecl (BinOp op _ _)) = op `elem` [Eq, Neq, Lt, Le, Gt, Ge]
        isComp _ = False

formatGroup :: FormatConfig -> [Decl] -> [Text]
formatGroup cfg decls
  | length decls >= alignAssignmentThreshold cfg = [formatAlignedGroup cfg decls]
  | otherwise = map (formatDecl cfg 0) decls

formatAlignedGroup :: FormatConfig -> [Decl] -> Text
formatAlignedGroup cfg decls =
  let maxWidth = maximum $ map (leftWidth cfg) decls
      formatted = map (formatAligned cfg maxWidth) decls
  in T.intercalate "\n" formatted

leftWidth :: FormatConfig -> Decl -> Int
leftWidth _cfg (DefineDecl names _) = T.length $ "define " <> T.intercalate " " names
leftWidth cfg (AssignDecl lhs _) = T.length $ formatExpr cfg 0 lhs
leftWidth _ _ = 0

formatAligned :: FormatConfig -> Int -> Decl -> Text
formatAligned cfg alignCol (DefineDecl names expr) =
  let left = "define " <> T.intercalate " " names
      pad = T.replicate (alignCol - T.length left + 1) " "
      sp = spaces (spaceAroundAssign cfg)
  in left <> pad <> "=" <> sp <> formatExpr cfg 0 expr
formatAligned cfg _ d = formatDecl cfg 0 d

addFunctionSpacing :: FormatConfig -> [Text] -> [Text]
addFunctionSpacing _cfg [] = []
addFunctionSpacing _cfg [x] = [x]
addFunctionSpacing cfg (x:y:xs)
  | isFn x && isFn y = x : T.replicate (newlinesBetweenFunctions cfg) "\n" : addFunctionSpacing cfg (y:xs)
  | otherwise = x : addFunctionSpacing cfg (y:xs)
  where isFn t = "fn " `T.isPrefixOf` T.stripStart t

formatDecl :: FormatConfig -> Int -> Decl -> Text
formatDecl cfg i (UseDecl p a) = ind <> "use " <> p <> maybe "" (\x -> spaces (spaceAroundAs cfg) <> "as" <> spaces (spaceAroundAs cfg) <> x) a
  where ind = makeIndent cfg i
formatDecl cfg i (DefineDecl ns e) = ind <> "define " <> T.intercalate " " ns <> sp <> "=" <> sp <> formatExpr cfg i e
  where ind = makeIndent cfg i; sp = spaces (spaceAroundAssign cfg)
formatDecl cfg i (AssignDecl l r) = ind <> formatExpr cfg i l <> sp <> "=" <> sp <> formatExpr cfg i r
  where ind = makeIndent cfg i; sp = spaces (spaceAroundAssign cfg)
formatDecl cfg i (CompoundAssignDecl l op r) = ind <> formatExpr cfg i l <> " " <> formatCompoundOp op <> " " <> formatExpr cfg i r
  where ind = makeIndent cfg i
formatDecl cfg i (FnDecl n ps rt d b) = formatFunction cfg i n ps rt d b
formatDecl cfg i (LayoutDecl n fs) = formatLayout cfg i n fs
formatDecl cfg i (ExprDecl e) = makeIndent cfg i <> formatExpr cfg i e
formatDecl cfg i (CommentDecl c) = formatComment cfg i c

formatCompoundOp :: CompoundOp -> Text
formatCompoundOp AddAssign = "+="
formatCompoundOp SubAssign = "-="
formatCompoundOp MulAssign = "*="
formatCompoundOp DivAssign = "/="
formatCompoundOp ModAssign = "%="
formatCompoundOp AndAssign = "&="
formatCompoundOp OrAssign = "|="
formatCompoundOp XorAssign = "^="
formatCompoundOp LShiftAssign = "<<="
formatCompoundOp RShiftAssign = ">>="

formatFunction :: FormatConfig -> Int -> Text -> [Param] -> Maybe Text -> Maybe DocString -> [Stmt] -> Text
formatFunction cfg i n ps rt d b =
  let ind = makeIndent cfg i
      pStr = formatParams cfg ps
      rStr = maybe "" (\t -> ":" <> spaces (spaceAfterTypeColon cfg) <> t) rt
      ob = if fnBraceOnNewLine cfg then "\n" <> ind <> "{" else spaces (spaceBeforeFnBrace cfg) <> "{"
      dStr = maybe "" (\x -> makeIndent cfg (i+1) <> "\"" <> x <> "\"\n") d
      bStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) b
  in ind <> "fn" <> spaces (spaceAfterFnKeyword cfg) <> n <> spaces (spaceBeforeFnParen cfg) <> "(" <> pStr <> ")" <> rStr <> ob <> "\n" <> dStr <> bStr <> "\n" <> ind <> "}"

formatLayout :: FormatConfig -> Int -> Text -> [LayoutField] -> Text
formatLayout cfg i n fs =
  let ind = makeIndent cfg i
      fStr = T.intercalate ",\n" $ map (\(LayoutField nm tp _) -> makeIndent cfg (i+1) <> nm <> ":" <> spaces (spaceAfterTypeColon cfg) <> tp) fs
  in ind <> "layout " <> n <> " {\n" <> fStr <> "\n" <> ind <> "}"

formatParams :: FormatConfig -> [Param] -> Text
formatParams cfg ps = T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") $ map fmt ps
  where fmt (Param n t d) = n <> maybe "" (\x -> ":" <> spaces (spaceAfterTypeColon cfg) <> x) t <>
                                  maybe "" (\e -> spaces (spaceAroundAssign cfg) <> "=" <> spaces (spaceAroundAssign cfg) <> formatExpr cfg 0 e) d

formatStmt :: FormatConfig -> Int -> Stmt -> Text
formatStmt cfg i (ExprStmt e) = makeIndent cfg i <> formatExpr cfg i e
formatStmt cfg i (VarDecl ns e) = makeIndent cfg i <> "def " <> T.intercalate " " ns <> sp <> "=" <> sp <> formatExpr cfg i e
  where sp = spaces (spaceAroundAssign cfg)
formatStmt cfg i (Assign l r) = makeIndent cfg i <> formatExpr cfg i l <> sp <> "=" <> sp <> formatExpr cfg i r
  where sp = spaces (spaceAroundAssign cfg)
formatStmt cfg i (CompoundAssign l op r) = makeIndent cfg i <> formatExpr cfg i l <> " " <> formatCompoundOp op <> " " <> formatExpr cfg i r
formatStmt cfg i (IfStmt c t e) = formatIf cfg i c t e
formatStmt cfg i (WhileStmt c b) = formatWhile cfg i c b
formatStmt cfg i (ForStmt v it b) = formatFor cfg i v it b
formatStmt cfg i (TryStmt tb ev cb) = formatTry cfg i tb ev cb
formatStmt cfg i (ReturnStmt me) = makeIndent cfg i <> "return" <> maybe "" (\e -> spaces (spaceAfterReturn cfg) <> formatExpr cfg i e) me
formatStmt cfg i BreakStmt = makeIndent cfg i <> "break"
formatStmt cfg i ContinueStmt = makeIndent cfg i <> "continue"
formatStmt cfg i (GotoStmt lbl) = makeIndent cfg i <> "goto" <> spaces (spaceAfterGoto cfg) <> lbl
formatStmt cfg i (LabelStmt lbl) = makeIndent cfg i <> lbl <> ":"
formatStmt cfg i (DeferStmt b) = formatDefer cfg i b
formatStmt cfg i (MatchStmt t arms def) = formatMatch cfg i t arms def
formatStmt cfg i (BlockStmt ss) = makeIndent cfg i <> "{\n" <> T.intercalate "\n" (map (formatStmt cfg (i+1)) ss) <> "\n" <> makeIndent cfg i <> "}"
formatStmt cfg i (CommentStmt c) = formatComment cfg i c

formatIf :: FormatConfig -> Int -> Expr -> [Stmt] -> Maybe [Stmt] -> Text
formatIf cfg i c t e =
  let ind = makeIndent cfg i
      ob = if ifBraceOnNewLine cfg then "\n" <> ind <> "{" else spaces (spaceBeforeFnBrace cfg) <> "{"
      tStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) t
      eStr = maybe "" (\ss -> " else" <> spaces (spaceBeforeFnBrace cfg) <> "{\n" <> T.intercalate "\n" (map (formatStmt cfg (i+1)) ss) <> "\n" <> ind <> "}") e
  in ind <> "if" <> spaces (spaceAfterIfKeyword cfg) <> formatExpr cfg i c <> ob <> "\n" <> tStr <> "\n" <> ind <> "}" <> eStr

formatWhile :: FormatConfig -> Int -> Expr -> [Stmt] -> Text
formatWhile cfg i c b =
  let ind = makeIndent cfg i
      ob = if whileBraceOnNewLine cfg then "\n" <> ind <> "{" else spaces (spaceBeforeFnBrace cfg) <> "{"
      bStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) b
  in ind <> "while" <> spaces (spaceAfterWhileKeyword cfg) <> formatExpr cfg i c <> ob <> "\n" <> bStr <> "\n" <> ind <> "}"

formatFor :: FormatConfig -> Int -> Text -> Expr -> [Stmt] -> Text
formatFor cfg i v it b =
  let ind = makeIndent cfg i
      ob = if forBraceOnNewLine cfg then "\n" <> ind <> "{" else spaces (spaceBeforeFnBrace cfg) <> "{"
      bStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) b
  in ind <> "for" <> spaces (spaceAfterForKeyword cfg) <> v <> " in " <> formatExpr cfg i it <> ob <> "\n" <> bStr <> "\n" <> ind <> "}"

formatTry :: FormatConfig -> Int -> [Stmt] -> Maybe Text -> [Stmt] -> Text
formatTry cfg i tb ev cb =
  let ind = makeIndent cfg i
      tbStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) tb
      cbStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) cb
      errVar = maybe "" (\v -> " " <> v) ev
  in ind <> "try {\n" <> tbStr <> "\n" <> ind <> "} catch" <> errVar <> " {\n" <> cbStr <> "\n" <> ind <> "}"

formatDefer :: FormatConfig -> Int -> [Stmt] -> Text
formatDefer cfg i b =
  let ind = makeIndent cfg i
      ob = if deferBraceOnNewLine cfg then "\n" <> ind <> "{" else spaces (spaceAfterDefer cfg) <> "{"
      bStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) b
  in ind <> "defer" <> ob <> "\n" <> bStr <> "\n" <> ind <> "}"

formatMatch :: FormatConfig -> Int -> Expr -> [MatchArm] -> Maybe [Stmt] -> Text
formatMatch cfg i t arms def =
  let ind = makeIndent cfg i
      armsStr = T.intercalate "\n" $ map (formatMatchArm cfg (i+1)) arms
      defStr = maybe "" (\ss -> makeIndent cfg (i+1) <> "else -> {\n" <> T.intercalate "\n" (map (formatStmt cfg (i+2)) ss) <> "\n" <> makeIndent cfg (i+1) <> "}\n") def
  in ind <> "match" <> spaces (spaceAfterMatchKeyword cfg) <> formatExpr cfg i t <> " {\n" <> armsStr <> "\n" <> defStr <> ind <> "}"

formatMatchArm :: FormatConfig -> Int -> MatchArm -> Text
formatMatchArm cfg i (MatchArm pat conseq) =
  let ind = makeIndent cfg i
      patStr = formatExpr cfg i pat
      conseqStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) conseq
  in ind <> patStr <> spaces (spaceBeforeMatchArrow cfg) <> "->" <> spaces (spaceAfterMatchArrow cfg) <> "{\n" <> conseqStr <> "\n" <> ind <> "}"

formatExpr :: FormatConfig -> Int -> Expr -> Text
formatExpr _cfg _i (Var n) = n
formatExpr _cfg _i (IntLit n) = T.pack $ show n
formatExpr _cfg _i (FloatLit f) = T.pack $ show f
formatExpr _cfg _i (StrLit s) = "\"" <> escapeString s <> "\""
formatExpr _cfg _i (BoolLit True) = "true"
formatExpr _cfg _i (BoolLit False) = "false"
formatExpr cfg i (ListLit xs) = "[" <> T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") (map (formatExpr cfg i) xs) <> "]"
formatExpr cfg i (TupleLit xs) = "(" <> T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") (map (formatExpr cfg i) xs) <> ")"
formatExpr cfg i (SetLit xs) = "{" <> T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") (map (formatExpr cfg i) xs) <> "}"
formatExpr cfg i (DictLit pairs) = "{" <> T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") (map formatPair pairs) <> "}"
  where formatPair (k, v) = formatExpr cfg i k <> ": " <> formatExpr cfg i v
formatExpr cfg i (Call f as) = formatExpr cfg i f <> "(" <> formatCallArgs cfg i as <> ")"
formatExpr cfg i (MemberCall obj m as) = formatExpr cfg i obj <> "." <> m <> "(" <> formatCallArgs cfg i as <> ")"
formatExpr _cfg _i (InferredMember m) = "." <> m
formatExpr cfg i (Index e start stop step) = formatExpr cfg i e <> "[" <> formatSlice start stop step <> "]"
  where
    formatSlice Nothing Nothing Nothing = ":"
    formatSlice (Just s) Nothing Nothing = formatExpr cfg i s
    formatSlice (Just s) (Just e') Nothing = formatExpr cfg i s <> ":" <> formatExpr cfg i e'
    formatSlice (Just s) (Just e') (Just st) = formatExpr cfg i s <> ":" <> formatExpr cfg i e' <> ":" <> formatExpr cfg i st
    formatSlice Nothing (Just e') Nothing = ":" <> formatExpr cfg i e'
    formatSlice Nothing (Just e') (Just st) = ":" <> formatExpr cfg i e' <> ":" <> formatExpr cfg i st
    formatSlice _ _ _ = ":"
formatExpr cfg i (BinOp op l r) = formatExpr cfg i l <> sp <> formatBinOpSym op <> sp <> formatExpr cfg i r
  where sp = spaces (spaceAroundBinOp cfg)
formatExpr cfg i (UnOp op e) = formatUnOpSym op <> spaces (spaceAfterUnaryOp cfg) <> formatExpr cfg i e
formatExpr cfg i (LogicalOp op l r) = formatExpr cfg i l <> sp <> formatLogicalOp op <> sp <> formatExpr cfg i r
  where sp = spaces (spaceAroundBinOp cfg)
formatExpr cfg i (Lambda ps rt b isVar) = formatLambda cfg i ps rt b isVar
formatExpr cfg i (FnExpr ps rt b isVar) = "fn(" <> formatParams cfg ps <> ")" <> maybe "" (\t -> ": " <> t) rt <> " {\n" <> T.intercalate "\n" (map (formatStmt cfg (i+1)) b) <> "\n" <> makeIndent cfg i <> "}"
formatExpr cfg i (FString parts) = "f\"" <> T.concat (map (formatFStringPart cfg i) parts) <> "\""
formatExpr cfg i (AsmExpr code constraints args) = "asm(\"" <> code <> "\"" <> (if T.null constraints then "" else ", \"" <> constraints <> "\"") <> T.concat (map (\a -> ", " <> formatExpr cfg i a) args) <> ")"
formatExpr _cfg _i (EmbedExpr path) = "embed(\"" <> path <> "\")"
formatExpr cfg i (ComptimeExpr stmts) = "comptime {\n" <> T.intercalate "\n" (map (formatStmt cfg (i+1)) stmts) <> "\n" <> makeIndent cfg i <> "}"

formatLambda :: FormatConfig -> Int -> [Param] -> Maybe Text -> [Stmt] -> Bool -> Text
formatLambda cfg i ps rt b _isVar =
  let lambdaKw = case lambdaStyle cfg of
        LambdaKeyword -> "lambda"
        LambdaSymbol -> "λ"
      pStr = formatParams cfg ps
      rStr = maybe "" (\t -> ": " <> t) rt
      ob = if lambdaBraceOnNewLine cfg then "\n" <> makeIndent cfg i <> "{" else " {"
      bStr = T.intercalate "\n" $ map (formatStmt cfg (i+1)) b
  in lambdaKw <> spaces (spaceAfterLambdaKeyword cfg) <> "(" <> pStr <> ")" <> rStr <> ob <> "\n" <> bStr <> "\n" <> makeIndent cfg i <> "}"

formatFStringPart :: FormatConfig -> Int -> FStringPart -> Text
formatFStringPart _cfg _i (FStrText t) = escapeString t
formatFStringPart cfg i (FStrExpr e) = "{" <> formatExpr cfg i e <> "}"

formatLogicalOp :: LogicalOp -> Text
formatLogicalOp And = "&&"
formatLogicalOp Or = "||"

formatCallArgs :: FormatConfig -> Int -> [CallArg] -> Text
formatCallArgs cfg i as = T.intercalate (if spaceAfterComma cfg > 0 then ", " else ",") $ map fmt as
  where fmt (CallArg Nothing e) = formatExpr cfg i e
        fmt (CallArg (Just n) e) = n <> spaces (spaceAroundAssign cfg) <> "=" <> spaces (spaceAroundAssign cfg) <> formatExpr cfg i e

formatBinOpSym :: BinOp -> Text
formatBinOpSym Add = "+"; formatBinOpSym Sub = "-"; formatBinOpSym Mul = "*"
formatBinOpSym Div = "/"; formatBinOpSym Mod = "%"; formatBinOpSym Eq = "=="
formatBinOpSym Neq = "!="; formatBinOpSym Lt = "<"; formatBinOpSym Le = "<="
formatBinOpSym Gt = ">"; formatBinOpSym Ge = ">="
formatBinOpSym BitAnd = "&"; formatBinOpSym BitOr = "|"; formatBinOpSym BitXor = "^"
formatBinOpSym LShift = "<<"; formatBinOpSym RShift = ">>"

formatUnOpSym :: UnOp -> Text
formatUnOpSym Neg = "-"; formatUnOpSym Not = "!"; formatUnOpSym BitNot = "~"

formatComment :: FormatConfig -> Int -> Comment -> Text
formatComment cfg i (LineComment InlineComment txt ind) =
  T.replicate ind " " <> ";" <> spaces (spaceAfterCommentDelimiter cfg) <> txt
formatComment cfg i (LineComment RegularComment txt ind) =
  T.replicate ind " " <> ";;" <> spaces (spaceAfterCommentDelimiter cfg) <> txt
formatComment cfg i (LineComment (HeadingComment lvl s txt) _ ind) =
  let sem = T.replicate (lvl + 2) ";"
      todo = case s of NoTodo -> ""; Todo -> "TODO "; Done -> "DONE "
  in T.replicate ind " " <> sem <> " " <> todo <> txt
formatComment cfg i (LineComment (CheckboxComment checked txt) _ ind) =
  let check = if checked then "[X]" else "[ ]"
  in T.replicate ind " " <> ";;" <> spaces (spaceAfterCommentDelimiter cfg) <> "- " <> check <> " " <> txt
formatComment cfg i (LineComment (ClosedComment ts) _ ind) =
  T.replicate ind " " <> ";;" <> spaces (spaceAfterCommentDelimiter cfg) <> "CLOSED: " <> ts
formatComment cfg i (BlockComment content ind) =
  T.replicate ind " " <> ";*" <> content <> "*;"

postProcess :: FormatConfig -> Text -> Text
postProcess cfg txt =
  let withBlankLines = collapseBlankLines cfg txt
      trimmed = if trimTrailingWhitespace cfg then trimTrailing withBlankLines else withBlankLines
      withEOF = if ensureNewlineAtEOF cfg && not (T.null trimmed) && not ("\n" `T.isSuffixOf` trimmed) then trimmed <> "\n" else trimmed
  in withEOF

collapseBlankLines :: FormatConfig -> Text -> Text
collapseBlankLines cfg txt
  | not (preserveBlankLines cfg) =
      let lns = T.lines txt
          collapsed = go lns
      in T.unlines collapsed
  | otherwise = txt
  where
    go [] = []
    go (l:ls) =
      let (blanks, rest) = span T.null ls
          keep = min (length blanks) (maxConsecutiveBlankLines cfg)
      in if T.null l then replicate keep "" ++ go rest else l : go ls

trimTrailing :: Text -> Text
trimTrailing = T.unlines . map T.stripEnd . T.lines

makeIndent :: FormatConfig -> Int -> Text
makeIndent cfg lvl = if useTabs cfg then T.replicate lvl "\t" else T.replicate (lvl * indentSize cfg) " "

spaces :: Int -> Text
spaces n = T.replicate n " "

escapeString :: Text -> Text
escapeString = T.replace "\\" "\\\\" . T.replace "\"" "\\\"" . T.replace "\n" "\\n"
