{-# LANGUAGE OverloadedStrings #-}
module Parser (file, expr, stmt, defStmt) where

import Control.Monad (void)
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AST
  ( Decl(..), Stmt(..), Expr(..), Param(..), LayoutField(..)
  , CallArg(..), MatchArm(..), FStringPart(..)
  , BinOp(..), LogicalOp(..), UnOp(..), CompoundOp(..)
  , DocString
  )

type Parser = Parsec Void Text

--- Whitespace and comments

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";"
    blockComment = L.skipBlockComment ";*" "*;"

-- Space consumer that does NOT consume newlines
scn :: Parser ()
scn = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment
  where
    lineComment  = L.skipLineComment ";"
    blockComment = L.skipBlockComment ";*" "*;"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Lexeme that preserves newlines for statement separation

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

parens, brackets :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

-- Custom braces that handles statement blocks properly
braces :: Parser a -> Parser a
braces p = do
  symbol "{"
  result <- p
  symbol "}"
  return result

-- Statement terminator: semicolon or newline
stmtSep :: Parser ()
stmtSep = void (symbol ";") <|> void (some (char '\n' <* scn)) <|> lookAhead (void (char '}'))

-- Optional statement terminator
optStmtSep :: Parser ()
optStmtSep = void (optional stmtSep)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = do
      f <- optional op
      case f of
        Nothing -> return x
        Just f' -> do
          y <- p
          rest (f' x y)

--- Keywords

keywords :: [Text]
keywords =
  [ "fn", "define", "use", "if", "else", "elif", "while", "return"
  , "for", "in", "try", "catch", "defer", "goto", "lambda"
  , "break", "continue", "match", "layout", "comptime"
  , "true", "false", "as", "asm", "embed", "def"
  ]

isKeyword :: Text -> Bool
isKeyword = (`elem` keywords)

identifier :: Parser Text
identifier = try $ lexeme (p >>= check)
  where
    p = do
      chars <- takeWhile1P Nothing isIdentChar
      if T.all isOperatorChar chars
        then fail "not an identifier"
        else return chars

    -- Allow '-' and '>' separately so they can form '->'
    -- But NOT '<' which should only be an operator
    isIdentChar c = isAlpha c || isDigit c || c `elem` ("_-?!$>/+*^~@#%" :: String)
    isOperatorChar c = c `elem` ("-+*/>!^~%" :: String)

    check x = if isKeyword x
              then fail $ "keyword cannot be identifier: " ++ T.unpack x
              else return x

identifierList :: Parser [Text]
identifierList = do
  first <- identifier
  -- Look ahead to see if there's a comma or another identifier
  choice
    [ do
        symbol ","
        rest <- identifier `sepBy1` symbol ","
        return (first : rest)
    , do
        rest <- many identifier
        return (first : rest)
    ]

--- Literals

integer :: Parser Integer
integer = lexeme $ try hexadecimal <|> L.decimal
  where
    hexadecimal = do
      _ <- string "0x" <|> string "0X"
      L.hexadecimal

float :: Parser Double
float = lexeme $ try L.float <|> (fromIntegral <$> (L.decimal :: Parser Integer))

stringLiteral :: Parser Text
stringLiteral = lexeme $ T.pack <$> stringBody
  where
    stringBody = choice
      [ tripleQuoted '"'
      , tripleQuoted '\''
      , singleQuoted '"'
      , singleQuoted '\''
      ]

    tripleQuoted q = try $ do
      _ <- count 3 (char q)
      manyTill L.charLiteral (try $ count 3 (char q))

    singleQuoted q = do
      _ <- char q
      manyTill L.charLiteral (char q)

fstringLiteral :: Parser [FStringPart]
fstringLiteral = lexeme $ do
  _ <- char 'f'
  q <- char '"' <|> char '\''
  isTriple <- option False (try $ count 2 (char q) >> return True)
  let endQuote = if isTriple then void (count 3 (char q)) else void (char q)
  manyTill (fstringPart q) (try endQuote)
  where
    fstringPart q = try interpolation <|> textPart q

    interpolation = do
      _ <- char '{'
      exprText <- manyTill anySingle (char '}')
      case parse expr "" (T.pack exprText) of
        Left _ -> fail "invalid f-string expression"
        Right e -> return $ FStrExpr e

    textPart q = do
      txt <- some $ noneOf ['{', q]
      return $ FStrText (T.pack txt)

--- Top-level

exprOrAssignDecl :: Parser Decl
exprOrAssignDecl = try assignmentDecl <|> pureExprDecl

assignmentDecl :: Parser Decl
assignmentDecl = do
  lhs <- term >>= postfix
  choice
    [ do
        op <- compoundAssignOp
        rhs <- expr
        optStmtSep
        return $ CompoundAssignDecl lhs op rhs
    , do
        symbol "="
        rhs <- expr
        optStmtSep
        return $ AssignDecl lhs rhs
    ]

pureExprDecl :: Parser Decl
pureExprDecl = do
  e <- expr
  optStmtSep
  return $ ExprDecl e

compoundAssignOp :: Parser CompoundOp
compoundAssignOp = choice
  [ try (symbol "+=" $> AddAssign)
  , try (symbol "-=" $> SubAssign)
  , try (symbol "*=" $> MulAssign)
  , try (symbol "/=" $> DivAssign)
  , try (symbol "%=" $> ModAssign)
  , try (symbol "&=" $> AndAssign)
  , try (symbol "|=" $> OrAssign)
  , try (symbol "^=" $> XorAssign)
  , try (symbol "<<=" $> LShiftAssign)
  , try (symbol ">>=" $> RShiftAssign)
  ]

file :: Parser [Decl]
file = sc *> many (topLevel <* sc) <* eof  -- Add 'sc' after each topLevel
  where
    topLevel = choice
      [ try useAsDecl
      , try defineDecl
      , try layoutDecl
      , try fnDecl
      , defError
      , exprOrAssignDecl
      ]

    defError = do
      try (symbol "def")
      fail "Cannot use 'def' in global scope. Use 'define' for global variables instead."

useAsDecl :: Parser Decl
useAsDecl = do
  symbol "use"
  parts <- identifier `sepBy1` symbol "."
  let path = T.intercalate "." parts
  alias <- optional $ symbol "as" >> identifier
  optStmtSep
  return $ UseDecl path alias

defineDecl :: Parser Decl
defineDecl = do
     symbol "define"
     names <- identifierList
     symbol "="
     e <- expr
     optStmtSep
     return $ DefineDecl names e

layoutDecl :: Parser Decl
layoutDecl = do
  symbol "layout"
  name <- identifier
  fields <- braces $ layoutField `sepEndBy` optional (symbol ",")
  return $ LayoutDecl name fields
  where
    layoutField = do
      fname <- identifier
      symbol ":"
      ftype <- identifier
      return $ LayoutField fname ftype 0

fnDecl :: Parser Decl
fnDecl = do
  symbol "fn"
  name <- identifier
  params <- parens (param `sepBy` symbol ",")
  retType <- optional (symbol ":" >> identifier)
  bodyStmts <- braces (many stmt)
  let (doc, rest) = extractDocString bodyStmts
  return $ FnDecl name params retType doc rest
  where
    param = do
      isVariadic <- option False (try (symbol "..." $> True))
      if isVariadic
        then do
          pname <- identifier
          -- Variadic params can't have type annotations or defaults
          return $ Param ("..." <> pname) Nothing Nothing
        else do
          pname <- identifier
          ptype <- optional (symbol ":" >> identifier)
          pdef <- optional (symbol "=" >> expr)
          return $ Param pname ptype pdef

extractDocString :: [Stmt] -> (Maybe DocString, [Stmt])
extractDocString (ExprStmt (StrLit s) : rest) = (Just s, rest)
extractDocString xs = (Nothing, xs)

stmt :: Parser Stmt
stmt = sc *> choice  -- Add this 'sc *>' to consume leading whitespace
  [ try ifStmt
  , try whileStmt
  , try forStmt
  , try tryStmt
  , try matchStmt
  , try returnStmt
  , try breakStmt
  , try continueStmt
  , try gotoStmt
  , try deferStmt
  , try labelStmt
  , try defStmt
  , try fnStmtDecl
  , try layoutStmtDecl
  , try useStmtDecl
  , try assignOrExprStmt
  , blockStmt
  ]

useStmtDecl :: Parser Stmt
useStmtDecl = do
  _ <- useAsDecl
  return $ ExprStmt (Var "use")

layoutStmtDecl :: Parser Stmt
layoutStmtDecl = do
  LayoutDecl name _fields <- layoutDecl
  return $ ExprStmt (StrLit $ "layout " <> name)

fnStmtDecl :: Parser Stmt
fnStmtDecl = do
  FnDecl name _params _retType _doc _body <- fnDecl
  return $ ExprStmt (StrLit $ "fn " <> name)

defStmt :: Parser Stmt
defStmt = do
     symbol "def"
     names <- identifierList
     symbol "="
     e <- expr
     optStmtSep
     return $ VarDecl names e

assignOrExprStmt :: Parser Stmt
assignOrExprStmt = try assignStmt <|> exprStmt
  where
    assignStmt = do
      lhs <- term >>= postfix
      choice
        [ do
            op <- compoundAssignOp
            rhs <- expr
            optStmtSep
            return $ CompoundAssign lhs op rhs
        , do
            symbol "="
            rhs <- expr
            optStmtSep
            return $ Assign lhs rhs
        ]

    exprStmt = do
      e <- expr
      optStmtSep
      return $ ExprStmt e

    compoundAssignOp = choice
      [ try (symbol "+=" $> AddAssign)
      , try (symbol "-=" $> SubAssign)
      , try (symbol "*=" $> MulAssign)
      , try (symbol "/=" $> DivAssign)
      , try (symbol "%=" $> ModAssign)
      , try (symbol "&=" $> AndAssign)
      , try (symbol "|=" $> OrAssign)
      , try (symbol "^=" $> XorAssign)
      , try (symbol "<<=" $> LShiftAssign)
      , try (symbol ">>=" $> RShiftAssign)
      ]

ifStmt :: Parser Stmt
ifStmt = do
  symbol "if"
  cond <- parens expr <|> expr
  thenBranch <- braces (many stmt)
  elseBranch <- optional elseOrElif
  return $ IfStmt cond thenBranch elseBranch
  where
    elseOrElif = choice
      [ try $ do
          symbol "elif"
          elifCond <- parens expr <|> expr
          elifThen <- braces (many stmt)
          elifElse <- optional elseOrElif
          return [IfStmt elifCond elifThen elifElse]
      , symbol "else" >> braces (many stmt)
      ]

whileStmt :: Parser Stmt
whileStmt = do
  symbol "while"
  cond <- parens expr <|> expr
  body <- braces (many stmt)
  return $ WhileStmt cond body

forStmt :: Parser Stmt
forStmt = do
  symbol "for"
  (var, iterable) <-
    -- Try: for (var in iterable)
    try (parens $ do
      v <- identifier
      symbol "in"
      i <- expr
      return (v, i))
    -- Or: for var in iterable
    <|> do
      v <- identifier
      symbol "in"
      i <- expr
      return (v, i)
  body <- braces (many stmt)
  return $ ForStmt var iterable body

tryStmt :: Parser Stmt
tryStmt = do
  symbol "try"
  tryBody <- braces (many stmt)
  symbol "catch"
  errVar <- optional $ parens identifier <|> identifier
  catchBody <- braces (many stmt)
  return $ TryStmt tryBody errVar catchBody

returnStmt :: Parser Stmt
returnStmt = do
  symbol "return"
  e <- optional expr
  optStmtSep
  return $ ReturnStmt e

breakStmt :: Parser Stmt
breakStmt = symbol "break" >> optStmtSep >> return BreakStmt

continueStmt :: Parser Stmt
continueStmt = symbol "continue" >> optStmtSep >> return ContinueStmt

gotoStmt :: Parser Stmt
gotoStmt = do
  symbol "goto"
  lbl <- identifier
  optStmtSep
  return $ GotoStmt lbl

labelStmt :: Parser Stmt
labelStmt = try $ do
  lbl <- identifier
  symbol ":"
  return $ LabelStmt lbl

deferStmt :: Parser Stmt
deferStmt = do
  symbol "defer"
  body <- braces (many stmt)
  return $ DeferStmt body

matchStmt :: Parser Stmt
matchStmt = do
  symbol "match"
  test <- expr
  symbol "{"
  arms <- many matchArm
  defaultCase <- optional $ do
    symbol "else"
    braces (many stmt)
  symbol "}"
  return $ MatchStmt test arms defaultCase
  where
    matchArm = do
      pattern <- expr
      void $ optional (symbol ":")
      conseq <- braces (many stmt)
      return $ MatchArm pattern conseq

blockStmt :: Parser Stmt
blockStmt = BlockStmt <$> braces (many stmt)

expr :: Parser Expr
expr = opOr

symbolOp :: Text -> Parser ()
symbolOp op = try $ do
  _ <- string op
  sc
  return ()

opOr :: Parser Expr
opOr = chainl1 opAnd (symbolOp "||" $> LogicalOp Or)

opAnd :: Parser Expr
opAnd = chainl1 opEq (symbolOp "&&" $> LogicalOp And)

opEq :: Parser Expr
opEq = chainl1 opRel equalityOp
  where
    equalityOp = choice
      [ try (symbol "==" $> BinOp Eq)
      , try (symbol "!=" $> BinOp Neq)
      ]

opRel :: Parser Expr
opRel = chainl1 opBit relOp
  where
    relOp = choice
      [ try (symbol "<=" $> BinOp Le)
      , try (symbol ">=" $> BinOp Ge)
      , try (symbol "<" $> BinOp Lt)
      , try (symbol ">" $> BinOp Gt)
      ]

opBit :: Parser Expr
opBit = chainl1 opAdd bitOp
  where
    bitOp = choice
      [ try (symbol "<<" $> BinOp LShift)
      , try (symbol ">>" $> BinOp RShift)
      , try (symbol "&" <* notFollowedBy (char '&') $> BinOp BitAnd)
      , try (symbol "|" <* notFollowedBy (char '|') $> BinOp BitOr)
      , try (symbol "^" $> BinOp BitXor)
      ]

opAdd :: Parser Expr
opAdd = chainl1 opMul addOp
  where
    addOp = choice
      [ try (symbol "+" $> BinOp Add)
      , try (symbol "-" $> BinOp Sub)
      ]

opMul :: Parser Expr
opMul = chainl1 unary mulOp
  where
    mulOp = choice
      [ try (symbol "*" $> BinOp Mul)
      , try (symbol "/" $> BinOp Div)
      , try (symbol "%" $> BinOp Mod)
      ]

unary :: Parser Expr
unary = choice
  [ try (symbol "-" >> UnOp Neg <$> unary)
  , try (symbol "!" >> UnOp Not <$> unary)
  , try (symbol "~" >> UnOp BitNot <$> unary)
  , term >>= postfix
  ]

term :: Parser Expr
term = choice
  [ try (FString <$> fstringLiteral)
  , try (FloatLit <$> float)
  , try (IntLit <$> integer)
  , try (BoolLit True <$ symbol "true")
  , try (BoolLit False <$ symbol "false")
  , try (StrLit <$> stringLiteral)
  , try inferredMember
  , try asmExpr
  , try embedExpr
  , try lambdaExpr
  , try fnExpr
  , try (parens tupleOrExpr)
  , try dictOrSet
  , try listLit
  , Var <$> identifier
  ]

inferredMember :: Parser Expr
inferredMember = do
  symbol "."
  InferredMember <$> identifier

asmExpr :: Parser Expr
asmExpr = do
  symbol "asm"
  parens $ do
    code <- stringLiteral
    constraints <- option "" (symbol "," >> stringLiteral)
    args <- many (symbol "," >> expr)
    return $ AsmExpr code constraints args

embedExpr :: Parser Expr
embedExpr = do
  symbol "embed"
  path <- parens stringLiteral
  return $ EmbedExpr path

comptimeExpr :: Parser Expr
comptimeExpr = do
  symbol "comptime"
  body <- braces (many stmt)
  return $ ComptimeExpr body

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "lambda"
  (params, isVariadic) <- parens paramList
  retType <- optional (symbol ":" >> identifier)
  body <- braces (many stmt)
  return $ Lambda params retType body isVariadic

fnExpr :: Parser Expr
fnExpr = do
  symbol "fn"
  (params, isVariadic) <- parens paramList
  retType <- optional (symbol ":" >> identifier)
  body <- braces (many stmt)
  return $ FnExpr params retType body isVariadic

paramList :: Parser ([Param], Bool)
paramList = do
  params <- param `sepBy` symbol ","
  isVariadic <- option False (symbol "..." $> True)
  return (params, isVariadic)
  where
    param = do
      pname <- identifier
      ptype <- optional (symbol ":" >> identifier)
      pdef <- optional (symbol "=" >> expr)
      return $ Param pname ptype pdef

tupleOrExpr :: Parser Expr
tupleOrExpr = do
  first <- optional expr
  case first of
    Nothing -> return $ TupleLit []
    Just e -> do
      rest <- many (symbol "," >> expr)
      if null rest
        then return e
        else return $ TupleLit (e : rest)

dictOrSet :: Parser Expr
dictOrSet = braces $ do
  first <- optional expr
  case first of
    Nothing -> return $ SetLit []
    Just e -> choice
      [ do
          symbol ":"
          v <- expr
          rest <- many (symbol "," >> dictPair)
          return $ DictLit ((e, v) : rest)
      , do
          rest <- many (symbol "," >> expr)
          return $ SetLit (e : rest)
      ]
  where
    dictPair = do
      k <- expr
      symbol ":"
      v <- expr
      return (k, v)

listLit :: Parser Expr
listLit = ListLit <$> brackets (expr `sepBy` symbol ",")

postfix :: Expr -> Parser Expr
postfix e = choice
  [ do
      args <- parens (callArg `sepBy` symbol ",")
      postfix (Call e args)
  , do
      symbol "."
      member <- identifier
      choice
        [ do
            args <- parens (callArg `sepBy` symbol ",")
            postfix (MemberCall e member args)
        , postfix (MemberCall e member [])
        ]
  , do
      idx <- brackets indexExpr
      postfix idx
  , return e
  ]
  where
    callArg = try namedArg <|> positionalArg
    namedArg = do
      name <- identifier
      symbol "="
      CallArg (Just name) <$> expr
    positionalArg = CallArg Nothing <$> expr

    indexExpr = do
      start <- optional expr
      hasColon <- option False (symbol ":" $> True)
      if hasColon
        then do
          stop <- optional expr
          step <- optional (symbol ":" >> expr)
          return $ Index e start stop step
        else return $ Index e start Nothing Nothing
