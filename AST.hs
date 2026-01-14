{-# LANGUAGE DeriveGeneric #-}
module AST where

import Data.Text (Text)
import GHC.Generics (Generic)

--- Comments

data Comment
  = LineComment CommentType Text Int  -- Added indentation level
  | BlockComment Text Int  -- Added indentation level
  deriving (Show, Eq, Generic)

data CommentType
  = InlineComment        -- ; single semicolon
  | RegularComment       -- ;; double semicolon
  | HeadingComment Int TodoState Text  -- ;;; H1, ;;;; H2, etc.
  | CheckboxComment Bool Text  -- ;; - [ ] or ;; - [X]
  | ClosedComment Text   -- ;;CLOSED: [date]
  deriving (Show, Eq, Generic)

data TodoState
  = NoTodo
  | Todo
  | Done
  deriving (Show, Eq, Generic)

--- Top-level declarations

data Decl
  = UseDecl Text (Maybe Text)
  | DefineDecl [Text] Expr
  | AssignDecl Expr Expr
  | CompoundAssignDecl Expr CompoundOp Expr
  | FnDecl Text [Param] (Maybe Text) (Maybe DocString) [Stmt]
  | LayoutDecl Text [LayoutField]
  | ExprDecl Expr
  | CommentDecl Comment  -- Comments at top level
  deriving (Show, Eq, Generic)

type DocString = Text

data LayoutField = LayoutField
  { fieldName :: Text
  , fieldType :: Text
  , fieldOffset :: Int
  } deriving (Show, Eq, Generic)

data Param = Param
  { paramName :: Text
  , paramType :: Maybe Text
  , paramDefault :: Maybe Expr
  } deriving (Show, Eq, Generic)

--- Statements

data Stmt
  = ExprStmt Expr
  | VarDecl [Text] Expr
  | Assign Expr Expr
  | CompoundAssign Expr CompoundOp Expr
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | ForStmt Text Expr [Stmt]
  | TryStmt [Stmt] (Maybe Text) [Stmt]
  | ReturnStmt (Maybe Expr)
  | BreakStmt
  | ContinueStmt
  | GotoStmt Text
  | LabelStmt Text
  | DeferStmt [Stmt]
  | MatchStmt Expr [MatchArm] (Maybe [Stmt])
  | BlockStmt [Stmt]
  | CommentStmt Comment  -- Comments in statement position
  deriving (Show, Eq, Generic)

data CompoundOp
  = AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | AndAssign | OrAssign | XorAssign | LShiftAssign | RShiftAssign
  deriving (Show, Eq, Generic)

data MatchArm = MatchArm
  { matchPattern :: Expr
  , matchConseq :: [Stmt]
  } deriving (Show, Eq, Generic)

--- Expressions

data Expr
  = Var Text
  | IntLit Integer
  | FloatLit Double
  | StrLit Text
  | BoolLit Bool
  | ListLit [Expr]
  | TupleLit [Expr]
  | SetLit [Expr]
  | DictLit [(Expr, Expr)]
  | Call Expr [CallArg]
  | MemberCall Expr Text [CallArg]
  | InferredMember Text
  | Index Expr (Maybe Expr) (Maybe Expr) (Maybe Expr)
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | LogicalOp LogicalOp Expr Expr
  | Lambda [Param] (Maybe Text) [Stmt] Bool
  | FnExpr [Param] (Maybe Text) [Stmt] Bool
  | FString [FStringPart]
  | AsmExpr Text Text [Expr]
  | EmbedExpr Text
  | ComptimeExpr [Stmt]
  deriving (Show, Eq, Generic)

data FStringPart
  = FStrText Text
  | FStrExpr Expr
  deriving (Show, Eq, Generic)

data CallArg = CallArg
  { argName :: Maybe Text
  , argValue :: Expr
  } deriving (Show, Eq, Generic)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Le  | Gt | Ge
  | BitAnd | BitOr | BitXor | LShift | RShift
  deriving (Show, Eq, Generic)

data LogicalOp = And | Or
  deriving (Show, Eq, Generic)

data UnOp = Neg | Not | BitNot
  deriving (Show, Eq, Generic)
