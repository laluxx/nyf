{-# LANGUAGE DeriveGeneric #-}
module AST where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)  -- ADD THIS IMPORT

--- Comments

data Comment
  = LineComment CommentType Text Int
  | BlockComment Text Int
  deriving (Show, Eq, Generic)

instance ToJSON Comment  -- ADD THIS

data CommentType
  = InlineComment
  | RegularComment
  | HeadingComment Int TodoState Text
  | CheckboxComment Bool Text
  | ClosedComment Text
  deriving (Show, Eq, Generic)

instance ToJSON CommentType  -- ADD THIS

data TodoState
  = NoTodo
  | Todo
  | Done
  deriving (Show, Eq, Generic)

instance ToJSON TodoState  -- ADD THIS

--- Top-level declarations

data Decl
  = UseDecl Text (Maybe Text)
  | DefineDecl [Text] Expr
  | AssignDecl Expr Expr
  | CompoundAssignDecl Expr CompoundOp Expr
  | FnDecl Text [Param] (Maybe Text) (Maybe DocString) [Stmt]
  | LayoutDecl Text [LayoutField]
  | ExprDecl Expr
  | CommentDecl Comment
  deriving (Show, Eq, Generic)

instance ToJSON Decl  -- ADD THIS

type DocString = Text

data LayoutField = LayoutField
  { fieldName :: Text
  , fieldType :: Text
  , fieldOffset :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON LayoutField  -- ADD THIS

data Param = Param
  { paramName :: Text
  , paramType :: Maybe Text
  , paramDefault :: Maybe Expr
  } deriving (Show, Eq, Generic)

instance ToJSON Param  -- ADD THIS

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
  | CommentStmt Comment
  deriving (Show, Eq, Generic)

instance ToJSON Stmt  -- ADD THIS

data CompoundOp
  = AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | AndAssign | OrAssign | XorAssign | LShiftAssign | RShiftAssign
  deriving (Show, Eq, Generic)

instance ToJSON CompoundOp  -- ADD THIS

data MatchArm = MatchArm
  { matchPattern :: Expr
  , matchConseq :: [Stmt]
  } deriving (Show, Eq, Generic)

instance ToJSON MatchArm  -- ADD THIS

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

instance ToJSON Expr  -- ADD THIS

data FStringPart
  = FStrText Text
  | FStrExpr Expr
  deriving (Show, Eq, Generic)

instance ToJSON FStringPart  -- ADD THIS

data CallArg = CallArg
  { argName :: Maybe Text
  , argValue :: Expr
  } deriving (Show, Eq, Generic)

instance ToJSON CallArg  -- ADD THIS

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Le  | Gt | Ge
  | BitAnd | BitOr | BitXor | LShift | RShift
  deriving (Show, Eq, Generic)

instance ToJSON BinOp  -- ADD THIS

data LogicalOp = And | Or
  deriving (Show, Eq, Generic)

instance ToJSON LogicalOp  -- ADD THIS

data UnOp = Neg | Not | BitNot
  deriving (Show, Eq, Generic)

instance ToJSON UnOp  -- ADD THIS
