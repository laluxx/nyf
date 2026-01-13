{-# LANGUAGE DeriveGeneric #-}
module AST where

import Data.Text (Text)
import GHC.Generics (Generic)

--- Top-level declarations

data Decl
  = UseDecl Text (Maybe Text)      -- use std.core (as alias)?
  | DefineDecl Text Expr           -- define x = expr
  | FnDecl Text [Param] (Maybe Text) (Maybe DocString) [Stmt]  -- return type
  | LayoutDecl Text [LayoutField]  -- layout Name { fields }
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
  | VarDecl [Text] Expr                 -- def x = expr or x, y = expr
  | Assign Expr Expr                    -- x = expr (lhs can be index/member)
  | CompoundAssign Expr CompoundOp Expr -- x += expr
  | IfStmt Expr [Stmt] (Maybe [Stmt])   -- if (cond) {..} else {..}
  | WhileStmt Expr [Stmt]
  | ForStmt Text Expr [Stmt]            -- for x in iterable {..}
  | TryStmt [Stmt] (Maybe Text) [Stmt]  -- try {..} catch (e) {..}
  | ReturnStmt (Maybe Expr)
  | BreakStmt
  | ContinueStmt
  | GotoStmt Text
  | LabelStmt Text
  | DeferStmt [Stmt]
  | MatchStmt Expr [MatchArm] (Maybe [Stmt])  -- match expr { arms } + default
  | BlockStmt [Stmt]
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
  | MemberCall Expr Text [CallArg]           -- obj.method(args)
  | InferredMember Text                      -- .field (for enum-like access)
  | Index Expr (Maybe Expr) (Maybe Expr) (Maybe Expr)  -- target[start:stop:step]
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  | LogicalOp LogicalOp Expr Expr
  | Lambda [Param] (Maybe Text) [Stmt] Bool  -- params, return type, body, is_variadic
  | FnExpr [Param] (Maybe Text) [Stmt] Bool  -- inline fn expression
  | FString [FStringPart]                    -- f"hello {x}"
  | AsmExpr Text Text [Expr]                 -- asm("code", "constraints", args)
  | EmbedExpr Text                           -- embed("file.txt")
  | ComptimeExpr [Stmt]                      -- comptime { ... }
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
