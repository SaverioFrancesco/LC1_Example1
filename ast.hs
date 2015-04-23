-- Haskell module generated by the BNF converter
--
-- Adapted by Marco Comini

module AbstractSyntaxTree where

import Text.PrettyPrint

newtype Ident = Ident String deriving (Eq,Ord,Show)

-- instance PrPrint Ident where
--   prettyDoc (Ident str) = text str

type FunIdent = Ident

newtype Program = Prog [Decl]
  deriving (Eq,Ord,Show)

data Decl
  = Dvar TypeSpec Ident (Maybe RExpr)
  | Dfun TypeSpec Ident [FormalParam] Stmt
   deriving (Eq,Ord,Show)

type VarDecl = (Ident,Maybe RExpr)

data FormalParam = FormalParam Modality TypeSpec Ident
  deriving (Eq,Ord,Show)

-- instance PrPrint FormalParameter where
--   prettyDoc (FormalParameter m t x) = prettyDoc m <+> prettyDoc t <+> prettyDoc x


data TypeSpec
  = BasicType BasicType
  | ArrayType (Maybe RExpr) TypeSpec
--  | RefType TypeSpec
--  | FunType [ModTypeSpec] TypeSpec
  deriving (Eq,Ord,Show)

-- instance PrPrint TypeSpec
--  where
--   prettyDoc = pd
--    where
--     pd (BasicType bt)    = prettyDoc bt
--     pd (Array n t)       = pd t <+> brackets (int n)
--     pd (RefType t)       = pd t <> text "*"
--     pd (FunType mts t)   = parensEmpty prettyDoc mts <+> text "->" <+> pd t
--     pd x = niyError "instance PrPrint TypeSpec" x
-- 
--     -- parensNonEmpty _ [] = error $ "internal error: empty tuple type should not exist."
--     -- parensNonEmpty fpd ts = parens $ prtNonEmpty fpd ts
-- 
--     prtNonEmpty fpd [x] = fpd x
--     prtNonEmpty fpd (x:xs) = fpd x <> comma <+> prtNonEmpty fpd xs
--     prtNonEmpty _ [] = error "instance PrPrint TypeSpec prtNonEmpty: the impossible happened"
-- 
--     parensEmpty _ [] = parens empty
--     parensEmpty fdp ts = parens $ prtNonEmpty fdp ts

data BasicType
 = BasicTypeBool
 | BasicTypeChar
 | BasicTypeFloat
 | BasicTypeInt
 | BasicTypeVoid
  deriving (Eq,Ord,Show)

-- instance PrPrint BasicType
--  where
--   prettyDoc = pd
--    where
--     pd (BasicTypeBool)   = text "bool"
--     pd (BasicTypeChar)   = text "char"
--     pd (BasicTypeFloat)  = text "float"
--     pd (BasicTypeInt)    = text "int"
--     pd (BasicTypeVoid)   = text "void"
--     pd x = niyError "instance PrPrint BasicType" x

-- isBasic :: TypeSpec -> Bool
-- isBasic (BasicType _) = True
-- isBasic _             = False

-- data ModTypeSpec = ModTypeSpec Modality TypeSpec
--   deriving (Eq,Ord,Show)

-- instance PrPrint ModTypeSpec
--  where
--   prettyDoc (ModTypeSpec m t) = prettyDoc m <+> prettyDoc t

data Modality = Value | Reference | Constant | Result | ValueResult | Name
  deriving (Eq,Ord,Show)

-- instance PrPrint Modality
--  where
--   prettyDoc = pd
--     where
--       pd Value = text "val"
--       pd ValueResult = text "valres"
--       pd Reference = text "ref"
--       pd Constant = text "const"
--       pd Result = text "res"
--       pd Name = text "name"
--       pd x = niyError "instance PrPrint Modality" x

data RExpr
 = InfixOp InfixOp RExpr RExpr
 | UnaryOp UnaryOp RExpr
 | FCall FunIdent [RExpr]
 | Const TypeSpec Const
 | LasRExpr LExpr
-- | Ref LExpr
-- | Coertion TypeSpec TypeSpec RExpr -- introduced by type checking
  deriving (Eq,Ord,Show)

data InfixOp = ArithOp ArithOp | RelOp RelOp | BoolOp BoolOp
  deriving (Eq,Ord,Show)

data ArithOp = Add | Sub | Mul | Mod | Pow | Div | IntDiv | FloatDiv
  deriving (Eq,Ord,Show)

data BoolOp = And | Or | Xor | Iff | Implies | IsImplied
  deriving (Eq,Ord,Show)

data RelOp = Eq | Neq | Lt | LtE | Gt | GtE
  deriving (Eq,Ord,Show)

data UnaryOp = Not | Neg
  deriving (Eq,Ord,Show)

data Const
 = Bool Bool
 | Int Int
 | Char Char
 | Float Double
 | Array [Const]
 | Null -- null pointer
  deriving (Eq,Ord,Show)


data LExpr
 = VarIdent Ident
 | ArrayElem LExpr RExpr
 | PrePostIncDecr PrePost IncDecr LExpr
-- | Deref RExpr
  deriving (Eq,Ord,Show)

data PrePost = Post | Pre
  deriving (Eq,Ord,Show)

data IncDecr = Inc | Decr
  deriving (Eq,Ord,Show)


data Stmt
 = Assgn LExpr AssignmentOp RExpr
 | LExprStmt LExpr
 | PCall FunIdent [RExpr]
 | Break
 | Continue
 | RetExp (Maybe RExpr)
 | If RExpr Stmt (Maybe Stmt)
 | For (Stmt,RExpr,Stmt) (Maybe Stmt)
 | While RExpr Stmt
 | Repeat Stmt RExpr
 | Block [Decl] [Stmt]
  deriving (Eq,Ord,Show)

data AssignmentOp = Assign | AssgnArith ArithOp | AssgnBool BoolOp
  deriving (Eq,Ord,Show)

-- class PrPrint a where
--   {- minimal complete definition -}
--   prettyDoc :: a -> Doc
-- 
--   {- defaulted methods -}
--   toString :: a -> String
--   toStringS :: a -> ShowS
-- 
--   {- defaults -}
--   toString = show . prettyDoc
--   toStringS = shows . prettyDoc

-- niyError :: Show a => String -> a -> err
-- niyError func x = error $ func++": case '"++show x++"' not implemented yet"
