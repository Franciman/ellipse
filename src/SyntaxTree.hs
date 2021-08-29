module SyntaxTree where

import Type

-- Simply typed lambda calculus, with a let expression
-- and top level definitions
-- We use locally nameless representation of variables

data EExpr = EIf EExpr EExpr EExpr
           | ETrue
           | EFalse
           | EIntLit Int
           | EFloatLit Float
           | EStringLit String
           | ELet String EExpr EExpr
           | EFreeVar String
           | EBoundVar String Int
           -- ^ For bound vars we also keep the original name
           | EAbs String EType EExpr
           -- ^ For abstractions we do the same, we keep the original name of the bound variable
           | EApp EExpr EExpr

-- A top level declaration
data EDecl = EDecl String EExpr
