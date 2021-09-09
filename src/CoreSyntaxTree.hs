module CoreSyntaxTree where

import Type

-- This is the AST of our Core language
-- Simply typed lambda calculus, with a let expression
-- and top level definitions
-- We use locally nameless representation of variables

data Expr = If Expr Expr Expr
           | True
           | False
           | IntLit Int
           | FloatLit Float
           | StringLit String
           | Let String Expr Expr
           | FreeVar String
           | BoundVar String Int
           -- ^ For bound vars we also keep the original name, so we can eventually reconstruct
           -- the named representation of the term
           | Abs String EType Expr
           -- ^ For abstractions we do the same, we keep the original name of the bound variable
           | App Expr Expr

-- A top level declaration
data Decl = Decl String Expr
