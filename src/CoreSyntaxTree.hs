module CoreSyntaxTree where

import Type

import qualified Data.Sequence as S

import qualified Data.Text as T

-- This is the AST of our Core language
-- Simply typed lambda calculus, with a let expression
-- and top level definitions
-- We use locally nameless representation of variables
-- with the additional quirk that free variables are indices too,
-- but in a different environment.
-- For reasons of efficiency we also represent functions and function application
-- with explicit support for multiple arguments.

data Expr = If Expr Expr Expr
           | True
           | False
           | IntLit Int
           | FloatLit Float
           | StringLit T.Text
           | Let T.Text Expr Expr
           | FreeVar T.Text Int
           | BoundVar T.Text Int
           -- ^ For bound vars we also keep the original name, so we can eventually reconstruct
           -- the named representation of the term
           | Abs (S.Seq T.Text) (S.Seq EType) Expr
           -- ^ For abstractions we do the same, we keep the original name of the bound variables
           -- ^ Together with their types (necessary for typechecking)
           | App Expr (S.Seq Expr)

-- A top level declaration
data Decl = Decl T.Text Expr
