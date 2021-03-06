{-# LANGUAGE DeriveGeneric #-}
module CoreSyntaxTree where
    
import Type

import qualified Data.Text as T

import GHC.Generics
import Control.DeepSeq


-- This is the AST of our Core language
-- Simply typed lambda calculus, with a let expression and y combinator.
-- and top level definitions
-- We use locally nameless representation of variables
-- with the additional quirk that free variables are indices too,
-- but in a different environment.
-- For reasons of efficiency we also represent functions and function application
-- with explicit support for multiple arguments.

-- We define some builtin operations for ease of use
data BuiltinOp = Sum | Sub | Prod | Div | And | Or | Not | LessThan | GreaterThan | Equal
    deriving(Show, Generic)

instance NFData BuiltinOp

data Expr = If Expr Expr Expr
           | BoolLit Bool
           | IntLit Int
           | FloatLit Float
           | StringLit T.Text
           | BuiltinOp BuiltinOp
           | Let T.Text Expr Expr
           | Fix Expr
           -- ^ This is the Y combinator, allowing recursion in our language, the body has a bound variable,
           -- which is itself lol.
           | FreeVar T.Text Int
           | BoundVar T.Text Int
           -- ^ For bound vars we also keep the original name, so we can eventually reconstruct
           -- the named representation of the term
           | Abs T.Text Type Expr
           -- ^ For abstractions we do the same, we keep the original name of the bound variables
           -- ^ Together with their types (necessary for typechecking)
           | App Expr Expr
           -- We want to optimize fully applied binary operations
           | BuiltinAp2 BuiltinOp Expr Expr
           deriving(Show, Generic)

instance NFData Expr

-- A top level declaration
data Decl = Decl T.Text Expr
    deriving(Show)

declName :: Decl -> T.Text
declName (Decl name _) = name
