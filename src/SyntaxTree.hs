module SyntaxTree where

import qualified Data.Text as T
import qualified CoreSyntaxTree as Core
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

import Data.Foldable (foldl')
import Control.Arrow (first)

import Type

-- In this module we describe the AST of the high level representation of the language,
-- later we write a function to convert it to a CoreSyntaxTree object which can be evaluated easily.

data ELit = EBool Bool
          | EInt Int
          | EFloat Float
          | EString T.Text

data Expr = ECond [(Expr, Expr)] Expr
    | ELiteral ELit
    | ELet [Decl] Expr
    | EVar T.Text
    | ELambda [(T.Text, Type)] Expr
    | EApp Expr [Expr]

data Decl = Decl T.Text Expr
    | FunDecl T.Text [(T.Text, Type)] Expr

declName :: Decl -> T.Text
declName (Decl name _) = name
declName (FunDecl name _ _) = name

-- During compilation we need to convert to nameless form, therefore
-- we need an environment associating each name to their definition.
-- To do this, we have to get access to all the declarations in the program.
-- We associate an index to them, according to their order. Note that each
-- definition can depend on all the other, but they can't depend on themselves.

type NamingEnv  = M.Map T.Text Int
type BindingEnv = S.Seq T.Text

addBinding :: T.Text -> BindingEnv -> BindingEnv
addBinding name env = name S.<| env

addBindings :: [T.Text] -> BindingEnv -> BindingEnv
addBindings names env = foldl' (flip addBinding) env names

lookupBinding :: T.Text -> BindingEnv -> Maybe Int
lookupBinding = S.elemIndexL

-- Decls must have unique names
compile :: [Decl] -> [Core.Decl]
compile [] = []
compile ds =
    -- Let us first assign to each declaration an index and let us put them
    -- in the naming environment
    let namingEnv = M.fromList $ zipWith (\decl idx -> (declName decl, idx)) ds [0..]
    in map (compileDecl namingEnv S.empty) ds

compileDecl :: NamingEnv -> BindingEnv -> Decl -> Core.Decl
compileDecl env boundEnv (Decl name body) = Core.Decl name (compileExpr env boundEnv body)

-- We desugar this into a Decl whose definiens is a lambda
compileDecl env boundEnv (FunDecl name args body) =
    let desugared = Decl name (ELambda args body)
    in compileDecl env boundEnv desugared

compileExpr :: NamingEnv -> BindingEnv -> Expr -> Core.Expr

-- ECond is compiled in a list of Ifs testing the possibilities in order
compileExpr env boundEnv (ECond conds defaultCase) =
    let makeIf (cond, body) = Core.If (compileExpr env boundEnv cond) (compileExpr env boundEnv body)
    in foldr makeIf (compileExpr env boundEnv defaultCase) conds

compileExpr _ _ (ELiteral (EBool True))  = Core.True
compileExpr _ _ (ELiteral (EBool False)) = Core.False
compileExpr _ _ (ELiteral (EInt n))      = Core.IntLit n
compileExpr _ _ (ELiteral (EFloat n))    = Core.FloatLit n
compileExpr _ _ (ELiteral (EString n))    = Core.StringLit n

-- Right now this is inefficient. It compiles
-- it in a list of nested lets, maybe it is better to move
-- the support for multiple lets in the core, as we did for multiple applications
compileExpr env boundEnv (ELet decls body) =
    let makeLet d body = let (Core.Decl name def) = compileDecl env boundEnv d
                         in Core.Let name def body
        -- We need to bind all the definitions we introduced with the let
        newBoundEnv = addBindings (map declName decls) boundEnv
    in foldr makeLet (compileExpr env newBoundEnv body) decls

-- We first check the variable has been bound
compileExpr env boundEnv (EVar name) =
    case lookupBinding name boundEnv of
        Just idx -> Core.BoundVar name idx
        Nothing ->
            -- Otherwise we check for a global definition
            case M.lookup name env of
                Just idx -> Core.FreeVar name idx
                Nothing  -> error "Unknown variable"

compileExpr env boundEnv (ELambda args body) =
    let argNames = map fst args
        argTypes = map snd args
        newBoundEnv = addBindings argNames boundEnv
        compiledBody = compileExpr env newBoundEnv body
    in Core.Abs (S.fromList argNames) (S.fromList argTypes) compiledBody

compileExpr env boundEnv (EApp f as) =
    let compiledF  = compileExpr env boundEnv f
        compiledAs = map (compileExpr env boundEnv) as
    in Core.App compiledF (S.fromList compiledAs)
