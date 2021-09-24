module SyntaxTree where
    
import qualified Data.Text as T
import qualified CoreSyntaxTree as Core
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

import Data.Foldable (foldl')

import Type

-- In this module we describe the AST of the high level representation of the language,
-- later we write a function to convert it to a CoreSyntaxTree object which can be evaluated easily.

data ELit = EBool Bool
          | EInt Int
          | EFloat Float
          | EString T.Text
          deriving(Show)

data EOp = ESum | ESub | EProd | EDiv | EAnd | EOr | ENot | ELessThan | EGreaterThan | EEqual
    deriving(Show)

data Expr = ECond [(Expr, Expr)] Expr
    | ELiteral ELit
    | EBuiltinOp EOp
    | ELet [Decl] Expr
    | EVar T.Text
    | ELambda [(T.Text, Type)] Expr
    | EFix Expr
    | EApp Expr [Expr]
    deriving(Show)

data Decl = Decl T.Text Expr
    | FunDecl T.Text [(T.Text, Type)] Expr
    deriving(Show)

declName :: Decl -> T.Text
declName (Decl name _) = name
declName (FunDecl name _ _) = name

-- During compilation we need to convert to nameless form, therefore
-- we need an environment associating each name to their definition.
-- To do this, we have to get access to all the declarations in the program.
-- We associate an index to them, according to their order. Note that each
-- definition can depend on all the other, but they can't depend on themselves.
-- We also return the environment to use for free variables.

type NamingEnv  = M.Map T.Text Int
type BindingEnv = S.Seq T.Text

addBinding :: T.Text -> BindingEnv -> BindingEnv
addBinding name env = name S.<| env

addBindings :: [T.Text] -> BindingEnv -> BindingEnv
addBindings names env = foldl' (flip addBinding) env names

lookupBinding :: T.Text -> BindingEnv -> Maybe Int
lookupBinding = S.elemIndexL

-- Decls must have unique names
-- Each decl can only refer to the previous ones.
-- The result is a list of core decls, ordered so that, each free variable with index i,
-- refers to the i-th element of the list
compile :: [Decl] -> [Core.Decl]
compile = compile' M.empty

compile' :: NamingEnv -> [Decl] -> [Core.Decl]
compile' _ [] = []
compile' accessibleDefs (d:ds) =
    -- We first compile this definition considering only accessible definitions
    let coreDecl = compileDecl accessibleDefs S.empty d
    -- then we add this definition to the list of accessible ones, by giving it
    -- the next index available (remember that we use consecutive integers,
    -- so this index is given by the size of the accessibleDefs map)
        newAccessibleDefs = M.insert (declName d) (M.size accessibleDefs) accessibleDefs
    in coreDecl : compile' newAccessibleDefs ds

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

compileExpr _ _ (ELiteral (EBool b))  = Core.BoolLit b
compileExpr _ _ (ELiteral (EInt n))      = Core.IntLit n
compileExpr _ _ (ELiteral (EFloat n))    = Core.FloatLit n
compileExpr _ _ (ELiteral (EString n))   = Core.StringLit n

compileExpr _ _ (EBuiltinOp ESum)         = Core.BuiltinOp Core.Sum
compileExpr _ _ (EBuiltinOp ESub)         = Core.BuiltinOp Core.Sub
compileExpr _ _ (EBuiltinOp EProd)        = Core.BuiltinOp Core.Prod
compileExpr _ _ (EBuiltinOp EDiv)         = Core.BuiltinOp Core.Div
compileExpr _ _ (EBuiltinOp EAnd)         = Core.BuiltinOp Core.And
compileExpr _ _ (EBuiltinOp EOr)          = Core.BuiltinOp Core.Or
compileExpr _ _ (EBuiltinOp ENot)         = Core.BuiltinOp Core.Not
compileExpr _ _ (EBuiltinOp ELessThan)    = Core.BuiltinOp Core.LessThan
compileExpr _ _ (EBuiltinOp EGreaterThan) = Core.BuiltinOp Core.GreaterThan
compileExpr _ _ (EBuiltinOp EEqual)       = Core.BuiltinOp Core.Equal

-- Right now this is inefficient. It compiles
-- it in a list of nested lets, maybe it is better to move
-- the support for multiple lets in the core, as we did for multiple applications
compileExpr env boundEnv (ELet decls body) =
    let makeLet d b = let (Core.Decl name def) = compileDecl env boundEnv d
                         in Core.Let name def b
        -- We need to bind all the definitions we introduced with the let
        newBoundEnv = addBindings (map declName decls) boundEnv
    in foldr makeLet (compileExpr env newBoundEnv body) decls

compileExpr env boundEnv (EFix body) =
    Core.Fix (compileExpr env boundEnv body)

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
        newBoundEnv = addBindings argNames boundEnv
        compiledBody = compileExpr env newBoundEnv body
    in foldr (\(name, ty) b -> Core.Abs name ty b) compiledBody args

-- Here we try to spot builtins that are fully applied.
-- They are at most binary operations, so we check for binary ops
compileExpr env boundEnv (EApp f as) =
    case f of
        EBuiltinOp op | length as == 2 ->
            let Core.BuiltinOp compiledOp = compileExpr env boundEnv (EBuiltinOp op)
                compiledArg1 = compileExpr env boundEnv (head as)
                compiledArg2 = compileExpr env boundEnv (head (tail as))
            in Core.BuiltinAp2 compiledOp compiledArg1 compiledArg2
        _ ->
            let compiledF  = compileExpr env boundEnv f
                compiledAs = map (compileExpr env boundEnv) as
            in foldl' Core.App compiledF compiledAs
