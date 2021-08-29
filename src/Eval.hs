module Eval where

import SyntaxTree
import Type

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- We define an interpreter for our language directly on SyntaxTree terms.
-- Since we allow definitions, how should we deal with them?
--
-- Our semantics is strongly based on substitution, so we go the explicit substitution way.
-- But instead of annotating terms with their substitution, we keep an explicit substitution context
-- as parameter of the evaluator.

-- Now we generate the free monoid over Subst, so we can have substitution append easily
-- The indices are automatically computed and shifted by the position in the list
type Subst = V.Vector EExpr

idSubst = V.empty

-- Finally let us evaluate substitutions
evalSubst :: Subst -> EExpr -> EExpr
evalSubst subst expr
    -- we have nothing to do here
    | V.null subst = expr
    | otherwise    = evalSubst' subst expr

evalSubst' :: Subst -> EExpr -> EExpr
evalSubst' _ ETrue = ETrue
evalSubst' _ EFalse = EFalse
evalSubst' s (EIf cond tBranch fBranch) =
    EIf (evalSubst' s cond) (evalSubst' s tBranch) (evalSubst' s fBranch)

evalSubst' _ (EIntLit n) = EIntLit n
evalSubst' _ (EFloatLit n) = EFloatLit n
evalSubst' _ (EStringLit n) = EStringLit n

evalSubst' _ (EFreeVar name) = EFreeVar name
evalSubst' s (EApp f a) = EApp (evalSubst' s f) (evalSubst' s a)

evalSubst' s (EBoundVar name n)
    | n < V.length s  = s V.! n
    | otherwise = EBoundVar name n

evalSubst' s (EAbs v ty term) =
    -- We enlarge the substitution context, because the 0 parameter is not free, so it must not be substituted,
    -- if not by itself
    let s' = V.cons (EBoundVar v 0) s
    in EAbs v ty (evalSubst' s' term)


-- Since we allow for definitions, we must also keep an environment of the available definitions
-- This separation is necessary because we work with locally nameless representation, so free variables
-- are named, are represented as strings.
type Env = M.Map String EExpr

-- eval implements the call-by-value evaluation strategy for function application
eval :: Env -> Subst -> EExpr -> EExpr
eval _ _ ETrue = ETrue
eval _ _ EFalse = EFalse
eval e s (EIf cond tBranch fBranch) =
    case eval e s cond of
        ETrue  -> eval e s tBranch
        EFalse -> eval e s fBranch

eval _ _ (EIntLit n)    = EIntLit n
eval _ _ (EFloatLit n)  = EFloatLit n
eval _ _ (EStringLit n) = EStringLit n

eval e s (ELet name def body) =
    let newEnv = M.insert name def e
    in eval newEnv s body

eval e s (EFreeVar name) =
    case M.lookup name e of
        Nothing   -> error $ "Unbound name: " ++ name
        Just exp  -> eval e s exp

eval e s v@(EBoundVar _ n) = eval e idSubst (evalSubst s v)

eval e s f@(EAbs _ _ _) = eval e idSubst (evalSubst s f)

eval e s (EApp f a) =
    -- In this case we don't want to perform substitutions, they will be performed later,
    -- when we run beta reduction, so we pass an empty substitution context
    case eval e idSubst f of
        (EAbs _ _ body) ->
            let a' = eval e s a
                s' = V.cons a' s
            in eval e s' body
        _ -> error $ "Invalid type of left argument in application, it should be a function"


