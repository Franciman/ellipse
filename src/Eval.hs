module Eval where

import qualified CoreSyntaxTree as C
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
type Subst = V.Vector C.Expr

idSubst = V.empty

-- Finally let us evaluate substitutions
evalSubst :: Subst -> C.Expr -> C.Expr
evalSubst subst expr
    -- we have nothing to do here
    | V.null subst = expr
    | otherwise    = evalSubst' subst expr

evalSubst' :: Subst -> C.Expr -> C.Expr
evalSubst' _ C.True = C.True
evalSubst' _ C.False = C.False
evalSubst' s (C.If cond tBranch fBranch) =
    C.If (evalSubst' s cond) (evalSubst' s tBranch) (evalSubst' s fBranch)

evalSubst' _ (C.IntLit n) = C.IntLit n
evalSubst' _ (C.FloatLit n) = C.FloatLit n
evalSubst' _ (C.StringLit n) = C.StringLit n

evalSubst' _ (C.FreeVar name) = C.FreeVar name
evalSubst' s (C.App f a) = C.App (evalSubst' s f) (evalSubst' s a)

evalSubst' s (C.BoundVar name n)
    | n < V.length s  = s V.! n
    | otherwise = C.BoundVar name n

evalSubst' s (C.Abs v ty term) =
    -- We enlarge the substitution context, because the 0 parameter is not free, so it must not be substituted,
    -- if not by itself
    let s' = V.cons (C.BoundVar v 0) s
    in C.Abs v ty (evalSubst' s' term)


-- Since we allow for definitions, we must also keep an environment of the available definitions
-- This separation is necessary because we work with locally nameless representation, so free variables
-- are named, are represented as strings.
type Env = M.Map String C.Expr

-- eval implements the call-by-value evaluation strategy for function application
eval :: Env -> Subst -> C.Expr -> C.Expr
eval _ _ C.True = C.True
eval _ _ C.False = C.False
eval e s (C.If cond tBranch fBranch) =
    case eval e s cond of
        C.True  -> eval e s tBranch
        C.False -> eval e s fBranch

eval _ _ (C.IntLit n)    = C.IntLit n
eval _ _ (C.FloatLit n)  = C.FloatLit n
eval _ _ (C.StringLit n) = C.StringLit n

eval e s (C.Let name def body) =
    let newEnv = M.insert name def e
    in eval newEnv s body

eval e s (C.FreeVar name) =
    case M.lookup name e of
        Nothing   -> error $ "Unbound name: " ++ name
        Just exp  -> eval e s exp

eval e s v@(C.BoundVar _ n) = eval e idSubst (evalSubst s v)

eval e s f@(C.Abs _ _ _) = eval e idSubst (evalSubst s f)

eval e s (C.App f a) =
    -- In this case we don't want to perform substitutions, they will be performed later,
    -- when we run beta reduction, so we pass an empty substitution context
    case eval e idSubst f of
        (C.Abs _ _ body) ->
            let a' = eval e s a
                s' = V.cons a' s
            in eval e s' body
        _ -> error $ "Invalid type of left argument in application, it should be a function"


