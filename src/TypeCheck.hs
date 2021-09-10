module TypeCheck where

import qualified CoreSyntaxTree as C
import Type
import qualified Env as E

import qualified Data.Sequence as S

import Control.Monad (foldM)

type TypingEnv = E.Env EType

-- We use the Either monad semantics to deal with errors compositionally
type TypingError = String

type TypeCheck a = Either TypingError a

-- Unify two types, in our simple type system unification is just checking for equality
unifyTypes :: EType -> EType -> TypeCheck EType
unifyTypes t1 t2
    | t1 == t2  = Right t1
    | otherwise = Left "Type mismatch"

-- typeCheck has two environments as input, one for global definitions,
-- i.e. free variables in the term, and one for bound variables, occurring
-- when analyzing lambda terms bodies.
typeCheck :: TypingEnv -> TypingEnv -> C.Expr -> TypeCheck EType
typeCheck _ _ C.True =  return EBool
typeCheck _ _ C.False = return EBool
typeCheck d b (C.If cond tBranch fBranch) = do
    typeCheck d b cond >>= unifyTypes EBool
    tBranchType <- typeCheck d b tBranch
    fBranchType <- typeCheck d b fBranch
    unifyTypes tBranchType fBranchType

typeCheck _ _ (C.IntLit _)    = return EInt
typeCheck _ _ (C.FloatLit _)  = return EFloat
typeCheck _ _ (C.StringLit _) = return EString

typeCheck d b (C.Let name def body) = do
    defType <- typeCheck d b def
    let newBoundEnv = E.bind defType b
    typeCheck d newBoundEnv body

typeCheck d b (C.FreeVar _ index) =
    maybe (Left "Unbound variable") Right (E.lookup index d)

typeCheck d b (C.BoundVar _ index) =
    maybe (Left "Unbound variable") Right (E.lookup index b)


typeCheck d b (C.Abs _ tys body) = do
    let newBoundEnv = E.bindMany tys b
    codomainTy <- typeCheck d newBoundEnv body
    return (curryFunction tys codomainTy)

typeCheck d b (C.App f as) = do
        fTy <- typeCheck d b f
        asTy <- mapM (typeCheck d b) as
        foldM partialApplyTypeCheck fTy asTy

    where partialApplyTypeCheck currTy aTy =
            case currTy of
                EFunction dom cod -> do
                    unifyTypes dom aTy
                    return cod
                _  -> Left "Can't unify domain with argument"
