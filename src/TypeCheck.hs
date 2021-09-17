module TypeCheck where

import qualified CoreSyntaxTree as C
import Type
import qualified Env as E

import qualified Data.Sequence as S

import Control.Monad (foldM)

type TypingEnv = E.Env Type

-- We use the Either monad semantics to deal with errors compositionally
type TypingError = String

type TypeCheck a = Either TypingError a

-- Unify two types, in our simple type system unification is just checking for equality
unifyTypes :: Type -> Type -> TypeCheck Type
unifyTypes t1 t2
    | t1 == t2  = Right t1
    | otherwise = Left $ "Can't unify type: `" ++ show t1 ++ "` with type: `" ++ show t2 ++ "`"

-- typeCheck has two environments as input, one for global definitions,
-- i.e. free variables in the term, and one for bound variables, occurring
-- when analyzing lambda terms bodies.
typeCheck :: TypingEnv -> TypingEnv -> C.Expr -> TypeCheck Type
typeCheck _ _ (C.BoolLit _) =  return TBool
typeCheck d b (C.If cond tBranch fBranch) = do
    typeCheck d b cond >>= unifyTypes TBool
    tBranchType <- typeCheck d b tBranch
    fBranchType <- typeCheck d b fBranch
    unifyTypes tBranchType fBranchType

typeCheck _ _ (C.IntLit _)    = return TInt
typeCheck _ _ (C.FloatLit _)  = return TFloat
typeCheck _ _ (C.StringLit _) = return TString

typeCheck _ _ (C.BuiltinOp C.Sum) = return $ TFunction TInt (TFunction TInt TInt)
typeCheck _ _ (C.BuiltinOp C.Sub) = return $ TFunction TInt (TFunction TInt TInt)
typeCheck _ _ (C.BuiltinOp C.Prod) = return $ TFunction TInt (TFunction TInt TInt)
typeCheck _ _ (C.BuiltinOp C.Div) = return $ TFunction TFloat (TFunction TFloat TFloat)
typeCheck _ _ (C.BuiltinOp C.And) = return $ TFunction TBool (TFunction TBool TBool)
typeCheck _ _ (C.BuiltinOp C.Or) = return $ TFunction TBool (TFunction TBool TBool)
typeCheck _ _ (C.BuiltinOp C.Not) = return $ TFunction TBool TBool
typeCheck _ _ (C.BuiltinOp C.LessThan) = return $ TFunction TInt (TFunction TInt TBool)
typeCheck _ _ (C.BuiltinOp C.GreaterThan) = return $ TFunction TInt (TFunction TInt TBool)
typeCheck _ _ (C.BuiltinOp C.Equal) = return $ TFunction TInt (TFunction TInt TBool)

typeCheck d b (C.Let name def body) = do
    defType <- typeCheck d b def
    let newBoundEnv = E.bind defType b
    typeCheck d newBoundEnv body

typeCheck d b (C.Fix body) = do
    ty <- typeCheck d b body
    case ty of
        -- Since we must feed us to ourselves domain and codomain must be the same
        TFunction dom cod -> unifyTypes dom cod
        _ -> Left "Expected function as argument of fix"


typeCheck d b (C.FreeVar name index) =
    maybe (Left $ "Undefined variable: " ++ show name) Right (E.lookup index d)

typeCheck d b (C.BoundVar name index) =
    maybe (Left $ "Unbound variable: " ++ show name ++ " with index: " ++ show index)
          Right (E.lookup index b)


typeCheck d b (C.Abs _ ty body) = do
    let newBoundEnv = E.bind ty b
    codomainTy <- typeCheck d newBoundEnv body
    return (TFunction ty codomainTy)

typeCheck d b (C.App f a) = do
        fTy <- typeCheck d b f
        aTy <- typeCheck d b a
        partialApplyTypeCheck fTy aTy

    where partialApplyTypeCheck currTy aTy =
            case currTy of
                TFunction dom cod -> do
                    unifyTypes dom aTy
                    return cod
                t  -> Left $ "Expected function type, but got: " ++ show t
