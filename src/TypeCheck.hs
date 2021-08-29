module TypeCheck where

import SyntaxTree
import Type

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

type TypingEnv = M.Map String EType

type BoundVarEnv = V.Vector EType

-- We use the Maybe monad semantics to deal with errors compositionally

-- Unify two types, in our simple type system unification is just checking for equality
unifyTypes :: EType -> EType -> Maybe EType
unifyTypes t1 t2
    | t1 == t2  = Just t1
    | otherwise = Nothing

typeCheck :: TypingEnv -> BoundVarEnv -> EExpr -> Maybe EType
typeCheck _ _ ETrue =  return EBool
typeCheck _ _ EFalse = return EBool
typeCheck e b (EIf cond tBranch fBranch) = do
    typeCheck e b cond >>= unifyTypes EBool
    tBranchType <- typeCheck e b tBranch
    fBranchType <- typeCheck e b fBranch
    unifyTypes tBranchType fBranchType

typeCheck _ _ (EIntLit _) = Just EInt
typeCheck _ _ (EFloatLit _) = Just EFloat
typeCheck _ _ (EStringLit _) = Just EString

typeCheck e b (ELet name def body) = do
    defType <- typeCheck e b def
    let newEnv = M.insert name defType e
    typeCheck newEnv b body

typeCheck e b (EFreeVar name) = M.lookup name e

typeCheck e b (EBoundVar _ n)
    | n < V.length b = return (b V.! n)
    | otherwise      = Nothing

typeCheck e b (EAbs _ ty body) = do
    let newBoundVarEnv = V.cons ty b
    codomainTy <- typeCheck e newBoundVarEnv body
    return $ EFunction ty codomainTy

typeCheck e b (EApp f a) = do
    fTy <- typeCheck e b f
    aTy <- typeCheck e b a
    case fTy of
        EFunction dom cod -> do
            unifyTypes dom aTy
            return cod
        _                 -> Nothing
