{-# LANGUAGE DeriveGeneric #-}
module Type where
    
-- Types are a central feature of Ellipse.
-- Let us start with a simply typed language with base types

import GHC.Generics
import Control.DeepSeq

data Type = TBool
    | TString
    | TInt
    | TFloat
    | TFunction Type Type
    deriving(Eq, Generic)

instance NFData Type

prettyPrintType :: Type -> String
prettyPrintType TBool = "Bool"
prettyPrintType TString = "String"
prettyPrintType TInt = "Int"
prettyPrintType TFloat = "Float"
prettyPrintType (TFunction left@(TFunction _ _) right) =
    "(" ++ prettyPrintType left ++ ")" ++ " -> " ++ prettyPrintType right

prettyPrintType (TFunction left right) =
    prettyPrintType left ++ " -> " ++ prettyPrintType right

instance Show Type where
    show = prettyPrintType

-- A funky function assigning size to a type

typeSize :: Type -> Int
typeSize (TFunction t1 t2) = 1 + max (typeSize t1) (typeSize t2)
typeSize _ = 1

-- Utility to convert a function accepting an n-ple of arguments to curried form
curryFunction :: Foldable t => t Type -> Type -> Type
curryFunction args res = foldr TFunction res args

-- Get the maximum number of arguments this function can be applied to
-- in order to get a meaningful result. I.e. this function retrieves
-- the arity of a given function application.
-- Constants have arity 0.
arity :: Type -> Int
arity (TFunction _ cod) = 1 + arity cod
arity _ = 0
