module Type where

-- Types are a central feature of Ellipse.
-- Let us start with a simply typed language with base types

data Type = TBool
    | TString
    | TInt
    | TFloat
    | TFunction Type Type
    deriving(Eq, Show)

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
arity (TFunction dom cod) = 1 + arity cod
arity _ = 0
