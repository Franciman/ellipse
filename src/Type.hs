module Type where

-- Types are a central feature of Ellipse.
-- Let us start with a simply typed language with base types

data EType = EBool
    | EString
    | EInt
    | EFloat
    | EFunction EType EType
    deriving(Eq)

-- A funky function assigning size to a type

typeSize :: EType -> Int
typeSize (EFunction t1 t2) = 1 + max (typeSize t1) (typeSize t2)
typeSize _ = 1

-- Utility to convert a function accepting an n-ple of arguments to curried form
curryFunction :: Foldable t => t EType -> EType -> EType
curryFunction args res = foldr EFunction res args

-- Get the maximum number of arguments this function can be applied to
-- in order to get a meaningful result. I.e. this function retrieves
-- the arity of a given function application.
-- Constants have arity 0.
arity :: EType -> Int
arity (EFunction dom cod) = 1 + arity cod
arity _ = 0
