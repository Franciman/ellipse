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
typeSize EBool  = 1
typeSize EInt   = 1
typeSize EFloat = 1
typeSize (EFunction t1 t2) = 1 + max (typeSize t1) (typeSize t2)
