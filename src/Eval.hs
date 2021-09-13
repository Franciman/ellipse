{-# LANGUAGE DeriveGeneric #-}
module Eval where
    
import qualified CoreSyntaxTree as C
import Type
import qualified Env as E
import qualified Data.Text as T

import qualified Data.Sequence as S

import Data.Maybe (fromJust)
import GHC.Generics

import Control.DeepSeq

-- We define an interpreter for our language directly on SyntaxTree terms.
-- Since we allow definitions, how should we deal with them?
-- We deal with them and with substitution using an environment based interpreter.
-- Since we are using locally nameless representation, we use two different environments,
-- one for free variables and one for bound variables.
-- Let bindings are interpreted as if they were lambda functions applied to the new definition

-- This is the result of the evaluation process
data Value = Closure Env Int C.Expr
-- Since we allow for multiple values to be bound, we keep track of how many vals we have already bound
    | IntLit Int
    | FloatLit Float
    | StringLit T.Text
    | BoolLit Bool
    -- Recursion closure, the first argument is always itself
    | Recursion Env Int C.Expr
    -- We keep a special closure type for builtin operators,
    -- to support currying we keep track of how many arguments we still need to apply before getting a value
    | Builtin Env Int C.BuiltinOp
    deriving (Generic)

instance NFData Value

instance Show Value where
    show (Closure _ _ _) = "<closure>"
    show (IntLit n) = show n
    show (FloatLit n) = show n
    show (StringLit n) = show n
    show (BoolLit n) = show n
    show (Recursion _ _ _) = "Recursion record"
    show (Builtin _ _ op) = "<closure for builtin op " ++ show op ++ ">"

type Env = E.Env Value

-- eval implements the call-by-value evaluation strategy for function application
-- This is a SECD machine, but the S(tack) and the D(ump) are encoded in the call stack
-- of the recursive function definition.
-- We assume that the input term is well typed! (in particular it is well scoped).
-- This assumption helps us ignoring some erroneous conditions. Furthermore,
-- the locally nameless representation, when well scoped, allows us to avoid
-- the need of variable shifting.
eval :: Env -> Env -> C.Expr -> Value
eval _ _ (C.BoolLit n) = BoolLit n
eval e b (C.If cond tBranch fBranch) =
    let (BoolLit condVal) = eval e b cond
    in if condVal then eval e b tBranch else eval e b fBranch


eval _ _ (C.IntLit n)    = IntLit n
eval _ _ (C.FloatLit n)  = FloatLit n
eval _ _ (C.StringLit n) = StringLit n

-- `let x = f in g(x)` is interpreted as if it were (\x. g(x)) f
eval e b (C.Let name def body) =
    let defValue = eval e b def
        newBoundEnv = E.bind defValue b
    in eval e newBoundEnv body

-- The fix operator works like this: if we write,
-- let f = fix (\f. g)
-- then evaluating f gives us: g (fix (\f. g)) = g f .....
eval e b f@(C.Fix body) =
    -- the body must be a function, the result of fix v is
    -- a recursion value, which reminds us to keep looping,
    -- so what we do is bind the recursion value as the first argument
    -- of the evaulated body, so it can call itself.
    let (Closure bodyB params body') = eval e b body
    in Recursion bodyB params body'

eval e b (C.FreeVar _ index) = fromJust (E.lookup index e)

eval e b v@(C.BoundVar _ index) = fromJust (E.lookup index b)

-- We create a closure with respect to the current bound variables environment
eval e b (C.Abs params _ body) = Closure b (S.length params) body

eval e b (C.BuiltinOp C.Not) = Builtin b 1 C.Not
eval e b (C.BuiltinOp op) = Builtin b 2 op

-- This is the most subtle clause of all. Here we leverage well typedness a lot.
-- First of all we can expect that the evaluation of the lhs results in a closure value,
-- secondly, we don't even check the number of arguments, because we know that the result is a value,
-- thanks to well typedness, so we insert them all in the environment, because sooner or later
-- they will be used.
--
-- Now, with recursion, we have two possibilities, not only the closure one,
-- but also
eval e b (C.App f as) =
    case eval e b f of
        (Closure bEnv' argsCount body) ->
            let aVals = fmap (eval e b) as
                newBoundEnv = E.bindMany aVals bEnv'
            in if S.length aVals < argsCount
               -- Let us check for partial application
               -- If we applied too few arguments, we need to keep the closure
               then Closure newBoundEnv (argsCount - S.length aVals) body
               else eval e newBoundEnv body

        r@(Recursion bEnv' argsCount body) ->
            let aVals = fmap (eval e b) as
                newBoundEnv = E.bindMany aVals (E.bind r bEnv')
            in if S.length aVals + 1 < argsCount
               -- As before we check for a partial application,
               -- remembering that the number of supplied arguments
               -- must be increased by one, because we also supply a
               -- continuation recursion.
               then Closure newBoundEnv (argsCount - S.length aVals - 1) body
               else eval e newBoundEnv body

        (Builtin bEnv' argsLeft op) ->
            let aVals = fmap (eval e b) as
                newBoundEnv = E.bindMany aVals bEnv'
            in if S.length aVals < argsLeft
               then Builtin newBoundEnv (argsLeft - S.length aVals) op
               else case op of
                   C.Sum -> runIntOp (+) newBoundEnv
                   C.Sub -> runIntOp (flip subtract) newBoundEnv
                   C.Prod -> runIntOp (*) newBoundEnv
                   C.Div -> runFloatOp (/) newBoundEnv
                   C.And -> runBoolOp (&&) newBoundEnv
                   C.Or -> runBoolOp (||) newBoundEnv
                   C.Not -> negateOp newBoundEnv
                   C.LessThan -> runIntComp (<) newBoundEnv
                   C.GreaterThan -> runIntComp (>) newBoundEnv
                   C.Equal -> runIntComp (==) newBoundEnv

        v -> error $ "Impossible: " ++ show v

runIntOp :: (Int -> Int -> Int) -> Env -> Value
runIntOp op env =
    let (Just (IntLit n2)) = E.lookup 0 env
        (Just (IntLit n1)) = E.lookup 1 env
    in IntLit (op n1 n2)

runFloatOp :: (Float -> Float -> Float) -> Env -> Value
runFloatOp op env =
    let (Just (FloatLit n2)) = E.lookup 0 env
        (Just (FloatLit n1)) = E.lookup 1 env
    in FloatLit (op n1 n2)

runBoolOp :: (Bool -> Bool -> Bool) -> Env -> Value
runBoolOp op env =
    let (Just (BoolLit n2)) = E.lookup 0 env
        (Just (BoolLit n1)) = E.lookup 1 env
    in BoolLit (op n1 n2)

negateOp :: Env -> Value
negateOp env =
    let (Just (BoolLit n)) = E.lookup 0 env
    in BoolLit (not n)

runIntComp :: (Int -> Int -> Bool) -> Env -> Value
runIntComp op env =
    let (Just (IntLit n2)) = E.lookup 0 env
        (Just (IntLit n1)) = E.lookup 1 env
    in BoolLit (op n1 n2)


-- Entry point for evaluation
-- Now if the result of evaluation is a Recursion record,
-- it means we are looping forever
runEval :: Env -> C.Expr -> Value
runEval env expr = eval env E.empty expr
