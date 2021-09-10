module Eval where

import qualified CoreSyntaxTree as C
import Type
import qualified Env as E
import qualified Data.Text as T

import qualified Data.Sequence as S

import Data.Maybe (fromJust)

-- We define an interpreter for our language directly on SyntaxTree terms.
-- Since we allow definitions, how should we deal with them?
-- We deal with them and with substitution using an environment based interpreter.
-- Since we are using locally nameless representation, we use two different environments,
-- one for free variables and one for bound variables.
-- Let bindings are interpreted as if they were lambda functions applied to the new definition

-- This is the result of the evaluation process
data Value = Closure Env C.Expr
    | IntLit Int
    | FloatLit Float
    | StringLit T.Text
    | BoolLit Bool

type Env = E.Env Value

-- eval implements the call-by-value evaluation strategy for function application
-- This is a SECD machine, but the S(tack) and the D(ump) are encoded in the call stack
-- of the recursive function definition.
-- We assume that the input term is well typed! (in particular it is well scoped).
-- This assumption helps us ignoring some erroneous conditions. Furthermore,
-- the locally nameless representation, when well scoped, allows us to avoid
-- the need of variable shifting.
eval :: Env -> Env -> C.Expr -> Value
eval _ _ C.True = BoolLit True
eval _ _ C.False = BoolLit False
eval e b (C.If cond tBranch fBranch) =
    case eval e b cond of
        BoolLit True  -> eval e b tBranch
        BoolLit False -> eval e b fBranch

eval _ _ (C.IntLit n)    = IntLit n
eval _ _ (C.FloatLit n)  = FloatLit n
eval _ _ (C.StringLit n) = StringLit n

-- `let x = f in g(x)` is interpreted as if it were (\x. g(x)) f
eval e b (C.Let name def body) =
    let defValue = eval e b def
        newBoundEnv = E.bind defValue b
    in eval e newBoundEnv body

eval e b (C.FreeVar _ index) = fromJust (E.lookup index e)

eval e b v@(C.BoundVar _ index) = fromJust (E.lookup index b)

-- We create a closure with respect to the current bound variables environment
eval e b f@(C.Abs params _ body) = Closure b body

-- This is the most subtle clause of all. Here we leverage well typedness a lot.
-- First of all we can expect that the evaluation of the lhs results in a closure value,
-- secondly, we don't even check the number of arguments, because we know that the result is a value,
-- thanks to well typedness, so we insert them all in the environment, because sooner or later
-- they will be used.
eval e b (C.App f as) =
    let (Closure bEnv' body) = eval e b f
        aVals = fmap (eval e b) as
        newBoundEnv = E.bindMany aVals bEnv'
    in eval e newBoundEnv body


