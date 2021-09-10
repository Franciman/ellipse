module UnTypedCalculus where

import qualified Data.Sequence as S

-- Nameless representation, lambda abstractions can accept multiple arguments
-- and so can function application. For lambda functions we record how many arguments it accepts "at once"
data Term = BoundVar Int
          | FreeVar Int
          | Abs Int Term
          | App Term (S.Seq Term)

type Env = S.Seq Value

-- Since we can have partially applied functions now, we need to keep track of how many
-- arguments still need to be applied to the function body
data Value = Closure Env Int Term

-- This is a SECD machine, but the S(tack) and the D(ump) are encoded in the call stack
-- of the recursive function definition.
-- Furthermore, we keep two separate environments, one for free variables, corresponding to definitions
-- and one for bound variables, necessary for evaluating function application. This helps in avoiding
-- the shifting of free variables during function application.
--
-- In the implementation we assume that the term we start from has no free variables.
-- This allows us to avoid problems with indices rescaling, in fact, no rescaling is needed.
-- When we evaluate beta reduction, we know that it can't happen inside a lambda function,
-- so the right hand side is guaranteed to have no unbound variable, hence when we put it in a larger
-- environment, none of its variables gets captured (This equals the lexical scoping strategy).
-- On the other hand, after performing beta reduction, the resulting term has at most one free variable,
-- and it is guaranteed to be bound by the environment in the correct position, by construction.

eval :: Env -> Env -> Term -> Value
eval defs env (BoundVar idx) =
    case S.lookup idx env of
        Nothing -> error "Unbound variable"
        Just v  -> v

eval defs env (FreeVar idx) =
    case S.lookup idx defs of
        Nothing -> error "Unbound variable"
        Just v  -> v

eval defs env (Abs argCount body) = Closure env argCount body

eval defs env (App f gs) =
        let fval = eval defs env f
        in evalApp fval gs

    where evalApp (Closure env' argCount body) gs =
            case compare argCount (S.length gs) of
                LT ->
                    -- Right now we can't know if this is ill posed,
                    -- because the parameters could change the number of applications
                    -- allowed, so we just take the first `argCount` params, apply them
                    -- and then see what happens to the resulting ones.
                    let (toApplyNow, toApplyLater) = S.splitAt argCount gs
                        vals = fmap (eval defs env) toApplyNow
                        newEnv = S.reverse vals S.>< env'
                        newLhs = eval defs newEnv body
                    in evalApp newLhs toApplyLater
                GT ->
                    -- We perform partial application, so we return a closure
                    -- with just some arguments added and the rest are left as arguments to the lambda
                    let argsLeft = argCount - S.length gs
                        gVals = fmap (eval defs env) gs
                        newEnv = S.reverse gVals S.>< env'
                    in Closure newEnv argsLeft body
                EQ ->
                    let gVals = fmap (eval defs env) gs
                        newEnv = S.reverse gVals S.>< env'
                    in eval defs newEnv body
