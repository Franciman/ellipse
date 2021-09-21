{-# LANGUAGE DeriveGeneric #-}
module ByteCode where
    
import qualified Data.Sequence as S
import qualified Env as E
import qualified CoreSyntaxTree as C
import qualified Data.Text as T
import GHC.Generics
import Control.DeepSeq
-- import Debug.Trace
import Control.Monad.ST.Strict
import Data.STRef.Strict

-- We want to devise a more compact and caching friendly
-- representation of an expression.

type Stack = S.Seq Value

emptyStack :: Stack
emptyStack = S.empty

type Env = E.Env Value

push :: Value -> Stack -> Stack
push v stack = stack S.|> v

data Value = IntLit Int
     | BoolLit Bool
     | FloatLit Float
     | StringLit T.Text
     | Closure Env Program
     | BuiltinClosure Env Int C.BuiltinOp
     deriving (Generic)

instance NFData Value

instance Show Value where
    show (Closure _ _) = "<closure>"
    show (IntLit n) = show n
    show (FloatLit n) = show n
    show (StringLit n) = show n
    show (BoolLit n) = show n
    show (BuiltinClosure _ count op) = "<closure for builtin op " ++ show op ++ " args left: " ++ show count ++ ">"

data ByteCode = Const Int -- ref to the const stack
    | BoundLookup Int -- ref to the env
    | FreeLookup Int -- ref to the env
    | Builtin C.BuiltinOp
    | Abs Int
    | App
    | Fix
    | If Int Int -- first index is where (rel) the else branch starts, second index is where (rel) the else branch ends
    deriving (Generic, Show)


instance NFData ByteCode

type Program = S.Seq ByteCode

data ProgState = ProgState
    { program :: Program
    , counter :: Int
    }

getInstr :: ProgState -> ByteCode
getInstr (ProgState prog idx) = S.index prog idx

skip :: Int -> ProgState -> ProgState
skip n (ProgState prog c) = ProgState prog (c + n)

nextInstr :: ProgState -> ProgState
nextInstr = skip 1

makeBuiltinClosure :: Env -> C.BuiltinOp -> Value
makeBuiltinClosure env C.Sum         = BuiltinClosure env 2 C.Sum
makeBuiltinClosure env C.Sub         = BuiltinClosure env 2 C.Sub         
makeBuiltinClosure env C.Prod        = BuiltinClosure env 2 C.Prod        
makeBuiltinClosure env C.Div         = BuiltinClosure env 2 C.Div         
makeBuiltinClosure env C.And         = BuiltinClosure env 2 C.And         
makeBuiltinClosure env C.Or          = BuiltinClosure env 2 C.Or          
makeBuiltinClosure env C.Not         = BuiltinClosure env 1 C.Not         
makeBuiltinClosure env C.LessThan    = BuiltinClosure env 2 C.LessThan    
makeBuiltinClosure env C.GreaterThan = BuiltinClosure env 2 C.GreaterThan 
makeBuiltinClosure env C.Equal       = BuiltinClosure env 2 C.Equal

evalOp :: Env -> C.BuiltinOp -> Value
evalOp newBoundEnv op = case op of
    C.Sum ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in IntLit (n1 + n2)

    C.Sub ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in IntLit (n1 - n2)

    C.Prod ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in IntLit (n1 * n2)

    C.Div -> 
       let (FloatLit n2) = E.index 0 newBoundEnv
           (FloatLit n1) = E.index 1 newBoundEnv
       in FloatLit (n1 / n2)

    C.And -> 
       let (BoolLit n2) = E.index 0 newBoundEnv
           (BoolLit n1) = E.index 1 newBoundEnv
       in BoolLit (n1 && n2)

    C.Or -> 
       let (BoolLit n2) = E.index 0 newBoundEnv
           (BoolLit n1) = E.index 1 newBoundEnv
       in BoolLit (n1 || n2)

    C.Not ->
       let (BoolLit n1) = E.index 0 newBoundEnv
       in BoolLit (not n1)

    C.LessThan ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in BoolLit (n1 < n2)

    C.GreaterThan ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in BoolLit (n1 > n2)

    C.Equal ->
       let (IntLit n2) = E.index 0 newBoundEnv
           (IntLit n1) = E.index 1 newBoundEnv
       in BoolLit (n1 == n2)

runProgram :: Stack -> Env -> Env -> STRef s ProgState -> ST s Value
runProgram s e b prog = do
    instr <- getInstr <$> readSTRef prog
    eval s e b instr prog

eval :: Stack -> Env -> Env -> ByteCode -> STRef s ProgState -> ST s Value
eval s _ _ (Const idx) _  = return $ S.index s idx
eval _ _ b (BoundLookup idx) _ = return $ E.index idx b
eval _ e _ (FreeLookup idx) _  = return $ E.index idx e
eval _ _ b (Builtin op) _   =  return $ makeBuiltinClosure b op
eval s _ b (Abs idx) _ = do
     let Closure _ body = S.index s idx
     return $ Closure b body

eval s e b App prog = do
    modifySTRef prog nextInstr
    fVal <- runProgram s e b prog
    modifySTRef prog nextInstr
    aVal <- runProgram s e b prog
    case fVal of
        Closure bEnv' body -> do
            let newBoundEnv = E.bind aVal bEnv'
            newProgState <- newSTRef $ ProgState body 0
            runProgram s e newBoundEnv newProgState

        BuiltinClosure bEnv' argsLeft op -> do
            let newBoundEnv = E.bind aVal bEnv'
            if argsLeft - 1 > 0
            then  return $ BuiltinClosure newBoundEnv (argsLeft - 1) op
            else do
                return $ evalOp newBoundEnv op

        _ -> error "Impossible"



eval s e b Fix prog = do
    modifySTRef prog nextInstr
    Closure bEnv' body <- runProgram s e b prog
    newProgState <- newSTRef $ ProgState body 0
    fixST $ \res -> runProgram s e (E.bind res bEnv') newProgState

eval s e b (If elseStart elseEnd) prog = do
    modifySTRef prog nextInstr
    BoolLit cond <- runProgram s e b prog
    modifySTRef prog nextInstr
    if cond
    then do
        v <- runProgram s e b prog
        modifySTRef prog (skip elseEnd)
        return v

    else do
        modifySTRef prog (skip elseStart)
        runProgram s e b prog

compileAST :: C.Expr -> STRef s Stack -> ST s Program
compileAST (C.BoolLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (BoolLit b))
    return $ S.singleton (Const idx)

compileAST (C.IntLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (IntLit b))
    return $ S.singleton (Const idx)


compileAST (C.FloatLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (FloatLit b))
    return $ S.singleton (Const idx)


compileAST (C.StringLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (StringLit b))
    return $ S.singleton (Const idx)


compileAST (C.FreeVar _ idx)  _ = return $ S.singleton (FreeLookup idx)
compileAST (C.BoundVar _ idx) _ = return $ S.singleton (BoundLookup idx)
compileAST (C.BuiltinOp op)   _ = return $ S.singleton (Builtin op)

compileAST (C.Abs _ _ body) stack = do
    bodyProg <- compileAST body stack
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (Closure E.empty bodyProg))
    return $ S.singleton (Abs idx)

compileAST (C.App f a) stack = do
    fProg <- compileAST f stack
    aProg <- compileAST a stack
    let prog = App S.<| (fProg S.>< aProg)
    return prog

compileAST (C.Fix f) stack = do
    fProg <- compileAST f stack
    let prog = Fix S.<| fProg
    return prog

compileAST (C.If cond tBranch fBranch) stack = do
    condProg <- compileAST cond stack
    tProg <- compileAST tBranch stack
    let elseStart = S.length tProg
    fProg <- compileAST fBranch stack
    let elseEnd = S.length fProg
    let prog = If elseStart elseEnd S.<| (condProg S.>< tProg S.>< fProg)
    return prog

compileAST (C.Let name def body) stack = compileAST (C.App (C.Abs name undefined body) def) stack

runEval :: Stack -> Env -> C.Expr -> (Stack, Value)
runEval stack env expr = runST $ do
    stackRef <- newSTRef stack
    compiledProgram <- compileAST expr stackRef
    stack' <- readSTRef stackRef
    progState <- newSTRef $ ProgState compiledProgram 0
    v <- runProgram stack' env E.empty progState
    return (stack', v)
