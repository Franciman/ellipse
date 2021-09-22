{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module CompressedByteCode where

import qualified CoreSyntaxTree as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as B
import Control.Monad.ST
import Data.STRef
import Data.Word
import Data.Bits
import qualified Env as E
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Foldable
import Control.DeepSeq
import GHC.Generics
import qualified Data.Vector as V

type Stack = S.Seq Value

type Consts = V.Vector Value

push :: Value -> Stack -> Stack
push v stack = stack S.|> v

emptyStack :: Stack
emptyStack = S.empty

type Env = E.Env Value

data Value = IntLit Int
    | BoolLit Bool
    | FloatLit Float
    | StringLit T.Text
    | Closure Env B.ByteString
    | BuiltinClosure Env Int Word8
    deriving (Show, Generic)

instance NFData Value

-- Now we define the 8 bytecodes for our instructions
-- const -> 0
-- boundLookup -> 1
-- freeLookup -> 2
-- abs -> 3
-- app -> 4
-- fix -> 5
-- if -> 6
-- builtin Sum         -> 7
-- builtin Sub         -> 8
-- builtin Prod        -> 9
-- builtin Div         -> 10
-- builtin And         -> 11
-- builtin Or          -> 12
-- builtin Not         -> 13
-- builtin LessThan    -> 14
-- builtin GreaterThan -> 15
-- builtin Equal       -> 16
--
-- WARNING:
-- Some instructions also have parameters, they are put after the operator bytecode encoded as word64 little endian
-- So for example: const 1 is encoded as: 01000000000000000000000000000000000000000000...

-- Externally we have to use Ints, because that's what we can use as index of the bytestring, sad.

-- Addresses are represented as Ints, because that's what haskell allows to access ByteString
type Counter = Int

{-# INLINE getInstr #-}
getInstr :: B.ByteString -> Counter  -> Word8
getInstr = B.unsafeIndex


-- Internally we represent addresses as words, though
{-# INLINE readAddress #-}
readAddress :: B.ByteString -> Counter -> (Counter, Int)
readAddress bs base =
    let b0 = fromIntegral $ B.unsafeIndex bs (base + 0) `shift` 0
        b1 = fromIntegral $ B.unsafeIndex bs (base + 1) `shift` 8
        b2 = fromIntegral $ B.unsafeIndex bs (base + 2) `shift` 16
        b3 = fromIntegral $ B.unsafeIndex bs (base + 3) `shift` 24
        b4 = fromIntegral $ B.unsafeIndex bs (base + 4) `shift` 32
        b5 = fromIntegral $ B.unsafeIndex bs (base + 5) `shift` 40
        b6 = fromIntegral $ B.unsafeIndex bs (base + 6) `shift` 48
        b7 = fromIntegral $ B.unsafeIndex bs (base + 7) `shift` 56
        word = b0 .|. b1 .|. b2 .|. b3 .|. b4 .|. b5 .|. b6 .|. b7 :: Word64
    -- traceM $ "Read address: " ++ show word
    -- Increment the counter to go to the end of the 8 bytes
    in (base + 7, fromIntegral word)

-- Serialization utils
writeAddress :: Int -> B.Builder
writeAddress = B.word64LE . fromIntegral

writeConst :: Int -> B.Builder
writeConst addr = B.word8 0 <> writeAddress addr

writeBoundLookup :: Int -> B.Builder
writeBoundLookup addr = B.word8 1 <> writeAddress addr

writeFreeLookup :: Int -> B.Builder
writeFreeLookup addr = B.word8 2 <> writeAddress addr

writeAbs :: Int -> B.Builder
writeAbs addr = B.word8 3 <> writeAddress addr

writeApp :: B.Builder
writeApp = B.word8 4

writeFix :: B.Builder
writeFix = B.word8 5

writeIf :: Int -> Int -> B.Builder
writeIf elseStart elseEnd = B.word8 6 <> writeAddress elseStart <> writeAddress elseEnd

writeBuiltin :: C.BuiltinOp -> B.Builder
writeBuiltin C.Sum         = B.word8 (7 + 0)
writeBuiltin C.Sub         = B.word8 (7 + 1)
writeBuiltin C.Prod        = B.word8 (7 + 2)
writeBuiltin C.Div         = B.word8 (7 + 3)
writeBuiltin C.And         = B.word8 (7 + 4)
writeBuiltin C.Or          = B.word8 (7 + 5)
writeBuiltin C.Not         = B.word8 (7 + 6)
writeBuiltin C.LessThan    = B.word8 (7 + 7)
writeBuiltin C.GreaterThan = B.word8 (7 + 8)
writeBuiltin C.Equal       = B.word8 (7 + 9)

-- Eval

{-# INLINE makeBuiltinClosure #-}
makeBuiltinClosure :: Env -> Word8 -> Value
makeBuiltinClosure env 13 = BuiltinClosure env 1 13
makeBuiltinClosure env opCode = BuiltinClosure env 2 opCode

evalOp :: Env -> Word8 -> Value
evalOp env 7 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in IntLit (n1 + n2)

evalOp env 8 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in IntLit (n1 - n2)

evalOp env 9 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in IntLit (n1 * n2)

evalOp env 10 =
    let !(FloatLit n2) = E.index 0 env
        !(FloatLit n1) = E.index 1 env
    in FloatLit (n1 / n2)

evalOp env 11 =
    let !(BoolLit n2) = E.index 0 env
        !(BoolLit n1) = E.index 1 env
    in BoolLit (n1 && n2)

evalOp env 12 =
    let !(BoolLit n2) = E.index 0 env
        !(BoolLit n1) = E.index 1 env
    in BoolLit (n1 || n2)

evalOp env 13 =
    let !(BoolLit n1) = E.index 0 env
    in BoolLit (not n1)

evalOp env 14 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in BoolLit (n1 < n2)

evalOp env 15 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in BoolLit (n1 > n2)

evalOp env 16 =
    let !(IntLit n2) = E.index 0 env
        !(IntLit n1) = E.index 1 env
    in BoolLit (n1 == n2)

evalOp _ _ = error "Impossible"

runProgram :: Consts -> Env -> Env -> B.ByteString -> Counter  -> (Counter, Value)
runProgram s e b prog c = do
    let instr = getInstr prog c
    eval instr s e b prog c

eval :: Word8 -> Consts -> Env -> Env -> B.ByteString -> Counter -> (Counter, Value)
-- Const
eval 0 s _ _ prog c =
    let (c', idx) = readAddress prog (c + 1)
    in (c', V.unsafeIndex s idx)

-- BoundLookup
eval 1 _ _ b prog c =
    let (c', idx) = readAddress prog (c + 1)
    -- traceM $ "BoundLookup: " ++ show idx
    in (c', E.index idx b)

-- FreeLookup
eval 2 _ e _ prog c =
    let (c', idx) = readAddress prog (c + 1)
    -- traceM $ "FreeLookup: " ++ show idx
    in (c', E.index idx e)

-- Abs
eval 3 s _ b prog c =
    let (c', idx) = readAddress prog (c + 1)
        Closure _ body = V.unsafeIndex s idx
    in (c', Closure b body)

-- App
eval 4 s e b prog c =
    let (c', fVal) = runProgram s e b prog (c + 1)
        (c'', aVal) = runProgram s e b prog (c' + 1)
    in case fVal of
        Closure bEnv' body ->
            let newBoundEnv = E.bind aVal bEnv'
                (_, res) = runProgram s e newBoundEnv body 0
            in (c'', res)

        BuiltinClosure bEnv' argsLeft op ->
            let newBoundEnv = E.bind aVal bEnv'
            in if argsLeft - 1 > 0
                then (c'', BuiltinClosure newBoundEnv (argsLeft - 1) op)
                else (c'', evalOp newBoundEnv op)

        _ -> error "Impossible"

-- Fix
eval 5 s e b prog c =
    let (c', Closure bEnv' body) = runProgram s e b prog (c + 1)
        (_, res) = runProgram s e (E.bind res bEnv') body 0
    in (c', res)

-- If
eval 6 s e b prog c =
    let (c', elseStart) = readAddress prog (c + 1)
        (c'', elseEnd)  = readAddress prog (c' + 1)
        (c''', BoolLit cond) = runProgram s e b prog (c'' + 1)
    in if cond
        then let (c4, v) = runProgram s e b prog (c''' + 1)
             in (c4 + elseEnd, v)
        else runProgram s e b prog (c''' + elseStart + 1)

-- BuiltinOp
eval n _ _ b _ c = (c, makeBuiltinClosure b n)


-- AST compilation
renderBuilder :: B.Builder -> B.ByteString
renderBuilder = BL.toStrict . B.toLazyByteString

compileAST :: C.Expr -> STRef s Stack -> ST s B.Builder
compileAST (C.BoolLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (BoolLit b))
    return $ writeConst idx

compileAST (C.IntLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (IntLit b))
    return $ writeConst idx


compileAST (C.FloatLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (FloatLit b))
    return $ writeConst idx


compileAST (C.StringLit b) stack = do
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (StringLit b))
    return $ writeConst idx


compileAST (C.FreeVar _ idx)  _ = return $ writeFreeLookup idx
compileAST (C.BoundVar _ idx) _ = return $ writeBoundLookup idx
compileAST (C.BuiltinOp op)   _ = return $ writeBuiltin op

compileAST (C.Abs _ _ body) stack = do
    bodyProg <- renderBuilder <$> compileAST body stack
    idx <- S.length <$> readSTRef stack
    modifySTRef' stack (push (Closure E.empty bodyProg))
    return $ writeAbs idx

compileAST (C.App f a) stack = do
    fProg <- compileAST f stack
    aProg <- compileAST a stack
    let prog = writeApp <> fProg <> aProg
    return prog

compileAST (C.Fix f) stack = do
    fProg <- compileAST f stack
    let prog = writeFix <> fProg
    return prog

compileAST (C.If cond tBranch fBranch) stack = do
    condProg <- compileAST cond stack
    tProg <- compileAST tBranch stack
    let elseStart = fromIntegral $ BL.length (B.toLazyByteString tProg)
    fProg <- compileAST fBranch stack
    let elseEnd = fromIntegral $ BL.length (B.toLazyByteString fProg)
    let prog = writeIf elseStart elseEnd <> condProg <> tProg <> fProg
    return prog

compileAST (C.Let name def body) stack = compileAST (C.App (C.Abs name undefined body) def) stack


{-dumpProgram :: Stack -> Env -> Env -> Counter RealWorld -> IO ()
dumpProgram s e b prog = do
    instr <- stToIO $ getInstr prog
    dumpOp instr s e b prog

dumpOp :: Word8 -> Stack -> Env -> Env -> Counter RealWorld -> IO ()
-- Const
dumpOp 0 s e b prog = do
    stToIO $ next prog
    idx <- stToIO $ readAddress prog
    putStrLn $ "Const " ++ show idx

-- BoundLookup
dumpOp 1 s e b prog = do
    stToIO $ next prog
    idx <- stToIO $ readAddress prog
    putStrLn $ "BoundLookup " ++ show idx

-- FreeLookup
dumpOp 2 s e b prog = do
    stToIO $ next prog
    idx <- stToIO $ readAddress prog
    putStrLn $ "FreeLookup " ++ show idx

-- Abs
dumpOp 3 s e b prog = do
    stToIO $ next prog
    idx <- stToIO $ readAddress prog
    putStrLn $ "Abs " ++ show idx

-- App
dumpOp 4 s e b prog = do
    putStrLn "App"
    stToIO $ next prog
    dumpProgram s e b prog
    stToIO $ next prog
    dumpProgram s e b prog

-- Fix
dumpOp 5 s e b prog = do
    putStrLn "Fix"
    stToIO $ next prog
    dumpProgram s e b prog

-- If
dumpOp 6 s e b prog = do
    stToIO $ next prog
    elseStart <- stToIO $ readAddress prog
    stToIO $ next prog
    elseEnd <- stToIO $ readAddress prog
    stToIO $ next prog
    putStrLn $ "If " ++ show elseStart ++ " " ++ show elseEnd
    dumpProgram s e b prog
    stToIO $ next prog
    dumpProgram s e b prog
    dumpProgram s e b prog

-- BuiltinOp
dumpOp n _ _ _  _ = putStrLn $ "BuiltinOp " ++ show n

runDumper :: Stack -> Env -> C.Expr -> IO ()
runDumper stack env expr = do
    let (s, p) = runST $ do
            stackRef <- newSTRef stack
            program <- renderBuilder <$> compileAST expr stackRef
            stack' <- readSTRef stackRef
            return (stack', program)

    prog <- stToIO $ newSTRef p
    dumpProgram s env E.empty prog-}

runEval :: Stack -> Env -> C.Expr -> (Stack, Value)
runEval stack env expr = runST $ do
    stackRef <- newSTRef stack
    program <- renderBuilder <$> compileAST expr stackRef
    stack' <- readSTRef stackRef
    let consts = V.fromList (toList stack')
    let (_, v) = runProgram consts env E.empty program 0
    return (stack', v)
 
