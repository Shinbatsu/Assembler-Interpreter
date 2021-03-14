module Evaluator
( CPUState(..)
, evaluate 
, startingState
) where

import Grammar

import Debug.Trace
import Data.List (uncons)
import qualified Data.Bits as B
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

type Stack = [Val]
type CallStack = [Int]

data CPUState = CPUState { registers  :: HM.HashMap Reg Val
                         , callStack  :: CallStack
                         , stack      :: Stack
                         , flags      :: Val
                         , currLine   :: Int
                         , printLns   :: S.Seq String
                         , program    :: Program
                         , terminated :: Bool
                         } deriving (Eq, Show)

startingState :: CPUState
startingState = CPUState
 { registers  = HM.empty
 , callStack  = []
 , stack      = []
 , flags      = 0
 , currLine   = 0
 , printLns   = S.empty
 , program    = V.empty
 , terminated = False
}

evaluate :: CPUState -> CPUState
evaluate cpu = advance $ case prg V.!? currLine cpu of
    Just (InstrLine (Inc reg)) -> add reg (Val 1) cpu
    Just (InstrLine (Dec reg)) -> subSafe reg (Val 1) cpu
    Just (InstrLine (Mov reg arg)) -> mov reg arg cpu
    Just (InstrLine (Add reg arg)) -> add reg arg cpu
    Just (InstrLine (Sub reg arg)) -> subSafe reg arg cpu
    Just (InstrLine (Mul arg)) -> mul arg cpu 
    Just (InstrLine (Div reg arg)) -> adjRegWithArg (flip div) reg arg cpu
    Just (InstrLine (Xor reg arg)) -> adjRegWithArg B.xor reg arg cpu
    Just (InstrLine (And reg arg)) -> adjRegWithArg (B..&.) reg arg cpu
    Just (InstrLine (Or  reg arg)) -> adjRegWithArg (B..|.) reg arg cpu
    Just (InstrLine (Not reg)) -> adjReg B.complement reg cpu
    Just (InstrLine (Neg reg)) -> adjReg negate reg cpu
    Just (InstrLine (Shr reg arg)) -> shift32 RIGHT reg arg cpu
    Just (InstrLine (Shl reg arg)) -> shift32 LEFT  reg arg cpu
    Just (InstrLine (Rol reg arg)) -> rotate32 LEFT  reg arg cpu
    Just (InstrLine (Ror reg arg)) -> rotate32 RIGHT reg arg cpu
    Just (InstrLine (Cmp arg0 arg1)) -> comp arg0 arg1 cpu
    Just (InstrLine (Jmp lbl))  -> jump lbl cpu
    Just (InstrLine (Jne lbl))  -> jmpNone 0x0040 lbl cpu
    Just (InstrLine (Je  lbl))  -> jmpAll  0x0040 lbl cpu
    Just (InstrLine (Jge lbl))  -> jmpXnor jmpAll 0x0800 jmpAll 0x0080 lbl cpu
    Just (InstrLine (Jl  lbl))  -> jmpXor  jmpAll 0x0800 jmpAll 0x0080 lbl cpu
    Just (InstrLine (Jg  lbl))  -> jmpAnd  jmpNone 0x0040 (jmpXnor jmpAll 0x0800 jmpAll) 0x0080 lbl cpu
    Just (InstrLine (Jle lbl))  -> jmpOr   jmpAll  0x0040 (jmpXor  jmpAll 0x0800 jmpAll) 0x0080 lbl cpu
    Just (InstrLine (Jo  lbl))  -> jmpAll  0x0400 lbl cpu
    Just (InstrLine (Jc  lbl))  -> jmpAll  0x0001 lbl cpu
    Just (InstrLine (Jp  lbl))  -> jmpAll  0x0004 lbl cpu
    Just (InstrLine (Js  lbl))  -> jmpAll  0x0080 lbl cpu
    Just (InstrLine (Jno lbl))  -> jmpNone 0x0400 lbl cpu
    Just (InstrLine (Jnc lbl))  -> jmpNone 0x0001 lbl cpu
    Just (InstrLine (Jnp lbl))  -> jmpNone 0x0004 lbl cpu
    Just (InstrLine (Jns lbl))  -> jmpNone 0x0080 lbl cpu
    Just (InstrLine (Call lbl)) -> call lbl cpu
    Just (InstrLine (Push arg)) -> push arg cpu
    Just (InstrLine (Pop  reg)) -> pop reg cpu
    Just (InstrLine PushF) -> pushf cpu
    Just (InstrLine PopF)  -> popf cpu
    Just (InstrLine Ret) -> ret cpu
    Just (InstrLine (Msg msgs)) -> msg cpu msgs
    Just (InstrLine End) -> msg (cpu { terminated = True }) [StrMsg "0"]
    Just (LabelLine lbl) -> cpu
    Nothing              -> msg (cpu { terminated = True }) [StrMsg "-1"]
    where
      prg      = program cpu
      cpuFlags = flags cpu

data ShiftDir = LEFT | RIGHT

rotate32 :: ShiftDir -> Reg -> Arg -> CPUState -> CPUState
rotate32 dir reg arg cpu = (mov reg (Val $ safe 32 shift + cout) cpu) {flags = flg}
  where
    argval = getArgVal arg cpu
    regval = toInteger $ getRegVal reg cpu
    flg = getFlags shift
    cout = flg B..&. 1
    dirnum = case dir of
              LEFT  ->  B.shiftL
              RIGHT ->  B.shiftR
    shift = dirnum regval $ fromIntegral argval

shift32 :: ShiftDir -> Reg -> Arg -> CPUState -> CPUState
shift32 dir reg arg cpu = (mov reg (Val $ safe 32 shift) cpu) {flags = flg}
  where
    argval = getArgVal arg cpu
    regval = toInteger $ getRegVal reg cpu
    flg = getFlags shift
    dirnum = case dir of
              LEFT  ->  B.shiftL
              RIGHT ->  B.shiftR
    shift = dirnum regval $ fromIntegral argval

safe :: (Integral a, B.Bits a) => Int -> a -> Val
safe n = fromIntegral . (B..&.) (B.shiftL 1 n - 1)

setFlags :: Reg -> CPUState -> CPUState
setFlags reg cpu = cpu {flags = getFlags $ getArgVal (Reg reg) cpu}

getFlags :: (Integral a, B.Bits a) => a -> Val
getFlags val = sum $ zipWith (*)
  (map (fromIntegral . fromEnum . ($ val)) flagFunc)
  (map B.bit [0x0.. 0xF])
  where
    flagFunc :: (Integral a, B.Bits a) => [a -> Bool]
    flagFunc = [ (0 /=) . (0x100000000 B..&.)
               , const False
               , (0 ==) . (1 B..&.) . B.popCount
               , const False
               , const False
               , const False
               , (0 ==)
               , (> 0) . (0x80000000 B..&.)
               , const False
               , const False
               , const False
               , const False
               ]

mul :: Arg -> CPUState -> CPUState
mul arg cpu = cpu'' {flags = setOV $ flags cpu''}
  where
    setOV = if prod > 0xFFFFFFFF
                then (0x800 B..|.)
                else id
    prod = regval cpu * argval cpu
    regval = toInteger . getRegVal "ax"
    argval = toInteger . getArgVal arg
    cpu'' = setFlags "ax" cpu'
    cpu' = mov "dx" (Val $ safe 32 (prod `B.shiftR` 32)) $ mov "ax" (Val $ safe 32 prod) cpu

add :: Reg -> Arg -> CPUState -> CPUState
add reg arg cpu = mov reg (Val $ getRegVal reg cpu'') $ cpu'' {flags = setOV $ flags cpu''}
  where
    setOV f = if cin /= cout then 0x400 B..|. f else f
    cin = ((regval cpu B..&. 0x7FFFFFFF) + (argval cpu B..&. 0x7FFFFFFF)) >= 0x80000000
    regval = toInteger . getRegVal reg
    argval = toInteger . getArgVal arg
    cout  = flags cpu'' B..&. 0x1 == 1
    cpu'' = setFlags reg cpu'
    cpu'  = adjRegWithArgUnsafe (+) reg arg cpu

sub :: Reg -> Arg -> CPUState -> CPUState
sub reg arg cpu = mov reg (Val $ regval cpu'') $ cpu'' {flags = setOV $ flags cpu''}
  where
    setOV f = if cin /= cout then 0x400 B..|. f else f
    cin = ((regval cpu B..&. 0x7FFFFFFF) + ((twoC . argval $ cpu) B..&. 0x7FFFFFFF)) >= 0x80000000
    regval = getRegVal reg
    argval = getArgVal arg
    cout = flags cpu'' B..&. 0x1 == 1
    cpu'' = setFlags reg cpu'
    cpu' = adjRegWithArgUnsafe subtract reg arg cpu

subSafe :: Reg -> Arg -> CPUState -> CPUState
subSafe reg arg cpu = add reg (Val newArg) cpu
  where
    newArg = twoC . (safe 32 . getArgVal arg) $ cpu

twoC :: Val -> Val
twoC v = B.complement v + 1

type CondJmp = Val -> Lbl -> CPUState -> CPUState
