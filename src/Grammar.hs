module Grammar where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import qualified Data.Array as A

defaultVal :: Val
defaultVal = 0


type Reg = String

type Val = Word32
type SVal = Int32

type Lbl = String

data Msg = UArgMsg Arg
         | ArgMsg  Arg
         | StrMsg  String
         deriving (Eq, Show)

data Arg = Val Val
         | Reg Reg
         deriving (Eq, Show)

type Program = V.Vector ProgramLine

data ProgramLine = InstrLine Instruction
                 | LabelLine Lbl
                 | Empty
                 deriving (Eq, Show)
