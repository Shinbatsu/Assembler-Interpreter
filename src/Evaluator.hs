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