module Lexer
( identifier
, reserved
, symbol
, comma
, integer
, whiteSpace
, stringLiteral
, natural
, commaSep ) where

import Text.Parsec ((<|>), Stream, (<?>))
import Text.Parsec.Language (emptyDef)
import Data.Char
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C

reservedInstrs :: [String]
reservedInstrs = ["mov"  , "inc" , "dec" , "add"  , "sub", "mul" , "div"
                 ,"jmp"  , "cmp" , "jne" , "je"   , "jge", "jg"  , "jle"
                 ,"jl"   , "call", "ret" , "msg"  , "end", "push", "pop"
                 ,"xor"  , "and" , "or"  , "not"  , "neg", "shr" , "shl"
                 ,"rol"  , "ror" , "popf", "pushf", "jo" , "jc"  , "jp"
                 ,"js"   , "jz"  , "jnz" , "jno"  , "jnc", "jnp" , "jns" ]

languageDef = 
  emptyDef { Token.commentLine   = ";"
           , Token.identStart    = P.letter
           , Token.identLetter   = P.alphaNum
           , Token.reservedNames = reservedInstrs
           }

lexer = (Token.makeTokenParser languageDef) { Token.stringLiteral = myStringLiteral
                                            , Token.octal = myOctal
                                            , Token.integer = lexeme int
                                            }