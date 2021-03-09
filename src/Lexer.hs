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
  where
    lexeme p = do{ x <- p; whiteSpace; return x  }
    myStringLiteral   = lexeme (
                          do{ str <- P.between (P.char '\'')
                                               (P.char '\'' <?> "end of string")
                                               (P.many stringChar)
                            ; return (foldr (maybe id (:)) "" str)
                            }
                          <?> "literal string")
    stringChar      =   Just <$> stringLetter
                        <?> "string character"
    stringLetter    = P.satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
    myOctal = do{ _ <- P.oneOf "oOqQ"; number 8 octDigit }
    number base baseDigit
        = do{ digits <- P.many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }
    octDigit = P.satisfy isOctDigit <?> "octal digit"
    zVal = do { _ <- P.char '0'
          ; hexadecimal
         <|> octal
         <|> decimal
         <|> return 0
          }
    nat = zVal <|> decimal
    sign =  (C.char '-' >> return negate)
        <|> (C.char '+' >> return id)
        <|> return id
    int = 
      do 
        f <- lexeme sign
        f <$> nat

identifier    = Token.identifier lexer
reserved      = Token.reserved lexer
symbol        = Token.symbol lexer
natural       = Token.natural lexer
integer       = Token.integer lexer
decimal       = Token.decimal lexer
octal         = Token.octal lexer
hexadecimal   = Token.hexadecimal lexer
comma         = Token.comma lexer
whiteSpace    = Token.whiteSpace lexer
stringLiteral = Token.stringLiteral lexer
commaSep      = Token.commaSep lexer
