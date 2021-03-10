module Parser where

import Grammar
import Lexer

import qualified Data.Vector as V
import Text.Parsec (ParseError, (<|>), sepBy1, (<?>), parse, try, letter)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Char as C


programLineParser :: Parser ProgramLine
programLineParser = 
  whiteSpace >>
     ((instrParser >>= \ins -> return (InstrLine ins))
 <|> (lblDecl >>= \lbl -> return (LabelLine lbl))
 <|> emptyLine)

emptyLine :: Parser ProgramLine
emptyLine = 
  do
    whiteSpace
    return Empty