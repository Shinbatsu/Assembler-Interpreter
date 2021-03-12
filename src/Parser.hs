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

instrParser :: Parser Instruction
instrParser =  (reserved "inc"   >> regName  >>= \r -> return (Inc r))
           <|> (reserved "dec"   >> regName  >>= \r -> return (Dec r))
           <|> (reserved "mov"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Mov r p)) -- Binary instruction
           <|> (reserved "add"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Add r p))
           <|> (reserved "sub"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Sub r p))
           <|> (reserved "mul"   >> argument >>= \p -> return (Mul p))
           <|> (reserved "div"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Div r p))
           <|> (reserved "xor"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Xor r p))
           <|> (reserved "and"   >> regName  >>= \r -> comma >> argument >>= \p -> return (And r p))
           <|> (reserved "or"    >> regName  >>= \r -> comma >> argument >>= \p -> return (Or  r p))
           <|> (reserved "not"   >> regName  >>= \r -> return (Not r))
           <|> (reserved "neg"   >> regName  >>= \r -> return (Neg r))
           <|> (reserved "shr"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Shr r p))
           <|> (reserved "shl"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Shl r p))
           <|> (reserved "rol"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Rol r p))
           <|> (reserved "ror"   >> regName  >>= \r -> comma >> argument >>= \p -> return (Ror r p))
           <|> (reserved "cmp"   >> argument >>= \p -> comma >> argument >>= \q -> return (Cmp p q))
           <|> (reserved "jmp"   >> lblName  >>= \lbl -> return (Jmp lbl)) -- jumps
           <|> (reserved "jne"   >> lblName  >>= \lbl -> return (Jne lbl))
           <|> (reserved "je"    >> lblName  >>= \lbl -> return (Je  lbl))
           <|> (reserved "jge"   >> lblName  >>= \lbl -> return (Jge lbl))
           <|> (reserved "jl"    >> lblName  >>= \lbl -> return (Jl  lbl))
           <|> (reserved "jg"    >> lblName  >>= \lbl -> return (Jg  lbl))
           <|> (reserved "jle"   >> lblName  >>= \lbl -> return (Jle lbl))
           <|> (reserved "jz"    >> lblName  >>= \lbl -> return (Je  lbl))
           <|> (reserved "jo"    >> lblName  >>= \lbl -> return (Jo  lbl))
           <|> (reserved "jc"    >> lblName  >>= \lbl -> return (Jc  lbl))
           <|> (reserved "jp"    >> lblName  >>= \lbl -> return (Jp  lbl))
           <|> (reserved "js"    >> lblName  >>= \lbl -> return (Js  lbl))
           <|> (reserved "jnz"   >> lblName  >>= \lbl -> return (Jne lbl))
           <|> (reserved "jno"   >> lblName  >>= \lbl -> return (Jno lbl))
           <|> (reserved "jnc"   >> lblName  >>= \lbl -> return (Jnc lbl))
           <|> (reserved "jnp"   >> lblName  >>= \lbl -> return (Jnp lbl))
           <|> (reserved "jns"   >> lblName  >>= \lbl -> return (Jns lbl))
           <|> (reserved "call"  >> lblName  >>= \lbl -> return (Call lbl))
           <|> (reserved "push"  >> argument >>= \p   -> return (Push p))
           <|> (reserved "pop"   >> regName  >>= \r   -> return (Pop r))
           <|> (reserved "pushf" >> return PushF)
           <|> (reserved "popf"  >> return PopF)
           <|> (reserved "ret"   >> return Ret)
           <|> (reserved "msg"   >> msgParse >>= \m -> return (Msg m))
           <|> (reserved "end"   >> return End)

msgParse :: Parser [Msg]
msgParse = commaSep message

argument :: Parser Arg
argument =  (regName  >>= \r  -> return (Reg r))
        <|> (value    >>= \v  -> return (Val v))

uArgument :: Parser Arg
uArgument =  (uRegName  >>= \r  -> return (Reg r))
         <|> (uValue    >>= \v  -> return (Val v))

message :: Parser Msg
message =  (argument  >>= \a  -> return (ArgMsg  a))
       <|> (uArgument >>= \ua -> return (UArgMsg ua))
       <|> (stringLiteral >>= \s -> return (StrMsg s))

uValue :: Parser Val
uValue = do
  symbol "_"
  fromIntegral <$> natural

uRegName :: Parser Reg
uRegName = do
  symbol "_"
  identifier

value :: Parser Val
value =  fromIntegral <$> integer

regName :: Parser Reg
regName = identifier

lblName :: Parser Lbl
lblName = identifier

lblDecl:: Parser Lbl
lblDecl = do
  lbl <- identifier
  symbol ":" <?> ("\"" ++ lbl ++ "\"" ++ " cannot be parsed")
  return lbl

mainParser :: String -> Program
mainParser str =
  case tryparse of
    Left err -> error (show err)
    Right result -> V.filter (/= Empty) result
  where
    ls = V.fromList $ lines str
    tryparse :: Either ParseError Program
    tryparse = V.mapM (parse programLineParser "") ls

mainFileParser :: String -> IO Program
mainFileParser filename = do
  f <- readFile filename
  return $ mainParser f