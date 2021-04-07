module SchemeParser (
    symbol,
    spaces,
    comment,
    spacesAndComments,
   parseString,
    parseAtom,
    parseNumber,
    parseCharacter,
    parseList,
    parseDottedList,
    parseQuoted,
    parseBracketed,
    parseFloat,
    parseExpr, 
)where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)
import System.Environment
import Data.IORef
import Control.Monad.Except
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

ws :: Parser ()
ws = skipMany (space <|> endOfLine)

spaces :: Parser ()
spaces = skipMany space

-- default Parsec spaces function just parses a whitespace character
-- jarl: should be called spaces1 since it requires at least 1 space to succeed
spaces1 :: Parser ()
spaces1 = skipMany1 space

comment :: Parser ()
comment = do
  char ';'
  manyTill anyToken (eof <|> (endOfLine >> return ()))
  return ()

spacesAndComments :: Parser ()
spacesAndComments = do
  ws
  -- after parsing spaces, we might be at a comment, a newline or the next expression
  many $ comment >> ws
  -- the previous line runs until we're at a position which is none of: a space, ';' or a newline
  return ()

{- jarl: Old comment parser
parseComments :: Parser LispVal
parseComments = do
            char ';'
            --(endOfLine >> "a") <|> many (anyChar)
            manyTill anyToken (eof <|> (endOfLine >> return ()))
           -- manyTill anyChar (try newline) 
            return Void -} 

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"") -- check this
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return (case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False 
                        _    -> Atom atom)
                        -- are there any other literals in Scheme?

parseNumber :: Parser LispVal
--parseNumber = return Number <*> read <$> many1 digit
parseNumber = do
                num <- many1 digit
                return $ (Number . read) num
                

parseCharacter :: Parser LispVal
parseCharacter = do
                    oneOf "#"
                    oneOf "\\"
                    c <- letter
                    return $ Character c

--  jarl: a list can span many lines and there may be comments interspersed. we need to account for that. 
parseList :: Parser LispVal
parseList = return List <*> (parseExpr <* ws) `sepBy` spacesAndComments

-- jarl: same, this may have comment interspersed
parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces1
                    tail <- char '.' >> spaces1 >> parseExpr
                    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]


parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
                    char '`'
                    x <- parseExpr
                    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                char ','
                x <- parseExpr
                return $ List [Atom "unquote", x]
--backtracking
parseBracketed :: Parser LispVal
parseBracketed = do
                    char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x


parseFloat :: Parser LispVal
parseFloat = do
                left <- many1 digit
                right <- ((:) <$> oneOf "." <*> many1 digit) <|> return ""
                return $ LispFloat (read (left ++ right))


-- parseExpr
parseExpr :: Parser LispVal
parseExpr =  ws >> (parseAtom 
                <|> parseString 
                <|> parseNumber 
                <|> parseCharacter
                <|> parseQuoted
                <|> parseBracketed
                <|> parseFloat
                <|> parseQuoted
                <|> parseQuasiQuoted
                <|> parseUnQuote) <* spacesAndComments

ym