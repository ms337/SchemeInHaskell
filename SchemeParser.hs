module SchemeParser (
    symbol,
    spaces,
   parseString,
    parseAtom,
    parseNumber,
    parseCharacter,
    parseList,
    parseDottedList,
    parseQuoted,
    parseBracketed,
    parseExpr
)where

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char hiding (spaces)
import System.Environment
import Data.IORef
import Control.Monad.Except
import Types

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- default Parsec spaces function just parses a whitespace character
spaces :: Parser ()
spaces = skipMany1 space

comments :: Parser LispVal
comments = do
            char ';'
            manyTill anyChar (try newline) 
            return Void

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
-- parseCharacter = oneOf "\\" >> oneOf "\\" >> letter >>= (\c -> Character c) 
parseCharacter = do
                    oneOf "#"
                    oneOf "\\"
                    c <- letter
                    return $ Character c

parseList :: Parser LispVal
parseList = return List <*> parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
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

-- parseExpr
parseExpr :: Parser LispVal
parseExpr =  comments 
                {->> (parseAtom 
                <|> parseString 
                <|> parseNumber 
                <|> parseCharacter
                <|> parseQuoted
                <|> parseBracketed
                <|> parseFloat
                <|> parseQuoted
                <|> parseQuasiQuoted
                <|> parseUnQuote) -}


parseFloat :: Parser LispVal
parseFloat = do
                left <- many1 digit
                right <- ((:) <$> oneOf "." <*> many1 digit) <|> return ""
                return $ LispFloat (read (left ++ right))


