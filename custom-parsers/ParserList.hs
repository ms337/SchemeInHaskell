import           Control.Monad
import           Control.Applicative
import           Data.Char

newtype Parser a = Parser (String -> [(a, String)])


item :: Parser Char
item = Parser (\cs -> case cs of 
                        "" -> []
                        (c:cs) -> [(c, cs)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p


instance Functor Parser where
    fmap f (Parser p) = Parser newFunc
        where 
            newFunc str = [(f c, cs) | (c, cs)  <- p str]



instance Applicative Parser where
    pure s = Parser (\str -> [(s, str)])
    (Parser pF) <*> p = Parser (\str ->  concat [parse (cF <$> p) cs | (cF, cs) <- pF str])
                        


instance Alternative Parser where
    empty = Parser (\cs -> [])
    (Parser p1) <|> (Parser p2) = Parser (\ str -> p1 str ++ p2 str)


instance Monad Parser where 
    return a = Parser(\s -> [(a, s)])
    p >>= f = Parser (\s -> concat [parse (f a) s' | (a, s') <- parse p s])

{-- instance MonadZero Parser where
    zero = Parser (\cs -> [])

instance MonadPlus Parser where
    p ++ q = Parser (\cs -> parse p cs ++ parse q cs) --}

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs) 

-- only return the first parser which succeeds
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of 
                                [] -> []
                                (x:xs) -> [x])

parser3Char :: Parser (Char, Char)
parser3Char = do {c <- item; item; d <- item; return (c, d)}

sat :: (Char -> Bool) -> Parser Char
sat pred = do {c <- item; if pred c then return c else mzero}

char :: Char -> Parser Char
char c = sat (c ==) 





--Recursion Combinators

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}


pmany :: Parser a -> Parser [a]
pmany p = many1p p +++ return []

many1p :: Parser a -> Parser [a]
many1p p = do {a <- p; as <- pmany p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep ) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep  = do {a <- p; as <- pmany (do {sep; p}); return (a:as)}

chain1 :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chain1 p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a}
                    where 
                        rest a = (do f <- op
                                     b <- p
                                     rest (f a b))
                                +++ return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

parseInt :: Parser Int
parseInt = read <$> many1p (sat isDigit) 

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = int +++ do {symb "("; n <- expr; symb ")"; return n}
int  = token parseInt


addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}