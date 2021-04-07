import          Control.Monad
import          Data.Either

newtype Parser a = Parser (String -> Either String a) -- Some Error type

parse :: Parser a -> String -> Either String a
parse (Parser p) s = case (p s) of
                        Left err -> Left "Error" 
                        Right val -> Right val


instance Functor Parser where
    fmap f (Parser p) = Parser (\s -> case (p s) of
                                        Left err -> Left err
                                        Right val -> Right (f val))

instance Applicative Parser where
    pure s = Parser (\s -> (Right s))
    (Parser pF) <*> p = Parser (\s -> case (pF s) of
                                        Left err -> Left "Error"
                                        Right valF -> Right $ parse (valF <$> p))


instance Monad Parser where
    return a = Parser (\s -> Right s)
    p >>= f = Parser (\s -> case (parse s) of
                            Left err -> Left "Error"
                            Right val -> Right (f val))
                
-- Binding operator in Parsec means "Attempt to match the first parser, then attempt to match the second with the remaining input, and fail if either fails."                          

