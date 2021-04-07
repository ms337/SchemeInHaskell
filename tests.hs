import Text.Parsec
import Evaluator
import SchemeParser

-- jarl: you can run this via "ghci tests.hs" in the shell, then "main" in the repl
main = do
  parseTest (spacesAndComments >> parseString) " ; comment\n\"hello world\""
  parseTest (spacesAndComments >> parseQuoted) " ; comment0\n   ; comment1\n   'quoted"
  parseTest parseList "1 ; first value\n 2 ; second value\n\n 3   \n ; comment"


{-
('this
 'is
  'a  ; third value
  'list)
   -}

