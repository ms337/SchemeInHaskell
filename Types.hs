module Types where

import System.IO
import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)] -- change to Map
type IOThrowsError = ExceptT LispError IO
type ThrowsError = Either LispError

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | LispFloat Double
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {isMacro:: Bool, params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Void

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String