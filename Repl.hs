module Main where 
import System.IO
import System.Environment
import SchemeParser
import Evaluator
import Control.Monad
import Types


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: [String] -> IO ()
--runOne expr = primitiveBindings >>= flip evalAndPrint expr
runOne args = do 
                env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
                (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
    else action result >> until_ pred prompt action

{-
runRepl :: IO ()
runRepl = until_ (== ":q") (readPrompt "Lisp>>> ") evalAndPrint
-}


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (=="quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args