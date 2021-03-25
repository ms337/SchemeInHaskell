module Evaluator (
    readExpr, 
    eval,
    trapError,
    extractValue,
    Env (..),
    liftThrows, 
    nullEnv, 
    runIOThrows,
    primitiveBindings,
    bindVars
) where


import SchemeParser
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Data.Char
import Data.String
import Control.Monad.Except
import Data.IORef
import Types
{------------
Evaluation
-------------}

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "lambda (" ++ unwords (map show args) ++ (case varargs of Nothing -> ""
                                                              Just arg -> " . " ++  arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

{-The purpose of an evaluator is to map some "code" data type into some "data" data type, the result of the evaluation-}

{-
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
-- missing vals?
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt])  = do
                                                result <- eval pred
                                                case result of 
                                                    Bool False -> eval alt
                                                    otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm
-}


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", form]) = evalUnquotes form
                                        where evalUnquotes form = 
                                                            case form of 
                                                                List [Atom "unquote", form] -> eval env form
                                                                List items -> do 
                                                                                results <- traverse evalUnquotes items
                                                                                return $ List results
                                                                _ -> return form
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    case result of
                                                        Bool False -> eval env alt
                                                        otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "define-syntax" : List (Atom var : params) : body)) = makeMacro env params body >>= defineVar env var
--eval env (List (Atom "define-syntax" : DottedList (var varags : params): body)) = makeMacro env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
--eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body )) = makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
                                        func <- eval env function
                                        case func of 
                                            Func {isMacro = True} -> apply func args >>= eval env
                                            _ -> mapM (eval env) args >>= apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

{-
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) (lookup func primitives)
-}
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func isMacro params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args




primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)), 
              ("/", numericBinop div),
              ("mod", numericBinop mod), 
              ("quotient", numericBinop quot), 
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
                ("<", numBoolBinop (<)),
                (">", numBoolBinop (>)),
                ("/=", numBoolBinop (/=)),
                (">=", numBoolBinop (>=)),
                ("<=", numBoolBinop (<=)),
                ("&&", boolBoolBinop (&&)),
                ("||", boolBoolBinop (||)),
                ("string=?", strBoolBinop (==)),
                ("string<?", strBoolBinop (<)),
                ("string>?", strBoolBinop (>)),
                ("string<=?", strBoolBinop (<=)),
                ("string>=?", strBoolBinop (>=)),
                ("car", car),
                ("cdr", cdr),
                ("cons", cons),
                ("eq?", eqv),
                ("eqv?", eqv),
                ("equal?", equal), 
                ("display", display)]
              -- ("symbol?", typeTest isSymbol),
              -- ("string?", typeTest isString),
              -- ("number?", typeTest isDouble) -- check this



boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                            else do
                                left <- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Character c) = return [c]
unpackStr (Number n) = return $ show n
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- weak type
unpackNum (String n) = let parsed = reads n in 
                            if null parsed then 
                                throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


car :: [LispVal]-> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ :xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg


cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs]= return $ List (x:xs)
cons [x, DottedList xs xLast] = return $ DottedList (x:xs) xLast
cons [x, x2] = return $ DottedList [x] x2
cons badArgList = throwError$ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = return $ Bool $ b1 == b2
eqv [(Number n1), (Number n2)] = return $ Bool $ n1 == n2
eqv [(String s1), (String s2)] = return $ Bool $ s1 == s2
eqv [(Atom a1), (Atom a2)]  = return $ Bool $ a1 == a2
eqv [(List arg1) , (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case (eqv [x1, x2]) of 
                                    Left err -> False
                                    Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList
   
display :: [LispVal] -> ThrowsError LispVal
display [val] = return $ val

{-
unpackString :: LispVal -> 
unpackString (String s) = s
unpackString (Atom s) = s
unpackString (Number n) = n
unpackString (Bool b) = b 


typeTest :: (a -> Bool) -> [LispVal] -> LispVal
typeTest pred (param:xs) = case (pred param) of
                            True -> Bool True
                            False -> Bool False
-}



-----
-----



showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values: \n" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


--In our program, we'll be converting all of our errors to their string representations and returning that as a normal value.
trapError action = catchError action (return . show)

--The result of calling trapError is another Either action which will always have valid (Right) data. We still need to extract that data from the Either monad so it can be passed around to other functions:
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
                        primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                                            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
                        eqvEquals <- eqv [arg1, arg2]
                        return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

makeFunc :: Bool -> Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc isMacro varargs env params body = return $ Func  isMacro (map showVal params) varargs body env

makeNormalFunc = makeFunc False Nothing
makeVarArgs = makeFunc False . Just . showVal
makeMacro = makeFunc True Nothing




---
---

-- Variables



-- use monads to simulate state in Haskell
-- State Monad: hide arbitrary state within the monad and pass it around behind the scenes

--- . For a simple top-level environment, we could get away with [(String, LispVal)], storing mappings from variable names to values. However, when we start dealing with function calls, these mappings become a stack of nested environments, arbitrarily deep

-- Instead, we use a feature called state threads, letting Haskell manage the aggregate state for us.

-- There are two flavors of state threads: the ST monad creates a stateful computation that can be executed as a unit, without the state escaping to the rest of the program. The IORef module lets you use stateful variables within the IO monad. Since our state has to be interleaved with IO anyway (it persists between lines in the REPL, and we will eventually have IO functions within the language itself), we'll be using IORefs



--type Env = IORef [(String, IORef LispVal)]
-- need IORefs for both the list itself and for individual values because there are two ways that the program can mutate the environment. 
    --  It might use set! to change the value of an individual variable, a change visible to any function that shares that environment (Scheme allows nested scopes, so a variable in an outer scope is visible to all inner scopes).
    
    -- or use define

-- IORefs can only be used within the IO monad ??

nullEnv :: IO Env
nullEnv = newIORef []

-- monad transformer that lets you combine the functionality of multiple monads

-- type IOThrowsError = ExceptT LispError IO


-- use lifting to bring values of the lower type IO into the combined Monad


-- need this because there is not function to bring a value pf tje untransformed upper type into the combined monad
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
--methods in typeclasses resolve based on the type of the expression, so throwError and return (members of MonadError and Monad, respectively) take on their IOThrowsError definitions

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
                        env <- liftIO $ readIORef envRef
                        maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
                            env <- liftIO $ readIORef envRef
                            maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env)
                            return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
                                alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                    then setVar envRef var value >> return value
                                    else liftIO $ do
                                                    valueRef <- newIORef value
                                                    env <- readIORef envRef
                                                    writeIORef envRef ((var, valueRef) : env)
                                                    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                        where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
                              addBinding (var, value) = do  ref <- newIORef value
                                                            return (var, ref)


-- Since Haskell has no global variables, we'll have to thread the environment through the evaluator as a parameter. While we're at it, we might as well add the set! and define special forms.


---
-- IO 
---
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]



primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
                where makeFunc constructor (var, func) = (var, constructor func)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _  = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

{-
Implementing the actual Scheme load function is a little tricky, because load can introduce bindings into the local environment. Apply, however, doesn't take an environment argument, and so there's no way for a primitive function (or any function) to do this. We get around this by implementing load as a special form: -}