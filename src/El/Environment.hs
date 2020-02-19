module El.Environment (nullEnv, initEnv, getTypeName, getFunc, matchFunc, lsFuncs, setFunc, bindFuncs) where
import El.Data
import Text.Regex.TDFA ((=~))
import Data.IORef
import Data.Maybe (isJust)
import Data.List (find)
import Data.Function (on)
import Control.Monad (liftM2)
import Control.Monad.Extra (notM, maybeM)
import Control.Monad.Loops (takeWhileM, dropWhileM)

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    envRef <- newIORef []
    bindFuncs envRef [("___(SUM|DIF|MUL|DIV)___", "___BINOP___", Prim "___BINOP___" envRef),
                      ("\\+", "binop", Func [([], [("___SUM___", "___BINOP___")], envRef)]),
                      ("-", "binop", Func [([], [("___DIF___", "___BINOP___")], envRef)]),
                      ("\\*", "binop", Func [([], [("___MUL___", "___BINOP___")], envRef)]),
                      ("/", "binop", Func [([], [("___DIV___", "___BINOP___")], envRef)]),
                      ("-?[0-9]+(\\.[0-9]+)?", "num", Func [([("op", "___BINOP___"), ("arg2", "num")],
                                                             [("op", "funcArg"), ("___SELF___", "funcArg"), ("arg2", "funcArg")],
                                                             envRef)]),
                      ("inc", "inc", Func [([("a", "num")],
                                            [("___SUM___", "___BINOP___"), ("a", "nil"), ("1", "num")],
                                            envRef)]),
                      ("=", "assign", Prim "assign" envRef)]
                      
nil :: IO Func
nil = Prim "nil" <$> nullEnv

nilToken :: String -> String -> IO Token
nilToken name typeName = (,,) name typeName <$> nil

getTypeName :: Env -> String -> IO String
getTypeName envRef funcName = maybe "nil" unwrap <$> findFunc matchF envRef funcName where
    matchF :: String -> TokenRef -> Bool
    matchF key (name, _, _) = key =~ ('^' : name ++ "$")
    unwrap (_, typeName, _) = typeName

getFunc :: Env -> String -> IO Token
getFunc envRef typeName = maybeM (nilToken "unknown_type" typeName) unwrapToken (findFunc matchF envRef typeName) where
    matchF key (_, typeName, _) = key == typeName

matchFunc :: Env -> String -> IO Token
matchFunc envRef funcName = maybeM (nilToken funcName "nil") unwrap (findFunc matchF envRef funcName) where
    matchF :: String -> TokenRef -> Bool
    matchF key (name, _, _) = key =~ ('^' : name ++ "$")
    unwrap (_, typeName, funcRef) = unwrapToken (funcName, typeName, funcRef)

findFunc :: (String -> TokenRef -> Bool) -> Env -> String -> IO (Maybe TokenRef)
findFunc matchF envRef key = find (matchF key) <$> readIORef envRef

lsFuncs :: Env -> IO [Token]
lsFuncs envRef = readIORef envRef >>= mapM unwrapToken

setFunc :: Env -> Token -> IO Token
setFunc envRef (name, typeName, func) = findFunc matchF envRef name >>= maybe defineFunc (setFunc' . unwrap) >> return (name, typeName, func) where
    defineFunc = do
        env <- readIORef envRef
        token <- wrapToken (name, typeName, func)
        writeIORef envRef (token : env)
    setFunc' oldFuncRef = do
        oldFunc <- readIORef oldFuncRef
        funcRef <- wrapFunc func
        concatFuncRef funcRef oldFunc >>= writeIORef oldFuncRef
    concatFuncRef (FuncRef ref1) (FuncRef ref2) = concatFuncRef' (ref1, ref2, [])
    concatFuncRef ref1 _ = return ref1
    concatFuncRef' ([], ref2, acum) = return . FuncRef $ acum ++ ref2
    concatFuncRef' (r1 : ref1, ref2, acum) = do
        (a, b) <- breakM (`funcRefEq` r1) ref2
        concatFuncRef' $ if null b
            then (ref1, ref2, r1 : acum)
            else (ref1, a ++ r1 : tail b, acum)
    breakM p = spanM $ notM <$> p
    spanM p xs = (,) <$> takeWhileM p xs <*> dropWhileM p xs
    funcRefEq = liftM2 (==) `on` (unwrapFunc . FuncRef . (:[]))
    matchF key (name, _, _) = key == name
    unwrap (_, _, func) = func
    
bindFuncs :: Env -> [Token] -> IO Env
bindFuncs envRef bindings = mapM_ (setFunc envRef) (reverse bindings) >> return envRef