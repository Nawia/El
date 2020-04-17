module El.Environment (nullEnv, initEnv, getTypeName, getFunc, lsFuncs, newType, setVar, bindVars) where
import El.Data
import Text.Regex.TDFA ((=~))
import Data.IORef
import Data.Maybe (isJust)
import Data.List (find)
import Data.Tuple.Select (sel1, sel2)
import Data.Function (on)
import Control.Monad (liftM2)
import Control.Monad.Extra (notM, maybeM)
import Control.Monad.Loops (takeWhileM, dropWhileM)

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    envRef <- newIORef []
    bindVars envRef [("___(ADD|SUB|MUL|DIV|IDIV|MOD)___", "___MATHFUNC___", Func []),
                     ("___(EQ|NEQ|LS|LQ|GT|GQ)___", "___CMPFUNC___", Func []),
                     ("___TYPE___", "___TYPE___", Func []),
                     ("___SET___", "___SET___", Func []),
                     ("___ALIAS___", "___ALIAS___", Func []),
                     ("___\\(BLOCK___", "___(BLOCK___", Func []),
                     ("___BLOCK\\)___", "___BLOCK)___", Func [([("a", "___BLOCK\"___")], [], envRef),
                                                              ([], [("0", "___BLOCK)___")], envRef)]),
                     ("___\"BLOCK___", "___\"BLOCK___", Func []),
                     ("___BLOCK\"___", "___BLOCK\"___", Func [([], [("0", "___BLOCK\"___")], envRef)])]
                     
nil :: IO Func
nil = return $ Func []

nilVar :: Token -> IO Var
nilVar (funcName, typeName) = (,,) funcName typeName <$> nil

getTypeName :: Env -> String -> IO String
getTypeName envRef funcName = maybe "nil" unwrap <$> (find (matchFunc funcName) <$> readIORef envRef) where
    unwrap (_, typeName, _) = typeName
    
getFunc :: Env -> Token -> IO Func
getFunc envRef (funcName, typeName) = maybeM nil unwrap findVar where
    findVar = find (matchFunc funcName) . filter ((== typeName) . sel2) <$> readIORef envRef
    unwrap (_, _, funcRef) = readIORef funcRef >>= unwrapFunc
    
matchFunc :: String -> VarRef -> Bool
matchFunc key (funcName, _, _) = key =~ ('^' : funcName ++ "$")

lsFuncs :: Env -> IO [Var]
lsFuncs envRef = readIORef envRef >>= mapM unwrapVar

newType :: Env -> Token -> IO Token
newType envRef tok = nilVar tok >>= setVar envRef >> return tok

setVar :: Env -> Var -> IO Var
setVar envRef var@(pattern, typeName, func) = findVar >>= maybe defineVar setFunc >> return var where
    findVar = find ((== pattern) . sel1) <$> readIORef envRef
    defineVar = do
        env <- readIORef envRef
        varRef <- wrapVar var
        writeIORef envRef (varRef : env)
    setFunc (_, _, oldFuncRef) = do
        oldFunc <- readIORef oldFuncRef
        funcRef <- wrapFunc func
        concatFuncRefs funcRef oldFunc >>= writeIORef oldFuncRef
    concatFuncRefs (FuncRef ref1) (FuncRef ref2) = concatFuncRefs' (ref1, ref2, [])
    concatFuncRefs' ([], ref2, acum) = return . FuncRef $ acum ++ ref2
    concatFuncRefs' (r1 : ref1, ref2, acum) = do
        (a, b) <- breakM (`funcRefEq` r1) ref2
        concatFuncRefs' $ if null b
            then (ref1, ref2, r1 : acum)
            else (ref1, a ++ r1 : tail b, acum)
    breakM p = spanM $ notM <$> p
    spanM p xs = (,) <$> takeWhileM p xs <*> dropWhileM p xs
    funcRefEq = liftM2 (==) `on` (unwrapFunc . FuncRef . pure)
    
bindVars :: Env -> [Var] -> IO Env
bindVars envRef bindings = mapM_ (setVar envRef) (reverse bindings) >> return envRef
