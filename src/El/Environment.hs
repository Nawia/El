module El.Environment (nullEnv, initEnv, isBound, getFunc, matchFunc, lsFuncs, setFunc, bindFuncs) where
import El.Data
import Text.Regex.TDFA ((=~))
import Data.IORef
import Data.Maybe (isJust)
import Data.List (find)
import Data.Function (on)
import Control.Monad (liftM2)
import Control.Monad.Extra (notM)
import Control.Monad.Loops (takeWhileM, dropWhileM)

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    envRef <- newIORef []
    bindFuncs envRef [("^[0-9]+$", Prim "num" envRef),
                      ("^[+*/-]$", Prim "numop" envRef),
                      ("inc", Func [([("a", Prim "num" envRef)],
                                     [("a", Prim "nil" envRef), ("+", Prim "numop" envRef), ("1", Prim "num" envRef)],
                                     envRef)]),
                      ("=", Prim "assign" envRef)]
                      
nil :: IO Func
nil = Prim "nil" <$> nullEnv

isBound :: Env -> String -> IO Bool
isBound envRef name = isJust <$> findFunc (==) envRef name

getFunc, matchFunc :: Env -> String -> IO Token
(getFunc, matchFunc) = (getFunc' (==), getFunc' (=~)) where
    getFunc' matchOp envRef name = (,) name <$> (findFunc matchOp envRef name >>= maybe nil (unwrap . snd))
    unwrap funcRef = readIORef funcRef >>= unwrapFunc
    
findFunc :: (String -> String -> Bool) -> Env -> String -> IO (Maybe TokenRef)
findFunc matchOp envRef name = find (matchOp name . fst) <$> readIORef envRef

lsFuncs :: Env -> IO [Token]
lsFuncs envRef = readIORef envRef >>= mapM unwrapToken

setFunc :: Env -> Token -> IO Token
setFunc envRef (name, func) = findFunc (==) envRef name >>= maybe defineFunc (setFunc' . snd) >> return (name, func) where
    defineFunc = do
        env <- readIORef envRef
        funcRef <- wrapFunc func >>= newIORef
        writeIORef envRef ((name, funcRef) : env)
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
    
bindFuncs :: Env -> [Token] -> IO Env
bindFuncs envRef bindings = mapM_ (setFunc envRef) bindings >> return envRef