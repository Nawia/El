module Lib (nullEnv, initEnv, readExpr) where
import System.IO hiding (try)
import Text.Parsec
import Text.Regex.TDFA
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Data.Maybe
import Data.Either (fromRight)
import Data.List
import Data.Foldable (foldrM)
import Data.Functor.Classes (liftEq)
import Data.Function (on)
import Control.Monad.Extra (notM)
import Control.Monad.Loops

readExpr :: Env -> String -> IO String
readExpr envRef input = do
    toks <- fromRight [] <$> runParserT (parseExpr envRef) () "El" input
    unwords . map showExpr <$> evalAll toks where
        showExpr x = show $ fst x ++ " " ++ show (snd x)
        
parseExpr :: Env -> ParsecT String () IO [Token]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

data Func = Func [([Token], [Token], Env)]
          | Prim String
          
instance Show Func where
    show (Func func) = unwords $ map showFunc func where
        showFunc (args, body, _) = "(" ++ show args ++ "): " ++ show body
    show (Prim name) = name
    
instance Eq Func where
    (Func func1) == (Func func2) = liftEq eqFunc func1 func2 where
        eqFunc (args1, _, _) (args2, _, _) = liftEq eqArgs args1 args2
        eqArgs (_, arg1) (_, arg2) = arg1 == arg2
    (Prim func1) == (Prim func2) = func1 == func2
    _ == _                       = False
    
parseFunc :: Env -> ParsecT String () IO Token
parseFunc envRef = many1 (noneOf " \t\n") >>= liftIO . matchFunc envRef

type Token = (String, Func)

data FuncRef = FuncRef [IORef ([Token], [Token], Env)]
             | PrimRef String
             
type TokenRef = (String, IORef FuncRef)

type Env = IORef [TokenRef]

printEnv :: Env -> IO ()
printEnv env = lsFuncs env >>= putStrLn . unlines . map show

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    env <- nullEnv
    bindFuncs env [("^[0-9]+$", Prim "num"),
                   ("^[+*/-]$", Prim "numop"),
                   ("inc", Func [([("a", Prim "num")],
                                  [("a", Prim "nil"), ("+", Prim "numop"), ("1", Prim "num")],
                                  env)])]
                                  
primitives :: [(String, [Token] -> [Token])]
primitives = [("num",   num),
              ("numop", nop)]
              
num, nop :: [Token] -> [Token]
num ((arg1, Prim "num") : (op, Prim "numop") : (arg2, Prim "num") : xs) =
    (show $ fromJust (lookup op numOps) (read arg1) (read arg2), Prim "num") : xs
num args = args
nop args = args

numOps :: [(String, Integer -> Integer -> Integer)]
numOps = [("+", (+)),
          ("-", (-)),
          ("*", (*)),
          ("/", div)]
          
nil :: IO Func
nil = return $ Prim "nil"

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
setFunc envRef (name, func) = findFunc (==) envRef name >>= maybe defineFunc (setFunc' . snd) >>
    return (name, func) where
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
bindFuncs envRef bindings = readIORef envRef >>= extendEnv >>= newIORef where
    extendEnv env = fmap (++ env) (mapM wrapToken bindings)
    
wrapToken :: Token -> IO TokenRef
wrapToken (name, func) = (,) name <$> (wrapFunc func >>= newIORef)

unwrapToken :: TokenRef -> IO Token
unwrapToken (name, funcRef) = (,) name <$> (readIORef funcRef >>= unwrapFunc)

wrapFunc :: Func -> IO FuncRef
wrapFunc (Func func) = FuncRef <$> mapM newIORef func
wrapFunc (Prim func) = return $ PrimRef func

unwrapFunc :: FuncRef -> IO Func
unwrapFunc (FuncRef func) = Func <$> mapM readIORef func
unwrapFunc (PrimRef func) = return $ Prim func

evalAll :: [Token] -> IO [Token]
evalAll = foldrM (\x y -> eval $ x : y) []

eval :: [Token] -> IO [Token]
eval args@((_, Prim func) : _) = return $ maybe args ($ args) (lookup func primitives)
