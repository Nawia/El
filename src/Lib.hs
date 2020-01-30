module Lib (nullEnv, initEnv, readExpr) where
import Text.Parsec
import System.IO hiding (try)
import Data.IORef
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Regex.TDFA
import Data.List

main = join (readExpr <$> initEnv <*> getLine) >>= putStrLn

readExpr :: Env -> String -> IO String
readExpr envRef input = do
    output <- runParserT (parseExpr envRef) () "guardDemo" input
    return $ case output of
        Left err  -> "No match: " ++ show err
        Right val -> show val
        
parseExpr :: Env -> ParsecT String () IO [Func]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

data Func = Func { args :: [Func], body :: String, closure :: Env }

instance Show Func where
    show (Func args body _) = "Func(" ++ show args ++ "): " ++ body

parseFunc :: Env -> ParsecT String () IO Func
parseFunc envRef = many (noneOf " \t\n") >>= liftIO . getFunc envRef

type Env = IORef [(String, IORef Func)]

nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    env <- nullEnv
    bindFuncs env [("^[0-9]+$", Func [] "digit" env),
                   ("foo", Func [] "foo" env),
                   ("bar", Func [] "bar" env),
                   ("baz", Func [] "baz" env)]
                   
lsFunc :: Env -> IO [(String, Func)]
lsFunc envRef = readIORef envRef >>= mapM getF where
    getF (label, func) = (,) label <$> readIORef func

getFunc :: Env -> String -> IO Func
getFunc envRef label = do
    env <- readIORef envRef
    maybe (return $ Func [] ":(" envRef) (readIORef . snd) (find ((label =~) . fst) env)
        
defineFunc :: Env -> String -> [Func] -> String -> IO Func
defineFunc envRef label args body = do
    env <- readIORef envRef
    let func = Func args body envRef
    funcRef <- newIORef func
    writeIORef envRef $ (label, funcRef) : env
    return func
    
bindFuncs :: Env -> [(String, Func)] -> IO Env
bindFuncs envRef bindings = readIORef envRef >>= extendEnv >>= newIORef where
    extendEnv env = fmap (++ env) (mapM addBinding bindings)
    addBinding (name, value) = do
        ref <- newIORef value
        return (name, ref)
