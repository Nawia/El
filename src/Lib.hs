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

readExpr envRef input = do
    toks <- fromRight [] <$> runParserT (parseExpr envRef) () "guardDemo" input
    Lib.showList . map showExpr <$> evalAll toks where
        showExpr x = fst x ++ show (snd x)

showList :: Show a => [a] -> String
showList = unwords . map show

parseExpr :: Env -> ParsecT String () IO [Token]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

data Func = Func [Token] [Token] Env
          | Prim String
          
instance Show Func where
    show (Func args body _) = "(" ++ Lib.showList args ++ "): " ++ Lib.showList body
    show (Prim name) = name
    
parseFunc :: Env -> ParsecT String () IO Token
parseFunc envRef = many (noneOf " \t\n") >>= liftIO . getFunc envRef

type Token = (String, [Func])

type Env = IORef [(String, IORef [IORef Func])]
    
nullEnv :: IO Env
nullEnv = newIORef []

initEnv :: IO Env
initEnv = do
    env <- nullEnv
    bindFuncs env [("^[0-9]+$", [Prim "num"]),
                   ("^[+*/-]$",   [Prim "numop"])]

primitives :: [(String, [Token] -> [Token])]
primitives = [("num",   num),
              ("numop", nop),
              ("nil",   nop)]
              
num, nop :: [Token] -> [Token]
num ((arg1, [Prim "num"]) : (op, [Prim "numop"]) : (arg2, [Prim "num"]) : xs) =
    (show $ fromJust (lookup op numOps) (read arg1) (read arg2), [Prim "num"]) : xs
num args = args
nop args = args

numOps = [("+", (+)),
          ("-", (-)),
          ("*", (*)),
          ("/", div)]

nil :: IO [Func]
nil = return [Prim "nil"]

getFunc :: Env -> String -> IO Token
getFunc envRef label = do
    funcs <- find ((label =~) . fst) <$> readIORef envRef
    (,) label <$> maybe nil (unpack . snd) funcs where
        unpack funcs = readIORef funcs >>= mapM readIORef
    
bindFuncs :: Env -> [(String, [Func])] -> IO Env
bindFuncs envRef bindings = readIORef envRef >>= extendEnv >>= newIORef where
    extendEnv env = fmap (++ env) (mapM addBinding bindings)
    addBinding (name, values) = (,) name <$> (mapM newIORef values >>= newIORef)

evalAll :: [Token] -> IO [Token]
evalAll = foldrM (\x y -> eval <$> return (x : y)) []

eval :: [Token] -> [Token]
eval args@((_, [Prim func]) : _) = fromJust (lookup func primitives) args
