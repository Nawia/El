module El.Parser (runRepl, runFile, loadFile) where
import El.Data
import El.Environment
import El.Evaluator
import Text.Parsec
import System.IO
import Data.Either (fromRight)
import Control.Monad.Trans (liftIO)
import Control.Monad.Loops (iterateUntil)
import Paths_El (getDataFileName)

runRepl :: Env -> IO [Token]
runRepl envRef = iterateUntil cond $ readPrompt ">" >>= evalString envRef >>= (<$) <*> print where
    cond []   = False
    cond toks = (== "quit") . fst $ head toks
    
runFile :: Env -> [String] -> IO [Token]
runFile envRef args = getDataFileName (head args) >>= loadFile envRef

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

loadFile :: Env -> String -> IO [Token]
loadFile envRef filename = last <$> (lines <$> readFile filename >>= mapM (evalString envRef))

evalString :: Env -> String -> IO [Token]
evalString envRef expr = readExpr envRef expr >>= evalAll envRef

readExpr :: Env -> String -> IO [Token]
readExpr envRef input = fromRight [] <$> runParserT (parseExpr envRef) () "El" input

parseExpr :: Env -> ParsecT String () IO [Token]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

parseFunc :: Env -> ParsecT String () IO Token
parseFunc envRef = do
    funcName <- many1 (noneOf " \t\n")
    liftIO $ (,) funcName <$> getTypeName envRef funcName
