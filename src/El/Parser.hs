module El.Parser (runRepl) where
import El.Data
import El.Environment
import El.Evaluator
import Text.Parsec
import System.IO
import Data.Either (fromRight)
import Control.Monad.Trans (liftIO)
import Control.Monad.Loops (iterateUntil)

runRepl :: Env -> IO [(String, String)]
runRepl envRef = iterateUntil ((== "quit") . fst . head) (readPrompt ">" >>= evalString envRef)

readPrompt :: String -> IO String
readPrompt prompt = putStr prompt >> hFlush stdout >> getLine

evalString :: Env -> String -> IO [(String, String)]
evalString envRef expr = readExpr envRef expr >>= evalAll envRef >>= (<$) <*> print

readExpr :: Env -> String -> IO [(String, String)]
readExpr envRef input = fromRight [] <$> runParserT (parseExpr envRef) () "El" input
    
parseExpr :: Env -> ParsecT String () IO [(String, String)]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

parseFunc :: Env -> ParsecT String () IO (String, String)
parseFunc envRef = do
    name <- many1 (noneOf " \t\n")
    liftIO $ (,) name <$> getTypeName envRef name