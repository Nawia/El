module El.Parser (readExpr) where
import El.Data
import El.Environment
import El.Evaluation
import Text.Parsec
import Data.Either (fromRight)
import Control.Monad.Trans (liftIO)

readExpr :: Env -> String -> IO [Token]
readExpr envRef input = do
    toks <- fromRight [] <$> runParserT (parseExpr envRef) () "El" input
    evalAll toks
    
parseExpr :: Env -> ParsecT String () IO [Token]
parseExpr envRef = parseFunc envRef `sepBy` many1 (oneOf " \t\n")

parseFunc :: Env -> ParsecT String () IO Token
parseFunc envRef = many1 (noneOf " \t\n") >>= liftIO . matchFunc envRef
