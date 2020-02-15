module Main where
import Lib
import Control.Monad (join)

main :: IO ()
main = join (readExpr <$> initEnv <*> getLine) >>= print
