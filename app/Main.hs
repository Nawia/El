import El.Parser
import El.Environment
import Control.Monad (join)

main :: IO ()
main = join (readExpr <$> initEnv <*> getLine) >>= print
