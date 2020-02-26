import El.Environment
import El.Parser
import System.Environment (getArgs)
import Paths_El (getDataFileName)

main = do
    args <- getArgs
    envRef <- initEnv
    getDataFileName "std.el" >>= loadFile envRef 
    if null args then runRepl envRef else runFile envRef args
