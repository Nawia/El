module El.Evaluator (evalAll) where
import El.Data
import El.Environment
import Text.Read (readMaybe)
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Control.Monad (liftM2, join)

internalFuncs :: [(String, [Token] -> IO [Token])]
internalFuncs = [("___BINOP___",   binop)]

binop :: [Token] -> IO [Token]
binop args@((op, "___BINOP___") : (arg1, "num") : (arg2, "num") : argTail) =
    return $ fromMaybe args $ do
        func <- lookup op binOps
        val <- func arg1 arg2
        return $ (val, "num") : argTail
binop args = return args

binOps :: [(String, String -> String -> Maybe String)]
binOps = [("___ADD___", numBinOp (+)),
          ("___SUM___", numBinOp (-)),
          ("___MUL___", numBinOp (*)),
          ("___DIV___", numBinOp (/))]
          
numBinOp :: (Double -> Double -> Double) -> String -> String -> Maybe String
numBinOp f a b = show <$> (f <$> readMaybe a <*> readMaybe b)

anyFunc :: [Token] -> IO [Token]
anyFunc args = return args

evalAll :: Env -> [Token] -> IO [Token]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

printEnv :: Env -> IO ()
printEnv env = lsFuncs env >>= putStrLn . unlines . map show

eval :: Env -> [Token] -> IO [Token]
eval envRef args = getVar envRef (head args) >>= eval' where
    eval' (_, funcType, Func [])     = maybe anyFunc ($) (lookup funcType internalFuncs) args
    eval' (funcName, funcType, func) = case fetchFunc func $ tail args of
        Nothing                               -> return args
        Just (Func [(_, [], _)], _, _)        -> return args
        Just (fetchedFunc, funcArgs, argTail) -> do
            (Func [(_, funcBody, funcEnvRef)]) <- buildFunc (funcName, funcType, fetchedFunc) funcArgs
            evalAll funcEnvRef funcBody >>= eval envRef . (++ argTail)
    getVar envRef tok@(name, typeName) = (,,) name typeName <$> getFunc envRef tok
    
buildFunc :: Var -> [Var] -> IO Func
buildFunc (funcName, funcType, Func [(funcArgs, funcBody, funcEnvRef)]) args = do
    newFuncEnvRef <- join $ bindVars <$> nullEnv <*> lsFuncs funcEnvRef
    setVar newFuncEnvRef ("___SELF___", "funcArg", Func [([], [(funcName, funcType)], newFuncEnvRef)])
    mapM_ (setVar newFuncEnvRef) args
    return $ Func [([], funcBody, newFuncEnvRef)]
    
fetchFunc :: Func -> [Token] -> Maybe (Func, [Var], [Token])
fetchFunc (Func func) args = listToMaybe $ mapMaybe fetchArgs func where
    fetchArgs func@(funcArgs, _, funcEnv) = case fetchArgs' funcArgs [] args of
        Nothing                  -> Nothing
        Just (funcArgs, argTail) -> Just (Func [func], funcArgs, argTail)
        where
            fetchArgs' [] acum args                                      = Just (reverse acum, args)
            fetchArgs' _ _ []                                            = Nothing
            fetchArgs' ((name, t1) : funcArgs) acum (val@(_, t2) : args) = if t1 == t2
                then fetchArgs' funcArgs ((name, "funcArg", Func [([], [val], funcEnv)]) : acum) args
                else Nothing
