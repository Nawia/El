module El.Evaluator (primitives, evalAll, eval) where
import El.Data
import El.Environment
import Text.Read (readMaybe)
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Control.Monad (liftM2, join)

primitives :: [(String, [(String, String)] -> IO [(String, String)])]
primitives = [("___BINOP___",   binop)]
              
binop :: [(String, String)] -> IO [(String, String)]
binop args@((op, "___BINOP___") : (arg1, "num") : (arg2, "num") : argTail) =
    return $ fromMaybe args $ do
        func <- lookup op binOps
        val <- func arg1 arg2
        return $ (val, "num") : argTail
binop args = return args

binOps :: [(String, String -> String -> Maybe String)]
binOps = [("___SUM___", numBinOp (+)),
          ("___DIF___", numBinOp (-)),
          ("___MUL___", numBinOp (*)),
          ("___DIV___", numBinOp (/))]

numBinOp :: (Double -> Double -> Double) -> String -> String -> Maybe String
numBinOp f a b = show <$> (f <$> readMaybe a <*> readMaybe b)

anyFunc :: [(String, String)] -> IO [(String, String)]
anyFunc args = return args
          
evalAll :: Env -> [(String, String)] -> IO [(String, String)]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

printEnv :: Env -> IO ()
printEnv env = lsFuncs env >>= putStrLn . unlines . map show

eval :: Env -> [(String, String)] -> IO [(String, String)]
eval envRef args = getTok envRef (head args) >>= flip eval' args where
    eval' (_, _, Prim func _) args                 = maybe anyFunc ($) (lookup func primitives) args
    eval' (funcName, funcType, func@(Func _)) args = case fetchFunc func $ tail args of
        Nothing                                        -> return args
        Just (fetchedFunc@(Func _), funcArgs, argTail) -> do
            f@(Func [(_, fB, fE)]) <- buildFunc (funcName, funcType, fetchedFunc) funcArgs
            y <- evalAll fE fB
            eval envRef (y ++ argTail)
    getTok envRef (name, typename) = do
        (_, _, func) <- matchFunc envRef name
        return $ (,,) name typename func
        
buildFunc :: Token -> [Token] -> IO Func
buildFunc (funcName, funcType, Func [(funcArgs, funcBody, funcEnv)]) args = do
    closure <- join $ bindFuncs <$> newIORef [] <*> lsFuncs funcEnv
    setFunc closure ("___SELF___", "funcArg", Func [([], [(funcName, funcType)], closure)])
    mapM_ (setFunc closure) args
    return $ Func [([], funcBody, closure)] where
    mergeF (name, typeName) func = (name, typeName, func)
    
fetchFunc :: Func -> [(String, String)] -> Maybe (Func, [Token], [(String, String)])
fetchFunc (Func func) args = listToMaybe $ mapMaybe (`fetchArgs` args) func where
    fetchArgs func@(funcArgs, _, funcEnv) args = case fetchArgs' funcArgs [] args of
        Nothing               -> Nothing
        Just (fArgs, argTail) -> Just (Func [func], fArgs, argTail)
        where
            fetchArgs' :: [(String, String)] -> [Token] -> [(String, String)] -> Maybe ([Token], [(String, String)])
            fetchArgs' [] acum args                                 = Just (reverse acum, args)
            fetchArgs' _ _ []                                       = Nothing
            fetchArgs' ((name, t1) : funcArgs) acum (val@(_, t2) : args) = if t1 == t2
                then fetchArgs' funcArgs ((name, "funcArg", Func [([], [val], funcEnv)]) : acum) args
                else Nothing
