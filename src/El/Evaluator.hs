module El.Evaluator (evalAll) where
import El.Data
import El.Environment
import Text.Read (readMaybe)
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Control.Monad (liftM2, join)

internalFuncs :: Env -> [(String, [Token] -> IO [Token])]
internalFuncs envRef = [("___BINOP___", binop envRef),
                        ("anyFunc",     anyFunc envRef)]
                        
binop :: Env -> [Token] -> IO [Token]
binop envRef     (("___TYPE___", "___BINOP___") : (pattern, _) : (typeName, _) : argTail) = (: argTail) <$> (newType envRef (pattern, typeName))
binop _      args@(("___DIV___", "___BINOP___") : (_, "int") : (_, "int") : _) = numBinOp args "int"
binop _      args@(("___DIV___", "___BINOP___") : (_, "int") : (_, "float") : _) = numBinOp args "float"
binop _      args@(("___DIV___", "___BINOP___") : (_, "float") : (_, "int") : _) = numBinOp args "float"
binop _      args@(("___DIV___", "___BINOP___") : (_, "float") : (_, "float") : _) = numBinOp args "float"
binop _           (("___IDIV___", "___BINOP___") : (_, "int") : ("0", "int") : argTail) = return $ ("div0", "err") : argTail
binop _      args@((op, "___BINOP___") : (_, "int") : (_, "int") : _) = numBinOp args "int"
binop _      args@((op, "___BINOP___") : (_, "int") : (_, "float") : _) = numBinOp args "float"
binop _      args@((op, "___BINOP___") : (_, "float") : (_, "int") : _) = numBinOp args "float"
binop _      args@((op, "___BINOP___") : (_, "float") : (_, "float") : _) = numBinOp args "float"
binop envRef      args = anyFunc envRef args

numBinOp :: [Token] -> String -> IO [Token]
numBinOp args@((op, _) : (arg1, _) : (arg2, _) : argTail) opTypeName = return $ fromMaybe args $ do
    func <- lookup op $ binOps opTypeName
    val <- func arg1 arg2
    return $ (val, opTypeName) : argTail where
    binOps "int"   = binOps' intWrap ++ [("___IDIV___", intWrap div), ("___MOD___", intWrap mod)]
    binOps "float" = binOps' flWrap
    binOps' wrap = [("___ADD___", wrap (+)),
                    ("___SUM___", wrap (-)),
                    ("___MUL___", wrap (*)),
                    ("___DIV___", flWrap (/))]
    intWrap :: (Integer -> Integer -> Integer) -> String -> String -> Maybe String
    intWrap f a b = show <$> (f <$> readMaybe a <*> readMaybe b)
    flWrap :: (Double -> Double -> Double) -> String -> String -> Maybe String
    flWrap f a b = show <$> (f <$> readMaybe a <*> readMaybe b)

anyFunc :: Env -> [Token] -> IO [Token]
anyFunc envRef ((funcName, "nil") : args@(("=", "=") : val : argTail)) = anyFunc envRef $ (funcName, funcName) : args
anyFunc envRef (tok@(funcName, typeName) : ("=", "=") : val : argTail) = do
    setVar envRef (funcName, typeName, Func [([], [val], envRef)])
    return $ tok : argTail
anyFunc _ args = return args

evalAll :: Env -> [Token] -> IO [Token]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

printEnv :: Env -> IO ()
printEnv env = lsFuncs env >>= putStrLn . unlines . map show

eval :: Env -> [Token] -> IO [Token]
eval envRef args = getVar envRef (head args) >>= eval' where
    eval' (_, funcType, Func [])     = maybe (anyFunc envRef) ($) (lookup funcType $ internalFuncs envRef) args
    eval' (funcName, funcType, func) = case fetchFunc func $ tail args of
        Nothing                               -> return args
        Just (Func [(_, [], _)], _, _)        -> return args
        Just (fetchedFunc, funcArgs, argTail) -> do
            (Func [(_, funcBody, funcEnvRef)]) <- buildFunc (funcName, funcType, fetchedFunc) funcArgs
            evalAll funcEnvRef funcBody >>= eval envRef . (++ argTail)
    getVar envRef tok@(funcName, typeName) = (,,) funcName typeName <$> getFunc envRef tok
    
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
                
buildFunc :: Var -> [Var] -> IO Func
buildFunc (funcName, funcType, Func [(funcArgs, funcBody, funcEnvRef)]) args = do
    newFuncEnvRef <- join $ bindVars <$> nullEnv <*> lsFuncs funcEnvRef
    setVar newFuncEnvRef ("___SELF___", "funcArg", Func [([], [(funcName, funcType)], newFuncEnvRef)])
    mapM_ (setVar newFuncEnvRef) args
    return $ Func [([], funcBody, newFuncEnvRef)]
