module El.Evaluator (evalAll) where
import El.Data
import El.Environment
import Text.Read (readMaybe)
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Bifoldable (bifoldMap)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.List (lookup)
import Control.Monad (liftM2, join)

internalFuncs :: Env -> [(String, [Token] -> IO [Token])]
internalFuncs envRef = [("___BINOP___", binop envRef)]
                        --("___SET___", setFunc envRef)]
                        
binop :: Env -> [Token] -> IO [Token]
--binop _      args@(("___TYPE___", "___BINOP___", _) : (_, "___BLOCK)___", _)             : _)       = blockInsert args
--binop _      args@(("___TYPE___", "___BINOP___", _) : (_, "___BLOCK\"___", _)            : _)       = blockInsert args
binop envRef      ((Token ("___TYPE___", "___BINOP___", _)) : (Token (pattern, _, _)) : (Token (typeName, _, _)) : argTail) = return $ (Token (pattern, typeName, [])) : argTail
binop _      args@((Token ("___DIV___", "___BINOP___", _))  : (Token (_, "int", _))   : (Token (_, "int", _))    : _)       = numBinOp args "int"
binop _      args@((Token ("___DIV___", "___BINOP___", _))  : (Token (_, "int", _))   : (Token (_, "float", _))  : _)       = numBinOp args "float"
binop _      args@((Token ("___DIV___", "___BINOP___", _))  : (Token (_, "float", _)) : (Token (_, "int", _))    : _)       = numBinOp args "float"
binop _      args@((Token ("___DIV___", "___BINOP___", _))  : (Token (_, "float", _)) : (Token (_, "float", _))  : _)       = numBinOp args "float"
binop _           ((Token ("___IDIV___", "___BINOP___", _)) : (Token (_, "int", _))   : (Token ("0", "int", _))  : argTail) = return $ (Token ("div0", "err", [])) : argTail
binop _      args@((Token (op, "___BINOP___", _))           : (Token (_, "int", _))   : (Token (_, "int", _))    : _)       = numBinOp args "int"
binop _      args@((Token (op, "___BINOP___", _))           : (Token (_, "int", _))   : (Token (_, "float", _))  : _)       = numBinOp args "float"
binop _      args@((Token (op, "___BINOP___", _))           : (Token (_, "float", _)) : (Token (_, "int", _))    : _)       = numBinOp args "float"
binop _      args@((Token (op, "___BINOP___", _))           : (Token (_, "float", _)) : (Token (_, "float", _))  : _)       = numBinOp args "float"
binop envRef      args = anyFunc envRef args

numBinOp :: [Token] -> String -> IO [Token]
numBinOp args@((Token (op, _, _)) : (Token (arg1, _, _)) : (Token (arg2, _, _)) : argTail) opTypeName = return $ fromMaybe args $ do
    func <- lookup op $ binOps opTypeName
    val <- func arg1 arg2
    return $ val : argTail where
    binOps "int"   = binOps' intWrap ++ [("___IDIV___", intWrap div), ("___MOD___", intWrap mod)]
    binOps "float" = binOps' flWrap
    binOps' wrap = [("___ADD___", wrap (+)),
                    ("___SUB___", wrap (-)),
                    ("___MUL___", wrap (*)),
                    ("___DIV___", flWrap (/))]
    intWrap :: (Integer -> Integer -> Integer) -> String -> String -> Maybe Token
    intWrap f a b = Token <$> ((,,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "int" <*> return [])
    flWrap :: (Double -> Double -> Double) -> String -> String -> Maybe Token
    flWrap f a b = Token <$> ((,,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "float" <*> return [])

{-    
setFunc :: Env -> [Token] -> IO [Token]
setFunc envRef args@(("___SET___", "___SET___") : (funcName, _) : (typeName, _) : argTail@((_, "___\"BLOCK___") : _)) = fromMaybe (return args) $ do
    (funcArgs, rest) <- evalBlock envRef argTail
    checkArgs rest
    (funcBody, rest) <- evalBlock envRef rest
    Just $ do
        var <- (makeFunc <$> funcArgs <*> funcBody) >>= setVar envRef
        return $ unwrap var : rest
    where
        checkArgs args@((_, "___\"BLOCK___") : _) = Just args
        checkArgs _ = Nothing
        makeFunc funcArgs funcBody = (funcName, typeName, Func [(funcArgs, map makeFuncArg funcBody, envRef)]) where
            makeFuncArg tok@(funcName, "nil") = case lookup funcName $ ("___SELF___", "___FUNCARG___") : funcArgs of
                Just _  -> (funcName, "___FUNCARG___")
                Nothing -> tok
            makeFuncArg tok = tok
        unwrap (funcName, typeName, _) = (funcName, typeName)
setFunc _ args = return args
-}

anyFunc :: Env -> [Token] -> IO [Token]
--anyFunc envRef args@(("___(BLOCK___", "___(BLOCK___") : argTail@((_, "___BLOCK)___") : _)) = fromMaybe (return args) $ do
--    block <- evalBlock envRef argTail
--    Just $ bifoldMap id return block >>= evalAll envRef
--anyFunc _ args@(_ : (_, "___BLOCK)___") : _) = blockInsert args
--anyFunc _ (("___\"BLOCK___", "___\"BLOCK___") : (len, "___BLOCK\"___") : argTail) = return $ (len, "___\"BLOCK___") : argTail
--anyFunc _ args@(_ : (_, "___BLOCK\"___") : _) = blockInsert args
--anyFunc envRef ((funcName, "nil") : args@(("=", "=") : val : argTail)) = anyFunc envRef $ (funcName, funcName) : args
--anyFunc envRef (tok@(funcName, typeName) : ("=", "=") : val : argTail) = do
--    setVar envRef (funcName, typeName, Func [([], [val], envRef)])
--    return $ tok : argTail
anyFunc _ args = return args

--blockInsert :: [Token] -> IO [Token]
--blockInsert args@(arg1 : (len, typeName) : argTail) = return $ fromMaybe args $ do
--    val <- show <$> ((+ 1) <$> readMaybe len)
--    return $ (val, typeName) : arg1 : argTail
    
--evalBlock :: Env -> [Token] -> Maybe (IO [Token], [Token])
--evalBlock envRef ((len, _) : argTail) = do
--    val <- readMaybe len
--    Just (evalAll envRef (take val argTail), drop val argTail)
    
evalAll :: Env -> [Token] -> IO [Token]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

eval :: Env -> [Token] -> IO [Token]
eval envRef args = getVar (head args) >>= eval' where
    eval' (_, funcType, Func [])     = maybe (anyFunc envRef) ($) (lookup funcType $ internalFuncs envRef) args
    eval' (funcName, funcType, func) = case fetchFunc func $ tail args of
        Nothing                               -> anyFunc envRef args
        Just (Func [(_, [], _)], _, _)        -> return args
        Just (fetchedFunc, funcArgs, argTail) -> do
            (Func [(_, funcBody, funcEnvRef)]) <- buildFunc (funcName, funcType, fetchedFunc) funcArgs
            evalAll funcEnvRef funcBody >>= eval envRef . (++ argTail)
    getVar tok@(Token (funcName, typeName, _)) = (,,) funcName typeName <$> getFunc envRef tok
    
fetchFunc :: Func -> [Token] -> Maybe (Func, [Var], [Token])
fetchFunc (Func func) args = listToMaybe $ mapMaybe fetchArgs func where
    fetchArgs func@(funcArgs, _, funcEnv) = case fetchArgs' funcArgs [] args of
        Nothing                  -> Nothing
        Just (funcArgs, argTail) -> Just (Func [func], funcArgs, argTail)
        where
            fetchArgs' [] acum args                                                 = Just (reverse acum, args)
            fetchArgs' _ _ []                                                       = Nothing
            fetchArgs' ((Token (name, t1, _)) : funcArgs) acum (val@(Token (_, t2, _)) : args) = if t1 == t2
                then fetchArgs' funcArgs ((name, "___FUNCARG___", Func [([], [val], funcEnv)]) : acum) args
                else Nothing
                
buildFunc :: Var -> [Var] -> IO Func
buildFunc (funcName, funcType, Func [(_, funcBody, funcEnvRef)]) args = do
    newFuncEnvRef <- join $ bindVars <$> nullEnv <*> lsFuncs funcEnvRef
    setVar newFuncEnvRef ("___SELF___", "___FUNCARG___", Func [([], [(Token (funcName, funcType, []))], newFuncEnvRef)])
    mapM_ (setVar newFuncEnvRef) args
    return $ Func [([], funcBody, newFuncEnvRef)]
