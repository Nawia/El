module El.Evaluator (evalAll) where
import El.Data
import El.Environment
import Text.Read (readMaybe)
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.List (find)
import Control.Monad (liftM2, join)

internalFuncs :: Env -> [(String, [Token] -> IO [Token])]
internalFuncs envRef = [("___BINOP___", binop envRef),
                        ("___SET___", setFunc envRef),
                        ("___ALIAS___", aliasFunc envRef)]
                        
binop :: Env -> [Token] -> IO [Token]
binop envRef args = case args of
    (Token "___TYPE___" _ _ : Token _ "___BLOCK___" _                : _)       -> anyFunc envRef args
    (Token "___TYPE___" _ _ : Token pattern _ _ : Token typeName _ _ : argTail) -> return $ Token pattern typeName [] : argTail
    (Token "___IDIV___" _ _ : Token _ "int" _   : Token "0" "int" _  : argTail) -> return $ Token "div0" "err" [] : argTail
    (Token op _ _           : Token _ "int" _   : Token _ "int" _    : _)       -> numBinOp args "int"
    (Token op _ _           : Token _ "int" _   : Token _ "float" _  : _)       -> numBinOp args "float"
    (Token op _ _           : Token _ "float" _ : Token _ "int" _    : _)       -> numBinOp args "float"
    (Token op _ _           : Token _ "float" _ : Token _ "float" _  : _)       -> numBinOp args "float"
    _                                                                           -> anyFunc envRef args
    
numBinOp :: [Token] -> String -> IO [Token]
numBinOp args@(Token op _ _ : Token arg1 _ _ : Token arg2 _ _ : argTail) opTypeName = return $ fromMaybe args $ do
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
    intWrap f a b = Token <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "int" <*> return []
    flWrap :: (Double -> Double -> Double) -> String -> String -> Maybe Token
    flWrap f a b = Token <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "float" <*> return []
    
setFunc :: Env -> [Token] -> IO [Token]
setFunc envRef args@(Token "___SET___" _ _ : Token funcName _ _ : Token typeName _ _ : Token "___\"BLOCK\"___" "___BLOCK___" funcArgs : Token "___\"BLOCK\"___" "___BLOCK___" funcBody : argTail) = do
    fArgs <- evalAll envRef funcArgs
    var <- setVar envRef (funcName, typeName, Func [(fArgs, map makeFuncArg funcBody, envRef)])
    return $ unwrap var : argTail where
        makeFuncArg tok@(Token argName "nil" _) = case find (isArg argName) $ Token "___SELF___" "___FUNCARG___" [] : funcArgs of
            Just _  -> Token argName "___FUNCARG___" []
            Nothing -> tok
        makeFuncArg tok = tok
        isArg argName (Token tokName _ _) = argName == tokName
        unwrap (funcName, typeName, _) = Token funcName typeName []
setFunc envRef args = anyFunc envRef args

aliasFunc :: Env -> [Token] -> IO [Token]
aliasFunc envRef args@(Token "___ALIAS___" _ _ : _) = case args of
    (_ : Token "___BLOCK)___" "___BLOCK___" [tok]  : argTail) -> aliasFunc' $ tok : Token "___BLOCK)___" "___BLOCK___" [] : argTail
    (_ : Token "___BLOCK\"___" "___BLOCK___" [tok] : argTail) -> aliasFunc' $ tok : Token "___BLOCK\"___" "___BLOCK___" [] : argTail
    (_ : Token funcName "nil" _ : val              : argTail) -> aliasFunc' $ Token funcName funcName [] : val : argTail
    (_ : tok                    : val              : argTail) -> aliasFunc' $ tok : val : argTail
    _                                            -> anyFunc envRef args
    where
        aliasFunc' (tok@(Token funcName typeName _) : val : argTail) = do
            setVar envRef (funcName, typeName, Func [([], [val], envRef)])
            return $ tok : argTail

anyFunc :: Env -> [Token] -> IO [Token]
anyFunc envRef args = case args of
    (_                               :          Token _ "___BLOCK___" _ : _) -> block envRef args
    _                                                                        -> return args

block :: Env -> [Token] -> IO [Token]
block envRef args = case args of
    (Token "___(BLOCK___" "___BLOCK___" _  : Token "___BLOCK)___" _ params  : argTail) -> ((++ argTail) <$> evalAll envRef params) >>= evalAll envRef
    (Token "___BLOCK)___" "___BLOCK___" _  : Token "___BLOCK)___" _ _       : _)       -> return args
    (_                                     : Token "___BLOCK)___" _ _       : _)       -> blockInsert args
    (Token "___\"BLOCK___" "___BLOCK___" _ : Token "___BLOCK\"___" _ params : argTail) -> return $ Token "___\"BLOCK\"___" "___BLOCK___" params : argTail
    (Token "___BLOCK\"___" "___BLOCK___" _ : Token "___BLOCK\"___" _ _      : _)       -> return args
    (_                                     : Token "___BLOCK\"___" _ _      : _)       -> blockInsert args
    _                                                                                  -> return args

blockInsert :: [Token] -> IO [Token]
blockInsert (arg1 : (Token funcName _ params) : argTail) = return $ Token funcName "___BLOCK___" (arg1 : params) : argTail

evalAll :: Env -> [Token] -> IO [Token]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

eval :: Env -> [Token] -> IO [Token]
eval envRef args = getVar (head args) >>= eval' where
    eval' (_, funcType, Func [])     = maybe (anyFunc envRef) ($) (lookup funcType $ internalFuncs envRef) args
    eval' (funcName, funcType, func) = case fetchFunc func $ tail args of
        Nothing                               -> anyFunc envRef args
        Just (Func [(_, [], _)], _, _)        -> anyFunc envRef args
        Just (fetchedFunc, funcArgs, argTail) -> do
            (Func [(_, funcBody, funcEnvRef)]) <- buildFunc (funcName, funcType, fetchedFunc) funcArgs
            evalAll funcEnvRef funcBody >>= eval envRef . (++ argTail)
    getVar tok@(Token funcName typeName _) = (,,) funcName typeName <$> getFunc envRef tok
    
fetchFunc :: Func -> [Token] -> Maybe (Func, [Var], [Token])
fetchFunc (Func func) args = listToMaybe $ mapMaybe fetchArgs func where
    fetchArgs func@(funcArgs, _, funcEnv) = case fetchArgs' funcArgs [] args of
        Nothing                  -> Nothing
        Just (funcArgs, argTail) -> Just (Func [func], funcArgs, argTail)
        where
            fetchArgs' [] acum args                                                  = Just (reverse acum, args)
            fetchArgs' _ _ []                                                        = Nothing
            fetchArgs' (Token name t1 _ : funcArgs) acum (val@(Token _ t2 _) : args) = if t1 == t2
                then fetchArgs' funcArgs ((name, "___FUNCARG___", Func [([], [val], funcEnv)]) : acum) args
                else Nothing
                
buildFunc :: Var -> [Var] -> IO Func
buildFunc (funcName, funcType, Func [(_, funcBody, funcEnvRef)]) args = do
    newFuncEnvRef <- join $ bindVars <$> nullEnv <*> lsFuncs funcEnvRef
    setVar newFuncEnvRef ("___SELF___", "___FUNCARG___", Func [([], [Token funcName funcType []], newFuncEnvRef)])
    mapM_ (setVar newFuncEnvRef) args
    return $ Func [([], funcBody, newFuncEnvRef)]
