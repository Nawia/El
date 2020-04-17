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
internalFuncs envRef = [("___MATHFUNC___", mathFunc envRef),
                        ("___CMPFUNC___",  cmpFunc envRef),
                        ("___TYPE___",     typeFunc envRef),
                        ("___SET___",      setFunc envRef),
                        ("___ALIAS___",    aliasFunc envRef)]
                        
mathFunc :: Env -> [Token] -> IO [Token]
mathFunc envRef args = case args of
    (("___IDIV___", _) : (_, "int")   : ("0", "int")  : argTail) -> return $ ("div0", "err") : argTail
    (("___MOD___", _)  : (_, "int")   : ("0", "int")  : argTail) -> return $ ("div0", "err") : argTail
    (_                 : (_, "int")   : (_, "int")    : _)       -> mathF "int" args
    (_                 : (_, "int")   : (_, "float")  : _)       -> mathF "float" args
    (_                 : (_, "float") : (_, "int")    : _)       -> mathF "float" args
    (_                 : (_, "float") : (_, "float")  : _)       -> mathF "float" args
    _                                                            -> anyFunc envRef args

mathF :: String -> [Token] -> IO [Token]
mathF opTypeName args@((op, _) : (arg1, _) : (arg2, _) : argTail) = return $ fromMaybe args $ do
    func <- lookup op $ mathOps opTypeName
    val <- func arg1 arg2
    return $ val : argTail where
    mathOps "int"   = mathOps' intWrap ++ [("___IDIV___", intWrap div), ("___MOD___", intWrap mod)]
    mathOps "float" = mathOps' flWrap
    mathOps' wrap = [("___ADD___", wrap (+)),
                     ("___SUB___", wrap (-)),
                     ("___MUL___", wrap (*)),
                     ("___DIV___", flWrap (/))]
    intWrap :: (Integer -> Integer -> Integer) -> String -> String -> Maybe Token
    intWrap f a b = (,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "int"
    flWrap :: (Double -> Double -> Double) -> String -> String -> Maybe Token
    flWrap f a b = (,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "float"
    
cmpFunc :: Env -> [Token] -> IO [Token]
cmpFunc envRef args = case args of
    (_ : (_, "int")   : (_, "int")   : _) -> cmpF "int" args
    (_ : (_, "int")   : (_, "float") : _) -> cmpF "float" args
    (_ : (_, "float") : (_, "int")   : _) -> cmpF "float" args
    (_ : (_, "float") : (_, "float") : _) -> cmpF "float" args
    _                                      -> anyFunc envRef args

cmpF :: String -> [Token] -> IO [Token]
cmpF opTypeName args@((op, _) : (arg1, _) : (arg2, _) : argTail) = return $ fromMaybe args $ do
    func <- lookup op $ cmpOps opTypeName
    val <- func arg1 arg2
    return $ val : argTail where
    cmpOps "int"   = cmpOps' intWrap
    cmpOps "float" = cmpOps' flWrap
    cmpOps' wrap = [("___EQ___",  wrap (==)),
                    ("___NEQ___", wrap (/=)),
                    ("___LS___",  wrap (<)),
                    ("___LQ___",  wrap (<=)),
                    ("___GT___",  wrap (>)),
                    ("___GQ___",  wrap (>=))]
    intWrap :: (Integer -> Integer -> Bool) -> String -> String -> Maybe Token
    intWrap f a b = (,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "bool"
    flWrap :: (Double -> Double -> Bool) -> String -> String -> Maybe Token
    flWrap f a b = (,) <$> (show <$> (f <$> readMaybe a <*> readMaybe b)) <*> return "bool"

typeFunc :: Env -> [Token] -> IO [Token]
typeFunc envRef args = case args of
    (("___TYPE___", _) : (_, "___BLOCK)___")          : _)       -> blockInsert args
    (("___TYPE___", _) : (_, "___BLOCK\"___")         : _)       -> blockInsert args
    (("___TYPE___", _) : (pattern, _) : (typeName, _) : argTail) -> return $ (pattern, typeName) : argTail
    _                                                            -> anyFunc envRef args
    
setFunc :: Env -> [Token] -> IO [Token]
setFunc envRef args@(("___SET___", _) : (funcName, _) : (typeName, _) : argTail@((_, "___\"BLOCK___") : _)) = fromMaybe (return args) $ do
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
setFunc envRef args = anyFunc envRef args

aliasFunc :: Env -> [Token] -> IO [Token]
aliasFunc envRef args@(("___ALIAS___", _) : _) = case args of
    (_ : ("1", "___BLOCK)___")  : tok : argTail) -> aliasFunc' $ tok : ("___BLOCK)___", "___BLOCK)___") : argTail
    (_ : ("1", "___BLOCK\"___") : tok : argTail) -> aliasFunc' $ tok : ("___BLOCK\"___", "___BLOCK\"___") : argTail
    (_ : (funcName, "nil")      : val : argTail) -> aliasFunc' $ (funcName, funcName) : val : argTail
    (_ : tok                    : val : argTail) -> aliasFunc' $ tok : val : argTail
    _                                            -> anyFunc envRef args
    where
        aliasFunc' (tok@(funcName, typeName) : val : argTail) = do
            setVar envRef (funcName, typeName, Func [([], [val], envRef)])
            return $ tok : argTail

anyFunc :: Env -> [Token] -> IO [Token]
anyFunc envRef args = case args of
    (_ : (_, "___BLOCK)___")  : _) -> blockParen args
    (_ : (_, "___BLOCK\"___") : _) -> blockQuote args
    _                              -> return args
    where
        blockParen args@(("___(BLOCK___", "___(BLOCK___") : argTail) = fromMaybe (return args) $ do
            (block, rest) <- evalBlock envRef argTail
            Just $ block >>= flip evalTail rest
        blockParen args                                              = blockInsert args
        evalTail block (arg1@(_, "___BLOCK)___") : argTail) = (++) <$> evalAll envRef (block ++ [arg1]) <*> return argTail
        evalTail block rest                                 = evalAll envRef $ block ++ rest
        blockQuote ((_, "___\"BLOCK___") : (len, _) : argTail) = return $ (len, "___\"BLOCK___") : argTail
        blockQuote args                                        = blockInsert args

blockInsert :: [Token] -> IO [Token]
blockInsert args@(arg1 : (len, typeName) : argTail) = return $ fromMaybe args $ do
    val <- show <$> ((+ 1) <$> readMaybe len)
    return $ (val, typeName) : arg1 : argTail
    
evalBlock :: Env -> [Token] -> Maybe (IO [Token], [Token])
evalBlock envRef ((len, _) : argTail) = do
    val <- readMaybe len
    Just (evalAll envRef (take val argTail), drop val argTail)
    
evalAll :: Env -> [Token] -> IO [Token]
evalAll envRef = foldrM (\x y -> eval envRef $ x : y) []

eval :: Env -> [Token] -> IO [Token]
eval envRef args = getVar envRef (head args) >>= eval' where
    eval' (_, funcType, Func [])     = maybe (anyFunc envRef) ($) (lookup funcType $ internalFuncs envRef) args
    eval' (funcName, funcType, func) = case fetchFunc func $ tail args of
        Nothing                               -> anyFunc envRef args
        Just (Func [(_, [], _)], _, _)        -> anyFunc envRef args
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
            fetchArgs' [] acum args                                     = Just (reverse acum, args)
            fetchArgs' _ _ []                                           = Nothing
            fetchArgs' ((n1, t1) : funcArgs) acum (val@(n2, t2) : args) = if t1 == "___CONST___" && n1 == n2
                then fetchArgs' funcArgs acum args
                else if t1 == t2
                    then fetchArgs' funcArgs ((n1, "___FUNCARG___", Func [([], [val], funcEnv)]) : acum) args
                    else Nothing
                
buildFunc :: Var -> [Var] -> IO Func
buildFunc (funcName, funcType, Func [(_, funcBody, funcEnvRef)]) args = do
    newFuncEnvRef <- join $ bindVars <$> nullEnv <*> lsFuncs funcEnvRef
    setVar newFuncEnvRef ("___SELF___", "___FUNCARG___", Func [([], [(funcName, funcType)], newFuncEnvRef)])
    mapM_ (setVar newFuncEnvRef) args
    return $ Func [([], funcBody, newFuncEnvRef)]
