module El.Evaluator (primitives, evalAll, eval) where
import El.Data
import El.Environment
import Data.IORef (newIORef)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Control.Monad (liftM2, join)

primitives :: [(String, [Token] -> IO [Token])]
primitives = [("num",   num),
              ("funcArg", funcArg),
              ("anyFunc", anyFunc)]
              
num, funcArg, anyFunc :: [Token] -> IO [Token]
num ((arg1, Prim "num" _) : (op, Prim "numop" _) : (arg2, Prim "num" _) : args) = do
    envRef <- nullEnv
    return $ (show $ fromJust (lookup op numOps) (read arg1) (read arg2), Prim "num" envRef) : args
num args = return args
funcArg ((name, Prim "funcArg" envRef) : args) = liftM2 (:) (getFunc envRef name) (return args) >>= eval
anyFunc ((name, _) : ("=", Prim "assign" envRef) : val : args) =
    liftM2 (:) (setFunc envRef (name, Func [([], [val], envRef)])) (return args)
anyFunc args = return args

numOps :: [(String, Integer -> Integer -> Integer)]
numOps = [("+", (+)),
          ("-", (-)),
          ("*", (*)),
          ("/", div)]
          
evalAll :: [Token] -> IO [Token]
evalAll = foldrM (\x y -> eval $ x : y) []

eval :: [Token] -> IO [Token]
eval args@(     (_, Prim func _)   : _) = maybe anyFunc ($) (lookup func primitives) args
eval args@(func@(funcName, Func _) : _) = case fetchFunc args of
    Nothing                     -> return args
    Just (fetchedFunc, argTail) -> do
        (_, Func [(_, funcBody, _)]) : args <- buildFunc $ (funcName, fetchedFunc) : argTail
        eval $ funcBody ++ args
        
buildFunc :: [Token] -> IO [Token]
buildFunc ((funcName, Func [(funcArgs, funcBody, funcEnv)]) : args) = do
    closure <- join $ bindFuncs <$> newIORef [] <*> lsFuncs funcEnv
    mapM_ (setFunc closure) funcArgs
    return $ (funcName, Func [([], map (bindBody closure) funcBody, closure)]) : args where
    bindBody envRef (name, Prim "nil" _) = (name, Prim "funcArg" envRef)
    bindBody _ token = token
    
fetchFunc :: [Token] -> Maybe (Func, [Token])
fetchFunc ((_, Func func) : args) = listToMaybe $ mapMaybe (`fetchArgs` args) func where
    fetchArgs (funcArgs, funcBody, funcEnv) args = case fetchArgs' funcArgs [] args of
        Nothing               -> Nothing
        Just (fArgs, argTail) -> Just (Func [(fArgs, funcBody, funcEnv)], argTail)
        where
            fetchArgs' [] acum args                                       = Just (reverse acum, args)
            fetchArgs' _ _ []                                             = Nothing
            fetchArgs' ((name, t1) : funcArgs) acum (arg2@(_, t2) : args) = if t1 == t2
                then fetchArgs' funcArgs ((name, Func [([], [arg2], funcEnv)]) : acum) args
                else Nothing