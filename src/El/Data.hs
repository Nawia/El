module El.Data where
import Data.IORef
import Data.Functor.Classes (liftEq)
import Data.List (intercalate)

data Token = Token String String [Token]
instance Show Token where
    show (Token funcName typeName _) = show (funcName, typeName)
    
data Func = Func [([Token], [Token], Env)]
instance Show Func where
    show (Func func) = intercalate ", " $ map showFunc func where
        showFunc (args, body, _) = show args ++ ": ..."
instance Eq Func where
    (Func func1) == (Func func2) = liftEq eqFunc func1 func2 where
        eqFunc (args1, _, _) (args2, _, _) = liftEq eqArgs args1 args2
        eqArgs (Token _ typeName1 _) (Token _ typeName2 _) = typeName1 == typeName2
        
type Var = (String, String, Func)

data FuncRef = FuncRef [IORef ([Token], [Token], Env)]

type VarRef = (String, String, IORef FuncRef)

type Env = IORef [VarRef]

wrapVar :: Var -> IO VarRef
wrapVar (pattern, typeName, func) = (,,) pattern typeName <$> (wrapFunc func >>= newIORef)

unwrapVar :: VarRef -> IO Var
unwrapVar (pattern, typeName, funcRef) = (,,) pattern typeName <$> (readIORef funcRef >>= unwrapFunc)

wrapFunc :: Func -> IO FuncRef
wrapFunc (Func func) = FuncRef <$> mapM newIORef func

unwrapFunc :: FuncRef -> IO Func
unwrapFunc (FuncRef funcRef) = Func <$> mapM readIORef funcRef
