module El.Data where
import Data.IORef
import Data.Functor.Classes (liftEq)
import Data.List (intercalate)

type Token = (String, String)

newtype Func = Func [([Token], [Token], Env)]

instance Show Func where
    show (Func func) = intercalate ", " $ map showFunc func where
        showFunc (args, body, _) = show args ++ ": ..."
        
instance Eq Func where
    (Func func1) == (Func func2) = liftEq eqFunc func1 func2 where
        eqFunc (args1, _, _) (args2, _, _) = liftEq eqArgs args1 args2
        eqArgs (funcName1, typeName1) (funcName2, typeName2) = typeName1 == typeName2 && (typeName1 /= "___CONST___" || funcName1 == funcName2)
        
type Var = (String, String, Func)

newtype FuncRef = FuncRef [IORef ([Token], [Token], Env)]

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
