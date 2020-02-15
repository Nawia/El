module El.Data (Func (Func, Prim), Token, FuncRef (FuncRef, PrimRef), TokenRef, Env,
                wrapFunc, unwrapFunc, wrapToken, unwrapToken) where
import Data.IORef
import Data.Functor.Classes (liftEq)
import Data.List (intercalate)

data Func = Func [([Token], [Token], Env)]
          | Prim String Env
          
instance Show Func where
    show (Func func) = intercalate ", " $ map showFunc func where
        showFunc (args, body, _) = show args ++ ": ..."
    show (Prim name _) = name
    
instance Eq Func where
    (Func func1) == (Func func2) = liftEq eqFunc func1 func2 where
        eqFunc (args1, _, _) (args2, _, _) = liftEq eqArgs args1 args2
        eqArgs (_, arg1) (_, arg2) = arg1 == arg2
    (Prim func1 _) == (Prim func2 _) = func1 == func2
    _ == _ = False
    
type Token = (String, Func)

data FuncRef = FuncRef [IORef ([Token], [Token], Env)]
             | PrimRef String Env
             
type TokenRef = (String, IORef FuncRef)

type Env = IORef [TokenRef]

wrapToken :: Token -> IO TokenRef
wrapToken (name, func) = (,) name <$> (wrapFunc func >>= newIORef)

unwrapToken :: TokenRef -> IO Token
unwrapToken (name, funcRef) = (,) name <$> (readIORef funcRef >>= unwrapFunc)

wrapFunc :: Func -> IO FuncRef
wrapFunc (Func func) = FuncRef <$> mapM newIORef func
wrapFunc (Prim func env) = return $ PrimRef func env

unwrapFunc :: FuncRef -> IO Func
unwrapFunc (FuncRef func) = Func <$> mapM readIORef func
unwrapFunc (PrimRef func env) = return $ Prim func env
