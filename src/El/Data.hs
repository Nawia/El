module El.Data where
import Data.IORef
import Data.Functor.Classes (liftEq)
import Data.List (intercalate)

data Func = Func [([(String, String)], [(String, String)], Env)]
          | Prim String Env
          
instance Show Func where
    show (Func func) = intercalate ", " $ map showFunc func where
        showFunc (args, body, _) = show args ++ ": ..."
    show (Prim name _) = name
    
instance Eq Func where
    (Func func1) == (Func func2) = liftEq eqFunc func1 func2 where
        eqFunc (args1, _, _) (args2, _, _) = liftEq eqArgs args1 args2
        eqArgs (_, typename1) (_, typename2) = typename1 == typename2
    (Prim func1 _) == (Prim func2 _) = func1 == func2
    _ == _ = False
    
type Token = (String, String, Func)

data FuncRef = FuncRef [IORef ([(String, String)], [(String, String)], Env)]
             | PrimRef String Env
             
type TokenRef = (String, String, IORef FuncRef)

type Env = IORef [TokenRef]

wrapToken :: Token -> IO TokenRef
wrapToken (name, typename, func) = (,,) name typename <$> (wrapFunc func >>= newIORef)

unwrapToken :: TokenRef -> IO Token
unwrapToken (name, typename, funcRef) = (,,) name typename <$> (readIORef funcRef >>= unwrapFunc)

wrapFunc :: Func -> IO FuncRef
wrapFunc (Func func) = FuncRef <$> mapM newIORef func
wrapFunc (Prim func env) = return $ PrimRef func env

unwrapFunc :: FuncRef -> IO Func
unwrapFunc (FuncRef func) = Func <$> mapM readIORef func
unwrapFunc (PrimRef func env) = return $ Prim func env
