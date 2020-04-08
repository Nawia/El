import Test.Hspec
import El.Environment
import El.Evaluator

main :: IO ()
main = hspec $ before initEnv $ do
    describe "Parens tests: " $ do
        it "parses paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("hello", "nil"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("hello", "nil")]
        it "parses empty paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` []
        it "parses left paren embedded in paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("___(BLOCK___", "___(BLOCK___")]
        it "parses right paren embedded in paren block" $ \envRef -> do
            toks <- evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___BLOCK)___", "___BLOCK)___")]
            length toks `shouldBe` 1
            let ((funcName, typeName):_) = toks
            putStrLn $ "  funcName = " ++ show funcName
            typeName `shouldBe` "___BLOCK)___"
        it "parses embedded paren blocks" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` []
        it "parses chains of paren blocks" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` []
    describe "Quotes tests: " $ do
        it "parses quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("hello", "nil"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("1","___\"BLOCK___"),("hello","nil")]
        it "parses empty quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("0","___\"BLOCK___")]
        it "parses left quote embedded in quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("___\"BLOCK___", "___\"BLOCK___"), ("0", "___\"BLOCK___")]
        it "parses right quote embedded in quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("1", "___BLOCK\"___"), ("0", "___\"BLOCK___")]
        it "parses embedded quote blocks" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("1", "___\"BLOCK___"), ("0", "___\"BLOCK___")]
        it "parses chains of quote blocks" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("0","___\"BLOCK___"), ("0","___\"BLOCK___")]
    describe "Parens-quotes tests: " $ do
        it "parses left quote embedded in paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("___\"BLOCK___", "___\"BLOCK___")]
        it "parses right quote embedded in paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("0", "___BLOCK\"___")]
        it "parses empty quote block embedded in paren block" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("0","___\"BLOCK___")]
        it "parses interleaved paren and quote blocks" $ \envRef -> do
            evalAll envRef [("___(BLOCK___", "___(BLOCK___"), ("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("___(BLOCK___", "___(BLOCK___"), ("1", "___\"BLOCK___"), ("___BLOCK)___", "___BLOCK)___")]
    describe "Quotes-parens tests: " $ do
        it "parses left paren embedded in quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("1", "___\"BLOCK___"), ("___(BLOCK___", "___(BLOCK___")]
        it "parses right paren embedded in quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("1", "___\"BLOCK___"), ("___BLOCK)___", "___BLOCK)___")]
        it "parses empty paren block embedded in quote block" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___"), ("___BLOCK\"___", "___BLOCK\"___")] `shouldReturn` [("2", "___\"BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK)___", "___BLOCK)___")]
        it "parses interleaved quote and paren blocks" $ \envRef -> do
            evalAll envRef [("___\"BLOCK___", "___\"BLOCK___"), ("___(BLOCK___", "___(BLOCK___"), ("___BLOCK\"___", "___BLOCK\"___"), ("___BLOCK)___", "___BLOCK)___")] `shouldReturn` [("0", "___\"BLOCK___")]
