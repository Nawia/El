{-# LANGUAGE StandaloneDeriving #-}
import Test.Hspec
import El.Data
import El.Environment
import El.Evaluator

deriving instance Eq Token

main :: IO ()
main = hspec $ before initEnv $ do
    describe "Parens tests: " $ do
        it "parses paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "hello" "nil" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "hello" "nil" []]
        it "parses empty paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` []
        it "parses left paren embedded in paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___(BLOCK___" "___BLOCK___" []]
        it "parses right paren embedded in paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___BLOCK)___" "___BLOCK___" []]
        it "parses embedded paren blocks" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` []
        it "parses chains of paren blocks" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` []
    describe "Quotes tests: " $ do
        it "parses quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "hello" "nil" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [Token "hello" "nil" []]]
        it "parses empty quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" []]
        it "parses left quote embedded in quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK___" "___BLOCK___" [], Token "___\"BLOCK\"___" "___BLOCK___" []]
        it "parses right quote embedded in quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___BLOCK\"___" "___BLOCK___" [Token "___\"BLOCK\"___" "___BLOCK___" []]]
        it "parses embedded quote blocks" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [Token "___\"BLOCK\"___" "___BLOCK___" []]]
        it "parses chains of quote blocks" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [], Token "___\"BLOCK\"___" "___BLOCK___" []]
    describe "Parens-quotes tests: " $ do
        it "parses left quote embedded in paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK___" "___BLOCK___" []]
        it "parses right quote embedded in paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___BLOCK\"___" "___BLOCK___" []]
        it "parses empty quote block embedded in paren block" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" []]
        it "parses interleaved paren and quote blocks" $ \envRef -> do
            evalAll envRef [Token "___(BLOCK___" "___BLOCK___" [], Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___(BLOCK___" "___BLOCK___" [], Token "___\"BLOCK\"___" "___BLOCK___" [Token "___BLOCK)___" "___BLOCK___" []]]
    describe "Quotes-parens tests: " $ do
        it "parses left paren embedded in quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [Token "___(BLOCK___" "___BLOCK___" []]]
        it "parses right paren embedded in quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [Token "___BLOCK)___" "___BLOCK___" []]]
        it "parses empty paren block embedded in quote block" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" [Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []]]
        it "parses interleaved quote and paren blocks" $ \envRef -> do
            evalAll envRef [Token "___\"BLOCK___" "___BLOCK___" [], Token "___(BLOCK___" "___BLOCK___" [], Token "___BLOCK\"___" "___BLOCK___" [], Token "___BLOCK)___" "___BLOCK___" []] `shouldReturn` [Token "___\"BLOCK\"___" "___BLOCK___" []]
