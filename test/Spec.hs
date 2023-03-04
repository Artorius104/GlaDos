-- 
import Parser(parseChar)
import Test.Hspec
-- import Test.QuickCheck
-- 
main :: IO ()
main = hspec $ do
        describe "Parser" $ do
            it "ParseChar" $ do
                runParser (parseChar 'a') "a" `shouldBe` Just ('a', "")
            it "ParseChar" $ do
                runParser (parseChar 'a') "b" `shouldBe` Nothing
            it "ParseChar" $ do
                runParser (parseChar 'a') "" `shouldBe` Nothing

