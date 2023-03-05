-- 
import Parser(stringToAst, Ast(..))
import Test.Hspec
import Parser(stringToAst, Ast(..))
-- import Test.QuickCheck
-- 
main :: IO ()
main = hspec $ do
        describe "Parser" $ do
            it "ParseAst" $ do
                stringToAst "foo = 21 foo * 2" `shouldBe` [BinaryOp "=" (Symbol "foo") (Int 21),BinaryOp "*" (Symbol "foo") (Int 2)]
            it "ParseAst" $ do
                stringToAst "foo * 42" `shouldBe` [BinaryOp "*" (Symbol "foo") (Int 42)]
            it "ParseAst" $ do
                stringToAst "10 / 2" `shouldBe` [BinaryOp "/" (Int 10) (Int 2)]
