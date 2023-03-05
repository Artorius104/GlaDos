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
