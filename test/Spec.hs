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
                stringToAst "def main(){foo = 21 foo * 2} main()" `shouldBe` [Function "main" [] [BinaryOp "=" (Symbol "foo") (Int 21),BinaryOp "*" (Symbol "foo") (Int 2)],Call "main" []]
