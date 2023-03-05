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
            it "ParseAst" $ do
                stringToAst "def add (a, b) {a + b}" `shouldBe` [Function "add" [Symbol "a",Symbol "b"] [BinaryOp "+" (Symbol "a") (Symbol "b")]]
            it "ParseAst" $ do
                stringToAst "def add (a, b) {a + b} add(1, 2)" `shouldBe` [Function "add" [Symbol "a",Symbol "b"] [BinaryOp "+" (Symbol "a") (Symbol "b")],Call "add" [Int 1,Int 2]]
            it "ParseAst" $ do
                stringToAst "x = add (a, b) {a + b}x(3, 4)" `shouldBe` [BinaryOp "=" (Symbol "x") (Function "add" [Symbol "a",Symbol "b"] [BinaryOp "+" (Symbol "a") (Symbol "b")]),Call "x" [Int 3,Int 4]]
            it "ParseAst" $ do
                stringToAst "def test(a, b) {a + b}test(3, 4)" `shouldBe` [Function "test" [Symbol "a",Symbol "b"] [BinaryOp "+" (Symbol "a") (Symbol "b")],Call "test" [Int 3,Int 4]]
            it "ParseAst" $ do
                stringToAst "if (true) {1} else {2}" `shouldBe` [If (Boolean True) [Int 1] [Int 2]]
            it "ParseAst" $ do
                stringToAst "if (false) {1} else {2}" `shouldBe` [If (Boolean False) [Int 1] [Int 2]]
            it "ParseAst" $ do
                stringToAst "foo = 42 if (foo < 10) { foo * 3}else {foo / 2}" `shouldBe` [BinaryOp "=" (Symbol "foo") (Int 42),If (BinaryOp "<" (Symbol "foo") (Int 10)) [BinaryOp "*" (Symbol "foo") (Int 3)] [BinaryOp "/" (Symbol "foo") (Int 2)]]
            it "ParseAst" $ do
                stringToAst "(2 * 3) + (10 / 2)" `shouldBe` [BinaryOp "+" (BinaryOp "*" (Int 2) (Int 3)) (BinaryOp "/" (Int 10) (Int 2))]
            it "ParseAst" $ do
                stringToAst "(2 * 5) == (11 - 1)" `shouldBe` [BinaryOp "==" (BinaryOp "*" (Int 2) (Int 5)) (BinaryOp "-" (Int 11) (Int 1))]
            it "ParseAst" $ do
                stringToAst "1 < (10 % 3)" `shouldBe` [BinaryOp "<" (Int 1) (BinaryOp "%" (Int 10) (Int 3))]
            it "ParseAst" $ do
                stringToAst "def superior(a, b) {if (a == b) {false}else {if (a < b) {false}else {true}}}superior(10, -2)" `shouldBe` [Function "superior" [Symbol "a",Symbol "b"] [If (BinaryOp "==" (Symbol "a") (Symbol "b")) [Boolean False] [If (BinaryOp "<" (Symbol "a") (Symbol "b")) [Boolean False] [Boolean True]]],Call "superior" [Int 10,Int (-2)]]
            it "ParseAst" $ do
                stringToAst "def fact(x) {if (x == 1) {1}else {x * fact(x-1)}}fact(10)" `shouldBe` [Function "fact" [Symbol "x"] [If (BinaryOp "==" (Symbol "x") (Int 1)) [Int 1] [BinaryOp "*" (Symbol "x") (Call "fact" [BinaryOp "-" (Symbol "x") (Int (-1))])]],Call "fact" [Int 10]]
