-- 
import Parser(stringToAst, Ast(..))
import Test.Hspec
-- import Test.QuickCheck
-- 
main :: IO ()
main = hspec $ do
        describe "Parser" $ do
            it "ParseAst" $ do
                stringToAst "def main () { foo = 21 foo * 2 } main ( ) " `shouldBe` [Function "main" [] [BinaryOp "=" (Symbol "foo") (Int 21),BinaryOp "*" (Symbol "foo") (Int 2)],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {foo * 42} main()" `shouldBe` [Function "main" [] [BinaryOp "*" (Symbol "foo") (Int 42)],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() { 10 / 2} main()" `shouldBe` [Function "main" [] [BinaryOp "/" (Int 10) (Int 2)],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def add (a, b) {a + b} def main(){ add(1,1) } main()" `shouldBe` [Function "add" [Symbol "a",Symbol "b"] [BinaryOp "+" (Symbol "a") (Symbol "b")],Function "main" [] [Call "add" [Int 1,Int 1]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def div (a, b) {a / b} def main() {div(1, 2)} main()" `shouldBe` [Function "div" [Symbol "a",Symbol "b"] [BinaryOp "/" (Symbol "a") (Symbol "b")],Function "main" [] [Call "div" [Int 1,Int 2]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {x = 5 5*x} main()" `shouldBe` [Function "main" [] [BinaryOp "=" (Symbol "x") (Int 5),BinaryOp "*" (Int 5) (Symbol "x")],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {if (1 > 2) {1} else {0}} main()" `shouldBe` [Function "main" [] [If (BinaryOp ">" (Int 1) (Int 2)) [Int 1] [Int 0]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {if (1 < 2) {1} else {0}} main()" `shouldBe` [Function "main" [] [If (BinaryOp "<" (Int 1) (Int 2)) [Int 1] [Int 0]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {if (2 >= 2) {1} else {0}} main()" `shouldBe` [Function "main" [] [If (BinaryOp ">=" (Int 2) (Int 2)) [Int 1] [Int 0]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {foo = 42 if (foo < 10) { foo * 3}else {foo / 2}} main()" `shouldBe` [Function "main" [] [BinaryOp "=" (Symbol "foo") (Int 42),If (BinaryOp "<" (Symbol "foo") (Int 10)) [BinaryOp "*" (Symbol "foo") (Int 3)] [BinaryOp "/" (Symbol "foo") (Int 2)]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {(2 * 3) + (10 / 2)} main()" `shouldBe` [Function "main" [] [BinaryOp "+" (BinaryOp "*" (Int 2) (Int 3)) (BinaryOp "/" (Int 10) (Int 2))],Call "main" []]
            
            it "ParseAst" $ do
                stringToAst "def main() {(2 * 5) == (11-1)} main()" `shouldBe` [Function "main" [] [BinaryOp "==" (BinaryOp "*" (Int 2) (Int 5)) (BinaryOp "-" (Int 11) (Int 1))],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {1 < (10 % 3)} main()" `shouldBe` [Function "main" [] [BinaryOp "<" (Int 1) (BinaryOp "%" (Int 10) (Int 3))],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def superior(a, b) {if (a == b) {false}else {if (a < b) {false}else {true}}}def main() {superior(10, -2)} main()" `shouldBe` [Function "superior" [Symbol "a",Symbol "b"] [If (BinaryOp "==" (Symbol "a") (Symbol "b")) [Boolean False] [If (BinaryOp "<" (Symbol "a") (Symbol "b")) [Boolean False] [Boolean True]]],Function "main" [] [Call "superior" [Int 10,Int (-2)]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def fact(x) {if (x == 1) {1}else {x * fact(x-1)}} def main() {fact(10)} main()" `shouldBe` [Function "fact" [Symbol "x"] [If (BinaryOp "==" (Symbol "x") (Int 1)) [Int 1] [BinaryOp "*" (Symbol "x") (Call "fact" [BinaryOp "-" (Symbol "x") (Int 1)])]],Function "main" [] [Call "fact" [Int 10]],Call "main" []]
            it "ParseAst" $ do
                stringToAst "def main() {while (x < 10) {x += 1}} main()" `shouldBe` [Function "main" [] [While (BinaryOp "<" (Symbol "x") (Int 10)) [BinaryOp "+=" (Symbol "x") (Int 1)]],Call "main" []]
