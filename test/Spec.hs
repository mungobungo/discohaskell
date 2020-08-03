import Test.Hspec
import Lib
import Dsl
main :: IO ()
main = hspec $ do
    describe "rune one" $ do
        it "getting someting" $
            1 `shouldBe` 1
          
        it "now borke" $
            absolute 3 `shouldBe` 4
    