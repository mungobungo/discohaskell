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
        it "shoudl work with filter" $ do
            let x = defaultItem {Dsl.id = "1",  name = "aaa"}
            let y = defaultItem {Dsl.id = "2",  name= "bbb"}
            let f = filterName "aaa"
            (map f  [x,y, y,x,y,x]) `shouldBe` [True,False, False, True, False, True]
            filter f [x,y, y,x,y,x] `shouldBe` [x,x,x]

    