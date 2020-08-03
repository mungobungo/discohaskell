import Test.Hspec
import Lib
import Dsl
main :: IO ()
main = hspec $ do
    describe "rune one" $ do
        it "getting someting" $
            1 `shouldBe` 1
          
        it "shoudl work with filter" $ do
            let x = defaultItem {Dsl.id = "1",  name = "aaa"}
            let y = defaultItem {Dsl.id = "2",  name= "bbb"}
            let f = filterName "aaa"
            (map f  [x,y, y,x,y,x]) `shouldBe` [True,False, False, True, False, True]
            filter f [x,y, y,x,y,x] `shouldBe` [x,x,x]
        it "should work with filter extras" $ do
            let x = defaultItem { name = "aaa"}
            let y = defaultItem {  name= "bbb", freeChildren = [x,x]}
            let z = defaultItem {  name= "ccc", freeChildren = [x,x]}
            let b = defaultItem {  name = "ddd"}
            let c = defaultItem {  name= "ccc", freeChildren = [b,b]}
            let f = filterNameTree ["bbb","aaa"]
            filter f [y,y, z] `shouldBe` [y,y]


    