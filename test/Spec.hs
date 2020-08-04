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
            let deep = defaultItem {name = "deep", freeChildren = [c,z,y]}           
            filter (filterNameTree ["bbb","aaa"]) [y,y, z] `shouldBe` [y,y]
            filter (filterNameTree ["ccc", "aaa"]) [z,y,y,c,b] `shouldBe` [z]
            filter (filterNameTree ["deep", "aaa"]) [deep] `shouldBe` []
            filter (filterNameTree ["deep", "ccc", "ddd"]) [deep] `shouldBe` [deep]
        it "should work with yesno selector" $ do
            let page1 = defaultItem {name = "extraPage", amount =15}
            let page2 = defaultItem {name = "extraPage", amount =10}
            let x= defaultItem{name = "photobook", freeChildren = [page1]}
            
            let y = defaultItem{name = "photobook", freeChildren = [page2]}

            filter (filterTreeEqualAmount ["extraPage"] 10) [page2] `shouldBe` [page2]
            filter (filterTreeEqualAmount ["extraPage"] 10) [page1] `shouldBe` []
            filter (filterTreeEqualAmount ["extraPage"] 15) [page1] `shouldBe` [page1]
            filter (filterTreeEqualAmount ["extraPage"] 10) [page1, page2] `shouldBe` [page2]

            filter (filterTreeEqualAmount ["photobook", "extraPage"] 10) [x, y] `shouldBe` [y]
            filter (filterTreeLessOrEqualAmount ["photobook", "extraPage"] 10) [x, y] `shouldBe` [y]
            filter (filterTreeLessThanAmount ["photobook", "extraPage"] 11) [x, y] `shouldBe` [y]

            filter (filterTreeMoreOrEqualAmount ["photobook", "extraPage"] 10) [x, y] `shouldBe` [x,y]
            filter (filterTreeMoreThanAmount ["photobook", "extraPage"] 10) [x, y] `shouldBe` [x]



    