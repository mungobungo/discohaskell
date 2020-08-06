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
        describe "should work with HAVING" $ do
            let photobook = defaultItem{ category="Photobook"}
            let extraPage = defaultItem{name="extraPage", amount = 10}

            let book1 = photobook{name= "pap_194"}
            let book1extra = book1{  freeChildren = [extraPage]}
            let book2 = photobook{name = "pap_324"}
            let book2extra = book2{freeChildren = [extraPage {amount = 15}]}

            let cart = [book1, book1extra, book1extra, book2, book2extra, book1extra]

            -- splitOn "/" "photobook/extrapage" `shouldBe` ["photobook", "extrapage"] 
            it "should work with having amount selectors" $ do
                havingAmount "pap_194/extraPage" ">" 10 cart `shouldBe` []
                havingAmount "pap_324/extraPage" ">" 10 cart `shouldBe` [book2extra]
                havingAmount "_/extraPage" ">=" 10 cart `shouldBe` [book1extra, book1extra, book2extra, book1extra]
            it "should work with having property selectors" $ do
                havingProperty "pap_194/extraPage" cart `shouldBe` [book1extra, book1extra, book1extra]

    