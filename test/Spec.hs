import Test.Hspec
import Lib
import Dsl
main :: IO ()
main = hspec $ do
    describe "rune one" $ do
        it "getting someting" $
            1 `shouldBe` 1
        context "filterValue" $ do
            let x = dummy { value = "xxx"}
            let y = dummy {  value= "yyy"}
            it "should work in positive and negative cases" $ do
                filter (filterValue "xxx") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterValue "yyy") [x,y, y,x,y,x]  `shouldBe` [y,y,y]
                filter (filterValue "blabla") [x,y, y,x,y,x]  `shouldBe` []

            it "should work with wildcard" $ do
                filter (filterValue "_") [x,y, y,x,y,x]  `shouldBe` [x,y, y,x,y,x]
            
            it "should not care about cases of values" $ do
                filter (filterValue "XXX") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterValue "XxX") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
            it "should work with val:xxx" $do
                filter (filterValue "val:xxx") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterValue "val:yYy") [x,y, y,x,y,x]  `shouldBe` [y,y,y]
                filter (filterValue "val:_") [x,y, y,x,y,x]  `shouldBe` [x,y, y,x,y,x]
                filter (filterValue "val:blaBLA") [x,y, y,x,y,x]  `shouldBe` []
        context "filterType" $ do
            let x = dummy { itype = "xxx"}
            let y = dummy { itype = "yyy"}
            it "should work in positive and negative cases" $ do
                filter (filterType "xxx") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterType "yyy") [x,y, y,x,y,x]  `shouldBe` [y,y,y]
                filter (filterType "blabla") [x,y, y,x,y,x]  `shouldBe` []

            it "should work with wildcard" $ do
                filter (filterType "_") [x,y, y,x,y,x]  `shouldBe` [x,y, y,x,y,x]
            
            it "should not care about cases of values" $ do
                filter (filterType "XXX") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterType "XxX") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
            it "should work with type:xxx" $do
                filter (filterType "type:xxx") [x,y, y,x,y,x]  `shouldBe` [x,x,x]
                filter (filterType "type:yYy") [x,y, y,x,y,x]  `shouldBe` [y,y,y]
                filter (filterType "type:_") [x,y, y,x,y,x]  `shouldBe` [x,y, y,x,y,x]
                filter (filterType "type:blaBLA") [x,y, y,x,y,x]  `shouldBe` []    

        it "filterProperty should work for both val:alias and alias queries" $ do
            let x = dummy { value = "xxx", itype = "xXx"}
            let y = dummy { value= "yyy"}
            filter (filterProperty "xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]
            filter (filterProperty "val:xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]
            filter (filterProperty "type:xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]

        it "should work with filter extras" $ do
            let x = dummy { value = "aaa"}
            let y = dummy {  value= "bbb", freeChildren = [x,x]}
            let z = dummy {  value= "ccc", freeChildren = [x,x]}
            let b = dummy {  value = "ddd"}
            let c = dummy {  value= "ccc", freeChildren = [b,b]}
            let deep = dummy {value = "deep", freeChildren = [c,z,y]}           
            filter (filterValueTree ["bbb","aaa"]) [y,y, z] `shouldBe` [y,y]
            filter (filterValueTree ["ccc", "aaa"]) [z,y,y,c,b] `shouldBe` [z]
            filter (filterValueTree ["deep", "aaa"]) [deep] `shouldBe` []
            filter (filterValueTree ["deep", "ccc", "ddd"]) [deep] `shouldBe` [deep]
        
        it "should work with yesno selector" $ do
            let page1 = dummy {value = "extraPage", amount =15}
            let page2 = dummy {value = "extraPage", amount =10}
            let x= dummy{value = "photobook", freeChildren = [page1]}
            
            let y = dummy{value = "photobook", freeChildren = [page2]}

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
            let photobook = dummy{ itype="Photobook"}
            let extraPage = dummy{value="extraPage", amount = 10}

            let book1 = photobook{value= "pap_194"}
            let book1extra = book1{  freeChildren = [extraPage]}
            let book2 = photobook{value = "pap_324"}
            let book2extra = book2{freeChildren = [extraPage {amount = 15}]}

            let cart = [book1, book1extra, book1extra, book2, book2extra, book1extra]

            -- splitOn "/" "photobook/extrapage" `shouldBe` ["photobook", "extrapage"] 
            it "should work with having amount selectors" $ do
                havingAmount "pap_194/val:extraPage" ">" 10 cart `shouldBe` []
                havingAmount "pap_324/extraPage" ">" 10 cart `shouldBe` [book2extra]
                havingAmount "_/extraPage" ">=" 10 cart `shouldBe` [book1extra, book1extra, book2extra, book1extra]
            
            it "should work with having property selectors" $ do
                havingProperty "pap_194/extraPage" cart `shouldBe` [book1extra, book1extra, book1extra]
                havingProperty "_/extraPage" cart `shouldBe` [book1extra, book1extra, book2extra, book1extra]
            
            it "should work with property IN selectors" $ do
                let ct = dummy{value = "linen"}
                let zwartLeer = dummy{value = "zwartLeer"}
                1 `shouldBe` 1
    