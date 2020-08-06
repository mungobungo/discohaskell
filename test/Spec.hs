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
        context "filterProperty" $ do
            it "filterProperty should work for both val:alias and alias queries" $ do
                let x = dummy { value = "xxx", itype = "xXx"}
                let y = dummy { value= "yyy"}
                filter (filterProperty "xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]
                filter (filterProperty "val:xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]
                filter (filterProperty "type:xxx") [x,y, y,x,y,x] `shouldBe` [x,x,x]
        context "filterPropertyTree" $ do
            let x = dummy { value = "aaa", itype = "extraPage"}
            let y = dummy {  value= "bbb", itype= "calendar", freeChildren = [x,x]}
            let onlyc = dummy{value = "ccc"}
            let z = onlyc {   freeChildren = [x,x]}
            let b = dummy {  value = "ddd"}
            let c = onlyc {  freeChildren = [b,b]}
            let deep = dummy {value = "deep", freeChildren = [c,z,y]}           
            it "should work with filter extras" $ do               
                filter (filterPropertyTree ["bbb","aaa"]) [y,y, z] `shouldBe` [y,y]
            it "should work correctly with or without val: modifier" $ do
                filter (filterPropertyTree ["val:bbb","aaa"]) [y,y, z] `shouldBe` [y,y]
                filter (filterPropertyTree ["val:bbb","val:aaa"]) [y,y, z] `shouldBe` [y,y]
                filter (filterPropertyTree ["bbb","val:aaa"]) [y,y, z] `shouldBe` [y,y]
            it "should work correctly with or without type: modifier" $ do    
                filter (filterPropertyTree ["val:bbb","type:extraPage"]) [y,y, z] `shouldBe` [y,y]
                filter (filterPropertyTree ["type:calendar"]) [z] `shouldBe` []
                
                filter (filterPropertyTree ["type:calendar"]) [y,y, z] `shouldBe` [y,y]
                
                filter (filterPropertyTree ["type:calendar","type:extraPage"]) [y,y, z] `shouldBe` [y,y]
                
            it "wildcards require child item to exits" $do
                filter (filterPropertyTree ["ccc","_"]) [ onlyc] `shouldBe` []
                filter (filterPropertyTree ["ccc","_"]) [ onlyc, z,z,c,c] `shouldBe` [z,z,c,c]
                filter (filterPropertyTree ["ccc"]) [onlyc, z,z,c,c] `shouldBe` [onlyc, z,z,c,c]
 
            it "should work correclty with wildcards and modifiers" $ do    
                filter (filterPropertyTree ["_","type:extraPage"]) [y,y, z] `shouldBe` [y,y,z]
                filter (filterPropertyTree ["_","type:extraPage"]) [y,y, z] `shouldBe` [y,y,z]


            it "should work correctly with property selectors" $ do     
                filter (filterPropertyTree ["ccc", "aaa"]) [z,y,y,c,b] `shouldBe` [z]
                filter (filterPropertyTree ["deep", "aaa"]) [deep] `shouldBe` []
                filter (filterPropertyTree ["deep", "ccc", "ddd"]) [deep] `shouldBe` [deep]
        context "amount selector" $ do
            it "should work with different amounts and conditions" $ do
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
        context " HAVING query" $ do
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
    