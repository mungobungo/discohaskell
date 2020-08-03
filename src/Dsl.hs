module Dsl where

data Item = Item {
    id :: String,
    amount :: Int,
    name :: String,
    lockedChildren :: [Item],
    freeChildren :: [Item],
    basePrice :: Int,
    discountedBasePrice :: Maybe Int,
    discountRule :: Maybe String,
    totalPrice :: Int,
    discountedTotalPrice :: Maybe Int
} deriving(Show, Eq)

defaultItem = Item {Dsl.id = "1", amount= 1, name ="PAP_100", lockedChildren =[], freeChildren = [], 
    basePrice = 100, discountedBasePrice = Nothing, discountRule = Nothing, totalPrice = 10, discountedTotalPrice = Nothing
}

filterName :: String -> (Item -> Bool)
filterName fname = \x -> (name x) == fname

filterNameTree :: [String] -> (Item -> Bool)
-- filterNameTree [a,b]= \x -> 
--                                 let nm = (name x) == a
--                                     fn = filterName b
--                                 in
--                                     nm && ([] /= filter fn (freeChildren x))
filterNameTree [a] = filterName a

filterNameTree [] = \x -> True

filterNameTree (a:rest) = \x -> 
                                let fnx = (filterName a) x
                                    fnt = filterNameTree rest
                                in
                                    fnx &&  ([] /= filter fnt (freeChildren x))
