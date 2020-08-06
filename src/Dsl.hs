
module Dsl where
import Data.List.Split    

data Item = Item {
    id :: String,
    amount :: Int,
    name :: String,
    category :: String,
    lockedChildren :: [Item],
    freeChildren :: [Item],
    basePrice :: Int,
    discountedBasePrice :: Maybe Int,
    discountRule :: Maybe String,
    totalPrice :: Int,
    discountedTotalPrice :: Maybe Int
} deriving(Show, Eq)

defaultItem = Item {Dsl.id = "1", amount= 1, name ="PAP_100", category = "Photobook", lockedChildren =[], freeChildren = [], 
    basePrice = 100, discountedBasePrice = Nothing, discountRule = Nothing, totalPrice = 10, discountedTotalPrice = Nothing
}

filterName :: String -> (Item -> Bool)
filterName "_" = \x -> True
filterName fname = \x -> (name x) == fname

filterTreePredicate :: [String] -> (Item -> Bool) -> (Item->Bool)
-- predicate is being used only on the lowest level
filterTreePredicate [a] p = \x -> p x  && (filterName a) x

filterTreePredicate [] p = \x -> True

filterTreePredicate (a:rest) p = \x -> 
                                let fnx = (filterName a) x
                                    fnt = filterTreePredicate rest p
                                in
                                    fnx &&  ([] /= filter fnt (freeChildren x))


filterNameTree :: [String] -> (Item -> Bool)
filterNameTree items = filterTreePredicate items (\x -> True)


filterTreeEqualAmount :: [String] -> Int -> (Item->Bool)
filterTreeEqualAmount items limit = filterTreePredicate items (\x -> limit == Dsl.amount x)

filterTreeLessOrEqualAmount :: [String] -> Int -> (Item->Bool)
filterTreeLessOrEqualAmount items limit = filterTreePredicate items (\x ->  Dsl.amount x <= limit)

filterTreeLessThanAmount :: [String] -> Int -> (Item->Bool)
filterTreeLessThanAmount items limit = filterTreePredicate items (\x ->  Dsl.amount x < limit)

filterTreeMoreOrEqualAmount :: [String] -> Int -> (Item->Bool)
filterTreeMoreOrEqualAmount items limit = filterTreePredicate items (\x ->  Dsl.amount x >= limit)

filterTreeMoreThanAmount :: [String] -> Int -> (Item->Bool)
filterTreeMoreThanAmount items limit = filterTreePredicate items (\x -> Dsl.amount x > limit)

havingAmount :: String -> String -> Int -> [Item] -> [Item]
havingAmount expression ">" amount items = 
    filter (filterTreeMoreThanAmount (splitOn "/" expression) amount) items
havingAmount expression ">=" amount items = 
    filter (filterTreeMoreOrEqualAmount (splitOn "/" expression) amount) items
havingAmount expression "<" amount items = 
    filter (filterTreeLessThanAmount (splitOn "/" expression) amount) items
havingAmount expression "<=" amount items = 
    filter (filterTreeLessOrEqualAmount (splitOn "/" expression) amount) items
havingAmount expression "=" amount items = 
    filter (filterTreeEqualAmount (splitOn "/" expression) amount) items

havingProperty :: String -> [Item] -> [Item]
havingProperty expression = havingAmount expression ">=" 0