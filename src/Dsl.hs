
module Dsl (
    dummy, 

    Item(iid, amount, value, category, freeChildren, lockedChildren, basePrice, discountedBasePrice, discountRule, totalPrice, discountedTotalPrice),
    filterValue,
    filterValueTree,
    filterTreeEqualAmount,
    filterTreeLessOrEqualAmount,
    filterTreeLessThanAmount,
    filterTreeMoreThanAmount,
    filterTreeMoreOrEqualAmount,
    havingAmount,
    havingProperty
    ) where
import Data.List.Split    

data Item = Item {
    iid :: String,
    amount :: Int,
    value :: String,
    category :: String,
    lockedChildren :: [Item],
    freeChildren :: [Item],
    basePrice :: Int,
    discountedBasePrice :: Maybe Int,
    discountRule :: Maybe String,
    totalPrice :: Int,
    discountedTotalPrice :: Maybe Int
} deriving(Show, Eq)

dummy = Item {iid = "1", amount= 1, value ="PAP_100", category = "Photobook", lockedChildren =[], freeChildren = [], 
    basePrice = 100, discountedBasePrice = Nothing, discountRule = Nothing, totalPrice = 10, discountedTotalPrice = Nothing
}

filterValue :: String -> (Item -> Bool)
filterValue "_" = \x -> True
filterValue val = \x -> (value x) == val

filterTreePredicate :: [String] -> (Item -> Bool) -> (Item->Bool)
-- predicate is being used only on the lowest level
filterTreePredicate [a] p = \x -> p x  && (filterValue a) x

filterTreePredicate [] p = \x -> True

filterTreePredicate (a:rest) p = \x -> 
                                let fnx = (filterValue a) x
                                    fnt = filterTreePredicate rest p
                                in
                                    fnx &&  ([] /= filter fnt (freeChildren x))


filterValueTree :: [String] -> (Item -> Bool)
filterValueTree items = filterTreePredicate items (\x -> True)


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