
module Dsl (
    dummy, 

    Item(iid, itype, value, amount, freeChildren, lockedChildren, basePrice, discountedBasePrice, discountRule, totalPrice, discountedTotalPrice),
    filterProperty,
    filterValue,
    filterType,
    filterPropertyTree,
    filterTreeEqualAmount,
    filterTreeLessOrEqualAmount,
    filterTreeLessThanAmount,
    filterTreeMoreThanAmount,
    filterTreeMoreOrEqualAmount,
    havingAmount,
    havingProperty,
    havingPropertyValueIn,
    countVariants
    ) where
import Data.Char
import Data.List.Split    

data Item = Item {
    iid :: String,
    
    itype :: String,
    value :: String,
    amount :: Int,
    lockedChildren :: [Item],
    freeChildren :: [Item],
    basePrice :: Int,
    discountedBasePrice :: Maybe Int,
    discountRule :: Maybe String,
    totalPrice :: Int,
    discountedTotalPrice :: Maybe Int
} deriving(Show, Eq)

dummy = Item {iid = "1", amount= 1, value ="dummyValue", itype = "DummyType", lockedChildren =[], freeChildren = [], 
    basePrice = 100, discountedBasePrice = Nothing, discountRule = Nothing, totalPrice = 10, discountedTotalPrice = Nothing
}

filterValue :: String -> (Item -> Bool)
filterValue "_" = \x -> True
filterValue ('v':'a':'l':':':x) = filterValue x
filterValue val = \x -> map toUpper (value x) == map toUpper val

filterType :: String -> (Item -> Bool)
filterType "_" = \x -> True
filterType ('t':'y':'p':'e':':':x) = filterType x
filterType typ = \x -> map toUpper (itype x) == map toUpper typ


filterProperty :: String -> (Item->Bool)
filterProperty "_" = \x -> True
filterProperty ('v':'a':'l':':': x) = filterValue x
filterProperty ('t':'y':'p':'e':':': x) = filterType x
filterProperty s  = \x -> filterValue s x || filterType s x


filterTreePredicate :: [String] -> (Item -> Bool) -> (Item->Bool)
-- predicate is being used only on the lowest level
filterTreePredicate [a] p = \x -> p x  && (filterProperty a) x

filterTreePredicate [] p = \x -> True

filterTreePredicate (a:rest) p = \x -> 
                                let fnx = (filterProperty a) x
                                    fnt = filterTreePredicate rest p
                                in
                                    fnx &&  ([] /= filter fnt (freeChildren x))


filterPropertyTree :: [String] -> (Item -> Bool)
filterPropertyTree items = filterTreePredicate items (\x -> True)


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
havingProperty expression items = filter (filterPropertyTree (splitOn "/" expression)) items

havingPropertyValueIn :: String -> [String] -> [Item] -> [Item]

havingPropertyValueIn expression possibleValues items = 
    let exp = splitOn "/" expression
        pred = filterTreePredicate exp (\x -> elem (value x) possibleValues) 
    in  
        filter pred items

countVariants :: [String] -> [Item] -> Int
countVariants [pred] [x] = if filterProperty pred x then amount x else 0
countVariants (pred : rest) [x]  = if filterProperty pred x then
                                    let childVariants = sum (map (\c -> countVariants rest [c]) (freeChildren x))
                                    in (amount x) * childVariants
                                else 
                                    0


