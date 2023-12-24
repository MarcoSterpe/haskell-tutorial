module Chapter3 where

-- Types

pi :: Float
pi = 3.14

one, two :: Int
one = 1
two = 2

pi' = 3.14 :: Float

calculateTotalCost basePrice =
  let
    priceWithServiceFee :: Int
    priceWithServiceFee = basePrice + 1
    customaryTip = 7 :: Int
  in
    priceWithServiceFee + customaryTip

addOne :: Int -> Int
addOne = (+1)

addThreeNumbers :: Int -> Int -> Int -> Int
addThreeNumbers a b c = a + b + c

formatter :: Int -> String
formatter = show

incrementAndShow :: Int -> (Int -> String) -> String
incrementAndShow num formatter = formatter (num + 1)

pointful :: [Int] -> Int -> Int
pointful xs n = foldr (+) 0 xs * n
etaReduced :: [Int] -> Int -> Int
etaReduced xs = (*) (foldr (+) 0 xs)
pointfree :: [Int] -> Int -> Int
pointfree = (*) . foldr (+) 0