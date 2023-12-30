module Chapter3 where

import Data.List as L

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

identity :: a -> a
identity val = val

apply :: (a -> b) -> a -> b
apply f val = f val

incrementInt :: Int -> Int
incrementInt n = n + 1

incremented :: Int
incremented = apply incrementInt 1 -- apply :: (Int -> Int) -> Int -> Int

{--
sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  {- step 1  undefined -}
  let
    --step2
    getBiggest :: [Int] -> Int
    getBiggest = undefined
    
    getSmallest :: [Int] -> Int
    getSmallest :: undefined
    
    -- step 3
    allBiggests :: [[Int]]
    allBiggests = map getBiggests allNums
    
    allSmallests :: [[Int]]
    allSmallests = map getSmallests allNum

    -- step 4
    zipSizes :: [[Int]] -> [[Int]] -> [([Int], [Int])]
    zipSizes = undefined

    sizePairs :: [([Int], [Int])]
    {-- sizePairs = zipSizes allBiggests allSmallests --}
    sizePairs = zip allBiggests allSmallests -- step 4 b

    -- step 5
    differences :: ([Int],[Int]) -> Int
    differences = undefined

    differences' :: [String]
    differences' = map (show . differences) sizePairs   
  in
    undefined
--}

sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  let
    lowestInt :: Int
    lowestInt = minBound

    highestInt :: Int
    highestInt = maxBound 

    getBiggests :: [Int] -> Int
    getBiggests nums = helper lowestInt nums
      where
        helper :: Int -> [Int] -> Int
        helper big lst
          | null lst = big 
          | (head lst) > big = helper (head lst) (tail lst)
          | otherwise = helper big (tail lst)
    
    getSmallests :: [Int] -> Int
    getSmallests nums = helper highestInt nums
      where
        helper :: Int -> [Int] -> Int
        helper small  lst
          | null lst = small
          | (head lst) < small = helper (head lst) (tail lst)
          | otherwise = helper small (tail lst)
      
    allBiggests :: [Int]
    allBiggests = map getBiggests allNums
    
    allSmallests :: [Int]
    allSmallests = map getSmallests allNums

    sizePairs :: [(Int, Int)]
    sizePairs = zip allBiggests allSmallests 

    differences :: (Int,Int) -> Int
    differences (a,b) = a - b

    differences' :: [String]
    differences' = map (show . differences)  sizePairs   
  in
    L.intercalate "," differences' 

showBiggest =
  let biggestInfo = sumBiggest [[1,1,2,3,4,4],[1,2,5,5],[-1,-2,5,-10,5]]
  in print $ "sumBiggest says: " <> biggestInfo