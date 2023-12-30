module TypeHoleDemo where

exampleNumbers :: [Int]
exampleNumbers = [1..10]

getFiveNumbers :: [Int]
getFiveNumbers = take 5 _

getFiveNumbers' :: [Int]
getFiveNumbers' = let quantity = 5 in take quantity _
