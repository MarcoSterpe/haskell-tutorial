module Chapter2 where
import Prelude hiding (foldl, foldr)
import FoldExamples (foldr) 

listIsEmpty list =
  if null list -- list == []
  then putStrLn "this list is empty"
  else putStrLn ("the first element of this list is: " <> show (head list))

countdown n =
  if n <= 0 then []
  else n : countdown (n-1) 

factors num =
  factors' num 2
  where
    factors' num fact
      | num == 1 = []
      | (num `rem` fact) == 0 = fact : factors'(num `div` fact) fact
      | otherwise = factors' num (fact + 1)

isBalanced :: [Char] -> Bool
isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
     | null s = count
     | head s == '(' = isBalanced' (count + 1) (tail s)
     | head s == ')' = isBalanced' (count - 1) (tail s)
     | otherwise = isBalanced' count (tail s)


-- equal to Prelude foldl
reduce func carryValue lst =
  if null lst then carryValue
  else
    let intermediateValue = func carryValue (head lst)
    in reduce func intermediateValue (tail lst)

reducedIsBalanced s =
  0 == reduce checkBalance 0 s
  where
    checkBalance count letter
      | letter == '(' = count + 1
      | letter == ')' = count - 1
      | otherwise = count

mapDoubleElems :: [Int] -> [Int]
mapDoubleElems nums =
  let double x = 2 * x
  in map double nums

doubleElems' = foldr doubleElem []
  where
    doubleElem num lst = (2 * num) : lst

doubleElems'' elems = foldr (applyElem (*2)) [] elems
  where
    applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = (f elem) : accumulator

doubleWithMap elems = map' (*2) elems

map'' f xs =
  if null xs then []
  else f (head xs) : map'' f (f tail xs)

checkGuestList guestList name =
  name `elem` guestList

foodCost = 
  [("Matteo", 10.00)
  ,("Fabio", 4.00)
  ,("Nico", 27.50)
  ,("Pietro", 14.75)
  ]

-- partyBudget (checkGuestList ["Matteo","Pietro"]) foodCost
partyBudget isAttending =
  foldr (+) 0
  . map snd
  . filter (isAttending . fst)
  -- filter (\name -> fst name `elem` ["Matteo","Pietro"]) $ foodCost)

-- list comprehensions
double = [2 * number | number <- [0..10]]
doubleOdds = [2 * number | number <- [0..10], odd number]
doubleOdds' = map (\num -> 2 * num) . filter odd $ [0..10]

{-
Input: pairs [1..10] [2..5]
Output: [(2,3),(2,5),(3,3),(3,5),(4,3),(4,5),(5,3),(5,5)]
-}
pairs as bs =
  let
    as' = filter (`elem` bs) as
    bs' = filter odd bs
    mkPairs a = map (\b -> (a,b)) bs'
  in
    concat $ map mkPairs as'

pairs' as bs = 
  [(a,b) | a <- as, b <- bs, a `elem` bs, odd b]

{-
comprehensive party Budget
partyBudget' isAttending willEat foodCost guest =
  foldl (+) 0 $
  [foodCost food
  | guest <- map fst guestList
  , food <- map snd guests
  , willEat guest food
  , isAttending guest
  ]
-}

-- equal to zip standard library function
combineList as bs =
  let
    a = head as
    b = head bs
    as' = tail as
    bs' = tail bs
  in
    if null as || null bs
    then []
    else (a,b) : combineList as' bs'

pairwiseSum xs ys =
  let
    sumElems pairs =
      let
        a = fst pairs
        b = snd pairs
      in
        a + b
  in
    map sumElems $ zip xs ys

pairwiseSum' xs ys = map (uncurry (+)) $ zip xs ys