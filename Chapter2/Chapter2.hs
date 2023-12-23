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

-- Patter Matching

customGreeting "George" = "Oh, hey George!"
customGreeting name = "Hello, " <> name

matchTuple ("hello", "world") = "Hello there, you great big world"
matchTuple ("hello", name) = "Oh, hi there, " <> name
matchTuple (salutation, "George") = "Oh! " <> salutation <> " George!"
matchTuple n = show n

addValues [] = 0
addValues (first:rest) = first + (addValues rest)

fibs :: Int -> Int
fibs n
  | n == 0 = -1
  | n == 1 = 0
  | n == 2 = 1
  | otherwise = findFibs n 3 0 1
  where
    findFibs n index fst snd
      | n == index = fst + snd
      | otherwise = findFibs n (index + 1 ) (snd) (fst + snd)

primes :: Int -> Int
primes n
  | n == 0 = -1
  | otherwise = primes' n [1] 2 [1]
    where
      primes' :: Int -> [Int] -> Int -> [Int] -> Int 
      primes' n lst cur curLst
        | n == (length lst) = head lst
        | (isPrime cur curLst) == True =
          let
            lst' = cur : lst
          in
            primes' n lst' cur lst'
        | otherwise = primes' n lst (cur + 1) lst

isPrime :: Int -> [Int] -> Bool     
isPrime cur lst
  | lst == [1] = True
  | (cur `rem` (head lst)) == 0 = False
  | otherwise = isPrime cur (tail lst)

fancyNumbers n = 
  let
    fib = fibs n
    prime = primes n
  in
    "The fibonacci number is: " <> show fib <> " and the prime is: " <> show prime

-- @ for original value
-- _ for ignored value
printHead [] = "empty!"
printHead lst@(hd:_tail) =
  "the head of " <> (show lst) <> " is " <> show hd

-- Switch case
favoriteFood person =
  case person of
    "Ren" -> "Tofu"
    "Rebecca" -> "Falafel"
    "George" -> "Banana"
    name -> "I Don't Know what " <> name <> " likes!"

handleNums l =
  case l of
    [] -> "An empty list"
    [x] | x == 0 -> "a list called: [0]"
        | x == 1 -> "a singular list of [1]"
        | even x -> "a singleton list containing an even number"
        | otherwise -> "the list contains " <> (show x)
    _list -> "the list has more than 1 element"

-- co-recursion
radsToDegrees :: Float -> Int
radsToDegrees radians =
  let
    pi = 3.14
    degrees = cycle [0..359]
    converted = truncate $ (radians * 360) / 2 * pi
  in
    degrees !! converted
  
epicCycle inputList =
  cycleHelper inputList
  where
    cycleHelper [] = epicCycle inputList
    cycleHelper (x:xs) = x : cycleHelper xs

moreEpicCycle inputList =
  inputList <> moreEpicCycle inputList