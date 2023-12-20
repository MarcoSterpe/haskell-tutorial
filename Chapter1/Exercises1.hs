module Exercises1 where

-- 1. Factorial
factorial num
  | num == 0 = 1
  | otherwise = 
      let
        nextNum = num - 1
        nextResult = factorial nextNum 
      in
        num * nextResult

findFibonacci num index a b
 | index == num = a + b
 | otherwise =
    let
      nextIndex = index + 1
      nextA = b
      nextB = a + b
    in
      findFibonacci num nextIndex nextA nextB

-- 2. Fibonacci
fibonacci num 
 | num == 0 = 0
 | num == 1 = 1
 | otherwise = findFibonacci num 2 0 1

-- 3. Curry and uncurry
uncurriedAddition nums =
  let
    a = fst nums
    b = snd nums
  in a + b

myCurry fun a b = fun (a,b)
myUncurry fun (a,b) = fun a b

addition = myCurry uncurriedAddition
addOne = addition 1
addTwo = addition 2

uncurriedAddition' = myUncurry addition