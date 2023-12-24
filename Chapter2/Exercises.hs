module Excerises where

reverseLstFoldl :: [a] -> [a]
reverseLstFoldl lst =
  foldl helper [] lst
    where
      helper a b = b : a

{-
reverseLstFoldl xs = foldl (flip (:)) [] xs
-}

reverseLstFoldr :: [a] -> [a]
reverseLstFoldr lst =
  foldr helper [] lst
  where
    helper a b = b <> [a]

{-
reverseLstFoldl xs = foldr (\x acc -> acc ++ [x]) [] xs
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = [f x y | (x,y) <- zip xs ys]

zipWith''' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith''' f xs ys = foldl (\acc (x, y) -> acc ++ [f x y]) [] (zip xs ys)

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = foldl (\acc x -> acc ++ f x) []

concatMap'' :: (a -> [b]) -> [a] -> [b]
concatMap'' f = foldr ((++) . f) []


