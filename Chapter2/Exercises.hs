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

