module InfiniteFind where

findFirst predicate =
  foldr findHelper []
  where
    -- foldr => lst = [listElement : maybeFound]
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise  maybeFound

understandableFindFirst predicate lst =
  if null lst
  then []
  else
    if predicate (head lst)
    then [head lst]
    else findFirst predicate (tail lst)