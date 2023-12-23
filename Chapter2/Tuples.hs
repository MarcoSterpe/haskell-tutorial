module Tuples where

fst (x, _, _) = x
snd (_, x, _) = x
thrd (_, _, x) = x
-- map($ (1,2,3)) [fst, snd, thrd] => [1,2,3]