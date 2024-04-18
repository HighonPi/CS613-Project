module Source6 where

addRepeat:: Int -> Int -> Int -> Int
addRepeat a 0 sum_rn = sum_rn
addRepeat a n sum_rn = addRepeat a (n - 1) (sum_rn + a)

r = addRepeat 2 2 0
