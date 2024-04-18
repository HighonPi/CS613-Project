module Source5 where

f :: Bool -> Bool -> Bool
f x y = not x && y

r = f True False
