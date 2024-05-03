{-# OPTIONS -XNoImplicitPrelude #-}

module Examples where

import MiniPrelude

-- Support for pattern matching (Case statements)
myFun :: Integer -> String
myFun 1 = "One!"
myFun 2 = "Two!"
myFun 3 = "Three!"
myFun x = "Not between 1 and 3"

patternMatchingExample = myFun 4

-- Support for irreducible computations
irreducableExpressionEvaln = 1 + (2 * 3)

-- Support for anonymous functions
anonFn = (\x -> x + x)

lamdaApplicationExample = anonFn 1

-- Support for String type
getString :: Bool -> String
getString True = "Yes, its True"
getString False = "No its false"

aString = getString True

-- Support for standard function defns
double :: Num a => a -> a
double x = x + x

a = double 1

-- For more than 1 argument
f :: Bool -> Bool -> Bool
f x y = not x && y

result = f True False

-- For recursive calls
-- This example has partial support only because type constructors are not handled
addRepeat:: Int -> Int -> Int -> Int
addRepeat a 0 sum_rn = sum_rn
addRepeat a n sum_rn = addRepeat a (n - 1) (sum_rn + a)

rst = addRepeat 2 2 0

-- Support for list operations
reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

rList = reverseList [1, 2, 3]

-- Support for Float type
x = 1.0 + 2.0

-- This example is only partially supported because let statements are not supported yet
fltResult = maximum [1.0, 2.0, 3.0]

-- Nested function application
y = anonFn (double 1)
