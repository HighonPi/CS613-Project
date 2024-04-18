module Source4 where

x = 1
y = 2
z = x

namedValueDemo = 1 - y

arithmeticDemo = 1 + 2 * 3


lamdaDemo = (\x -> x + x)

lamdaApplicationDemo = lamdaDemo 1

usageOfPreludeFunctionsDemo = abs (negate 3)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

patternMatchingOnOperatorDemo = reverseList [1, 2, 3]

test :: Integer -> String
test _ = "some string"

applyTest = test 4

