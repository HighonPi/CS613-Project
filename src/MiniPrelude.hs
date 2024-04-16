{-# OPTIONS -XNoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

--
-- Source: The source code is taken from the Haskell Report 2010 Standard Prelude
module MiniPrelude (module MiniPrelude, module GHC.Maybe, module Data.Char, module Prelude) where

{-Imports-}

-- Provide standard functions
import Data.Char (isSpace, ord)
import GHC.Maybe hiding (Maybe (..))
import Prelude
  ( Applicative (..),
    Bool (..),
    Bounded (..),
    Char (..),
    Double (..),
    Eq (..),
    Float (..),
    Floating (..),
    Fractional (..),
    Functor (..),
    Int (..),
    Integer (..),
    Integral (..),
    Monad (..),
    Num (..),
    Ord (..),
    Rational (..),
    Real (..),
    RealFloat (..),
    RealFrac (..),
    String (..),
    abs,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atan2,
    atanh,
    ceiling,
    cos,
    cosh,
    decodeFloat,
    div,
    divMod,
    encodeFloat,
    error,
    exp,
    exponent,
    fail,
    floatDigits,
    floatRadix,
    floatRange,
    floor,
    fmap,
    fromInteger,
    isDenormalized,
    isIEEE,
    isInfinite,
    isNaN,
    isNegativeZero,
    log,
    logBase,
    max,
    min,
    mod,
    negate,
    otherwise,
    properFraction,
    quot,
    quotRem,
    recip,
    rem,
    return,
    round,
    scaleFloat,
    seq,
    significand,
    signum,
    sin,
    sinh,
    sqrt,
    tan,
    tanh,
    toInteger,
    toRational,
    truncate,
    (*),
    (**),
    (+),
    (-),
    (/),
    (/=),
    (<),
    (<=),
    (==),
    (>),
    (>=),
    (>>),
    (>>=),
  )

{-Infixr-}
infixr 9 .

infixr 8 ^, ^^

infixr 3 &&

infixr 2 ||

-- infixr 1 =<<

infixr 0 $, $!

infixl 9 !!

infixr 5 ++

infix 4 `elem`, `notElem`

{-Type: Ordering-}
data Ordering = LT | EQ | GT
  deriving (Eq, Ord, Bounded)

{-Functions: Tuple-}
fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

{-Functions: Numeric-}
subtract :: (Num a) => a -> a -> a
subtract = flip (-)

even, odd :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0
odd = not . even

gcd :: (Integral a) => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' x 0 = x
    gcd' x y = gcd' y (x `rem` y)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` (gcd x y)) * y)

(^) :: (Num a, Integral b) => a -> b -> a
x ^ 0 = 1
x ^ n | n > 0 = f x (n - 1) x
  where
    f _ 0 y = y
    f x n y = g x n
      where
        g x n
          | even n = g (x * x) (n `quot` 2)
          | otherwise = f x (n - 1) (x * y)
_ ^ _ = error "Prelude.^: negative exponent"

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x ^ n else recip (x ^ (-n))

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

{-Functions: General-}
id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($), ($!) :: (a -> b) -> a -> b
f $ x = f x
f $! x = x `seq` f x

{-Functions: On Boolean-}
(&&), (||) :: Bool -> Bool -> Bool
True && x = x
False && _ = False
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

{-Functions: Misc-}
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x = x
  | otherwise = until p f (f x)

asTypeOf :: a -> a -> a
asTypeOf = const

undefined :: a
undefined = error "Prelude.undefined"

{-Functions: List-}
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: [a] -> Int
length [] = 0
length (_ : l) = 1 + length l

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =
  q :
  ( case xs of
      [] -> []
      x : xs -> scanl f (f q x) xs
  )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x : xs')
  | p x = (x : ys, zs)
  | otherwise = ([], xs)
  where
    (ys, zs) = span p xs'
break p = span (not . p)

lines :: String -> [String]
lines "" = []
lines s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines s''

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s''
    where
      (w, s'') = break isSpace s'

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ' : s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (a -> Bool) -> [a] -> Bool
any p = or . map p
all p = and . map p

elem, notElem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)
notElem x = all (/= x)

sum, product :: (Num a) => [a] -> a
sum = foldl (+) 0
product = foldl (*) 1

maximum, minimum :: (Ord a) => [a] -> a  
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a : as) (b : bs) =
  z a b : zipWith z as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a : as) (b : bs) (c : cs) =
  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr
    (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs))
    ([], [], [])
