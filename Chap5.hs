module Chap5 where

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

nonsense :: Bool -> Integer
nonsense True = 670
nonsense False = 34635

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousAndManuallyNested = \i -> \b -> i + (nonsense b)

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i = \x -> x

c :: a -> b -> a
c = \x y -> x

c'' :: b -> a -> b
c'' = \x y -> x

c' :: a -> b -> b
c' = \x y -> y

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> (a -> c)
co f g x = f(g(x))

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' = ($)

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y where
  x = "Singin"
  y = "Somewhere"

-- Type-Kwon-Do 1

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g(f(x))

-- Type-Kwon-Do 2

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- Type-Kwon-Do 3

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- Type-Kwon-Do 4

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ g $ f x