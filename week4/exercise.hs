{-# OPTIONS_GHC -Wall #-}

module Week4 where

-- Exercise 1 : Wholemeal programming

fun1' :: [Integer] -> Integer
fun1' =  product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate next
  where
    next :: Integer -> Integer
    next x = if even x then div x 2 else 3 * x + 1

-- Exercise 2 : FoldTree

data Tree a = Leaf 
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insert' :: a -> Tree a -> Tree a
insert' a Leaf = Node 0 Leaf a Leaf
insert' a (Node _ l b r) =
    case compare (height l) (height r) of
        LT -> let x = insert' a l in Node (height x + 1) x b r
        _  -> let y = insert' a r in Node (height y + 1) l b y
    where
        height :: Tree a -> Integer
        height Leaf = -1
        height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf 

-- Exercise 3 : More folds

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> (f x) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x s -> f s x) base (reverse xs)

-- Excercise 4 : Sieve of Sundaram

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map prime xs
    where 
        m = div n 2
        prime x = x * 2 + 1
        xc = [xn | i <- [1..n], j <- [1..m], let xn = i+j+2*i*i, xn <= n]
        xs = filter (\x -> notElem x xc) [1..n]