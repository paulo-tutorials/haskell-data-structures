module Tree 
( Tree(..)
, member
, insert
, member')
where

data Tree a = T (Tree a) a (Tree a) | E 
    deriving (Show, Ord, Eq)

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member a (T left x right)
    | a < x = member a left
    | a > x = member a right
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert a E = T E a E
insert a t@(T left x right)
    | a < x = T (insert a left) x right
    | a > x = T left x (insert a right)
    | otherwise = t

member' :: Ord a => a -> Tree a -> Bool
member' _ E = False
member' a (T left x right)
    | a < x = member' a left
    | otherwise = member'1 a right x

member'1 :: Ord a => a -> Tree a -> a -> Bool
member'1 a E candidate
    | a > candidate = False
    | otherwise = True
member'1 a (T left x right) candidate
    | a < x = member'1 a left candidate
    | otherwise = member'1 a right x