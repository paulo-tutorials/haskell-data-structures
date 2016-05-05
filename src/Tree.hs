module Tree 
( Tree(..)
, member
, insert
, member')
where

data Tree a = Elem a (Tree a) (Tree a) | Empty 
    deriving (Show, Ord, Eq)

member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member a (Elem x left right)
    | a < x = member a left
    | a > x = member a right
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Elem a Empty Empty
insert a t@(Elem x left right)
    | a < x = Elem x (insert a left) right
    | a > x = Elem x left (insert a right)
    | otherwise = t

member' :: Ord a => a -> Tree a -> Bool
member' _ Empty = False
member' a (Elem x left right)
    | a < x = member' a left
    | otherwise = member'1 a right x

member'1 :: Ord a => a -> Tree a -> a -> Bool
member'1 a Empty candidate
    | a > candidate = False
    | otherwise = True
member'1 a (Elem x left right) candidate
    | a < x = member'1 a left candidate
    | otherwise = member'1 a right x