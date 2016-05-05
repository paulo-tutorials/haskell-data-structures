module Tree 
( Tree(..)
, member
, insert)
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