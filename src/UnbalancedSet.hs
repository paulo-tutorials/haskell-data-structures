{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module UnbalancedSet
( UnbalancedSet(..)
, member'
, insert'
, insert'1
, complete
, create2) 
where

import qualified Set.Set as S

data UnbalancedSet a = T (UnbalancedSet a) a (UnbalancedSet a) | E 
    deriving (Show, Ord, Eq)

instance (Ord a) => S.Set (UnbalancedSet a) a where
    empty = E
    
    insert a E = T E a E
    insert a t@(T left x right)
        | a < x = T (S.insert a left) x right
        | a > x = T left x (S.insert a right)
        | otherwise = t

    member _ E = False
    member a (T left x right)
        | a < x = S.member a left
        | a > x = S.member a right
        | otherwise = True

-- exercise 2.2
member' :: (Ord a) => a -> UnbalancedSet a -> Bool
member' _ E = False
member' a (T left x right)
    | a < x = member' a left 
    | otherwise = member'1 a right x
    where   member'1 b E candidate
                | b > candidate = False
                | otherwise = True
            member'1 b (T left y right) candidate
                | b < y = member'1 b left candidate
                | otherwise = member'1 b right y

-- exercise 2.3
insert' :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert' a E = T E a E 
insert' a t@(T left x right)
    | member' a t = t
    | otherwise = insert'1 a t
    where insert'1 b E = T E b E
          insert'1 b u@(T left y right)
              | b < y = T (insert'1 b left) y right
              | b > y = T left y (insert'1 b right)
              | otherwise = u

-- exercise 2.4
insert'1 :: Ord a => a -> UnbalancedSet a -> UnbalancedSet a
insert'1 a t = result $ tryInsert a t
    where   result Nothing = t
            result (Just x)  = x

tryInsert :: Ord a => a -> UnbalancedSet a -> Maybe (UnbalancedSet a)
tryInsert a E = Just (T E a E)
tryInsert a t@(T left x right)
    | a < x = makeMaybeTree (tryInsert a left) x (Just right)
    | otherwise = makeMaybeTree (Just left) x (tryInsert' a right x)
    where   tryInsert' b E candidate
                | b > candidate = Just (T (T E candidate E) b E)
                | otherwise     = Nothing
            tryInsert' b (T left y right) candidate
                | b < y         = makeMaybeTree (tryInsert' b left candidate) b (Just right)
                | otherwise     = makeMaybeTree (Just left) y (tryInsert' b right y)
            makeMaybeTree Nothing _ _         = Nothing
            makeMaybeTree _ _ Nothing         = Nothing
            makeMaybeTree (Just l) v (Just r) = Just (T l v r)

-- exercise 2.5.a
complete :: a -> Int -> UnbalancedSet a
complete _ 0 = E
complete x n = T subtree x subtree
    where subtree = complete x (n - 1)

-- exercise 2.5.b
create2 :: a -> Int -> UnbalancedSet a
create2 _ 0 = E
create2 x n = T subtree x (T E x subtree)
    where subtree = create2 x (n - 1)