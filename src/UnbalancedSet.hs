{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module UnbalancedSet
( UnbalancedSet(..)
, member'
, insert') 
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
    | a < x = S.member a left 
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
