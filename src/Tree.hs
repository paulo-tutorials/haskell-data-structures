{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Tree(Tree(..)) where

import qualified Set.Set as S

data Tree a = T (Tree a) a (Tree a) | E 
    deriving (Show, Ord, Eq)

instance (Ord a) => S.Set (Tree a) a where
    empty = E
    
    insert a E = T E a E
    insert a t@(T left x right)
        | a < x = T (S.insert a left) x right
        | a > x = T left x (S.insert a right)
        | otherwise = t

    member _ E = False
    member a (T left x right)
        | a < x = S.member a left 
        | otherwise = member' a right x

member' :: (Ord a) => a -> Tree a -> a -> Bool
member' a E candidate
    | a > candidate = False
    | otherwise = True
member' a (T left x right) candidate
    | a < x = member' a left candidate
    | otherwise = member' a right x