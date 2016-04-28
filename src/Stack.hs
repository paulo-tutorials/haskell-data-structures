module Stack 
( Stack(..)
, isEmpty
, cons
, shead
, stail)
where

data Stack a = Cons a (Stack a) | Empty deriving (Eq, Show)

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

cons :: a -> Stack a -> Stack a
cons x Empty = Cons x Empty
cons x stack = Cons x stack

shead :: Stack a -> a
shead Empty = error "Stack.head: empty stack"
shead (Cons a _) = a

stail :: Stack a -> Stack a
stail Empty = error "Stack.tail: empty stack"
stail (Cons _ tail') = tail'