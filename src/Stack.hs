module Stack 
( Stack(..)
, isEmpty
, cons
, shead
, stail
, update
, (+++)
, sufixes)
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

update :: Stack a -> Int -> a -> Stack a
update Empty _ _ = error "Stack.update: index too large"
update (Cons x rest) 0 y = Cons y rest
update (Cons x rest) index y = Cons x $ update rest (index - 1) y

(+++) :: Stack a -> Stack a -> Stack a
Empty +++ Empty = Empty
x +++ Empty = x
Empty +++ y = y
(Cons x Empty) +++ y = Cons x y
(Cons x rest) +++ y = Cons x $ rest +++ y

sufixes :: [a] -> [[a]]
sufixes [] = [[]]
sufixes r@(x:xs) = (r:sufixes xs)
