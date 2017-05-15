
--Programming Languages Review | May 02, 2017


--The Y Combinator

-- Write down the function in Haskell

import Prelude

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Rewrite it without using base cases

fib n = if n <= 1 then 1 else fib (n-1) + fib (n-2)

-- Rewrite with lambda function

fib = \n -> if n <= 1 then 1 else fib (n-1) + fib(n-2)

-- Rewrite with lambda expressions

fib = (\n -> if (lte n one) one (plus (fib (pred n)) (fib (pred (pred n)))))

-- Doesn't make sense to call recursion on fib if fib needs an answer. So rewrite using the Y combinator

fib = Y (\fibF n . (lte n one) one (plus (fibF (pred n)) (fibF (pred (pred n)))))

---------------------------

-- Type hunting


(f:t1, g:t2) (f) = t11 -> t12      (f:t1, g:t2) (g) = t2
________________________________________________________        t1 = t11 -> t12
f:t1, g:t2 |- f: t11 -> t12        f:t1, g:t2 |- g: t11         t2 = t11
________________________________________________________
f:t1, g:t2 |- f g : t12
________________________________________________________
f:t2 |- \g:t2. f g : t2 -> t12 
________________________________________________________
* |- \f:t1. \g:t2 . f g : t1 -> t2 -> t12

-- Case matching

t ::= bool | int | t1 -> t2 | intlist
e ::= x | e1 e2 | \x : t1 . e | n | n : l | cons e1 e2 | case e of { 
                                                            nil => e2
                                                            cons x1 x2 => e3
                                                        }

__________________
G |- nil : intlist

G |- e:int  G |- e2:intlist
___________________________
G |- cons e1 e2: intlist


G |- e1:intlist   G |- e2: t   G, x1:int, x2:intlist |- e3: t
______________________________________________________________
G |- case e1 of {nil => e2; cons x1 x2 => e3}

-- Type checking

b f x = f (f x) -- look at the smallest bit first of the left side

b :: (a -> a) -> a -> a

-- data types

data A a = 
        C1
      | C2 a (A a)

-- This is the structure of a list

    C1      C2          C2          C2
            /\          /\          /\
           v  C1      v1  C2      v1  C2
                         /  \        /  \
                       v2    C3     v2   C2
                                        /  \
                                      v3    C1

-- Applicative

instance Applicative Maybe where
    pure 

-- Monads

class Applicative f => Monad f where
    (>>=) :: f a -> (a -> f b) -> f b

instance Monad Maybe where
    
    Nothing >>= k = Nothing

    (Just v) >>= k = k v

instance Monad (Either e) where

    (Left err) >>= k = Left err

    (Right v) >>= k = k v

instance Monad ((->) r) where

    -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)

    ra >>= k = \r -> k (ra r) r



-- Functions on Lists

- head takes a list and returns its head. The head of a list is basically its first element.

- tail takes a list and returns its tail. In other words, it chops off a list's head.

- last takes a list and returns its last element.

- init takes a list and returns everything except its last element.

- length takes a list and returns its length, obviously.

- null checks if a list is empty. If it is, it returns True, otherwise it returns False. Use this function instead of xs == [] (if you have a list called xs)

- reverse reverses a list.

- take takes number and a list. It extracts that many elements from the beginning of the list. Watch.

- drop works in a similar way, only it drops the number of elements from the beginning of a list.

- maximum takes a list of stuff that can be put in some kind of order and returns the biggest element.

- minimum returns the smallest.

- sum takes a list of numbers and returns their sum.

- product takes a list of numbers and returns their product.

- elem takes a thing and a list of things and tells us if that thing is an element of the list. It's usually called as an infix function because it's easier to read that way.

- cycle takes a list and cycles it into an infinite list. If you just try to display the result, it will go on forever so you have to slice it off somewhere.

- repeat takes an element and produces an infinite list of just that element. It's like cycling a list with only one element.

- Although it's simpler to just use the replicate function if you want some number of the same element in a list. replicate 3 10 returns [10,10,10].

-- Functions on Tuples

- fst takes a pair and returns its first component.

- snd takes a pair and returns its second component. Surprise!





