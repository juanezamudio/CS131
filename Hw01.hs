-- Homework 1.0: Haskell warmup
-- Juan Zamudio
-- Veronica Rivera
-- 1-22-2017
-- Due 2017-01-29

{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

-- Let's learn some Haskell! We'll be going over some rudiments in class,
-- and there's excellent
-- [documentation](https://www.haskell.org/documentation) online.
--
-- In most places where I'd like you to fill in a definition, I've used
-- the convenient Haskell term `undefined`, which let's you compile
-- an incomplete program. (Running undefined parts of your program is an
-- error, and your program will crash.)
--
-- Please leave the following line in. (If you take it out, the grader
-- will reject your program.) We'll talk more about Haskell's module
-- system later in the semester.
--
module Hw01 where
--
-- You can test this program by running `ghci` on it. If you edit
-- your code, you can use the `:reload` command to load in your new
-- definitions.
--
-- If your program has type errors, it won't compile. If your submitted
-- program doesn't compile, you will get no points.

-- The following imports are needed for Problem 9.

import qualified Data.Map as Map
import Data.Map (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

-- **Problem 1: natural recursion**
--
-- Please don't use any Prelude functions to implement these---just write
-- natural recursion, like we did in class.
--
-- Write a function called `sumUp` that sums a list of numbers.

sumUp :: [Int] -> Int
sumUp []     = 0
sumUp (x:xs) = x + sumUp xs

-- Write a function called `evens` that selects out the even numbers
-- from a list. For example, `evens [1,2,3,4,5]` should yield
-- `[2,4]`. You can use the library function `even`.

evens :: [Int] -> [Int]
evens []     = []
evens (x:xs) =
  if even x
    then [x] ++ evens xs
    else evens xs

-- Write a function called `incAll` that increments a list of numbers
-- by one. You'll have to fill in the arguments and write the cases yourself.

incAll :: [Int] -> [Int]
incAll []     = []
incAll (x:xs) = [x + 1] ++ incAll xs

-- Now write a function called `incBy` that takes a number and
-- increments a list of numbers *by that number*.

incBy :: Int -> [Int] -> [Int]
incBy _ []     = []
incBy n (x:xs) = [x + n] ++ incBy n xs

-- Write a function `append` that takes two lists and appends them.  For
-- example, `append [1,2] [3,4] == [1,2,3,4]`. (This function is called
-- `(++)` in the standard library... but don't use that to define your
-- version!)

append :: [Int] -> [Int] -> [Int]
append [] []    = []
append x  []    = x
append [] y     = y
append (x:xs) y = x : append xs y

-- **Problem 2: data types**
--
-- Haskell (and functional programming in general) is centered around
-- datatype definitions. Here's a definition for a simple tree:

data IntTree = Empty | Node IntTree Int IntTree deriving (Eq,Show)

-- Write a function `isLeaf` that determines whether a given node is
-- a leaf, i.e., both its children are `Empty`.

isLeaf :: IntTree -> Bool
isLeaf Empty = True
isLeaf (Node l _ r) = l == Empty && r == Empty

-- Write a function `sumTree` that sums up all of the values in an
-- `IntTree`.

sumTree :: IntTree -> Int
sumTree Empty = 0
sumTree (Node l x r) = x + sumTree l + sumTree r

-- child3 = Node Empty 7 Empty
-- child2 = Node child3 10 Empty
-- child1 = Node Empty 2 Empty
-- child = Node Empty 1 child1
-- tree = Node child 5 child2

-- Write a function `fringe` that yields the fringe of the tree from
-- left to right, i.e., the list of values in the leaves of the tree,
-- reading left to right.
--
-- For example, the fringe of `Node (Node Empty 1 (Node Empty 2 Empty)) 5 (Node (Node Empty 7 Empty) 10 Empty)` is `[2,7]`.

fringe :: IntTree -> [Int]
fringe Empty = []
fringe (Node l x r) =
  if isLeaf (Node l x r)
    then [x]
    else
      fringe l ++ fringe r

-- **Problem 3: insertion sort**
--
-- Write a function `insertionSort` that takes a list of `Int`s
-- and produces one in sorted order. Use the [insertion sort
-- algorithm](https://en.wikipedia.org/wiki/Insertion_sort). You might
-- want to write a helper function.

insertionSort :: [Int] -> [Int]
insertionSort []         = []
insertionSort [a]        = [a]
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Int -> [Int] -> [Int]
insert a []             = [a]
insert a (y:ys) | y > a = a:y:ys
              | otherwise = y : insert a ys


-- **Problem 4: binary search trees **
--
-- Write a function `isBST` to determine whether or not a given tree
-- is a strict binary search tree, i.e., the tree is either empty, or it
-- is node such that:
--
-- * all values in the left branch are less than the value of the node, and
-- * all values in the right branch are greater than the value of the node,
-- * both children are BSTs.
--
-- I've given you a helper function `maybeBounded` that checks whether a
-- given `Int` is bounded. It uses the Haskell `Maybe` type, which is
-- essentially defined as:

-- ```haskell
-- data Maybe Int = Nothing | Just Int
-- ```

-- `Maybe` makes a type *nullable*. In Java, every non-primitive type is
-- nullable---the `null` object can have any class. In Haskell, you must
-- explicitly ask for nullability, and nullness and non-nullness are
-- *both* explicit: `Nothing` is null, and the non-null `Just x`
-- holds a value `x`. We'll look at this more deeply in the next
-- assignment, when we talk about datatypes.

maybeBounded :: Maybe Int -> Maybe Int -> Int -> Bool
maybeBounded Nothing Nothing _ = True
maybeBounded Nothing (Just upper) x = x < upper
maybeBounded (Just lower) Nothing x = lower < x
maybeBounded (Just lower) (Just upper) x = lower < x && x < upper

isBST :: IntTree -> Bool
isBST Empty = True
isBST (Node l x r) = check (Node l x r) Nothing Nothing

check :: IntTree -> Maybe Int -> Maybe Int-> Bool
check Empty Nothing Nothing = True --When a tree is empty, it doesn't matter what the bounds are because there are no leaves to be bounded
check Empty _ _ = True
check (Node l a r) lower upper | maybeBounded lower upper a = (check l lower (Just a)) && (check r (Just a) upper)
                               | otherwise = False

-- rootT = (Node Empty 10 Empty)
rootCheck :: IntTree
level1l :: IntTree
level2l :: IntTree
level2r :: IntTree
right :: IntTree
rootCheck = (Node level1l 10 right)
level1l = (Node level2l 5 level2r)
level2l = (Node Empty 3 Empty)
level2r = (Node Empty 6 Empty)
-- level3l = Empty
-- level3r = Empty
right = (Node Empty 15 Empty)
-- rr = (Node Empty 30 Empty)


-- Write a function `insertBST` that performs BST insert. You may
-- assume your input is a BST.

insertBST :: Int -> IntTree -> IntTree
insertBST a Empty = (Node Empty a Empty)
insertBST a (Node l x r) | (a == x) = (Node l x r)
                         | a < x = (Node (insertBST a l) x r)
                         | a > x = (Node l x (insertBST a r))


-- Write a function `deleteBST` that removes a given value from a
-- BST. You may assume your input is a BST. Feel free to look up the
-- algorithm... I had to!

-- It doesn't really matter which algorithm you use, so long as the
-- function works correctly, i.e., for all BSTs `t`:

-- * `deleteBST x t` is a BST,
-- * `deleteBST x t` runs in O(log n) time in expectation,
-- * `x` doesn't appear in `deleteBST x t`,
-- * for all `y` in `t`, if `y /= x`, then `y` appears in `deleteBST y t`.

-- You are, as always, free to introduce any helper functions you might need.
--
deleteBST :: Int -> IntTree -> IntTree
deleteBST _ Empty = Empty
deleteBST a (Node l x r) | a < x = Node (deleteBST a l) x r
deleteBST a (Node l x r) | a > x = Node l x (deleteBST a r)
deleteBST a (Node l x r) | a == x =
  case (l, r) of
       (Empty, Empty) -> Empty
       (Empty, r) -> r
       (l, Empty) -> l
       (l, r) -> (smallest a r)
--
smallest :: Int -> IntTree -> IntTree
smallest _ Empty = Empty
smallest a (Node l x r) | a < x = (Node l a r)
                        | otherwise = (Node l x r)

--  5
-- / \
--3  6
-- CASE1: Node to be removed has no children
-- CASE2: Node to be removed has 1 child
-- CASE3: Node to be removed has 2 children

-- **Problem 5: maps and folds**
--
-- We're going to define each of the functions we defined in Problem 1,
-- but we're going to do it using *higher-order functions* that are built
-- into [the
-- Prelude](http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html). In
-- particular, we're going to use `map`, `filter`, and the two folds,
-- `foldr` and `foldl`. To avoid name conflicts, we'll name all of the
-- new versions with a prime, `'`.
--
-- Define a function `sumUp'` that sums up a list of numbers.
--
sumUp' :: [Int] -> Int
sumUp' [] = 0
sumUp' (x:xs) = foldl (+) x xs
--
-- Define a function `evens'` that selects out the even numbers from a
-- list.
--
evens' :: [Int] -> [Int]
evens' [] = []
evens' l = filter (\x -> even x) l
--
-- Define a function `incAll'` that increments a list of numbers by
-- one.
--
incAll' :: [Int] -> [Int]
incAll' [] = []
incAll' l = map (\x -> x + 1) l
--
-- Define a function `incBy'` that takes a number and then increments
-- a list of numbers *by that number*.
--
incBy' :: Int -> [Int] -> [Int]
incBy' _ [] = []
incBy' n l = map (\x -> x + n) l
--
-- Define a function `rev'` that reverses a list. Don't use
-- anything but a folding function (your choice), the list
-- constructors, and lambdas/higher-order functions.
--
rev' :: [Int] -> [Int]
rev' [] = []
rev' (x:xs) = foldr (\y ys -> ys ++ [y]) [] (x:xs)

--
-- Define two versions of the function `append'` that appends two
-- lists.  One, `appendr`, should use `foldr`; the other,
-- `appendl`, should use `foldl`. You can use the list
-- constructors, higher-order functions, and `rev'`.
--
appendr :: [Int] -> [Int] -> [Int]
appendr [] [] = []
appendr (x:xs) [] = (x:xs)
appendr [] (y:ys) = (y:ys)
appendr (x:xs) (y:ys) = rev' (foldr (\m n -> rev' m ++ rev' n) (x:xs) [(y:ys)])

appendl :: [Int] -> [Int] -> [Int]
appendl [] [] = []
appendl (x:xs) [] = (x:xs)
appendl [] (y:ys) = (y:ys)
appendl (x:xs) (y:ys) = foldl (\m n -> m ++ n) (x:xs) [(y:ys)]
--
-- **Problem 6: defining higher-order functions**
--
-- We're going to define several versions of the `map` and `filter`
-- functions manually, using only natural recursion and folds---no using
-- the Prelude or list comprehensions. Note that I've written the
-- *polymorphic* types for you.
--
-- Define `map1` using natural recursion.
--
map1 :: (a -> b) -> [a] -> [b]
map1 _ [] = []
map1 f (x:xs) = f x : map1 f xs
--
-- Define `map2` using a folding function.
--
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\y ys -> f y: ys) [] xs

--
-- Define `filter1` using natural recursion.

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x:xs) | f x = x : (filter1 f xs)
                 | otherwise = filter1 f xs

-- Define `filter2` using a folding function.
--
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = foldr (\y ys -> if f y then y:ys else ys) [] xs
--
-- **Problem 7: polymorphic datatypes **
--
-- We've already briefly seen the `Maybe` type in the first homework. In
-- the next two problems, we'll look at `Maybe`, pairs, and `Either` in
-- more detail.
--
-- Haskell's type system is rigid compared to most other languages. In
-- time, you will come to view this as a *feature*---languages that let
-- you 'cheat' their safety mechanisms end up making you pay for it with
-- complexity elsewhere. But for now, let's get familiar with the
-- structures and strictures of types.
--
-- The `Maybe` datatype introduces *nullability* in a controlled
-- fashion---values of the type `Maybe a` can be `Nothing` or `Just x`,
-- where `x` is a value of type `a`. Note that `Maybe` is polymorphpic:
-- we can choose whatever type we want for `a`, e.g., `Just 5 :: Maybe
-- Int`, or we can leave `a` abstract, e.g., `Just x :: Maybe a` iff `x ::
-- a`.
--
-- Write a function `mapMaybe` that behaves like `map` when its
-- higher-order function argument returns `Just x`, but filters out
-- results where the function returns `Nothing`.
--
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = map fromMaybe (removeNothing (map f (x:xs)))

removeNothing :: [Maybe b] -> [Maybe b]
removeNothing [] = []
removeNothing (y:ys) =
  case y of
  Nothing -> removeNothing ys
  (Just y) -> (Just y) : removeNothing ys

fromMaybe :: Maybe b -> b
fromMaybe (Just b) = b

myFunc :: Char -> Maybe Int
myFunc a | a == 'a' = Just 1
         | otherwise = Nothing
--
-- The pair datatype allows us to aggregate values: values of type
-- `(a,b)` will have the form `(x,y)`, where `x` has type `a` and `y` has
-- type `b`.
--
-- Write a function `swap` that takes a pair of type `(a,b)` and returns
-- a pair of type `(b,a)`.
--
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
--
-- Write a function `pairUp` that takes two lists and returns a list of
-- paired elements. If the lists have different lengths, return a list of
-- the shorter length. (This is called `zip` in the prelude. Don't define
-- this function using `zip`!)
--
pairUp :: [a] -> [b] -> [(a,b)]
pairUp [] [] = []
pairUp _ [] = []
pairUp [] _ = []
pairUp (x:xs) (y:ys) = (x,y) : pairUp xs ys
--
-- Write a function `splitUp` that takes a list of pairs and returns a
-- pair of lists. (This is called `unzip` in the prelude. Don't define
-- this function using `unzip`!)
--
splitUp :: [(a,b)] -> ([a],[b])
splitUp [] = ([], [])
splitUp ((x,y):xs) =
  let
    (ys, zs) = splitUp xs
    in (x:ys,y:zs)
--
-- Write a function `sumAndLength` that simultaneously sums a list and
-- computes its length. You can define it using natural recursion or as a
-- fold, but---traverse the list only once!
--
sumAndLength :: [Int] -> (Int,Int)
sumAndLength [] = (0, 0)
sumAndLength (x:xs) =
  let
    (s, l) = sumAndLength xs
    in (x + s, acc l)
    where acc a = a + 1
--
-- **Problem 8: defining polymorphic datatypes**
--
-- The `Either` datatype introduces *choice* in a controlled
-- fashion---values of the type `Either a b` can be either `Left
-- x` (where `x` is an `a`) or `Right y` (where `y` is
-- a `b`).
--
-- Define a datatype `EitherList` that embeds the `Either` type into a
-- list. (This isn't a good idea, but it's a good exercise!)
--
-- To see what I mean, let's combine lists and the `Maybe`
-- datatype. Here's Haskell's list datatype:
--
-- ```
-- data [a] = [] | a:[a]
-- ```
--
-- Here's the Maybe datatype:
--
-- ```
-- data Maybe a = Nothing | Just a
-- ```
--
-- What kinds of values inhabit the type `[Maybe a]`? There are two cases:
--
-- - `[]`, the empty list
-- - `a:as`, where `a` has type `Maybe a` and `as` is a list of type `[Maybe a]`
--
-- But we can really split it into three cases:
--
-- - `[]`, the empty list
-- - `a:as`, where `as` is a list of type `[Maybe a]`, and:
--   - `a` is `Nothing`
--   - `a` is `Just a'`, where `a'` has type `a`
--
-- Put another way:
--
-- - `[]`, the empty list
-- - `Nothing:as`, where `as` is a list of type `[Maybe a]`
-- - `Just a:as`, where `a` has type `a` and `as` has type `[Maybe a]`
--
-- To define MaybeList, we'll write a data structure that has those
-- constructors expliclty.
--
-- ```
-- data MaybeList a =
--     Nil
--   | ConsNothing (MaybeList a)
--   | ConsJust a (MaybeList a)
-- ```
--
-- Note that these match up exactly with the last itemized list of cases.
--
-- Okay: do it for `Either`! Fill in the functions below---they should
-- behave like the Prelude functions. You'll also have to fill in the
-- type. We've given you the constructors' names. Make sure your `Cons`
-- constructors takes arguments in the correct order, or we won't be able
-- to give you credit for *any* of this problem.
--
-- > data EitherList a b =
-- >     Nil
-- >   | ConsLeft {- fill in -}
-- >   | ConsRight {- fill in -}
-- >   deriving (Eq, Show)
-- >
-- > toEither :: [Either a b] -> EitherList a b
-- > toEither = undefined
-- >
-- > fromEither :: EitherList a b -> [Either a b]
-- > fromEither = undefined
-- >
-- > mapLeft :: (a -> c) -> EitherList a b -> EitherList c b
-- > mapLeft = undefined
-- >
-- > mapRight :: (b -> c) -> EitherList a b -> EitherList a c
-- > mapRight = undefined
-- >
-- > foldrEither :: (a -> c -> c) -> (b -> c -> c) -> c -> EitherList a b -> c
-- > foldrEither = undefined
-- >
-- > foldlEither :: (c -> a -> c) -> (c -> b -> c) -> c -> EitherList a b -> c
-- > foldlEither = undefined
--
-- **Problem 9: maps and sets**
--
-- Haskell has many convenient data structures in its standard
-- library. We'll be playing with sets and maps
-- today. [Data.Map](http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Map-Lazy.html)
-- and
-- [Data.set](http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Set.html)
-- are well documented on-line.
--
-- In this problem, we'll use maps and sets to reason about graphs (in
-- the network/graph theory sense, not in the statistical plotting sense).
--
-- We can start by defining what we mean by the nodes of the graph: we'll
-- have them just be strings. We can achieve this by using a *type
-- synonym*.
--
-- > type Node = String
--
-- To create a `Node`, we can use the constructor, like so:
--
-- > a = "a"
-- > b = "b"
-- > c = "c"
-- > d = "d"
-- > e = "e"
--
-- We can define a graph now as a map from `Node`s to sets of
-- `Node`s. The `Map` type takes two arguments: the type of the
-- map's *key* and the type of the map's *value*. Here the keys will be
-- `Node`s and the values will be sets of nodes. The `Set` type
-- takes just one argument, like lists: the type of the set's elements.
--
-- > type Graph = Map Node (Set Node)
--
-- We don't need to use `newtype` here, because we're less worried
-- about confusing graphs with other kinds of maps.
--
-- Let's start by building a simple graph, `g1`:
--
-- ```pre
--     - b -
--    /     \
-- a -       - d
--    \     /
--     - c -
-- ```
--
-- > g1 = Map.fromList [(a, Set.fromList [b,c]),
-- >                    (b, Set.fromList [a,d]),
-- >                    (c, Set.fromList [a,d]),
-- >                    (d, Set.fromList [b,c])]
--
-- Note that we've been careful to make sure the links are bidirectional:
-- if the `b` is in the value mapped by `a`, then `a` is in the value
-- mapped by `b`.
--
-- We can see what `a` has edges to by looking it up in `g1`:
--
-- > aEdges = g1 ! a
--
-- Write a function `isBidi` that checks whether a mapping is
-- bidirectional. Feel free to use any function in `Data.Map`,
-- `Data.Set`, or the Prelude, and write as many helper functions as you
-- need.
--
-- > isBidi :: Graph -> Bool
-- > isBidi = undefined
--
-- Write a function `bidify` that takes an arbitrary graph and makes it
-- bidirectional by adding edges, i.e., if the node `a` points to `b` but
-- not vice versa in a graph `g`, then `a` points to `b` *and* `b` points
-- to `a` in the graph `bidify g`.
--
-- > bidify :: Graph -> Graph
-- > bidify = undefined
--
-- Be sure to test your code!
