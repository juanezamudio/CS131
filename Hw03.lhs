Homework 3.0: The "While" programming language
Due 2017-02-19

Juan Zamudio

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> {-# OPTIONS_GHC -W #-}
> module Hw03 where
>
> import qualified Data.Map as Map
> import Data.Map (Map)
> import qualified Data.Set as Set
> import Data.Set (Set)

Throughout this homework, we'll be experimenting with our first
interpreter for what really is a programming language. We'll need two
concepts throughout: variable names (which will just be strings) and
stores (a/k/a heaps, where we keep the contents of the variables of
our language). All of our variables will store integers.

> type VarName = String
>
> type Store = Map VarName Int

<h3>Problem 1: Interpreting While</h3>

We'll define an interpreter for a language that goes beyond the simple
WhileNZ language we saw in class.

> data AExp =
>     Var VarName
>   | Num Int
>   | Plus AExp AExp
>   | Times AExp AExp
>   | Neg AExp
>   deriving (Show, Eq, Ord)

Write an interpreter for these arithmetic expressions. When evaluating
variables, you should return 0 if they're not in the store (such
variables are called *unbound* or *undefined*).

> fromMaybe :: Maybe Int -> Int
> fromMaybe Nothing = 0
> fromMaybe (Just x) = x

> evalA :: Store -> AExp -> Int
> evalA store (Var varname) = fromMaybe (Map.lookup varname store)
> evalA _ (Num x)           = x
> evalA store (Plus a b)    = (evalA store a) + (evalA store b)
> evalA store (Times a b)   = (evalA store a) * (evalA store b)
> evalA store (Neg a)       = negate (evalA store a)


We can define boolean expressions similarly. Rather than concretely
specifying which arithmetic expressions they're defined over, we just
take in a parameter.

> data BExp a =
>     Bool Bool
>   | Equal a a
>   | Lt a a
>   | Not (BExp a)
>   | Or (BExp a) (BExp a)
>   | And (BExp a) (BExp a)
>   deriving (Show, Eq, Ord)

Write an interpreter for boolean expressions over our prior arithmetic expressions.

> evalB :: Store -> BExp AExp -> Bool
> evalB _ (Bool a)        = a
> evalB store (Equal a b) = (evalA store a) == (evalA store b)
> evalB store (Lt a b)    = (evalA store a) < (evalA store b)
> evalB store (Not a)     = not (evalB store a)
> evalB store (Or a b)    = (evalB store a) || (evalB store b)
> evalB store (And a b)   = (evalB store a) && (evalB store b)



Finally, we'll define a simple programming language. Its abstract
syntax tree (AST) takes two type parameters: one identifying the
arithmetic expressions we'll use, one identifying the boolean
expressions we'll use.

> data Stmt a b =
>     Skip
>   | Assign VarName a
>   | Seq (Stmt a b) (Stmt a b)
>   | If (b a) (Stmt a b) (Stmt a b)
>   | While (b a) (Stmt a b)
>   deriving (Show, Eq, Ord)

Write an interpreter for this language.

> eval :: Store -> Stmt AExp BExp -> Store
> eval store Skip         = store
> eval store (Assign s a) = Map.insert s (evalA store a) store
> eval store (Seq a b)    = eval (eval store a) b
> eval store (If a b c)   = if (evalB store a)
>                           then (eval store b)
>                           else (eval store c)
> eval store (While a b)  = if (evalB store a)
>                           then (eval (eval store b) (While a b))
>                           else store


<h3>Problem 2: While, with failures</h3>

Here's a new definition for arithmetic expressions, adding division.

> data AExp' =
>     Var' VarName
>   | Num' Int
>   | Plus' AExp' AExp'
>   | Times' AExp' AExp'
>   | Neg' AExp'
>   | Div' AExp' AExp'
>   deriving (Show, Eq)

Note that division is an operation that can fail. Write another
interpreter (defining whatever functions you need). Do not use the
`error` function.

In the interpreter above, variables not in the store were given the
default value of 0. In this version of the interpreter, make it so
that unbound variables in arithmetic expressions cause errors, just
like division. Here are the two errors that can happen:

> data Error = NoSuchVariable VarName | DivideByZero AExp' | BadInput String

When you encounter an unbound variable, the error has a slot for
identifying the culpable variable. Similarly, when you try to divide
by zero, you should record the entire division expression responsible,
not just the divisor. (In a more serious AST, we might keep track of
the source file and line number each expression came from, in order to
better indicate the source of the problem.)

> instance Show Error where
>     show (NoSuchVariable x) = "NoSuchVariable " ++ show(x)
>     show (DivideByZero x)   = "DivideByZero " ++ show(x)
>     show (BadInput x) = "BadInput " ++ show(x)

> fromEither :: Store -> VarName -> Either Error Int
> fromEither store var | find == Nothing = Left (NoSuchVariable var)
>                      | otherwise       = Right (fromMaybe find)
>                        where find      = Map.lookup var store

> isLeft :: Either Error Int -> Bool
> isLeft (Left _)  = True
> isLeft (Right _) = False

> getRValue :: Either Error Int -> Int
> getRValue (Right a) = a
> getRValue (Left _) = -1

> evalA' :: Store -> AExp' -> Either Error Int
> evalA' store (Var' a)     = fromEither store a
> evalA' _ (Num' a)         = Right a
> evalA' store (Plus' a b)  = if isLeft exp1
>                             then exp1
>                             else if isLeft exp2
>                             then exp2
>                             else Right ((getRValue exp1) + (getRValue exp2))
>                             where exp1 = evalA' store a
>                                   exp2 = evalA' store b
> evalA' store (Times' a b) = if isLeft exp1
>                             then exp1
>                             else if isLeft exp2
>                             then exp2
>                             else Right ((getRValue exp1) * (getRValue exp2))
>                             where exp1 = evalA' store a
>                                   exp2 = evalA' store b
> evalA' store (Neg' a) | isLeft (evalA' store a) = evalA' store a
>                       | otherwise = Right (negate (getRValue (evalA' store a)))
> evalA' store (Div' a b)   = if isLeft exp1
>                             then exp1
>                             else if isLeft exp2
>                             then exp2
>                             else if (getRValue exp2 == 0)
>                             then (Left (DivideByZero b))
>                             else Right ((getRValue exp1) `div` (getRValue exp2))
>                             where exp1 = evalA' store a
>                                   exp2 = evalA' store b

> getBValueInt :: String -> Either Error Int -> Either Error Int -> Either Error Bool
> getBValueInt _ (Left a)  _         = (Left a)
> getBValueInt _ _         (Left b)  = (Left b)
> getBValueInt p (Right a) (Right b) =
>     case p of
>     "==" -> (Right (a == b))
>     "<"  -> (Right (a < b))
>     _    -> Left (BadInput p)

> getBValueBool :: String -> Either Error Bool -> Either Error Bool -> Either Error Bool
> getBValueBool _ (Left a)  _         = Left a
> getBValueBool _ _         (Left b)  = Left b
> getBValueBool p (Right a) (Right b) =
>     case p of
>     "||" -> Right (a || b)
>     "&&" -> Right (a && b)
>     _    -> Left (BadInput p)

> getBValueNot :: Either Error Bool -> Either Error Bool
> getBValueNot (Left a)  = Left a
> getBValueNot (Right a) = (Right (not a))


> evalB' :: Store -> BExp AExp' -> Either Error Bool
> evalB' _ (Bool a) = Right a
> evalB' store (Equal a b) = getBValueInt "==" exp1 exp2
>                            where exp1 = evalA' store a
>                                  exp2 = evalA' store b
> evalB' store (Lt a b)    = getBValueInt "<" exp1 exp2
>                            where exp1 = evalA' store a
>                                  exp2 = evalA' store b
> evalB' store (Not a)     = getBValueNot e1
>                            where e1 = evalB' store a
> evalB' store (Or a b)    = getBValueBool "||" e1 e2
>                            where e1 = evalB' store a
>                                  e2 = evalB' store b
> evalB' store (And a b)   = getBValueBool "&&" e1 e2
>                            where e1 = evalB' store a
>                                  e2 = evalB' store b

> getRValue' :: Either Error Bool -> Bool
> getRValue' (Right a) = a
> getRValue' (Left _) = undefined

> getLValue :: Either Error Bool -> Either Error Store
> getLValue (Left a) = Left a
> getLValue (Right _) = undefined

> isLeft' :: Either Error Bool -> Bool
> isLeft' (Left _) = True
> isLeft' (Right _) = False

> eval' :: Store -> Stmt AExp' BExp -> Either Error Store
> eval' store Skip         = Right store
> eval' store (Assign s a) =
>     case evalA' store a of
>     (Left exp1) -> Left exp1
>     (Right exp1) -> Right (Map.insert s exp1 store)
> eval' store (Seq a b)    =
>     case eval' store a of
>     (Left exp1) -> Left exp1
>     (Right exp1) ->
>         case eval' exp1 b of
>         (Left exp2) -> Left exp2
>         (Right exp2) -> Right exp2
> eval' store (If a b c) | isLeft' (evalB' store a)    = getLValue (evalB' store a)
>                        | getRValue' (evalB' store a) = eval' store b
>                        | otherwise                   = eval' store c
> eval' store (While a b) =
>     case evalB' store a of
>     (Left exp1)   -> Left exp1
>     (Right False) -> eval' store Skip
>     (Right True)  ->
>         case eval' store b of
>         (Left exp2)  -> Left exp2
>         (Right exp2) -> eval' exp2 (While a b)


<h3>Problem 3: Static analysis</h3>

Can we determine in advance whether a given program will try to use an
unbound variable if they're run in an initially empty store? This kind
of analysis is called "def/use analysis", and it's a common early step
in compilation. More generally, this is "static analysis", becuase we
inspect our programs before we run them. (*Static* and *dynamic* are
opposites; you can read them as "at compile time" and "at run time",
respectively.)

In some programs, it's easy:

> unboundY = Assign "x" (Var' "y")

The program `unboundY` will always fail in an unbound store. It can be
more ambiguous, though, as in:

> ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY

Depending on what we know about `b`, we may or may not have a problem
on our hands. Absent any information about `b`, it *could* happen that
`ambiguous b` will try to read from `y` before it's defined.

In PLs, we tend to stay on the safe side: the general philosophy is
that's better to have a false positive (saying a program is unsafe
when it's actually fine) than to have a false negative (saying a
program is safe when it isn't!). That is, PL prioritizes *soundness*
(if we say X, then X is really true) over *completeness* (if X is
really true, then we say X). As a side note, observe that it's easy to
write a trivial sound analysis (everything's unsafe, please wear a
helmet) as it is a trivial complete analysis (everything's safe, take
it easy).

To get started, write functions that collect all of the variables that
appear in given arithmetic and boolean expressions.

> varsA :: AExp' -> Set VarName
> varsA (Var' x)     = Set.fromList [x]
> varsA (Num' _)     = Set.empty
> varsA (Neg' x)     = varsA x
> varsA (Plus' x y)  = Set.union (varsA x) (varsA y)
> varsA (Times' x y) = Set.union (varsA x) (varsA y)
> varsA (Div' x y)   = Set.union (varsA x) (varsA y)


For example, `varsA (Times' (Plus' (Var' "x") (Var' "y")) (Num' 3)) ==
Set.fromList ["x", "y"]`.

> varsB :: BExp AExp' -> Set VarName
> varsB (Bool x) = Set.empty
> varsB (Equal x y) = Set.union (varsA x) (varsA y)
> varsB (Lt x y) = Set.union (varsA x) (varsA y)
> varsB (Not x) = varsB x
> varsB (Or x y) = Set.union (varsB x) (varsB y)
> varsB (And x y) = Set.union (varsB x) (varsB y)

For example, `varsB (Or (Not (Equal (Var' "foo") (Var' "bar"))) (Bool True)) ==
Set.fromList ["bar", "foo"]`.

Now let's write our analysis: we'll take in a set of variables that we
know to be defined, a statement in our language, and we'll return a
pair of sets: the set of variables that have been defined and the set
of variables that have been used *but not defined*.

> useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
> useBeforeDef defs Skip = (defs, Set.empty)
> useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)


What should the other cases do? Remember, you have to be *sound*: the
variable in the first part of the pair (the defined variables) must
*always* be defined; if it's at all possible for a variable to
undefined, it must not appear in the first part. Similarly, if it's at
all possible for variable to *ever* be used before it's defined, it
must appear in the second part.

With these guiding principles, what should we do for `Seq s1 s2`?
Everything `s1` defines will be defined for `s2`. The final set of
definitions will also include what `s2` defines. What about the the
variables that are used before they're defined? If `x` is used in `s1`
before it's defined, it doesn't matter if it's later defined in
`s2`---it's too late.

What about `If b s1 s2`? It's too hard to know anything about the
condition `b`. But if we can be certain that both branches define a
variable, then we can be certain that it'll be defined at the
end. Conversely, if either branch could use a given variable before
it's defined, then that variable could potentially be used before
being defined.

Once you know how `If` and `Seq` works, you should have the general
principle for `While`. Sketch it out on the board!

> useBeforeDef _ _ = undefined



Be very careful testing your function. Strive for soundness.  The
tests below show the results for my `useBeforeDef`---don't feel
obligated to do better, but don't do worse. You can modify or delete
these tests---my grader ignores them.

> testUnbound, testAmbiguous :: Bool
> testUnbound = useBeforeDef Set.empty unboundY ==
>               (Set.singleton "x", Set.singleton "y")
>
> testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True)) ==
>                 (Set.singleton "x", Set.singleton "y")

<h3>Problem 4: Mission Impossible</h3>

Your final task is to solve the halting problem. We'll start by
writing a function that runs a program a little bit---just one
"step". Then we'll look at the *trace* of steps the program takes. If
we ever end up in a state we've seen before, then the program
diverges. This is a dynamic analysis, since we'll be running our
programs.

First, fill in the step function below.

> type Config = (Store, Stmt AExp BExp)
>
> step :: Config -> Maybe Config
> step (_,Skip) = Nothing
> step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
> step (st,Seq Skip s2) = Just (st,s2)
> step (st,Seq s1 s2) = undefined
> step (st,If b s1 s2) = undefined
> step (st,While b s) = undefined

Given a step function, we can compute a trace, i.e., the possibly
infinite list of `Config`s that the program will step through. Such a
program is safe to write in Haskell because Haskell is *lazy*, i.e.,
it will only compute things on demand.

> trace :: (a -> Maybe a) -> a -> [a]
> trace f v =
>   case f v of
>     Nothing -> [v]
>     Just v' -> v:trace f v'

I may have gotten excited earlier when I said we'd "solve" the halting
problem. We can *try* to solve it, but sometimes we'll have to throw
up our hands and say "Who knows?". To facilitate that, we'll use
*three-valued logic*, which extends the booleans with a notion of
"don't know".

> data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

Write a function `diverges` that checks for loops in a list of
configurations. (Note that I've written a much more general type.) The
integer paramter should serve as a timeout---a limit as to how far
we're willing to look.

What counts as a loop? Each element in the list will represent a
`Config`, i.e., a pair of a store and a statement currently being
executed. If we ever see the same pair twice, we know the program
diverges because our programs are *deterministic*, i.e., they do the
same thing every time. So your job is to check for duplicate
configurations, i.e., elements that appear more than once in the
loop. A wise choice of data structure here will make your life easier
(and speed up your program).

> diverges :: Ord a => Int -> [a] -> TVL
> diverges limit = undefined


Write a function `haltsIn` that takes a starting configuration and a
limit and tries to determine whether that configuration ever halts
(within the specified limit, from the empty store).

> haltsIn :: Stmt AExp BExp -> Int -> TVL
> haltsIn s limit = undefined


Now we have our analysis... let's see what it can do. Write a While
program `loop` that diverges and:

```
loop `haltsIn` 1000 == No
```

> loop :: Stmt AExp BExp
> loop = undefined

Write a While program `long` that converges and:

```
long `haltsIn` 1000 == Maybe
long `haltsIn` 5000 == Yes
```

> long :: Stmt AExp BExp
> long = undefined

Write a While program `tricky` that diverges but for all `n`:

```
tricky `haltsIn` n == Maybe
```

> tricky :: Stmt AExp BExp
> tricky = undefined

Explain why your `haltsIn` gives an imprecise answer.


Do you think you can write a program where `haltsIn` gives a wrong
answer? If so, explain your idea---or write it! If not, explain (or
prove!) why not.
