{-# LANGUAGE RankNTypes #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module HOF where

import Prelude hiding (const, curry, id, log, map, on, swap, uncurry, until, ($), (.))

-- TODO:
-- - remind about github classrooms -> in-class && HW00
-- - soon HW01?
--
-- TODO: talk (again) about
-- - sections (why (+) makes sense)
-- - guards
-- - lambdas, desugar function definition,
--   HOF(arguments), tuple-map hof example, currying, polymorphism
--
-- TODO: talk about (show some usages in last week's `../01/Solutions.hs`)
-- - , in type declarations
-- - @-bindings
-- - let
-- - where
-- - logging as example for DI via higher order functions
-- - ($), (.)
-- - what does "combinator" mean?
--
-- TODO: implement
-- - applyTwice :: (a -> a) -> a -> a
-- - id
-- - ($) (maybe go back to solutions and start rewriting stuff using ($))
-- - infixr 0 $ (draw AST of an operator-heavy expression)
-- - (.)

-- TODO: implement
data Tuple a b
  deriving (Show)

sumTuple :: Tuple Int Int -> Int
sumTuple = undefined

-- TODO: implement both, show C++ equivalent
fstTuple :: Tuple a b -> a
fstTuple = undefined

sndTuple :: Tuple a b -> b
sndTuple = undefined

-- TASK:
-- Take two arguments and return the first.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.

-- >>> const 5 6
-- 5
-- >>> applyTwice (const 42) 1337
-- 42

const :: a -> b -> a
const = undefined

-- TASK:
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f∘g

-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45

compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- Play around with the syntax (how many parenthesis can you stuff in here?)
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = compose

-- TASK:
-- Iterate a function f n times over a base value x.

-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024

iterateN :: (a -> a) -> a -> Integer -> a
iterateN = undefined

-- TASK:
-- Swap the two elements of a tuple

-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42

swap :: Tuple a b -> Tuple b a
swap = undefined

-- TASK:
-- Apply a function to only the first component of a tuple

-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337

first :: (a -> b) -> Tuple a c -> Tuple b c
first = undefined

-- TASK:
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- (Guess where the language gets its name from, ексди)

-- >>> curry (\(MkTuple x y) -> x * y) 23 3
-- 69

curry :: (Tuple a b -> c) -> a -> b -> c
curry = undefined

-- TASK:
-- Convert a two argument function, to one that takes a Tuple.

-- >>> uncurry (\x y -> x + y) $ MkTuple 23 46
-- 69

uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = undefined

-- TASK:
-- > p `on` f
-- Implement a combinator that allows you to "preapply" a function f on the arguments of a function p

-- >>> let maxOnFirst = max `on` fstTuple in maxOnFirst (MkTuple 1 20) (MkTuple 2 100000)
-- 2
-- >>> let maxOnSum = max `on` sumTuple in maxOnSum (MkTuple 20 39) (MkTuple 12 34)
-- 59

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = undefined

-- TASK:
-- Execute a function, until the result starts sastifying a given predicate

-- >>> until (>1000) (*7) 4
-- 1372

until :: (a -> Bool) -> (a -> a) -> a -> a
until = undefined

-- TASK:
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.

-- mapTuple :: ???
-- mapTuple = undefined

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- Look at addNat and multNat from last time.
--
-- addNat :: Nat -> Nat -> Nat
-- addNat Zero m = m
-- addNat (Succ n) m = Succ (addNat n m)
--
-- multNat :: Nat -> Nat -> Nat
-- multNat Zero _ = Zero
-- multNat (Succ n) m = addNat m (multNat n m)
--
-- They look very similar...

-- TASK:
-- Can you implement a general enough higher-order function (called `foldNat` here), such that you can then use to
-- implement both of `addNat` and `multNat` by passing suitable arguments? What are those arguments?
--
-- foldNat :: ???
-- foldNat = ???

-- TASK:
-- If your function is "good enough" you should also be able to implement exponentiation using it.

-- expNat :: Nat -> Nat -> Nat
-- expNat = undefined

-- TASK:
-- Can you also implement the following function using your foldNat? If not, (try to) modify your function so you can.

-- natToInteger :: Nat -> Integer
-- natToInteger = undefined

-- TASK:
-- Can you also implement the following "predecessor" function using it? Yes/No, and why?
-- If not, can you think of a foldNat' :: ??? with a different type signature, which would allow you to implement predNat?
-- predNat :: Nat -> Nat
-- predNat Zero = Zero
-- predNat (Succ n) = n

-- predNat :: Nat -> Nat
-- predNat = undefined
