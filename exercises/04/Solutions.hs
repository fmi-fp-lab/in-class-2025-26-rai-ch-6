-- vim: set fdm=marker:
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Folds where

import Prelude hiding (all, and, concat, drop, filter, foldr, length, map, null, or, product, reverse, subtract, sum, take, zip, zipWith, (++))

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat $ n - 1

addNat' :: Nat -> Nat -> Nat
addNat' n m = foldNat m Succ n

multNat' :: Nat -> Nat -> Nat
multNat' n m = foldNat Zero (addNat' m) n

foldNat :: a -> (a -> a) -> Nat -> a
foldNat bazov _rekursiven Zero = bazov
foldNat bazov rekursiven (Succ n) = rekursiven (foldNat bazov rekursiven n)

-- NOTE: signature is a little bit different than the one we used in class,
--       here, we take the "cons handler" before the "nil handler",
--       which is what the `foldr` from the `Prelude` does

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr op nv (x : xs) = op x (foldr op nv xs)

sum :: [Integer] -> Integer
sum = foldr (+) 0

product :: [Integer] -> Integer
product = foldr (*) 1

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

-- TASK:
-- Implement natToInteger using foldNat.

-- >>> natToInteger $ Succ $ Succ $ Succ Zero
-- 3

natToInteger :: Nat -> Integer
natToInteger = foldNat 0 (+ 1)

-- TASK:
-- Implement exponentiation(n ^ m) using foldNat.

-- >>> natToInteger $ expNat (integerToNat 2) (integerToNat 10)
-- 1024

expNat :: Nat -> Nat -> Nat
expNat base = foldNat (Succ Zero) (multNat' base)

---------------
-- EXERCISES --
---------------

-- TASK:
-- Implement and using foldr

-- >>> and [False]
-- False
-- >>> and [True, True]
-- True

and :: [Bool] -> Bool
and = foldr (&&) True

-- TASK:
-- Implement or using foldr

-- >>> or [False]
-- False
-- >>> or [True, True]
-- True

or :: [Bool] -> Bool
or = foldr (||) False

-- TASK:
-- Implement length using foldr

-- >>> length [1,2,8]
-- 3
-- >>> length []
-- 0

length :: [a] -> Integer
length = foldr (\_ acc -> acc + 1) 0

-- TASK:
-- Implement (++) using foldr
-- >>> [1,2,3]
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

-- TASK:
-- Implement concat using foldr

-- >>> concat [[1,2,3], [42,69], [5,7,8,9]]
-- [1,2,3,42,69,5,7,8,9]
-- >>> concat [[1,2,3], [], [5,7,8,9]]
-- [1,2,3,5,7,8,9]
-- >>> concat []
-- []

concat :: [[a]] -> [a]
concat = foldr (++) []

-- TASK:
-- Implement reverse using foldr (it's fine to do this in O(n^2)

-- >>> reverse [1,2,3]
-- [3,2,1]
-- >>> reverse []
-- []

reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) []

-- TASK:
-- Implement map using foldr

-- >>> map succ [1,2,3]
-- [2,3,4]
-- >>> map (\x -> x * x) [1,2,3] -- same as squareList
-- [1,4,9]
-- >>> map (\x -> (3,x)) [1,2,3] -- same as megaPair 3
-- [(3,1),(3,2),(3,3)]

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

-- TASK:
-- Implement filter using foldr

-- >>> even 2
-- True
-- >>> even 3
-- False
-- >>> filter even [1..10]
-- [2,4,6,8,10]
-- >>> filter isPrime [1..20]
-- [2,3,5,7,11,13,17,19]

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

-- TASK:
-- Implement null using foldr

-- >>> null []
-- True
-- >>> null [1]
-- False

null :: [a] -> Bool
null = foldr (\_ _ -> False) True

-- TASK:
-- Implement headMaybe using foldr

-- >>> headMaybe []
-- Nothing
-- >>> headMaybe [1,2,3]
-- Just 1

headMaybe :: [a] -> Maybe a
headMaybe = foldr (\x _ -> Just x) Nothing

-- TASK:
-- Implement a function that splits a list into two based on a predicate p
-- those that satisfy p and those that don't.

-- >>> partition (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partition even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition _ [] = ([], [])
partition p (x : xs) =
  let (ts, fs) = partition p xs
   in if p x
        then (x : ts, fs)
        else (ts, x : fs)

-- TASK:
-- Implement partition using foldr

-- >>> partitionfoldr (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partitionfoldr even [1..10]
-- ([2,4,6,8,10],[1,3,5,7,9])

partitionfoldr :: (a -> Bool) -> [a] -> ([a], [a])
partitionfoldr p = foldr step ([], [])
  where
    step x (ts, fs)
      | p x = (x : ts, fs)
      | otherwise = (ts, x : fs)

-- TASK:
-- Implement validateList using foldr.

-- >>> validateList []
-- Just []
-- >>> validateList [Just 42, Just 6, Just 9]
-- Just [42,6,9]
-- >>> validateList [Nothing, Just 6, Just 9]
-- Nothing
-- >>> validateList [Just 42, Nothing, Just 9]
-- Nothin
-- >>> validateList [Just 42, Just 6, Nothing]
-- Nothing

validateList :: [Maybe a] -> Maybe [a]
validateList = foldr step (Just [])
  where
    step Nothing _ = Nothing
    step (Just _) Nothing = Nothing
    step (Just x) (Just xs) = Just (x : xs)

-- TASK:
-- Look at the recursor for nats - foldNat. In there we replaced {Nat}'s constructors with "things".
-- Think about how a fold for tuples should look like, and implement it.
-- Does this function remind you of another function we've previously implemented?

foldTuple :: (a -> b -> c) -> (a, b) -> c
foldTuple f (x, y) = f x y

-- TASK:
-- Same as above, but this time for Maybe

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe z _ Nothing = z
foldMaybe _ g (Just x) = g x

-- TASK:
-- Same as above, but this time for Either
-- Reminder: Either is defined like so:
-- data Either a b = Left a | Right b

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f _ (Left x) = f x
foldEither _ g (Right y) = g y

-- TASK:
-- If Nats can be converted to "n times applications" via foldNat,
-- is it perhaps true that "n times applications" can also be converted to Nats somehow?
--
-- You can ignore this "forall explanation" bit below if you want to - just assume the forall means "the passed function must be polymorphic over a"
-- {{{ forall explanation
-- Usually when we have a polymorphic function, like id :: a -> a
-- the *caller* chooses what a will be - when the caller writes {id 'a'}, they instantiate {a} with {Char}, so {id} becomes {id :: Char -> Char}
-- However, here we will need our function to work for any {a}, and so we must *require* something of the caller -
-- that they provide a function working *for any* {a} - meaning *we*(the callee) can decide what {a} to apply it for, reversing who can pick what the type is.
--
-- As a concrete example, consider
-- ```
-- f :: (a -> a) -> Bool
-- f g = g True
-- ```
-- this does *not* compile - let's assume it did:
-- If we have
-- ```
-- h :: Int -> Int
-- h x = x + 1
-- ```
-- then the caller would be able to write {f h}, (as they pick what {a} is) which is not valid,
-- since {h} requires its argument and return types to be {Int}, and {True :: Bool}, which is not {Int}
-- If we instead consider
-- ```
-- f :: (forall a. a -> a) -> Bool
-- f g = g True
-- ```
-- we, as implementors of {f}, are now allowed to pick what {a} to use, hence we are allowed to pick {a ~ Bool},
-- by calling {g} with {True :: Bool}
-- Hence, this function now compiles, and furthermore, if our hypothetical caller now attempts to do
-- {f h}
-- They will get a compilation error, since the argument to {f} needs to work *for any* type, while {h} has the concrete type {Int -> Int}
-- }}} forall explanation

-- >>> iterateToNat (\f x -> f (f (f x)))
-- Succ (Succ (Succ Zero))

iterateToNat :: (forall a. (a -> a) -> a -> a) -> Nat
iterateToNat f = f Succ Zero

-- EXERCISE
-- This is the same as foldNat, except with arguments reaarranged to mirror {iterateToNat}
natToIterate :: Nat -> (a -> a) -> a -> a
natToIterate n f v = foldNat v f n

type Natural = forall a. (a -> a) -> a -> a

-- TASK:
-- Hey, if we can convert between Natural (the type argument to {iterateToNat}, now with a synonym) and {Nat} without losing information
-- wouldn't that mean that they are equivalent, and we can do the same things with both?
-- Let's reimplement some of the operations over {Nat} with {Natural} instead
-- These are called "church encoded" natural numbers - they're used to represent natural numbers when the only thing you have is functions.
--
-- Here's some exposition:
-- As you saw in the {iterateToNat} example, these Naturals are essentially applying some function to some value a number of times.
-- The idea is that we represent the number {n} as applying a function {f} {n} times to a value {v}.
-- For example:
-- 0 is represented by \f v -> v

zero :: Natural
zero _f v = v

-- 1 is represented by \f v -> f v
-- 2 is represented by \f v -> f (f v)
-- 3 is represented by \f v -> f (f (f v))
-- and so on
-- With this function, we need to somehow "add another f".

-- >>> iterateToNat zero
-- Zero
-- >>> iterateToNat $ suc $ suc zero
-- Succ (Succ Zero)
-- >>> natToInteger $ iterateToNat $ suc $ natToIterate $ integerToNat 5
-- 6

suc :: Natural -> Natural
-- successor means apply f one more time than n already does
suc n f v = f (n f v)

-- TASK:
-- We can also add these. Here we need to think about how to add f n times to another Natural.

-- >>> iterateToNat $ add (suc (suc zero)) zero
-- Succ (Succ Zero)
-- >>> iterateToNat $ add (suc (suc zero)) (suc (suc (suc zero)))
-- Succ (Succ (Succ (Succ (Succ Zero))))
-- >>> natToInteger $ iterateToNat $ add (suc (suc zero)) (suc (suc (suc (suc zero))))
-- 6

add :: Natural -> Natural -> Natural
-- first run m applications, then let n tack on its count
add n m f v = n f (m f v)

-- TASK:
-- Now multiply them

-- >>> iterateToNat $ mult (suc (suc zero)) zero
-- Zero
-- >>> iterateToNat $ mult zero (suc (suc zero))
-- Zero
-- >>> iterateToNat $ mult (suc (suc zero)) (suc (suc zero))
-- Succ (Succ (Succ (Succ Zero)))
-- >>> natToInteger $ iterateToNat $ mult (suc (suc zero)) (suc (suc (suc zero)))
-- 6

mult :: Natural -> Natural -> Natural
-- replacing f with m f makes each of n steps expand to m extra f applications
mult n m f v = n (m f) v

-- Is the same true for lists? Is there some function type that is "isomorphic" to lists - you can convert
-- back and forth between lists and the function, without losing data? Like how Natural is to Nat
-- (or if you prefer - can you express lists by only using lambdas?)

-- Church encoded lists
type ChurchList a = forall r. (a -> r -> r) -> r -> r

-- turning a plain list into its "apply constructor" form is just a fold
listToChurch :: [a] -> ChurchList a
listToChurch xs consF nilF = foldr consF nilF xs

-- and to get a list back we instantiate the fold with (:) and []
churchToList :: ChurchList a -> [a]
churchToList cl = cl (:) []

cNil :: ChurchList a
cNil _cons nilF = nilF

cCons :: a -> ChurchList a -> ChurchList a
cCons x xs consF nilF = consF x (xs consF nilF)

cAppend :: ChurchList a -> ChurchList a -> ChurchList a
cAppend xs ys consF nilF = xs consF (ys consF nilF)

cMap :: (a -> b) -> ChurchList a -> ChurchList b
cMap g xs consF nilF = xs (\a acc -> consF (g a) acc) nilF
