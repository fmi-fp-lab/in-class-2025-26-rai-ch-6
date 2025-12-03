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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Typeclasses where

import Prelude hiding (Monoid (..), Semigroup (..), all, any, find, fold, foldMap, lookup, mconcat, mtimes, reverse)

-- TODO:
-- motivation: lookup, sort, insert
-- type*classes* - "sets of types" sometimes useful
-- {MyEq} (as a demo, will use the builtin {Eq} for exercises)
-- laws (show hoogle)
-- instance Animal - mention InstanceSigs
-- using, e.g. lookup
-- deriving
-- {MyOrd}
-- superclass constraint, default impls
--
-- {Read}, {Show}, {Num}, {Enum}
-- deriving {Read}+{Show} very easy naive serialisation
-- cool RPS example with derived {Eq}, {Enum}
--
-- {Semigroup} (with laws) (<https://en.wikipedia.org/wiki/Semigroup>)
-- {Monoid} (with laws) (<https://en.wikipedia.org/wiki/Monoid>)
-- examples: (+) (*) (&&) (++)
-- alternative definition of {Semigroup} and {Monoid} via {mconcat}
-- useful to think about
--
-- two usual uses: overloading names (ex: {Pretty}), lawful classes ({Monoid})
--
-- can define class instance anywhere, not the same for abstract class
-- both can have dict/vtable for impl, both typeclasses usually have laws

class Semigroup a where
  (<>) :: a -> a -> a

class (Semigroup a) => Monoid a where
  mempty :: a

-- TASK:
-- Implement (<=), called {leq} here, using the compare function:
-- Open up ghc and execute {:t compare} to see its type.

-- >>> leq 3 5
-- True
-- >>> leq 5 5
-- True
-- >>> leq 6 5
-- False

leq :: (Ord a) => a -> a -> Bool
leq = undefined

-- TASK:
-- Implement compare using (<=)

-- >>> compare' 3 5
-- LT
-- >>> compare' 5 5
-- EQ
-- >>> compare' True False
-- GT
-- >>> compare' 'a' 'b'
-- LT

compare' :: (Ord a) => a -> a -> Ordering
compare' = undefined

-- TASK:
-- Given a function to convert a values, compare them using the ordering in b.
-- This function is useful partially applied, when we have.
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- >>> comparing fst (5, 0) (4, 69)
-- GT
-- >>> comparing snd (5, 0) (4, 69)
-- LT

comparing :: (Ord b) => (a -> b) -> a -> a -> Ordering
comparing = undefined

data Nat = Zero | Succ Nat
 deriving (Show)

-- TASK:
-- Implement a Monoid instance for Nat based on addition

-- instance Semigroup Nat where

-- instance Monoid Nat where

-- TASK:
-- Implement a Monoid instance for [a]
-- Note how regardless of what a is, [a] is always a Monoid.
-- This is similar to what is usually called a "free" structure in mathematics.
-- And indeed, lists are "the free Monoid"

-- instance Semigroup [a] where

-- instance Monoid [a] where

-- TASK:
-- Implement a monoid instance for the Any type, with the following semantics:
-- When combining things via (<>) we want to see if any of the arguments are True.

newtype Any = MkAny {getAny :: Bool}

-- instance Semigroup Any where

-- instance Monoid Any where

-- TASK:
-- Implement a monoid instance for the All type, with the following semantics:
-- When combining things via (<>) we want to see if all of the arguments are True.

newtype All = MkAll {getAll :: Bool}

-- instance Semigroup All where

-- instance Monoid All where

-- TASK:
-- We can lift monoids over tuples by doing the monoidal operation component-wise. Implement the instance

-- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where

-- instance (Monoid a, Monoid b) => Monoid (a, b) where

-- TASK:
-- "Monoid multiplication"
-- mtimes 5 x is intuitively supposed to be the same as 5 * x,
-- in other words, x <> x <> x <> x <> x

-- >>> mtimes (Succ $ Succ Zero) $ [1,2,3]
-- [1,2,3,1,2,3]
-- >>> mtimes (Succ $ Succ Zero) $ Succ $ Succ $ Succ Zero
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

mtimes :: (Monoid a) => Nat -> a -> a
mtimes = undefined

-- TASK:
-- Combine a list of elements, assuming that the type in the list is a Monoid.
-- Try implementing this using foldr.

-- >>> fold [Zero, Succ Zero, Succ (Succ Zero)]
-- Succ (Succ (Succ Zero))
-- >>> fold $ [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]

fold :: (Monoid a) => [a] -> a
fold = undefined

-- TASK:
-- "Fold" a Maybe using a monoid and a mapping function.
-- This is useful when you want to default a Nothing to some monoid.

-- >>> foldMapMaybe (:[]) $ Just 'a'
-- "a"
-- >>> foldMapMaybe (:[]) Nothing
-- []

foldMapMaybe :: (Monoid b) => (a -> b) -> Maybe a -> b
foldMapMaybe = undefined

-- TASK:
-- Fold a list using a mapping function.
-- Try implementing this with fold and map.
-- ** Extremely** useful function.

foldMap :: (Monoid b) => (a -> b) -> [a] -> b
foldMap = undefined

-- TASK:
-- Implement all using the All monoid and foldMap.

all :: (a -> Bool) -> [a] -> Bool
all = undefined

-- TASK:
-- Implement any using the Any monoid and foldMap.

any :: (a -> Bool) -> [a] -> Bool
any = undefined

-- TASK:
-- Maybe lifts any Semigroup into a Monoid by adding an extra element (Nothing) to be the mempty.
-- Implement the instance that witnesses this.

-- TASK:
-- Maybe can also be made into a monoid by always "taking the first Just", i.e. when combining elements,
-- we ignore all the Nothings, and if we ever find a Just on the left, we always return that.
-- Implement this instance.

newtype First a = MkFirst {getFirst :: Maybe a}

-- instance Semigroup (First a) where

-- instance Monoid (First a) where

-- TASK:
-- Utility function converting a predicate to a Maybe returning function.

guarded :: (a -> Bool) -> a -> Maybe a
guarded = undefined

-- TASK:
-- Now implement the find function by using First and foldMap.

find :: (a -> Bool) -> [a] -> Maybe a
find = undefined

-- TASK:
-- If we have a Monoid for an a, we can make another monoid by simply flipping the operation, i.e.
-- For example, we want something like this:
-- >>> Dual [1,2,3] <> Dual [4,5,6]
-- Dual [4,5,6,1,2,3]
-- >>> Dual (First (Just 5)) <> Dual (First (Just 8))
-- Dual (First (Just 8))

newtype Dual a = MkDual {getDual :: a}

-- Implement Semigroup and Monoid for Dual

-- TASK:
-- Now use Dual and foldMap to implement reverse.

reverse :: [a] -> [a]
reverse = undefined

-- TASK:
-- Given a list of key-value pairs, update the value for a given key, or if it doesn't exist
-- insert it with a default value. This is what the Maybe b is for - so that the caller
-- can supply a modifying function and a default value at the same time.
-- Think about what the constraint is you will require.
-- Is there a reason to use any other constraint?
-- we can put function definitions on one line if we separate the clauses with a ;

-- >>> let f Nothing = 5; f (Just x) = x * 5
-- >>> upsert f "pesho" []
-- [("pesho",5)]
-- >>> upsert f "pesho" [("gosho", 42)]
-- [("gosho",42),("pesho",5)]
-- >>> upsert f "pesho" [("gosho", 42), ("pesho", 84)]
-- [("gosho",42),("pesho",420)]

upsert :: _ => (Maybe b -> b) -> a -> [(a, b)] -> [(a, b)]
upsert = undefined

-- TASK:
-- For a given list, return a key-value list with the keys being the original elements,
-- and the values being how many times each element was present in the original list. (aka a histogram)
-- Think about what the minimal constraint is you will require.

-- >>> histo [1,2,3]
-- [(3,1),(2,1),(1,1)]
-- >>> histo "How much wood could a wood chuck chuck if a wood chuck could chuck wood?"
-- [('?',1),('d',6),('o',10),('w',5),(' ',14),('k',4),('c',11),('u',8),('h',5),('l',3),('a',2),('f',1),('i',1),('m',1),('H',1)]

histo :: _ => [a] -> [(a, Integer)]
histo = undefined

-- TASK:
-- Insert a value into an ordered list. Write the constraint yourself.

-- >>> insert 5 [1..10]
-- [1,2,3,4,5,5,6,7,8,9,10]
-- >>> insert 5 [2, 4 .. 42]
-- [2,4,5,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42]

insert :: _ => a -> [a] -> [a]
insert = undefined

-- TASK:
-- Implement insertion sort.

-- >>> sort [4,12,3,1,1,2,34]
-- [1,1,2,3,4,12,34]

sort :: _ => [a] -> [a]
sort = undefined

-- TASK:
-- Now use First, foldMap, the monoid for tuples and Dual to implement a function which works like find
-- but instead returns the first and the last element matching a predicate **in one traversal** of the input list.

-- >>> findFirstAndLast even [1..10]
-- Just (2,10)
-- >>> findFirstAndLast (<5) [1..10]
-- Just (1,4)
-- >>> findFirstAndLast (>5) [1..10]
-- Just (6,10)

findFirstAndLast :: (a -> Bool) -> [a] -> Maybe (a, a)
findFirstAndLast = undefined

-- TASK:
-- Functions with the same domain and codomain form a monoid.
-- Implement it.

newtype Endo a = MkEndo {getEndo :: a -> a}

-- >>> getEndo (foldMap Endo [succ, succ, (*2), succ]) 5
-- 14
-- >>> getEndo (foldMap Endo [(3:), (++[1,2,3])]) [4,2]
-- [3,4,2,1,2,3]

-- instance Semigroup (Endo a) where

-- instance Monoid (Endo a) where

-- TASK:
-- Implement foldr via foldMap, by using the Endo Monoid.

-- >>> foldrViaFoldMap (++) [] [[1,2,3],[4,5,6],[7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
-- >>> foldrViaFoldMap (+) 0 [1..10]
-- 55

foldrViaFoldMap :: (a -> b -> b) -> b -> [a] -> b
foldrViaFoldMap = undefined

-- TASK:
-- The Flux data type is used to count how many times a "change occurred"
-- across a container, for example a list.
-- The {sides} field contains the leftmost and rightmost element of the container,
-- if any, and {changes} contains how many times "change occurred".
-- The Semigroup operation for {Flux} combines two {Flux}es, adjusting their
-- {sides} and {changes} fields to take into account what the new leftmost and rightmost element are,
-- as well as how {changes} should be adjusted.
--
-- It's best to look at the examples below.
-- Your task is to implement the Semigroup and Monoid instances for Flux.

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

-- Inject a single value into {Flux}, causing no changes at all.
flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

-- instance (Eq a) => Semigroup (Flux a) where

-- instance (Eq a) => Monoid (Flux a) where

-- >>> flux 1
-- Flux {sides = Just (1,1), changes = 0}
-- >>> flux 2
-- Flux {sides = Just (2,2), changes = 0}
-- >>> flux 1 <> flux 2
-- Flux {sides = Just (1,2), changes = 1}
-- >>> flux 1 <> flux 2 <> flux 3
-- Flux {sides = Just (1,3), changes = 2}
-- >>> flux 1 <> flux 2 <> flux 3 <> flux 1
-- Flux {sides = Just (1,1), changes = 3}
-- >>> flux 1 <> mempty
-- Flux {sides = Just (1,1), changes = 0}
-- >>> mempty <> flux 1
-- Flux {sides = Just (1,1), changes = 0}
-- >>> changes $ foldMap flux [1,1,1,1]
-- 0
-- >>> changes $ foldMap flux [1,2,1,2]
-- 3
-- >>> changes $ foldMap flux [1,2,1,2,2,2,2]
-- 3
-- >>> changes $ foldMap flux [1,2,1,2,2,2,2,3,2]
-- 5
-- >>> changes $ foldMap flux [1,2,1,2,2,2,2,3,2,4]
-- 6

-- TASK:
-- Use the same idea of function composition from {foldrViaFoldMap} to use {foldr} to implement {foldl}.

foldlViaFoldr :: (b -> a -> b) -> b -> [a] -> b
foldlViaFoldr = undefined
