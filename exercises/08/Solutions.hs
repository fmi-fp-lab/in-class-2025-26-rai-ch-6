{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

module IO where

import Text.Read (readMaybe)
import Prelude hiding (getLine, putStrLn, readLn)

putStrLn :: String -> IO ()
putStrLn [] = putChar '\n'
putStrLn (x : xs) = do
  putChar x
  putStrLn xs

get2Char :: IO (Char, Char)
get2Char = do
  c1 <- getChar
  c2 <- getChar
  pure (c1, c2)

getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n'
    then pure ""
    else do
      s <- getLine
      pure $ c : s

-- NOTE: Use {error :: String -> a} when you need to error out.

-- TASK:
-- Implement reading an {Integer} from stdin.
-- Use {readMaybe :: Read a => String -> Maybe a}

getNumber :: IO Integer
getNumber = readLn

-- TASK:
-- Implement reading a {Bool} from stdin.

getBool :: IO Bool
getBool = readLn

-- TASK:
-- Implement a function which runs the provided action when
-- the given {Maybe} is a nothing, and otherwise returns the value within the {Just}.
-- This is actually a common bit in both {getBool} and {getNumber}
-- and is an overall very useful function.

whenNothing :: Maybe a -> IO a -> IO a
whenNothing (Just x) _ = pure x
whenNothing Nothing action = action

-- TASK:
-- {getNumber} and {getBool} are practically the same.
-- We can generalise them to use the {Read} type class, so that they
-- work for any type which has an instance of {Read}.
-- Try to use {whenNothing} here.
--
-- Note that sometimes when using this function you'll need to specify a type annotation,
-- because the compiler will not be able to figure out what exactly type you want to read.
-- e.g. {x <- readLn :: IO Int} to read an {Int}, or similarly you could do {x :: Int <- readLn}

readLn :: (Read a) => IO a
readLn = do
  line <- getLine
  whenNothing (readMaybe line) readLn

-- TASK:
-- Same as {readLn}, but don't error out, returning a {Maybe} instead.

readLnMay :: (Read a) => IO (Maybe a)
readLnMay = do
  line <- getLine
  pure (readMaybe line)

-- TASK:
-- Run an IO action only when the given {Bool} is @True 2

when :: Bool -> IO () -> IO ()
when cond act =
  if cond
    then act
    else pure ()

-- TASK:
-- Repeat a {Maybe a} producing action, until it produces a {Just}, returning the result.

untilJustM :: IO (Maybe a) -> IO a
untilJustM action = do
  res <- action
  case res of
    Just x -> pure x
    Nothing -> untilJustM action

-- TASK:
-- We're going to implement a very simplified version of the Hangman game.
--
-- The {startHangman} function takes in an argument a path to a file containing a dictionary of words.
-- The dictionary is expected to be newline seperated for each word, and to have at least 100 words.
-- You can find such a dictionary in directory of this file called {words.txt}
--
-- After doing that, it prompts the user for a number between 0 and 99, inclusive, and then
-- uses that number as an index into the dictionary to pick a word for the game hangman.
-- If the user errors a number which is outside that range, we should prompt them again ({untilJustM} is useful here)
--
-- You can use {lines :: String -> [String]} to split up the dictionary.
-- You can use {(!!) :: [a] -> Int -> a} to index the dictionary.
--
--
-- The {playHangman guessedSoFar target} function is the actual "gameplay" we're going to implement.
-- In it, we keep an argument {guessedSoFar} as "state" for the letters we've guessed so far,
-- as well as which the target word is.
-- On each turn of the game, we must
-- 1. Print out the "guessed" version of the target word, i.e., we display only letters which the player has
--    guessed so far, "censoring" the others with some symbol (e.g. '-' or '_')
-- 1. Ask the player to make a guess for what the word is
-- 2. If the player guesses the word, we terminate the game and print a cheerful message
-- 3. Otherwise, we continue playing the game, extending the list of guessed letters.

startHangman :: FilePath -> IO ()
startHangman path = do
  dict <- fmap lines (readFile path)
  putStrLn "Pick a number between 0 and 99:"
  idx <-
    untilJustM $ do
      mN <- readLnMay :: IO (Maybe Int)
      case mN of
        Just n | n >= 0 && n < min 100 (length dict) -> pure (Just n)
        _ -> do
          putStrLn "Invalid index, try again."
          pure Nothing
  let target = dict !! idx
  playHangman [] target

playHangman :: [Char] -> String -> IO ()
playHangman guessed target = do
  let display = [if c `elem` guessed then c else '_' | c <- target]
  putStrLn display
  putStrLn "Enter a guess (letter or word):"
  guess <- getLine
  if guess == target
    then putStrLn "Congrats, you guessed the word!"
    else playHangman (guessed ++ guess) target

-- TASK:
-- Run an IO action an infinite amount of times

forever :: IO a -> IO b
forever action = action >> forever action

-- TASK:
-- Map a function over the result of an IO action.

mapIO :: (a -> b) -> IO a -> IO b
mapIO f action = do
  x <- action
  pure (f x)

-- TASK:
-- "Lift" a function of two arguments to work over two IO actions instead.

lift2IO :: (a -> b -> c) -> IO a -> IO b -> IO c
lift2IO f ma mb = do
  a <- ma
  b <- mb
  pure (f a b)

-- TASK:
-- Read two numbers and sum them using using {lift2IO}

sumTwo :: IO Integer
sumTwo = lift2IO (+) getNumber getNumber

-- TASK:
-- Given an IO action producing a function, and an IO action producing an argument for that function
-- run thefunction over the argument.
-- Try to implement this using {lift2IO}

apIO :: IO (a -> b) -> IO a -> IO b
apIO = lift2IO ($)

-- TASK:
-- Lift a three argument function to work over three IO actions.
-- Try to use {apIO}, {pure}/{mapIO} to implement this

lift3IO :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
lift3IO f ma mb mc = apIO (apIO (mapIO f ma) mb) mc

-- TASK:
-- Given a number and an IO action, run that IO action the number many times,
-- returning all the results in a list.
-- Try to also implement this with {lift2IO}.

replicateIO :: Int -> IO a -> IO [a]
replicateIO n action
  | n <= 0 = pure []
  | otherwise = lift2IO (:) action (replicateIO (n - 1) action)

-- TASK:
-- For each number in a list, read in that many strings from stdin, returning them in a list of lists.

readInLists :: [Int] -> IO [[String]]
readInLists [] = pure []
readInLists (n : ns) = lift2IO (:) (replicateIO n getLine) (readInLists ns)

-- TASK:
-- Map over a list, executing an IO action for each element, and collect the results in a list.

traverseListIO :: (a -> IO b) -> [a] -> IO [b]
traverseListIO _ [] = pure []
traverseListIO f (x : xs) = lift2IO (:) (f x) (traverseListIO f xs)

-- TASK:
-- Implement {readInLists} using {traverseListIO}

readInLists' :: [Int] -> IO [[String]]
readInLists' = traverseListIO (\n -> replicateIO n getLine)

-- TASK:
-- Extend the hangman game to support a turn limit.
-- If the player wants to win, they must guess the word before the turn limit.

-- TASK:
-- Extend the hangman game so it supports entering either a letter, or a word,
-- closer to the actual game of hangman.
-- You can choose whatever format you like for the input, for example it could be a prefix
-- indicating what was entered, e.g.
-- w:<some word here> and c:<some char here>
-- or
-- guess:<some word here> and <char>
