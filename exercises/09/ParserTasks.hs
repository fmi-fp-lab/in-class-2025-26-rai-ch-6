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

{-# OPTIONS_GHC -iexercises/09/ #-}
{-# OPTIONS_GHC -i./ #-}

module ParserTasks where

import Control.Applicative (many, some, (<|>))
import Data.Char (Char)

-- Parser a - an effect which, when ran on some input string, produces a value of type {a}
import Parser (Parser, nom, parse, parseMany, parseFailure)

-- TODO:
-- "Constructors"/producers:
-- {nom :: Parser Char} - consume a single char and return it
-- {pure :: a -> Parser a} - consume no input and produce the original argument to pure, same as for IO
-- {parseFailure :: Parser a} - fail regardless of the input
-- "Destructors"/consumers:
-- {parse :: Parser a -> String -> Maybe a} - runs the parser and produces a valid result, if any exists
-- {parseMany :: Parser a -> String -> [(String, a)]} - runs the parser and produces all the valid results, as well as any leftover strings for each result
-- Combinators:
-- do syntax
--    sequence multiple parsers, allowing you to use their results
--    this sequencing automatically advances the internal String "variable", as well
--    as automatically aborts when a parse error occurs
-- ```
-- (<|>) :: Parser a -> Parser a -> Parser a
--    px <|> py will first try px, backtracking and trying py if px fails
-- many :: Parser a -> Parser [a]
--    run the given parser as many times as possible, until it fails, permitting 0 successful results
-- some :: Parser a -> Parser [a]
--    same as many, but some requires that the given parser succeed at least once, i.e. the result list is always non empty
-- ```

-- Things we have available:

-- TODO:
char :: Char -> Parser Char
char c = do
  c' <- nom
  if c == c' then
    pure c
  else
    parseFailure

-- TODO:
parseThreeChars' :: Parser (Char, Char, Char)
parseThreeChars' = do
  x <- nom
  y <- nom
  z <- nom
  pure (x, y, z)

-- TODO:
parseNChar :: Int -> Parser [Char]
parseNChar 0 = pure []
parseNChar n = do
  x <- nom
  xs <- parseNChar (n - 1)
  pure (x : xs)

-- >>> parse (parseNChar 5) ""
-- Nothing

-- TODO:
optional :: Parser a -> Parser (Maybe a)
optional p = fmap Just p <|> pure Nothing

-- ($) :: (a -> b) -> a -> b

-- TODO:
-- Map a function over the result of a parser
-- Reimplement optional using this
-- (<$>) :: (a -> b) -> Parser a -> Parser b
-- (<$>) = fmap

-- >>> map (map (+1)) [[1,2,3], [4,5,6]]
-- [[2,3,4],[5,6,7]]

-- >>> (succ <$>) <$> [[1,2,3], [4,5,6]]
-- [[2,3,4],[5,6,7]]

-- >>> (+) <$> Just (1 :: Integer) <*> Just 2
-- Just 3

data Animal = Cat | Dog
  deriving (Show)

-- TODO:
parseAnimal :: Parser Animal
parseAnimal = do
  str <- parseNChar 3
  case str of
    "cat" -> pure Cat
    "dog" -> pure Dog
    _ -> parseFailure

-- >>> parse parseAnimal "minko"
-- Nothing

-- TASK:
-- Same as char, except instead of a specific char, we pass a
-- predicate that the char must satisfy

-- >>> parse (satisfy isDigit) "0"
-- Just '0'
-- >>> parse (satisfy isDigit) "a"
-- Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

-- TASK:
-- Lift a function to work over the result of two parsers

liftParser2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
liftParser2 = undefined

-- TASK:
-- run the given parser n times, i.e. parseNChar n == times n nom
-- Try to use liftParser2 here

times :: Int -> Parser a -> Parser [a]
times = undefined

-- TASK:
-- Parse a single digit. You'll need the {ord} and {fromIntegral} functions here.
-- {ord} returns the ascii code(or unicode stuff) for a character
-- >>> ord 'a'
-- 97
-- >>> ord '0'
-- 48
-- >>> ord '9'
-- 57
-- >>> parse digitParser "0"
-- Just 0
-- >>> parse digitParser "20"
-- Just 2
-- >>> parse digitParser "a"
-- Nothing

digitParser :: Parser Integer
digitParser = undefined

-- Converts a list of integers, assuming they are digits, to a number.
--- >>> digitListToInteger [1, 2, 3]
--- 123
--- >>> digitListToInteger [0, 1, 2, 3]
--- 123
digitListToInteger :: [Integer] -> Integer
digitListToInteger = go . reverse
 where
  go = foldr (\x r -> r * 10 + x) 0

-- TASK:
-- Combine digitParser and digitListToInteger to produce a parser for numbers.

-- >>> parse numberParser "asdf"
-- Nothing
-- >>> parse numberParser "0123"
-- Just 123
-- >>> parse numberParser "21303"
-- Just 21303

numberParser :: Parser Integer
numberParser = undefined

-- TASK:
-- Parse a lot of as seperated by bs

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy = undefined

-- TASK:
-- parse as many as possible from the first parser, then parse as many things with the second parser, as you did with the first
-- not really a^nb^n, but kind of

anbn :: Parser a -> Parser b -> Parser ([a], [b])
anbn = undefined

-- TASK:
-- Parsing s-expressions (the syntax of lisps)

data SExp = SVar String | SList [SExp]
  deriving (Show)

-- Parse a variable name. Think about which things *should not* be a variable name.

-- >>> parse sVarParser "1"
-- Just (SVar "1")
-- >>> parse sVarParser "xyz"
-- Just (SVar "xyz")
-- >>> parse sVarParser "x y"
-- Just (SVar "x")
-- >>> parse sVarParser "(x y)"
-- Nothing
sVarParser :: Parser SExp
sVarParser = undefined

-- TASK:
-- Parse an SExpr. This comes before sListParser, which parses lists,
-- since they will need to be mutually recursive - an SList contains other SExprs,
-- after all. You can return to these examples after you've also implemented
-- sListParser.

-- >>> parse sExpParser "x"
-- Just (SVar "x")
-- >>> parse sExpParser "123"
-- Just (SVar "123")
-- >>> parse sExpParser "(1 2 3)"
-- Just (SList [SVar "1",SVar "2",SVar "3"])
-- >>> parse sExpParser "(1 (4 5))"
-- Just (SList [SVar "1",SList [SVar "4",SVar "5"]])
-- >>> parse sExpParser "((f 2 (3 4  ))   1 (4 5))"
-- Just (SList [SList [SVar "f",SVar "2",SList [SVar "3",SVar "4"]],SVar "1",SList [SVar "4",SVar "5"]])

sExpParser :: Parser SExp
sExpParser = undefined

-- TASK:
-- consume as many things that are spaces as possible
-- You can use isSpace to detect what a space is.
-- This parser should always succeed, since even 0 spaces are "as many as possible".

eatSpace :: Parser ()
eatSpace = undefined

-- TASK:
-- Parse a list of SExprs. As this description implies
-- this will need to be mutually recursive with the next function.
-- The easiest way to write this is t

-- >>> parse sListParser "(1 2 3)"
-- Just (SList [SVar "1",SVar "2",SVar "3"])
-- >>> parse sListParser "2"
-- Nothing
-- >>> parse sListParser "(1 2 3"
-- Nothing
-- >>> parse sListParser "1 2 3)"
-- Nothing
-- >>> parse sListParser "(2 3 (4 5))"
-- Just (SList [SVar "2",SVar "3",SList [SVar "4",SVar "5"]])

sListParser :: Parser SExp
sListParser = undefined

-- TASK:
-- We'll be builiding up a parser for json values from here on out.
-- Note that I didn't have time to review this, so call me if something seems confusing


data Value
  = Null
  | Bool Bool
  | Number Integer
  | String String
  | Arrays [Value]
  | Object [(String, Value)]
  deriving (Show)

-- JSON EXAMPLES:
-- https://json.org/example.html
-- Null:
-- - null
-- Bools:
-- - true
-- - false
-- Numbers:
-- - 123
-- - 023
-- - 141
-- Strings:
-- - "lol"
-- - "nice d00d"
-- - "a#f#∞"
-- Arrays:
-- - [1]
-- - [null]
-- - [false,1,null]
-- - [[1,true],[false],null,"lol"]
-- - [{"heh":5},true]
-- Objects:
-- - {}
-- - {"nice":"dude"}
-- - {"nice":null}
-- - {"heh":true,"kek":null,{"array":[1,2,3]}}
-- - {"heh":true,"kek":null,{"array":[{"single":"thing"}]}}

-- TASK:
-- Parse the given string, and only the given string
-- Proceed recursively!

-- >>> parse (string "kami") "kami"
-- Just "kami"
-- >>> parse (string "kami") "kam"
-- Nothing
-- >>> parse (string "kami") "hair"
-- Nothing
-- >>> parse (string "kami") "kamipaper"
-- Just "kami"

string :: String -> Parser String
string = undefined

-- TASK:

-- >>> parse nullParser "null"
-- Just Null
-- >>> parse nullParser "nul"
-- Nothing
-- >>> parse nullParser "true"
-- Nothing

nullParser :: Parser Value
nullParser = undefined

-- TASK:

-- >>> parse falseParser "false"
-- Just (Bool False)
-- >>> parse falseParser "falsE"
-- Nothing
-- >>> parse falseParser "true"
-- Nothing

falseParser :: Parser Value
falseParser = undefined

-- TASK:

-- >>> parse trueParser "true"
-- Just (Bool True)
-- >>> parse trueParser "True"
-- Nothing
-- >>> parse trueParser "false"
-- Nothing

trueParser :: Parser Value
trueParser = undefined

-- TASK:

-- >>> parse boolParser "true"
-- Just (Bool True)
-- >>> parse boolParser "false"
-- Just (Bool False)
-- >>> parse boolParser "fls"
-- Nothing
-- >>> parse boolParser "no"
-- Nothing

boolParser :: Parser Value
boolParser = undefined

-- TASK:

-- >>> parse numberValueParser "69"
-- Just (Number 69)
-- >>> parse numberValueParser "0420"
-- Just (Number 420)
-- >>> parse numberValueParser "a0420"
-- Nothing
-- >>> parse numberValueParser "aasdf"
-- Nothing

numberValueParser :: Parser Value
numberValueParser = undefined

-- TASK:
-- Get two parser and "surround" the second one with the first.

-- >>> parse (surround (char '"') number) "\"123\""
-- Just 123
-- >>> parse (surround (char '"') number) "\"123"
-- Nothing
-- >>> parse (surround (char '"') number) "123\""
-- Nothing
-- >>> parse (surround number nullParser) "345null123"
-- Just Null

surround :: Parser around -> Parser b -> Parser b
surround = undefined

-- TASK:
-- You can assume that there are no double quotes in the string "

-- >>> parse stringParser "\"nice d00d\""
-- Just (String "nice d00d")
-- >>> parse stringParser "\"\""
-- Just (String "")
-- >>> parse stringParser "\"bleh\""
-- Just (String "bleh")
-- >>> parse stringParser ""
-- Nothing
-- >>> parse stringParser "\"bleh"
-- Nothing
-- >>> parse stringParser "bleh\""
-- Nothing

stringParser :: Parser Value
stringParser = undefined

-- TASK:
-- Surround a parser with an opening and closing one

-- >>> parse (between (char '"') (char '"') number) "\"123\""
-- Just 123
-- >>> parse (between (char '{') (char '}') (string "nice")) "{nice}"
-- Just "nice"
-- >>> parse (between (char '{') (char '}') (string "nice")) "{noice}"
-- Nothing

between :: Parser open -> Parser close -> Parser a -> Parser a
between = undefined

-- TASK:
-- This is traditionally (<*), and is available for all Applicatives

-- >>> parse (ignoreRight (char 'a') (char 'b')) "ab"
-- Just 'a'
-- >>> parse (ignoreRight (char 'a') (char 'b')) "a"
-- Nothing
-- >>> parse (ignoreRight (char 'a') (char 'b')) "ba"
-- Nothing

ignoreRight :: Parser a -> Parser b -> Parser a
ignoreRight = undefined

-- TASK:
-- You can assume (you will write it in a second)
-- that there already exists (it does, down below) a global valueParser,
-- which parses any kind of json value

-- >>> parse arrayParser "[]"
-- Just (Array [])
-- >>> parse arrayParser "[1,2,3]"
-- Just (Array [Number 1,Number 2,Number 3])
-- >>> parse arrayParser "[true,null,3]"
-- Just (Array [Bool True,Null,Number 3])
-- >>> parse arrayParser "[true,\"nulllol\",3]"
-- Just (Array [Bool True,String "nulllol",Number 3])
--
-- ---- EXAMPLES BELOW THIS POINT REQUIRE YOU TO HAVE IMPLEMENTED objectParser
-- >>> parse arrayParser "[{}]"
-- Just (Array [Object []])
-- >>> parse arrayParser "[{},{},{}]"
-- Just (Array [Object [],Object [],Object []])
-- >>> parse arrayParser "[{\"key0\":null},{\"key1\":2},{\"key2\":true}]"
-- Just (Array [Object [("key0",Null)],Object [("key1",Number 2)],Object [("key2",Bool True)]])

arrayParser :: Parser Value
arrayParser = undefined

-- TASK:

-- >>> parse objectElementParser "\"name\":\"val\""
-- Just ("name",String "val")
-- >>> parse objectElementParser "\"name\":null"
-- Just ("name",Null)
-- >>> parse objectElementParser "\"name\":true"
-- Just ("name",Bool True)
--
-- ---- EXAMPLES BELOW THIS POINT REQUIRE YOU TO HAVE IMPLEMENTED arrayParser
-- >>> parse objectElementParser "\"name\":[1,2,3]"
-- Just ("name",Array [Number 1,Number 2,Number 3])
-- >>> parse objectElementParser "\"name\":[1,null,3]"
-- Just ("name",Array [Number 1,Null,Number 3])
--
-- ---- EXAMPLES BELOW THIS POINT REQUIRE YOU TO HAVE IMPLEMENTED objectParser
-- >>> parse objectElementParser "\"name\":{}"
-- Just ("name",Object [])
-- >>> parse objectElementParser "\"name\":{\"nameagain\":null}"
-- Just ("name",Object [("nameagain",Null)])

objectElementParser :: Parser (String, Value)
objectElementParser = undefined

-- TASK:
-- This has a "dummy" implementation right now, because of technical reasons*.
-- Delete it and write your own!
-- It is assumed all other things are implemented in the examples!
-- I've placed "prettified" versions of the strings before their corresponding example
--
-- >>> parse objectParser "{}"
-- Just (Object [])
--
-- -- rendered normally:
-- -- {"pesho": "krava"}
--
-- >>> parse objectParser "{\"pesho\":\"krava\"}"
-- Just (Object [("pesho",String "krava")])
--
-- -- rendered normally:
-- -- {"pesho": {"krava":null, "doggo":"Deogie"}}
--
-- >>> parse objectParser "{\"pesho\":{\"krava\":null,\"doggo\":\"Deogie\"}}"
-- Just (Object [("pesho",Object [("krava",Null),("doggo",String "Deogie")])])
--
-- -- rendered normally:
-- -- { "pesho": {"krava": null, "doggo": "Deogie"}
-- -- , "69": [420, 1337]
-- -- }
--
-- >>> parse objectParser "{\"pesho\":{\"krava\":null,\"doggo\":\"Deogie\"},\"69\":[420,1337]}"
-- Just (Object [("pesho",Object [("krava",Null),("doggo",String "Deogie")]),("69",Array [Number 420,Number 1337])])

objectParser :: Parser Value
objectParser = do
  char 'c'
  pure $ Object []

-- \* Technical reasons:
--
-- Because arrayParser (most likely) uses 'many' to parse objects within it,
-- and many will infinitely loop
-- if the parser you are calling it with doesn't consume any input.

valueParser :: Parser Value
valueParser =
  nullParser
    <|> boolParser
    <|> numberValueParser
    <|> stringParser
    <|> arrayParser
    <|> objectParser
