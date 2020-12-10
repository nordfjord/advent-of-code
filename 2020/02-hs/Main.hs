{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Either (rights)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

type Password = (Int, Int, Char, String)

parsePassword :: Parser Password
parsePassword = do
  from <- P.many1 P.digit
  P.char '-'
  to <- P.many1 P.digit
  P.spaces
  letter <- P.anyChar
  P.char ':'
  P.spaces
  password <- P.many1 P.anyChar
  return (read from :: Int, read to :: Int, letter, password)

validatePart1 :: Password -> Bool
validatePart1 (from, to, letter, password) =
  letterCount >= from && letterCount <= to
  where
    letterCount = length $ filter (== letter) password

validatePart2 :: Password -> Bool
validatePart2 (from, to, letter, password) =
  (pos1 || pos2) && not (pos1 && pos2)
  where
    pos1 = letter == password !! (from - 1)
    pos2 = letter == password !! (to - 1)

parse rule = P.parse rule "(source)"

main :: IO ()
main = do
  contents <- lines <$> getContents
  let passwordPolicies = rights $ parse parsePassword <$> contents
  putStrLn "Part 1"
  print $ length $ filter validatePart1 passwordPolicies

  putStrLn "Part 2"
  print $ length $ filter validatePart2 passwordPolicies
