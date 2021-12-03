{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Attoparsec.Text          as P
import           Data.Attoparsec.Text           ( Parser )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T
import qualified Data.Text                     as T
import           Data.List
import           Debug.Trace
import           Control.Monad

type Field = (Int -> Bool)

type Ticket = [Int]

valueP :: Parser (Int -> Bool)
valueP =
  (\min max value -> value >= min && value <= max)
    <$> (P.decimal <* "-")
    <*> P.decimal

fieldP :: Parser (Text, Field)
fieldP = do
  name <- P.takeWhile (/= ':')
  ": "
  filters <- valueP `P.sepBy` " or "

  return (name, or . mapM id filters)
  -- (,) <$>  <* ": " <*> (mapM or $ valueP `P.sepBy` " or ")

ticketP :: Parser [Int]
ticketP = P.decimal `P.sepBy` ","

sc :: Parser ()
sc = P.skipWhile (\c -> c == ' ' || c == '\t' || c == '\n')

inputP :: Parser ([(Text, Field)], Ticket, [Ticket])
inputP = do
  fields <- fieldP `P.sepBy` "\n"
  sc
  "your ticket:\n"
  mt <- ticketP
  sc
  "nearby tickets:\n"
  tickets <- ticketP `P.sepBy` "\n"
  return $ (fields, mt, tickets)

isValid :: [Field] -> Int -> Bool
isValid []       _ = False
isValid (f : fs) v = f v || isValid fs v

keepIfInvalid :: [Field] -> Int -> Int
keepIfInvalid fields value = if not $ isValid fields value then value else 0

part1 :: [Field] -> [Ticket] -> Int
part1 fields tickets = sum $ tickets >>= fmap (keepIfInvalid fields)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)


part2 :: [(Text, Field)] -> Ticket -> [Ticket] -> Int
part2 fields ticket tickets =
  product $ map fst $ filter (T.isPrefixOf "departure" . snd) $ zip ticket
                                                                    fieldNames
 where
  filterAttribs xs = map fst $ filter (\(_, r) -> all r xs) $ fields
  attributes = map filterAttribs $ transpose tickets

  fieldNames = map head $ converge removeKnowns attributes
  removeKnowns names =
    let knowns = concat $ filter ((== 1) . length) names
        doRemove ns =
          if length ns /= 1 then filter (`notElem` knowns) ns else ns
    in  map doRemove names


main :: IO ()
main = do
  (Right (fields, mt, tickets)) <- P.parseOnly inputP <$> T.getContents

  let validators = snd <$> fields

  print $ part1 validators tickets
  print $ part2 fields mt $ filter (all $ isValid validators) tickets
