{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.Text
import qualified Data.Text.IO                  as T
import           Data.Either
import           Data.Maybe
import           Data.List
import           Data.Functor
import           Control.Applicative

data Bus = Bus Int | X deriving (Show, Read, Eq)

busP :: Parser Bus
busP = (Bus <$> decimal) <|> (char 'x' $> X)



inputParserP1 :: Parser (Int, [Int])
inputParserP1 = do
  earliestTime <- decimal
  "\n"
  buses <- busP `sepBy` ","

  return (earliestTime, (\(Bus b) -> b) <$> filter (/= X) buses)

inputParserP2 :: Parser [Bus]
inputParserP2 = do
  decimal
  "\n"
  busP `sepBy` ","

busSchedule :: Int -> [Int]
busSchedule bus = iterate (+ bus) bus

busSchedules :: [Int] -> [[Int]]
busSchedules = fmap busSchedule

part1 :: Int -> [Int] -> Int
part1 target buses = busId * waitTime
 where
  schedules = zip buses (head . filter (>= target) <$> busSchedules buses)
  (busId, time) =
    foldr1 (\(a, t1) (b, t2) -> if t1 < t2 then (a, t1) else (b, t2)) schedules
  waitTime = time - target

sieve :: [(Int, Int)] -> Maybe Int
sieve []              = Nothing
sieve ((t, bus) : xs) = Just $ sieveWith t bus xs

sieveWith :: Int -> Int -> [(Int, Int)] -> Int
sieveWith candidate _    []                 = candidate
sieveWith candidate step list@((t, x) : xs) = if candidate `mod` x == t
  then sieveWith candidate (step * x) xs
  else sieveWith (candidate + step) step list

createBusPairs :: [Bus] -> [(Int, Int)]
createBusPairs buses = fmap mkModulo buses'
 where
  zipped   = zip [0 ..] buses
  filtered = filter ((/= X) . snd) zipped
  buses'   = fmap (\(t, Bus b) -> (t, b)) filtered
  mkModulo (i, x) = ((x - i) `mod` x, x)

part2 :: [Bus] -> Int
part2 buses = fromMaybe 0 $ sieve buspairs
  where buspairs = sortOn (negate . snd) $ createBusPairs buses




main :: IO ()
main = do
  contents <- T.getContents
  let (targetTime, buses) =
        fromRight (0, []) $ parseOnly inputParserP1 contents
  print $ part1 targetTime buses

  let buses2 = fromRight [] $ parseOnly inputParserP2 contents
  print $ part2 buses2
