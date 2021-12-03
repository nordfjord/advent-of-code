{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import           Data.Attoparsec.Text    hiding ( count
                                                , take
                                                )
import qualified Data.Text.IO                  as T
import qualified Data.IntMap                   as IM
import           Data.Either
import           Debug.Trace

rule :: IM.IntMap [Int] -> Int -> Int -> (Int, Int)
rule cache turn value = (turn + 1, cached)
 where
  cached = case IM.lookup value cache of
    Nothing          -> 0
    Just []          -> 0
    Just [_        ] -> 0
    Just (x : y : _) -> x - y

prepend :: a -> Maybe [a] -> Maybe [a]
prepend x mxs = case mxs of
  Just xs -> Just (x : xs)
  Nothing -> Just [x]

playGame :: IM.IntMap [Int] -> (Int, Int) -> [(Int, Int)]
playGame cache (turn, value) = (turn, value)
  : playGame (IM.alter (prepend newTurn) newValue cache) (newTurn, newValue)
  where (newTurn, newValue) = rule cache turn value

solve :: Int -> [Int] -> (Int, Int)
solve target numbers = head . filter ((== target) . fst) $ playGame
  cache
  (last $ zip [1 ..] numbers)
  where cache = IM.fromList $ zip numbers (fmap (: []) [1 ..])


part1 :: [Int] -> (Int, Int)
part1 = solve 2020

part2 :: [Int] -> (Int, Int)
part2 = solve 30000000

main :: IO ()
main = do
  contents <- fromRight [] . parseOnly (decimal `sepBy` ",") <$> T.getContents

  print $ part1 contents
  print $ part2 contents
