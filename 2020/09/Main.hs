{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO                  as T
import qualified Data.Attoparsec.Text          as P
import           Data.Either                    ( fromRight )
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( guard )
import           Data.List.Split                ( divvy )
import           System.Environment             ( lookupEnv )

part1 :: [Int] -> [Int] -> Int
part1 _        []       = 0
part1 preamble (n : ns) = if n `notElem` sums
  then n
  else part1 (tail preamble ++ [n]) ns
  where sums = liftA2 (+) preamble preamble

part2 :: Int -> [Int] -> [Int]
part2 target ns = do
  cat  <- [2 .. length ns]
  list <- divvy cat 1 ns
  guard $ sum list == target
  return $ maximum list + minimum list

inputP :: P.Parser [Int]
inputP = P.decimal `P.sepBy` "\n"

main :: IO ()
main = do
  contents     <- T.getContents
  preambleSize <- maybe 25 read <$> lookupEnv "PREAMBLE_SIZE"
  let ns     = fromRight [] $ P.parseOnly inputP contents

  let target = part1 (take preambleSize ns) (drop preambleSize ns)

  putStrLn "Part 1:"
  print target

  putStrLn "Part 1:"
  print $ part2 target ns
