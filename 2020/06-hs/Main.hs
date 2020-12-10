-- |
module Main where

import Data.List.Split (splitOn)
import Data.Set (Set, fromList, intersection, size)

part1 :: [String] -> Int
part1 groups = sum sizes
  where
    sizes = size . fromList . filter (/= '\n') <$> groups

groupToSet :: String -> [Set Char]
groupToSet group = fromList <$> lines group

part2 groups = sum answers
  where
    people = groupToSet <$> groups
    answers = size . foldr1 intersection <$> people

main :: IO ()
main = do
  contents <- getContents
  let groups = splitOn "\n\n" contents

  let p1 = part1 groups

  putStrLn "Part 1:"
  print p1

  let p2 = part2 groups

  putStrLn "Part 2:"
  print p2
