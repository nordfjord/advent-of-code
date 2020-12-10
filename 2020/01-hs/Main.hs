module Main where

loadNumber :: String -> Int
loadNumber = read

part1 :: [Int] -> Int
part1 xs = product nums
  where
    nums = head . filter ((== 2020) . sum) $ do
      x <- xs
      y <- xs
      pure [x, y]

part2 :: [Int] -> Int
part2 xs = product nums
  where
    nums = head . filter ((== 2020) . sum) $ do
      x <- xs
      y <- xs
      z <- xs
      pure [x, y, z]

main :: IO ()
main = do
  content <- lines <$> getContents
  numbers <- pure $ loadNumber <$> content
  print $ part1 $ numbers
  print $ part2 $ numbers
