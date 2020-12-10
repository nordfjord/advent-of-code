module Main where

toTreeRow = fmap (== '#')

boolToInt False = 0
boolToInt True = 1

-- Uses cycle to make an infinite represenation of the row
isTree :: Int -> [Bool] -> Bool
isTree x row = cycle row !! x

traverseTrees :: [[Bool]] -> Int -> (Int, Int) -> (Int, Int) -> Int
traverseTrees [] c _ _ = c
traverseTrees matrix count (x, y) (ruleX, ruleY) =
  traverseTrees nextMatrix newCount nextPos (ruleX, ruleY)
  where
    nextMatrix = drop ruleY matrix
    nextPos = (ruleX + x, ruleY + y)
    newCount = count + boolToInt (isTree x (head matrix))

printResults :: ((Int, Int), Int) -> IO ()
printResults ((x, y), result) = do
  print (x, y)
  print result

main :: IO ()
main = do
  contents <- lines <$> getContents
  let matrix = fmap toTreeRow contents
  let rules = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  let results = traverseTrees matrix 0 (0, 0) <$> rules

  mapM_ printResults $ zip rules results

  putStrLn "Product"
  print $ product results
