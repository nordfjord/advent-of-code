import Data.List

type Seat = (Int, Int)

increase min max = min + ((max - min) `div` 2)

decrease min max = max - ((max - min) `div` 2)

seat :: (Int, Int) -> (Int, Int) -> String -> Seat
seat (min, max) c ('B' : xs) = seat (increase min max, max) c xs
seat (min, max) c ('F' : xs) = seat (min, decrease min max) c xs
seat r (min, max) ('L' : xs) = seat r (min, decrease min max) xs
seat r (min, max) ('R' : xs) = seat r (increase min max, max) xs
seat (row, _) (col, _) _ = (row, col)

seatID :: Seat -> Int
seatID (row, col) = row * 8 + col

main :: IO ()
main = do
  lines <- fmap lines getContents

  putStrLn "Part 1:"
  print $ maximum $ seatID . seat (0, 128) (0, 8) <$> lines

  putStrLn "Part 2:"
  print $ (\\) [26 .. 996] $ sort $ seatID . seat (0, 128) (0, 8) <$> lines
