{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text           ( Parser
                                                , char
                                                , parseOnly
                                                , sepBy
                                                )
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as M
import           Data.Functor
import           Data.Either
import           Data.Maybe
import           Control.Applicative


data Seat = Floor | Empty | Occupied deriving (Show, Read, Eq)

type Coord = (Int, Int)
type Pos = (Coord, Seat)

floorP :: Parser Seat
floorP = (char '.' $> Floor) <|> (char 'L' $> Empty) <|> (char '#' $> Occupied)

inputP :: Parser [[Seat]]
inputP = many floorP `sepBy` "\n"

makePositions :: [[Seat]] -> [Pos]
makePositions rows = do
  (row, seats) <- zip [1 ..] rows
  (col, seat ) <- zip [1 ..] seats
  return ((row, col), seat)

transitionP1 :: Seat -> Int -> Seat
transitionP1 Floor _             = Floor
transitionP1 Empty 0             = Occupied
transitionP1 Empty _             = Empty
transitionP1 Occupied c | c >= 4 = Empty
transitionP1 Occupied _          = Occupied

runSimulation :: (Coord -> Maybe Seat) -> [Pos] -> [Pos]
runSimulation getSeatAt seats = do
  (coord, seat) <- seats
  let adjacentOccupiedSeatCount =
        length
          $   filter (== Occupied)
          $   catMaybes
          $   getSeatAt
          <$> adjacentCoords coord

  return (coord, transitionP1 seat adjacentOccupiedSeatCount)


directions :: [Coord]
directions =
  [(1, 0), (0, 1), (-1, 0), (0, -1), (-1, 1), (1, -1), (1, 1), (-1, -1)]

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

adjacentSeats :: (Coord -> Maybe Seat) -> Coord -> [Seat]
adjacentSeats getSeatAt coord = findSeatInDir coord <$> directions
 where
  findSeatInDir c dir = case getSeatAt (add c dir) of
    (Just Floor) -> findSeatInDir (add c dir) dir
    (Just x    ) -> x
    Nothing      -> Floor

transitionP2 :: Seat -> Int -> Seat
transitionP2 Floor _             = Floor
transitionP2 Empty 0             = Occupied
transitionP2 Empty _             = Empty
transitionP2 Occupied c | c >= 5 = Empty
transitionP2 Occupied _          = Occupied


runSimulationP2 :: (Coord -> Maybe Seat) -> [Pos] -> [Pos]
runSimulationP2 getSeatAt seats = do
  (coord, seat) <- seats

  let adjacentOccupiedSeatCount =
        length $ filter (== Occupied) $ adjacentSeats getSeatAt coord

  return (coord, transitionP2 seat adjacentOccupiedSeatCount)


adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) = add (x, y) <$> directions

part2 :: [Pos] -> Int
part2 positions = count
 where
  cache     = M.fromList positions
  nextState = runSimulationP2 (`M.lookup` cache) positions
  count     = if nextState == positions
    then length $ filter ((==) Occupied . snd) positions
    else part2 nextState

part1 :: [Pos] -> Int
part1 positions = count
 where
  cache     = M.fromList positions
  nextState = runSimulation (`M.lookup` cache) positions
  count     = if nextState == positions
    then length $ filter ((==) Occupied . snd) positions
    else part1 nextState


main :: IO ()
main = do
  contents <- fromRight [] . parseOnly inputP <$> T.getContents

  let positions = makePositions contents

  print $ part1 positions

  print $ part2 positions
