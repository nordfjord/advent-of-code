{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO                  as T
import           Data.Attoparsec.Text           ( char
                                                , decimal
                                                , sepBy
                                                , parseOnly
                                                , choice
                                                , Parser
                                                )
import           Data.Either
import           Data.Functor

data Direction = N | S | W | E | L | R | F deriving (Show, Read, Eq)
data Action = Action Direction Int
  deriving (Show, Read, Eq)

type Coord = (Int, Int)

directionP :: Parser Direction
directionP = choice
  [ char 'N' $> N
  , char 'S' $> S
  , char 'W' $> W
  , char 'E' $> E
  , char 'L' $> L
  , char 'R' $> R
  , char 'F' $> F
  ]

actionP :: Parser Action
actionP = Action <$> directionP <*> decimal

inputP :: Parser [Action]
inputP = actionP `sepBy` "\n"

rotateShip :: Direction -> Int -> Direction
rotateShip N 90           = E
rotateShip E 90           = S
rotateShip S 90           = W
rotateShip W 90           = N
rotateShip dir n | n < 90 = rotateShip dir (n + 360)
rotateShip dir n          = rotateShip (rotateShip dir 90) (n - 90)

runCourse :: Coord -> Direction -> [Action] -> Int
runCourse (e, n) _       []                              = abs e + abs n
runCourse (e, n) shipDir ((Action dir amount) : actions) = runCourse (e', n')
                                                                     shipDir'
                                                                     actions
 where
  shipDir' = case dir of
    R -> rotateShip shipDir amount
    L -> rotateShip shipDir (negate amount)
    _ -> shipDir
  e' = case dir of
    E                -> e + amount
    W                -> e - amount
    F | shipDir == E -> e + amount
    F | shipDir == W -> e - amount
    _                -> e
  n' = case dir of
    N                -> n + amount
    S                -> n - amount
    F | shipDir == N -> n + amount
    F | shipDir == S -> n - amount
    _                -> n

rotate :: Coord -> Int -> Coord
rotate (e, n) 90   = (n, negate e)
rotate x n | n < 0 = rotate x (n + 360)
rotate x n         = rotate (rotate x 90) (n - 90)

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

runCourse2 :: Coord -> Coord -> [Action] -> Int
runCourse2 _        (se, sn) []                              = abs se + abs sn
runCourse2 (we, wn) (se, sn) ((Action dir amount) : actions) = runCourse2
  (we', wn')
  (se', sn')
  actions
 where
  (we', wn') = case dir of
    E -> (we + amount, wn)
    W -> (we - amount, wn)
    N -> (we, wn + amount)
    S -> (we, wn - amount)
    L -> rotate (we, wn) (negate amount)
    R -> rotate (we, wn) amount
    _ -> (we, wn)
  (se', sn') = case dir of
    F -> iterate (add (we, wn)) (se, sn) !! amount
    _ -> (se, sn)


part2 :: [Action] -> Int
part2 = runCourse2 (10, 1) (0, 0)

part1 :: [Action] -> Int
part1 = runCourse (0, 0) E

main :: IO ()
main = do
  contents <- fromRight [] . parseOnly inputP <$> T.getContents

  print $ part1 contents
  print $ part2 contents
