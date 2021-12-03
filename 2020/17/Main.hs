{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import           Data.Functor
import           Control.Applicative
import qualified Data.IntMap as IM
import qualified Data.Map as M

data Cell = Active | Inactive deriving (Show, Eq)


cellP :: Parser Cell
cellP = choice [char '#' $> Active, char '.' $> Inactive]

lineP :: Parser [Cell]
lineP = many cellP

inputP :: Parser [[Cell]]
inputP = lineP `sepBy` "\n"


type State = IM.IntMap (M.Map Int [Int])

type Coord = (Int, Int, Int)

neighbors = [Coord]
neighbors = [(x,y,z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x,y,z) /= (0,0,0)]

rule z ns = let n = count Active ns in n == 3 || x == Active && n == 2
                                 ]

runSimulation :: State -> State
runSimulation state = newState
  where

part1 :: State -> Int
part1 state = simulate

main :: IO ()
main = do
  (Right contents) <- parseOnly inputP <$> T.getContents

  let matrix = M.fromList $ [(1, M.fromList $ fmap (zip [0..]) contents)]
  print contents
