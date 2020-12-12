{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import qualified Data.Attoparsec.Text          as P

preamble n = take n

inputP :: P.Parser [Int]
inputP = P.decimal `P.sepBy` "\n"

main :: IO ()
main = do
  contents <- P.parseOnly inputP <$> T.getContents

  print $ contents
