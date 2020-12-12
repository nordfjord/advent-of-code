module Main where

import           Data.Graph
import qualified Data.Map                      as Map
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Data.Either                    ( rights
                                                , fromRight
                                                )
import           Data.Maybe                     ( catMaybes )
import           Maybes                         ( fromJust )
import           Data.Tree                      ( foldTree
                                                , drawTree
                                                )
import           Debug.Trace

type Parser = Parsec Void String

color :: Parser String
color = do
  w1 <- many letterChar
  char ' '
  w2 <- many letterChar
  char ' '
  string "bags" <|> string "bag"
  return (w1 ++ w2)

additionalColor :: Parser (Int, String)
additionalColor = do
  n <- decimal
  char ' '
  c <- color
  return (n, c)

noBags :: Parser [a]
noBags = string "no other bags" >> return []

parseLine :: Parser (String, [(Int, String)])
parseLine = do
  bagColor <- color
  string " contain "
  xs <- noBags <|> additionalColor `sepBy` string ", "
  char '.'
  return (bagColor, xs)

rootNode :: String
rootNode = "shinygold"

createMapP2 :: [(String, [a])] -> Map.Map String [a]
createMapP2 = Map.fromListWith (++)

createTreeP2 :: Ord k => (a, k) -> Map.Map k [(a, k)] -> Tree (a, k)
createTreeP2 root m = case Map.lookup (snd root) m of
  Just xs -> Node root (fmap (`createTreeP2` m) xs)
  Nothing -> Node root []

solveP2 :: Int -> Tree Int -> Int
solveP2 mult (Node n xs) = n' + sum (fmap (solveP2 n') xs) where n' = n * mult

part1 :: Graph -> (String -> Maybe Vertex) -> Int
part1 graph vertex =
  subtract 1 . length . filter (\v -> path graph v sg) . vertices $ graph
  where (Just sg) = vertex rootNode


part2 :: [(String, [(Int, String)])] -> Int
part2 = solveP2 1 . fmap fst . createTreeP2 (1, rootNode) . createMapP2

main :: IO ()
main = do
  contents <- getContents
  let parsed = fromJust . parseMaybe (parseLine `sepEndBy` newline) $ contents
  let (graph, node, vertex) = graphFromEdges $ fmap pairToTriple parsed

  print $ part1 graph vertex
  print $ subtract 1 $ part2 parsed
