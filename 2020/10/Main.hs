{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Data.Attoparsec.Text           ( parseOnly
                                                , decimal
                                                , sepBy
                                                , Parser
                                                )
import qualified Data.Text.IO                  as T
import           Data.Either
import           Data.Maybe
import qualified Data.Set                      as S
import           Control.Monad                  ( guard )

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.BFS
import           Data.Graph.Inductive.PatriciaTree
                                                ( Gr )

inputP :: Parser [Int]
inputP = decimal `sepBy` "\n"

lookupElem :: Ord a => a -> S.Set a -> Maybe a
lookupElem elem set = if elem `S.member` set then Just elem else Nothing

graphFromPairs :: [(Node, [Node])] -> Gr () ()
graphFromPairs pairs = mkUGraph lnodes ledges
 where
  ledges = pairs >>= (\(a, bs) -> fmap (a, ) bs)
  lnodes = fmap fst pairs


-- This is a modified version of dfs from fgl
-- We know the graph doesn't contain any cycles
-- so instead of traversing the subgraph
-- we continue to use the whole graph
-- at all times
--
-- This has horrible time complexity O(N!)
dfsPathCount :: (Graph gr) => [Node] -> Int -> gr a b -> Int -> Int
dfsPathCount [] _ _ paths                        = paths
dfsPathCount _ _ graph paths | isEmpty graph     = paths
dfsPathCount (visit : visits) target graph paths = case match visit graph of
  (Just c, _) -> dfsPathCount (suc' c ++ visits) target graph paths'
    where paths' = if node' c == target then paths + 1 else paths
  (Nothing, _) -> dfsPathCount visits target graph paths



countPaths :: (Graph gr) => Node -> Int -> gr a b -> Int
countPaths start end graph = dfsPathCount [start] end graph 0


addAdapters :: S.Set Int -> S.Set Int
addAdapters adapters =
  (S.findMax adapters + 3) `S.insert` (0 `S.insert` adapters)

buildGraph :: S.Set Node -> Gr () ()
buildGraph adapters = graphFromPairs adjPairs
 where
  completeAdapters = addAdapters adapters
  adjPairs         = S.toList $ S.map toAdjPair completeAdapters
  toAdjPair rating =
    ( rating
    , catMaybes
      [ (rating + 1) `lookupElem` completeAdapters
      , (rating + 2) `lookupElem` completeAdapters
      , (rating + 3) `lookupElem` completeAdapters
      ]
    )

-- Don't run this, it's O(N!)
part2 :: S.Set Int -> Int
part2 adapters = countPaths 0 end graph
 where
  graph = buildGraph adapters
  end   = S.findMax adapters

part2smart :: S.Set Int -> Int
part2smart adapters =
  snd $ head $ foldr visit [] (S.toList (addAdapters adapters))

visit :: Int -> [(Int, Int)] -> [(Int, Int)]
visit current cache = (current, result) : take 2 cache
 where
  result = max 1 $ sum $ do
    (key, value) <- cache
    guard $ key - current <= 3
    pure value


part1 :: S.Set Node -> Int
part1 adapters = countJumps (0, 0) path
 where
  graph = buildGraph adapters
  path  = bfs 0 graph
  countJumps (       a   , b     ) []           = a * b
  countJumps (       a   , b     ) [_         ] = a * b
  countJumps counts@(ones, threes) (x : y : xs) = case y - x of
    1 -> countJumps (ones + 1, threes) (y : xs)
    3 -> countJumps (ones, threes + 1) (y : xs)
    _ -> countJumps counts (y : xs)


main :: IO ()
main = do
  numbers <- fromRight [] . parseOnly inputP <$> T.getContents
  let numberSet = S.fromList numbers

  print $ part1 numberSet
  print $ part2smart numberSet
