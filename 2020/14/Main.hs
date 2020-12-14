{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO                  as T
import           Data.Attoparsec.Text
import           Data.Functor
import           Data.Bifunctor
import           Data.Word
import           Data.Bits
import           Data.Either
import           Control.Monad.State
import qualified Data.Map                      as M


data MaskType = M0 | M1 | MX deriving (Show, Read, Eq)
type Mask = [(Int, MaskType)]
type Operation = (Word64, Word64)

maskP :: Parser Mask
maskP = do
  "mask = "
  bits <- count 36 $ choice [char 'X' $> MX, char '1' $> M1, char '0' $> M0]
  "\n"
  return $ zip [0 ..] (reverse bits)

opP :: Parser Operation
opP = do
  "mem["
  v <- decimal
  "] = "
  v2 <- decimal
  return (v, v2)

blockP :: Parser (Mask, [Operation])
blockP = (,) <$> maskP <*> opP `sepBy` "\n"


blocksP :: Parser [(Mask, [Operation])]
blocksP = blockP `sepBy` "\n"

maskToActionP1 :: Mask -> Word64 -> Word64
maskToActionP1 = flip (foldr go)
 where
  go (_, MX) w = w
  go (x, M1) w = w `setBit` x
  go (x, M0) w = w `clearBit` x

maskToActionP2 :: Mask -> Word64 -> [Word64]
maskToActionP2 mask mem = foldr go [mem] mask
 where
  go :: (Int, MaskType) -> [Word64] -> [Word64]
  go (_, M0) mems = mems
  go (x, M1) mems = (`setBit` x) <$> mems
  go (x, MX) mems = ((`clearBit` x) <$> mems) <> ((`setBit` x) <$> mems)

data ProgramState = ProgramState
  { blocks :: [(Mask, [Operation])]
  , memory :: M.Map Word64 Word64
  }

overMemory f p = p { memory = f $ memory p }
overBlocks f p = p { blocks = f $ blocks p }


compute
  :: (Mask -> [Operation] -> [Operation])
  -> [(Mask, [Operation])]
  -> M.Map Word64 Word64
compute comp ops = fst $ runState go $ ProgramState { blocks = ops
                                                    , memory = mempty
                                                    }
 where
  go = do
    xs <- gets blocks
    case xs of
      []                 -> gets memory
      ((mask, ops) : xs) -> do
        modify $ overMemory $ M.union $ M.fromList $ comp mask ops
        modify $ overBlocks $ const xs
        go

computeP1 :: Mask -> [Operation] -> [Operation]
computeP1 mask ops = second f <$> ops where f = maskToActionP1 mask

computeP2 :: Mask -> [Operation] -> [Operation]
computeP2 mask ops =
  first (maskToActionP2 mask) <$> ops >>= (\(xs, x) -> zip xs (repeat x))


part1 :: [(Mask, [Operation])] -> Word64
part1 blocks = sum . M.elems $ compute computeP1 blocks

part2 :: [(Mask, [Operation])] -> Word64
part2 blocks = sum . M.elems $ compute computeP2 blocks

main :: IO ()
main = do
  contents <- fromRight [] . parseOnly blocksP <$> T.getContents

  print $ part1 contents
  print $ part2 contents
