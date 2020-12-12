{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Attoparsec.Text
import qualified Data.Text.IO                  as T
import qualified Data.Set                      as S
import qualified Data.IntMap                   as IntMap
import           Data.Text                      ( Text )
import           Data.Either                    ( fromRight )
import           Data.Functor
import           Control.Applicative
import           Control.Monad.State            ( runState
                                                , State
                                                , gets
                                                , modify
                                                )
data Operation = Acc | Jmp | Nop deriving (Bounded, Enum, Eq, Ord, Show, Read)
data Instruction = MkInstruction
  { operation :: Operation
  , value     :: Int
  }
  deriving (Bounded, Show, Eq, Ord, Read)
type Program = IntMap.IntMap Instruction
data ProgramResult = Looped | Stopped | Crashed
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

data ProgramState = MkProgramState
  { instrPtr    :: Int
  , accumulator :: Int
  , program     :: Program
  , visited     :: S.Set Int
  }
  deriving (Show, Eq, Ord, Read)

operationP :: Parser Operation
operationP =
  skipSpace
    *> (   (string "acc" $> Acc)
       <|> (string "jmp" $> Jmp)
       <|> (string "nop" $> Nop)
       )

instructionP :: Parser Instruction
instructionP = MkInstruction <$> operationP <*> (skipSpace *> signed decimal)

programP :: Parser [Instruction]
programP = instructionP `sepBy` "\n"


runProgram :: Program -> (ProgramResult, ProgramState)
runProgram prog = runState interprateProgram $ MkProgramState
  { instrPtr    = 0
  , accumulator = 0
  , program     = prog
  , visited     = S.empty
  }

setVisited :: Int -> ProgramState -> ProgramState
setVisited i state = state { visited = S.insert i (visited state) }

interprateProgram :: State ProgramState ProgramResult
interprateProgram = do
  ptr <- gets instrPtr
  vis <- gets visited
  if ptr `S.member` vis
    then pure Looped
    else do
      modify (setVisited ptr)
      instr <- gets (IntMap.lookup ptr . program)
      case instr of
        Just i -> do
          applyInstruction i
          interprateProgram
        Nothing -> do
          len <- gets (IntMap.size . program)
          pure $ if ptr == len then Stopped else Crashed

overInstrPtr :: (Int -> Int) -> ProgramState -> ProgramState
overInstrPtr f state = state { instrPtr = f $ instrPtr state }
overAccumulator :: (Int -> Int) -> ProgramState -> ProgramState
overAccumulator f state = state { accumulator = f $ accumulator state }

overOperation :: (Operation -> Operation) -> Instruction -> Instruction
overOperation f instr = instr { operation = f $ operation instr }

applyInstruction :: Instruction -> State ProgramState ()
applyInstruction (MkInstruction op n) = case op of
  Nop -> modify (overInstrPtr (+ 1))
  Jmp -> modify (overInstrPtr (+ n))
  Acc -> do
    modify (overAccumulator (+ n))
    modify (overInstrPtr (+ 1))

isFixableOp :: Operation -> Bool
isFixableOp Jmp = True
isFixableOp Nop = True
isFixableOp Acc = False

flipOp :: Operation -> Operation
flipOp Jmp = Nop
flipOp Nop = Jmp
flipOp Acc = Acc

invertOperationAt :: Program -> Int -> Program
invertOperationAt prog i = IntMap.adjust (overOperation flipOp) i prog

genPrograms :: Program -> [Program]
genPrograms prog = invertOperationAt prog <$> possibleFixLocations
 where
  progL                = IntMap.toList prog
  possibleFixLocations = fst <$> filter (isFixableOp . operation . snd) progL

runUntilSuccessful :: [Program] -> Either Text ProgramState
runUntilSuccessful []             = Left "Nononno"
runUntilSuccessful (prog : progs) = case returnStatus of
  Crashed -> runUntilSuccessful progs
  Looped  -> runUntilSuccessful progs
  Stopped -> Right $ snd result
 where
  result       = runProgram prog
  returnStatus = fst result

main :: IO ()
main = do
  contents <- T.getContents
  let result = IntMap.fromList . zip [0 ..] $ fromRight [] $ parseOnly
        programP
        contents

  let part1Result = accumulator . snd . runProgram $ result

  putStrLn "Part 1:"
  print part1Result

  putStrLn "Part 2:"
  print $ accumulator <$> runUntilSuccessful (genPrograms result)
