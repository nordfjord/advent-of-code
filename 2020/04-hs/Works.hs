{-# LANGUAGE OverloadedStrings #-}

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  blocks <- map (parseBlock . splitField) . splitBlock <$> T.getContents

  putStrLn "Part 1:"
  print $ part1 blocks

  putStrLn "Part 2:"
  print $ part2 blocks

-- split whole file into blocks
splitBlock :: Text -> [Text]
splitBlock = T.splitOn "\n\n"

-- split one block into fields, throw out empty field
-- (there is one at the end of the file)
splitField :: Text -> [Text]
splitField = filter (not . T.null) . T.split (\c -> c == '\n' || c == ' ')

-- split fields and store in map
parseBlock :: [Text] -> Block
parseBlock = M.fromList . map ((\[a, b] -> (a, b)) . T.splitOn ":")

-- set of required fields
requiredFields :: Set Text
requiredFields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- check that a block has all the required fields
validBlock :: Block -> Bool
validBlock b = requiredFields `S.isSubsetOf` M.keysSet b

-- filter out invalid blocks and count
part1 :: [Block] -> Int
part1 = length . filter validBlock

-- everything that is necessary to validate a block in part 2 ... Oo
validField :: Text -> Text -> Bool
validField "byr" s = T.length s == 4 && t2i s `elem` [1920 .. 2002]
validField "iyr" s = T.length s == 4 && t2i s `elem` [2010 .. 2020]
validField "eyr" s = T.length s == 4 && t2i s `elem` [2020 .. 2030]
validField "hgt" s
  | "cm" `T.isSuffixOf` s = hgt `elem` [150 .. 193]
  | "in" `T.isSuffixOf` s = hgt `elem` [59 .. 76]
  | otherwise = False
  where
    hgt = t2i (T.dropEnd 2 s)
validField "hcl" s = T.length s == 7 && s `T.index` 0 == '#' && isHex s
  where
    isHex = T.all (`elem` (['a' .. 'f'] ++ ['0' .. '9'])) . T.drop 1
validField "ecl" s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField "pid" s = T.length s == 9 && T.all (`elem` ['0' .. '9']) s
validField "cid" _ = True

-- text to int ... quick hack
t2i :: Text -> Int
t2i = read . T.unpack

-- check that every field is valid
validBlock2 :: Block -> Bool
validBlock2 = and . M.mapWithKey validField

-- filter out invalid blocks and count
-- ... thanks Sascha :-)
part2 :: [Block] -> Int
part2 = length . filter validBlock2 . filter validBlock

-- done ... was run in the interpreter

-- so we don't have to write it out everytime:
type Block = Map Text Text
