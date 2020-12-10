{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative (liftA2)
import Data.Either (rights)
import Data.List (find)
import Data.List.Split (splitOn)
import Text.Parsec
import Text.Parsec.String (Parser)

data Field = Year Int | Height Int String | Color String | EyeColor String | Pid Int | Cid String deriving (Show, Read)

whitespace = many $ oneOf "\n\t "

notWhitespace = many1 $ noneOf "\n\t "

passportPart1 = sepBy fieldPart1 whitespace

requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

fieldPart1 = choice $ try . (`mkFieldParser` notWhitespace) <$> requiredFields

validatePassport :: [String] -> [(String, String)] -> Bool
validatePassport [] _ = True
validatePassport (key : keys) passport = hasKey && validatePassport keys passport
  where
    hasKey = key `elem` (fst <$> passport)

boolToInt False = 0
boolToInt True = 1

-- PART 2

isBetween :: Int -> Int -> Int -> Bool
isBetween a b n = n >= a && n <= b

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse (liftA2 (,) p leftOver) "(source)"
  where
    leftOver = manyTill anyToken eof

isValidHeight unit height =
  if unit == "cm"
    then isBetween 150 193 height
    else isBetween 59 76 height

parseHeight :: Parser Field
parseHeight = do
  h <- many1 digit
  unit <- try (string "cm") <|> try (string "in")
  let height = read h :: Int
  if' (isValidHeight unit height) (return (Height height unit)) (fail "Needs valid height")

hexColor :: Parser Field
hexColor = do
  prefix <- char '#'
  digits <- count 6 hexDigit
  return (Color (prefix : digits))

parseEcl :: Parser Field
parseEcl = do
  val <-
    choice
      [ try $ string "amb",
        try $ string "blu",
        try $ string "brn",
        try $ string "gry",
        try $ string "grn",
        try $ string "hzl",
        try $ string "oth"
      ]
  return (EyeColor val)

parsePid :: Parser Field
parsePid = do
  pid <- count 9 digit
  return $ Pid $ read pid

parseCid :: Parser Field
parseCid = Cid <$> notWhitespace

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

mkNumberParser :: Int -> Int -> Parser Field
mkNumberParser min max = do
  v <- many1 digit
  let n = read v :: Int
  if' (isBetween min max n) (return (Year n)) (fail "Number should be between inputs")

mkFieldParser field p = do
  string field
  char ':'
  p

byr = mkFieldParser "byr" $ mkNumberParser 1920 2020

iyr = mkFieldParser "iyr" $ mkNumberParser 2010 2020

eyr = mkFieldParser "eyr" $ mkNumberParser 2020 2030

hgt = mkFieldParser "hgt" parseHeight

hcl = mkFieldParser "hcl" hexColor

ecl = mkFieldParser "ecl" parseEcl

pid = mkFieldParser "pid" parsePid

cid = mkFieldParser "cid" parseCid

passportFields =
  [ byr,
    iyr,
    eyr,
    hgt,
    hcl,
    ecl,
    pid
  ]

passportField :: Parser Field
passportField =
  choice
    [ try byr,
      try iyr,
      try eyr,
      try hgt,
      try hcl,
      try ecl,
      try pid
    ]

withWhitespace :: Parser a -> Parser a
withWhitespace p = do
  many anyChar
  v <- p
  whitespace
  return v

tryParse :: String -> Parser a -> Either ParseError a
tryParse s p = parse p "(source)" s

passportPart2P :: Parser [Field]
passportPart2P = sepBy passportField whitespace

passportPart2 :: String -> Either ParseError [Field]
passportPart2 passport =
  result >>= checkResult
  where
    checkResult fields = length fields >= 7
    result = tryParse passport passportPart2P

main = do
  contents <- getContents
  let passports = splitOn "\n\n" contents
  let part1passports = parse passportPart1 "(source)" <$> passports

  putStrLn "Part 1:"
  print $ length $ rights part1passports

  let part2Passports = passportPart2 <$> passports
  putStrLn "Part 2:"
  print part2Passports
  print $ length $ part2Passports
