{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Parse where

import Data.Char (ord)
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.Message
import Text.Parsec
import Text.Parsec.Text

parseOx :: Text -> GenParser st Text
parseOx key = do
  char '['
  spaces
  string $ T.unpack key
  spaces
  char '|'
  spaces
  v <- many1 $ noneOf "[|]"
  spaces
  string "|]"
  return $ T.pack v

parseImg :: GenParser st Message
parseImg = do
  v <- parseOx "img"
  return $ Image "" v Nothing

parseOxInt :: Text -> GenParser st Int
parseOxInt key = do
  char '['
  spaces
  string $ T.unpack key
  spaces
  char '|'
  spaces
  digits <- many1 digit
  spaces
  string "|]"
  return $ foldl' (\n x -> 10 * n + ord x - ord '0') 0 digits

parseAt :: GenParser st Message
parseAt = do
  v <- parseOxInt "at"
  return $ if v == 0 then AtAll else At v ""

escapedChar :: GenParser st Char
escapedChar = do
  try
    ( do
        char '\\'
        char '['
        return '['
    )
    <|> noneOf "["

parseText :: GenParser st Message
parseText = do
  s <- many1 escapedChar
  return . Plain . T.pack $ s

comb :: GenParser st Message
comb = try parseImg <|> try parseAt <|> parseText

parser :: GenParser st MessageChain
parser = do
  ms <- many comb
  return $ V.fromList ms

parseMessage :: Text -> MessageChain
parseMessage = fromRight [] . parse parser ""
