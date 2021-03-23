{-# LANGUAGE OverloadedLists #-}

module Model.Parse where

import           Data.Char        (ord)
import           Data.Either      (fromRight)
import           Data.List        (foldl')
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Vector      as V
import           Model.Message
import           Text.Parsec
import           Text.Parsec.Text

data SendMessage
  = Message Message
  | SendChar Char

parseOx :: Text -> GenParser st Text
parseOx key = do
  char '['
  spaces
  string $ T.unpack key
  spaces
  char '|'
  spaces
  v <- many1 $ noneOf "[|]"
  string "|]"
  return $ T.pack v

parseImg :: GenParser st SendMessage
parseImg = do
  v <- parseOx "img"
  return $ Message (Image "" v Nothing)

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
  return $ foldl'
    (\n x -> 10 * n + ord x - ord '0')
    0
    digits

parseAt :: GenParser st SendMessage
parseAt = do
  v <- parseOxInt "at"
  return $ Message (if v == 0 then AtAll else At v "")

escapedChar :: GenParser st Char
escapedChar = try esc <|> noneOf "["
  where
    esc = do
      char '\\'
      char '['
      return '['

parseText :: GenParser st Message
parseText = do
  s <- many1 escapedChar
  return . Plain . T.pack $ s

parseAny :: Stream s m Char => ParsecT s u m SendMessage
parseAny = SendChar <$> noneOf ""

comb :: GenParser st SendMessage
comb = try parseImg <|> try parseAt <|> parseAny

parser :: GenParser st MessageChain
parser = do
  sms <- many comb
  eof
  let f (Message m)  xs = m:xs
      f (SendChar c) xs
          | (Plain t : xs') <- xs = Plain (tc <> t) : xs'
          | otherwise             = Plain tc : xs
          where
            tc = T.pack [c]
      ms = foldr f [] sms
  return $ V.fromList ms

parseMessage :: Text -> MessageChain
parseMessage = fromRight [] . parse parser ""
