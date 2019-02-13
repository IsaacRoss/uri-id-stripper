{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative hiding (many, (<|>))
import           Data.Char
import           Data.List
import           Data.Text           as T hiding (lines, map, unlines)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

data Uri = Uri
  { prefix   :: Text
  , segments :: [Text]
  }

mapSegments :: ([Text] -> [Text]) -> Uri -> Uri
mapSegments f (Uri p xs) = Uri p (f xs)

instance Show Uri where
  show (Uri p xs) = prefix ++ "://" ++ segments
    where
      merge = mconcat . Data.List.intersperse "/"
      prefix = T.unpack p
      segments = T.unpack (merge xs)

run :: String -> String
run s =
  case ret of
    Left e -> "Unable to Parse URI"
    Right x -> show $ mapSegments (removeIds . T.splitOn "/" . Data.List.head) x
  where
    ret = parse mainParser "" s

go :: IO ()
go = interact $ unlines . map run . lines

toText :: [String] -> [Text]
toText = fmap T.pack

text :: Parser String
text = many (noneOf ",\n")

protocolParse :: Parser [String]
protocolParse = string "://" >> sepBy text (char '/')

httpParser :: Parser String
httpParser = try (string "https") <|> string "http"

mainParser :: Parser Uri
mainParser = do
  http <- httpParser
  p <- protocolParse
  return $ Uri {prefix = T.pack http, segments = toText p}

meetsLength :: Text -> Bool
meetsLength = (<) 16 . T.length

isId :: Text -> Bool
isId = liftA2 (&&) meetsLength (T.all isIdCharacter)

isIdCharacter :: Char -> Bool
isIdCharacter = liftA2 (||) isDigit isUpper

removeIds :: [Text] -> [Text]
removeIds = Data.List.filter (not . isId)
-- -- mainParser = httpParser >> protocolParse
-- isId :: String -> Bool
-- isId = liftA2 (&&) longEnough (all isIdCharacter)
--
-- isIdText = isId . Data.Text.unpack
--
-- longEnough :: String -> Bool
-- longEnough = (<) 16 . length
--
-- isIdCharacter :: Char -> Bool
-- isIdCharacter = liftA2 (||) isDigit isUpper
--
-- removeIds :: [String] -> [String]
-- removeIds = filter $ not . isId
--
-- process :: String -> String
-- process =
--   mconcat .
--   intersperse "/" .
--   map Data.Text.unpack .
--   filter (not . isIdText) . Data.Text.splitOn "/" . Data.Text.pack
--
