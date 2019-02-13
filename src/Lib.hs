{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import           Control.Applicative hiding (many, (<|>))
import           Data.Char
import qualified Data.List           as L
import qualified Data.Text           as T
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

type Text = T.Text

data Uri = Uri
  { prefix   :: Text
  , segments :: [Text]
  }

instance Show Uri where
  show (Uri p xs) = prefix ++ "://" ++ segments
    where
      merge = mconcat . L.intersperse "/"
      prefix = T.unpack p
      segments = T.unpack (merge xs)

removeIds :: [Text] -> [Text]
removeIds = filter (not . isId)

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

isId :: Text -> Bool
isId = liftA2 (&&) meetsLength (T.all isIdCharacter)

isIdCharacter :: Char -> Bool
isIdCharacter = liftA2 (||) isDigit isUpper

meetsLength :: Text -> Bool
meetsLength = (<) 16 . T.length

toText :: [String] -> [Text]
toText = fmap T.pack

mapSegments :: ([Text] -> [Text]) -> Uri -> Uri
mapSegments f (Uri p xs) = Uri p (f xs)

run :: String -> String
run s =
  case ret of
    Left e  -> "Unable to Parse URI"
    Right x -> show $ mapSegments (removeIds . T.splitOn "/" . head) x
  where
    ret = parse mainParser "" s
