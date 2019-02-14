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

text :: Parser String
text = many $ noneOf ",\n"

protocolParse :: Parser [String]
protocolParse = string "://" >> sepBy text (char '/')

httpParser :: Parser String
httpParser = try (string "https") <|> string "http"

mainParser :: Parser Uri
mainParser = do
  http <- httpParser
  p <- protocolParse
  return $ Uri {prefix = T.pack http, segments = toText p}
  where
    toText = fmap T.pack

isId :: Text -> Bool
isId = liftA2 (&&) meetsLength allIdChars
  where
    allIdChars = T.all isIdCharacter
    meetsLength = (<) 16 . T.length

isIdCharacter :: Char -> Bool
isIdCharacter = liftA2 (||) isDigit isUpper

run :: String -> String
run =
  either (const "Unable to Parse URI") id .
  fmap (show . transform) . parse mainParser ""
  where
    transform = mapSegments $ removeIds . T.splitOn "/" . head
    mapSegments f (Uri p xs) = Uri p (f xs)
    removeIds = filter (not . isId)
