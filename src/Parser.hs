{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser where

import           Data.Functor               (($>))
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe, (<|>), some)
import           Text.Megaparsec.Char       (char, space, string', alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import qualified Types                      as T

type Parser = Parsec Void String

data Email = Email
  { eFirstPart :: String
  , eDomain :: String
  } deriving Show

email :: String
email = "alistair@email.com"

emailParser :: Parser Email
emailParser = do
  firstPart <- some alphaNumChar
  char '@'
  domain <- some alphaNumChar
  string' ".com"
  pure $ Email firstPart domain















move :: Parser T.Command
move = string' "MOVE" $> T.Move

move' :: Parser T.Command
move' = (\_ -> T.Move) <$> string' "MOVE"

move'' :: Parser T.Command
move'' = do
  _ <- string' "MOVE"
  pure T.Move

left :: Parser T.Command
left = string' "LEFT" $> T.TurnLeft

right :: Parser T.Command
right = string' "RIGHT" $> T.TurnRight

report :: Parser T.Command
report = string' "REPORT" $> T.Report

direction :: Parser T.Direction
direction = (string' "NORTH" $> T.North)
        <|> (string' "EAST"  $> T.East)
        <|> (string' "SOUTH" $> T.South)
        <|> (string' "WEST"  $> T.West)

place :: Parser T.Command
place = do
  string' "PLACE"
  space
  x :: Int <- L.decimal
  char ','
  y :: Int <- L.decimal
  char ','
  d <- direction
  pure $ T.Place (T.Coordinate x y) d

commandParser :: Parser T.Command
commandParser = move <|> left <|> right <|> place <|> report

parseCommand :: String -> Maybe T.Command
parseCommand = parseMaybe commandParser
