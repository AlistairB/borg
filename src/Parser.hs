{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser
  ( parseCommand
  ) where

import           Data.Functor               (($>))
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseMaybe, (<|>))
import           Text.Megaparsec.Char       (char, space, string')
import qualified Text.Megaparsec.Char.Lexer as L (decimal)

import qualified Types                      as T

type Parser = Parsec Void String

move :: Parser T.Command
move = string' "MOVE" $> T.Move

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
  x <- L.decimal
  char ','
  y <- L.decimal
  char ','
  d <- direction
  pure $ T.Place (T.Coordinate x y) d

commandParser :: Parser T.Command
commandParser = move <|> left <|> right <|> place <|> report

parseCommand :: String -> Maybe T.Command
parseCommand = parseMaybe commandParser
