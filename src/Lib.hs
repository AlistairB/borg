module Lib (runRobot) where

import Parser
import Types
import BoardProcessor

import Data.Maybe (catMaybes)
import Data.Foldable (foldl')

-- hmm feel like this could use the state monad?
runRobot :: [String] -> [String]
runRobot input =
  let commands = catMaybes $ fmap parseCommand input
   in fst $ foldl' foldCommands ([], startingBoard) commands

foldCommands :: ([String], Board) -> Command -> ([String], Board)
foldCommands (messages, board) c = (appendMessage messages newMessage, newBoard)
 where
   (newMessage, newBoard) = execute c board
   -- this feels ugly.. nicer way?
   appendMessage m Nothing = m
   appendMessage m (Just newm) = m ++ [newm]

startingBoard :: Board
startingBoard = Board 5 5 Nothing
