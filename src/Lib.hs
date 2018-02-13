module Lib
  ( runRobot
  ) where

import           Data.Maybe           (catMaybes)
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

import           BoardProcessor       (getAction)
import           Parser               (parseCommand)
import           Types                (Board (Board), Coordinate (Coordinate))

runRobot :: [String] -> [String]
runRobot input =
  let commands = catMaybes $ parseCommand <$> input
      action   = foldMap getAction commands
      final    = runWriter $ evalState startingBoard action
  in  snd $ run final

startingBoard :: Board
startingBoard = Board (Coordinate 5 5) Nothing
