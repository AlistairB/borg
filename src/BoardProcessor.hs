{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BoardProcessor
  ( getAction
  , GameApp
  ) where

import           Control.Lens          (over, use, (%=), _Just)
import           Control.Monad.State   (MonadState, StateT, modify)
import           Control.Monad.Writer  (MonadWriter, Writer, tell)

import           BoardFunctions        (left, move, place, report, right,
                                        validate)
import qualified Types                 as T

getAction :: T.Command -> GameApp T.Board
getAction (T.Place coords facing) = placeAction coords facing
getAction T.TurnLeft              = leftAction
getAction T.TurnRight             = rightAction
getAction T.Move                  = moveAction
getAction T.Report                = reportAction

placedRobot :: Applicative f => (T.Robot -> f T.Robot) -> T.Board -> f T.Board
placedRobot = T.boardRobot . _Just

placedRobotFacing :: Applicative f => (T.Direction -> f T.Direction) -> T.Board -> f T.Board
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp s = StateT s (Writer [String]) ()

type GameAction = MonadState T.Board

instance Semigroup (GameApp s) where
  (<>) = (>>)

instance Monoid (GameApp s) where
  mempty = pure ()

placeAction :: GameAction m => T.Coordinate -> T.Direction -> m ()
placeAction coords facing = validatedAction $ place (T.Robot coords facing)

moveAction :: GameAction m => m ()
moveAction = validatedAction $ over placedRobot move

leftAction :: GameAction m => m ()
leftAction = placedRobotFacing %= left

rightAction :: GameAction m => m ()
rightAction = placedRobotFacing %= right

validatedAction :: GameAction m => (T.Board -> T.Board) -> m ()
validatedAction updateBoard = modify maybeUpdate
  where
    maybeUpdate b = let newB = updateBoard b
                    in  if validate newB
                          then newB
                          else b

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  case r of
    (Just r') -> tell [report r']
    _         -> pure ()
