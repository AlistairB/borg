{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BoardProcessor
  ( getAction
  , GameApp
  ) where

import           Control.Lens          (over, use, (%=), (%~), _Just)
import           Control.Monad.State   (MonadState, StateT, modify)
import           Control.Monad.Writer  (MonadWriter, Writer, tell)
import           Data.Functor.Identity (Identity)
import           Data.Semigroup        (Semigroup, (<>))
import Control.Monad.Freer
import qualified Control.Monad.Freer.State as EffS
import qualified Control.Monad.Freer.Writer as EffW

import           BoardFunctions        (left, move, place, report, right,
                                        validate)
import qualified Types                 as T

getAction :: T.Command -> GameApp T.Board
getAction (T.Place coords facing) = placeAction coords facing
getAction T.TurnLeft              = leftAction
getAction T.TurnRight             = rightAction
getAction T.Move                  = moveAction
getAction T.Report                = reportAction

placedRobot :: (T.Robot -> Identity T.Robot) -> T.Board -> Identity T.Board
placedRobot = T.boardRobot . _Just

placedRobotFacing :: (T.Direction -> Identity T.Direction) -> T.Board -> Identity T.Board
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp s = StateT s (Writer [String]) ()

type GameApp2 s = Eff '[EffS.State s, EffW.Writer [String]] ()

type GameAction = MonadState T.Board

instance Semigroup (GameApp s) where
  (<>) = (>>)

instance Monoid (GameApp s) where
  mempty = pure ()
  mappend = (<>)

placeAction :: GameAction m => T.Coordinate -> T.Direction -> m ()
placeAction coords facing = validatedAction $ place (T.Robot coords facing)

leftAction2 :: Member (EffS.State T.Board) r => Eff r ()
leftAction2 = EffS.modify (placedRobotFacing %~ left)

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
