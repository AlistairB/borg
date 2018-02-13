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
import           Data.Functor.Identity (Identity)
import           Data.Semigroup        (Semigroup, (<>))
import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

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

type GameApp s = Eff '[State s, Writer [String]] ()

type GameAction = Member (State T.Board)

type MessageWriter = Member (Writer [String])

instance Semigroup (GameApp s) where
  (<>) = (>>)

instance Monoid (GameApp s) where
  mempty = pure ()
  mappend = (<>)

placeAction :: GameAction r => T.Coordinate -> T.Direction -> Eff r ()
placeAction coords facing = validatedAction $ place (T.Robot coords facing)

moveAction :: GameAction r => Eff r ()
moveAction = validatedAction (placedRobot %~ move)

leftAction :: GameAction r => Eff r ()
leftAction = modify (placedRobotFacing %~ left)

rightAction :: GameAction r => Eff r ()
rightAction = modify (placedRobotFacing %~ right)

validatedAction :: GameAction r => (T.Board -> T.Board) -> Eff r ()
validatedAction updateBoard = modify maybeUpdate
  where
    maybeUpdate b = let newB = updateBoard b
                    in  if validate newB
                          then newB
                          else b

reportAction :: (GameAction r, MessageWriter r) => Eff r ()
reportAction = do
  r <- T._boardRobot <$> get
  case r of
    (Just r') -> tell [report r']
    _         -> pure ()
