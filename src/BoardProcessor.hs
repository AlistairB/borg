{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BoardProcessor (getAction) where

import qualified Types                 as T

import           BoardFunctions        (left, move, place, right, validate)
import           Control.Lens          (over, use, (%=), _Just)
import           Control.Monad.State   (MonadState, StateT (StateT), get, put)
import           Control.Monad.Writer  (MonadWriter, Writer, WriterT (WriterT),
                                        tell)
import           Data.Functor.Identity (Identity (Identity))

getAction :: T.Command -> GameApp
getAction (T.Place coords facing) = placeAction coords facing
getAction T.Left                  = leftAction
getAction T.Right                 = rightAction
getAction T.Move                  = moveAction
getAction T.Report                = reportAction

placedRobot = T.boardRobot . _Just
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp = StateT T.Board (Writer [String]) ()

type GameAction = MonadState T.Board

instance Monoid GameApp where
  mempty = StateT $ \s -> WriterT $ Identity (((), s), [])
  mappend = (>>)

placeAction :: GameAction m => T.Coordinate -> T.Direction -> m ()
placeAction coords facing = validatedAction $ place (T.Robot coords facing)

moveAction :: GameAction m => m ()
moveAction = validatedAction (over placedRobot move)

leftAction :: GameAction m => m ()
leftAction = placedRobotFacing %= left

rightAction :: GameAction m => m ()
rightAction = placedRobotFacing %= right

validatedAction :: GameAction m => (T.Board -> T.Board) -> m ()
validatedAction updateBoard = do
  current <- get
  let updated = updateBoard current
      final   = if validate updated then updated else current
  put final

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  case r of
    (Just r) -> tell [show r]
    _        -> return ()
