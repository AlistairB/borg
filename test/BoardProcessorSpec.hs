module BoardProcessorSpec where

import           Control.Lens         (set, _Just)
import           Control.Monad.State  (execStateT)
import           Control.Monad.Writer (execWriter, runWriter)
import           Hedgehog.Gen         (sample)
import           Test.Hspec           (Spec, describe, it, shouldBe)

import           BoardProcessor       (getAction)
import           Generators           (genBoardNoRobot, genBoardValidRobot,
                                       genPlaceCommandWithin)
import qualified Types                as T

spec :: Spec
spec = describe "getAction" $ do

  describe "placeAction" $ do
    it "performs a place" $ do
      board@(T.Board bc@(T.Coordinate x y) _) <- sample genBoardNoRobot
      command@(T.Place cc cdir)               <- sample $ genPlaceCommandWithin x y
      let action      = getAction command
          (result, _) = runWriter $ execStateT action board
          expected    = T.Board bc (Just $ T.Robot cc cdir)
      result `shouldBe` expected

    it "rejects invalid places" $
      let action      = getAction $ T.Place (T.Coordinate 6 6) T.North
          board       = T.Board (T.Coordinate 5 5) Nothing
          (result, _) = runWriter $ execStateT action board
      in  result `shouldBe` board

  describe "leftAction" $
    it "performs a left" $ do
      board@(T.Board bc (Just (T.Robot rc _))) <- sample genBoardValidRobot
      let testBoard   = set (T.boardRobot . _Just . T.robotFacing) T.North board
          action      = getAction T.TurnLeft
          (result, _) = runWriter $ execStateT action testBoard
          expected    = T.Board bc (Just $ T.Robot rc T.West)
      result `shouldBe` expected

  describe "rightAction" $
    it "performs a right" $ do
      board@(T.Board bc (Just (T.Robot rc _))) <- sample genBoardValidRobot
      let testBoard   = set (T.boardRobot . _Just . T.robotFacing) T.North board
          action      = getAction T.TurnRight
          (result, _) = runWriter $ execStateT action testBoard
          expected    = T.Board bc (Just $ T.Robot rc T.East)
      result `shouldBe` expected

  describe "moveAction" $ do
    it "performs a move" $
      let action      = getAction T.Move
          board       = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 2) T.North)
          (result, _) = runWriter $ execStateT action board
          expected    = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 3) T.North)
      in  result `shouldBe` expected

    it "rejects invalid moves" $
      let action      = getAction T.Move
          board       = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 5 5) T.North)
          (result, _) = runWriter $ execStateT action board
      in  result `shouldBe` board

  describe "reportAction" $
    it "produces a report" $
      let action   = getAction T.Report
          board    = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 2) T.North)
          result   = execWriter $ execStateT action board
          expected = ["2,2,NORTH"]
      in  result `shouldBe` expected
