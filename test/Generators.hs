module Generators where

import           Hedgehog       (Gen)
import           Hedgehog.Gen   (enum, int)
import qualified Hedgehog.Range as HR

import qualified Types          as T

genDirection :: Gen T.Direction
genDirection = enum T.North T.West

genRobot :: Gen T.Robot
genRobot = do
  c <- genCoordinate
  f <- genDirection
  pure $ T.Robot c f

genCoordinate :: Gen T.Coordinate
genCoordinate = do
  x <- int $ HR.constant 1 5
  y <- int $ HR.constant 1 5
  pure $ T.Coordinate x y

genRobotWithin :: Int -> Int -> Gen T.Robot
genRobotWithin x y = do
  x' <- int $ HR.constant 1 x
  y' <- int $ HR.constant 1 y
  f  <- genDirection
  pure $ T.Robot (T.Coordinate x' y') f

genRobotGreaterThan :: Int -> Int -> Gen T.Robot
genRobotGreaterThan x y = do
  x' <- int $ HR.constant (x + 1) maxBound
  y' <- int $ HR.constant (y + 1) maxBound
  f  <- genDirection
  pure $ T.Robot (T.Coordinate x' y') f

genBoardValidRobot :: Gen T.Board
genBoardValidRobot = do
  c <- genCoordinate
  r <- genRobotWithin (T._coordinateX c) (T._coordinateY c)
  pure $ T.Board c (Just r)

genBoardInvalidRobot :: Gen T.Board
genBoardInvalidRobot = do
  c <- genCoordinate
  r <- genRobotGreaterThan (T._coordinateX c) (T._coordinateY c)
  pure $ T.Board c (Just r)

genBoardNoRobot :: Gen T.Board
genBoardNoRobot = do
  c <- genCoordinate
  pure $ T.Board c Nothing

genPlaceCommandWithin :: Int -> Int -> Gen T.Command
genPlaceCommandWithin x y = do
  x' <- int $ HR.constant 1 x
  y' <- int $ HR.constant 1 y
  d  <- genDirection
  pure $ T.Place (T.Coordinate x' y') d

