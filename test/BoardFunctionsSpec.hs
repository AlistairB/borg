module BoardFunctionsSpec where

import           HaskellWorks.Hspec.Hedgehog (require)
import           Hedgehog                    (forAll, property, (===))
import           Test.Hspec                  (Spec, describe, it, shouldBe)

import           BoardFunctions              (left, move, place, report, right,
                                              validate)
import           Generators                  (genBoardInvalidRobot,
                                              genBoardNoRobot,
                                              genBoardValidRobot, genRobot)
import qualified Types                       as T

spec :: Spec
spec = describe "BoardFunctions" $ do

  describe "place" $
    it "places a robot on the board" $ require $ property $ do
      r <- forAll genRobot
      place r (T.Board (T.Coordinate 5 5) Nothing) === T.Board (T.Coordinate 5 5) (Just r)

  describe "move" $ do
    it "moves north" $
      let starting = T.Robot (T.Coordinate 1 1) T.North
          expected = T.Robot (T.Coordinate 1 2) T.North
      in  move starting `shouldBe` expected

    it "moves east" $
      let starting = T.Robot (T.Coordinate 1 1) T.East
          expected = T.Robot (T.Coordinate 2 1) T.East
      in  move starting `shouldBe` expected

    it "moves south" $
      let starting = T.Robot (T.Coordinate 2 2) T.South
          expected = T.Robot (T.Coordinate 2 1) T.South
      in  move starting `shouldBe` expected

    it "moves west" $
      let starting = T.Robot (T.Coordinate 2 2) T.West
          expected = T.Robot (T.Coordinate 1 2) T.West
      in  move starting `shouldBe` expected

  describe "right" $ do
    it "turns right from north" $ right T.North `shouldBe` T.East
    it "turns right from east"  $ right T.East  `shouldBe` T.South
    it "turns right from south" $ right T.South `shouldBe` T.West
    it "turns right from west"  $ right T.West  `shouldBe` T.North

  describe "left" $ do
    it "turns left from north" $ left T.North `shouldBe` T.West
    it "turns left from east"  $ left T.West  `shouldBe` T.South
    it "turns left from south" $ left T.South `shouldBe` T.East
    it "turns left from west"  $ left T.East  `shouldBe` T.North

  describe "validateBoard" $ do
    it "validates a board with no robot" $ require $ property $ do
      b <- forAll genBoardNoRobot
      validate b === True

    it "validates a board with a valid robot" $ require $ property $ do
      b <- forAll genBoardValidRobot
      validate b === True

    it "validates a board with an invalid robot" $ require $ property $ do
      b <- forAll genBoardInvalidRobot
      validate b === False

  describe "report" $
    it "produces a report" $
      report (T.Robot (T.Coordinate 2 3) T.North) `shouldBe` "2,3,NORTH"
