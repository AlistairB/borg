module Main where

import           Data.List          (intercalate)
import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs)

import           Lib                (runRobot)

main :: IO ()
main = do
  maybeFilePath <- listToMaybe <$> getArgs
  case maybeFilePath of
    Just filePath -> do
      fileContents <- readFile filePath
      let result = intercalate "\n" $ runInput fileContents
      putStrLn result
    _ -> do
      putStrLn instructions
      interact (unlines . runInput)

runInput :: String -> [String]
runInput = runRobot . lines

instructions :: String
instructions = "---------------------------------------------\n\
               \We are Borg. You will be assimilated.\n\
               \Give us instructions to move around the board.\n\
               \PLACE X,Y,FACING - MOVE - LEFT - RIGHT"
