{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module TypeSafeTypes where

data Nat = S Nat | Z

data TBoard (size :: Nat) = TBoard TRobot

data TRobot = TRobot Nat Nat
