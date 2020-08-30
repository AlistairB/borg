{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module TypeSafeTypes where

data Nat = S Nat | Z

data TBoard (size :: Nat) = TBoard TRobot

data TRobot = TRobot Nat Nat

data SmallerThan (n :: Nat) where
  STZero :: SmallerThan ('S n)
  STSucc :: SmallerThan m -> SmallerThan ('S m)

pass :: SmallerThan ('S ('S ('S 'Z)))
pass = STSucc (STSucc (STZero))
