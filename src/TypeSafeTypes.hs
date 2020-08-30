{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module TypeSafeTypes where

data Nat = Z | S Nat

data SmallerThan (limit :: Nat) where
  -- Z is smaller than the successor of any number.
  SmallerThanZ :: SmallerThan ('S any)

  -- The successor of a number smaller than X is a number smaller than the
  -- successor of X.
  SmallerThanS :: SmallerThan any -> SmallerThan ('S any)

blahblah :: SmallerThan ('S ('S 'Z))
blahblah = SmallerThanZ

data PositiveNat (size :: Nat) where
  PNatOne :: PositiveNat ('S 'Z)
  PNatS :: PositiveNat ('S n) -> PositiveNat ('S ('S n))

data Board (boardSize :: Nat) = Board
  { _boardSize     :: PositiveNat boardSize
  , _boardRobot    :: Robot boardSize
  }

data Robot (boardSize :: Nat) = Robot
  { _robotPosition :: SmallerThanCoord boardSize
  }

data SmallerThanValue (smallerThan :: Nat) where
  STV :: SmallerThan smallerThan -> SNat smallerThan -> SmallerThanValue smallerThan

data SmallerThanCoord (smallerThan :: Nat) = SmallerThanCoord
  { _stCoordinateX :: SmallerThan smallerThan
  , _stCoordinateY :: SmallerThan smallerThan
  }

board :: Board ('S ('S 'Z))
board =
  let robotCoord = SmallerThanCoord SmallerThanZ SmallerThanZ
      boardSize  = PNatS PNatOne
  in  Board boardSize $ Robot $ robotCoord

moveX :: Board n -> Board n
moveX (Board size (Robot (SmallerThanCoord x y))) =


increment :: SmallerThan n -> SmallerThan ('S n)
increment = SmallerThanS

data SNat (value :: Nat) where
  SZ ::           SNat  'Z
  SS :: SNat n -> SNat ('S n)

type family Dec (a :: Nat) :: Nat where
  Dec 'Z = 'Z
  Dec ('S n) = n

type family Incr (a :: Nat) :: Nat where
  Incr n = 'S n

-- decrement :: SmallerThan n -> SmallerThan (Dec n)
-- decrement SmallerThanZ = SmallerThanZ
-- decrement (SmallerThanS n) = n

instance Show (SmallerThan n) where
  show SmallerThanZ = "SmallerThanZ"
  show (SmallerThanS r) = "SmallerThanS (" ++ show r ++ ")"
