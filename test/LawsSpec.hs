{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LawsSpec where

import           Control.Monad.State      (evalStateT)
import           Control.Monad.Writer     (execWriter, tell)
import           Test.Hspec               (Spec)
import           Test.Hspec.Checkers      (testBatch)
import           Test.QuickCheck          (Arbitrary, arbitrary)
import           Test.QuickCheck.Checkers (EqProp, eq, (=-=))
import           Test.QuickCheck.Classes  (monoid)

import           BoardProcessor           (GameApp)

instance Eq (GameApp String) where
  (==) a b =
    let aResult = execWriter $ evalStateT a ""
        bResult = execWriter $ evalStateT b ""
    in  aResult == bResult

instance Show (GameApp String) where
  show a = mconcat $ execWriter $ evalStateT a ""

instance Arbitrary (GameApp String) where
  arbitrary = tell <$> arbitrary

instance EqProp (GameApp String) where
  (=-=) = eq

spec :: Spec
spec = testBatch (monoid (undefined :: GameApp String))
