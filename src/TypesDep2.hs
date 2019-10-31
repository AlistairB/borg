{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module TypesDep2 where

import Refined

type BoardPosition =
  Refined (And (Not (LessThan 0)) (Not (GreaterThan 2))) Int


-- move :: BoardPosition
-- move = $$(refineTH @BoardPosition 1)

odd :: Refined Odd Int
odd  = $$(refineTH @Odd        @Int 3)
