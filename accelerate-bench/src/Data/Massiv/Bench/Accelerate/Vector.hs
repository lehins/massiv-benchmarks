{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Bench.Accelerate.Vector
  ( randomV1
  , benchV1
  , VxV(..)
  , randomVxV
  , benchVxV
  ) where

import Criterion
import Data.Array.Accelerate
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Vector as A
import Data.Massiv.Bench.Accelerate.Matrix
import System.Random
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as LA

randomV1 ::
     forall e. (A.Prim e, Random e, Elt e)
  => Array DIM1 e
randomV1 = vectorToAccelerate (A.randomV1 :: A.Vector A.P e)


benchV1 :: Array DIM1 Double -> Benchmark
benchV1 v =
  bgroup
    "normL2"
    [ bench ("Array DIM1 Double - (" <> show (arrayShape v) <> ")") $
      whnf (\v' -> sqrt $ indexArray (CPU.runN (v' LA.<.> v')) Z) (use v)
    ]
{-# INLINEABLE benchV1 #-}




data VxV e =
  VxV
    { aVxV :: !(Array DIM1 e)
    , bVxV :: !(Array DIM1 e)
    }

randomVxV ::
     forall e. (A.Prim e, Random e, Elt e)
  => VxV e
randomVxV =
  case A.randomVxV :: A.VxV A.P e of
    A.VxV a b -> VxV (vectorToAccelerate a) (vectorToAccelerate b)


showSizeVxV :: VxV e -> String
showSizeVxV VxV {..} = show n1 <> " X " <> show n2
  where
    Z :. n1 = arrayShape aVxV
    Z :. n2 = arrayShape bVxV

benchVxV :: VxV Double -> Benchmark
benchVxV mxm@VxV {..} =
  bgroup "dotProduct"
  [ bgroup ("VxV Double - (" <> showSizeVxV mxm <> ")")
    [ bench "Par" $ whnf (CPU.runN . (use aVxV LA.<.>) . use) bVxV
    ]
  ]
{-# INLINEABLE benchVxV #-}
