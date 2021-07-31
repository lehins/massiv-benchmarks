{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Bench.Accelerate.Matrix
  ( matrixToAccelerate
  , vectorToAccelerate
  , MxM(..)
  , randomMxM
  , benchMxM
  , MxV(..)
  , randomMxV
  , benchMxV
  , VxM(..)
  , randomVxM
  , benchVxM
  ) where

import Criterion
import Data.Array.Accelerate
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import System.Random
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as LA


vectorToAccelerate ::
     (A.Prim e, A.Manifest r' e, Elt e)
  => A.Vector r' e
  -> Array DIM1 e
vectorToAccelerate arr = fromFunction (Z :. n) $ \(Z :. i) -> arr A.! i
  where
    A.Sz n = A.size arr

matrixToAccelerate ::
     (A.Prim e, A.Manifest r' e, Elt e)
  => A.Matrix r' e
  -> Array DIM2 e
matrixToAccelerate arr = fromFunction (Z :. m :. n) $ \(Z :. i :. j) -> arr A.! (i A.:. j)
  where
    A.Sz2 m n = A.size arr

data MxM e =
  MxM
    { aMxM :: !(Array DIM2 e)
    , bMxM :: !(Array DIM2 e)
    }

randomMxM ::
     forall e. (A.Prim e, Random e, Elt e)
  => MxM e
randomMxM =
  case A.randomMxM :: A.MxM A.P e of
    A.MxM a b -> MxM (matrixToAccelerate a) (matrixToAccelerate b)


showSizeMxM :: MxM e -> String
showSizeMxM MxM {..} = show m1 <> "x" <> show n1 <> " X " <> show m2 <> "x" <> show n2
  where
    Z :. m1 :. n1 = arrayShape aMxM
    Z :. m2 :. n2 = arrayShape bMxM

benchMxM :: MxM Double -> Benchmark
benchMxM mxm@MxM {..} =
  bgroup (A.showsType @(MxM Double) (" - (" <> showSizeMxM mxm <> ")"))
  [ bench "Par" $ whnf (CPU.runN . (use aMxM LA.<>) . use) bMxM
  ]
{-# INLINEABLE benchMxM #-}



data MxV e =
  MxV
    { aMxV :: !(Array DIM2 e)
    , bMxV :: !(Array DIM1 e)
    }

randomMxV ::
     forall e. (A.Prim e, Random e, Elt e)
  => MxV e
randomMxV =
  case A.randomMxV :: A.MxV A.P e of
    A.MxV a b -> MxV (matrixToAccelerate a) (vectorToAccelerate b)


showSizeMxV :: MxV e -> String
showSizeMxV MxV {..} = show m1 <> "x" <> show n1 <> " X " <> show n <> "x1"
  where
    Z :. m1 :. n1 = arrayShape aMxV
    Z :. n = arrayShape bMxV

benchMxV :: MxV Double -> Benchmark
benchMxV mxm@MxV {..} =
  bgroup (A.showsType @(MxV Double) (" - (" <> showSizeMxV mxm <> ")"))
  [ bench "Par" $ whnf (CPU.runN . (use aMxV LA.#>) . use) bMxV
  ]
{-# INLINEABLE benchMxV #-}



data VxM e =
  VxM
    { aVxM :: !(Array DIM1 e)
    , bVxM :: !(Array DIM2 e)
    }

randomVxM ::
     forall e. (A.Prim e, Random e, Elt e)
  => VxM e
randomVxM =
  case A.randomVxM :: A.VxM A.P e of
    A.VxM a b -> VxM (vectorToAccelerate a) (matrixToAccelerate b)


showSizeVxM :: VxM e -> String
showSizeVxM VxM {..} = "1x" <> show n <> " X " <> show m2 <> "x" <> show n2
  where
    Z :. n = arrayShape aVxM
    Z :. m2 :. n2 = arrayShape bVxM

benchVxM :: VxM Double -> Benchmark
benchVxM mxm@VxM {..} =
  bgroup (A.showsType @(VxM Double) (" - (" <> showSizeVxM mxm <> ")"))
  [ bench "Par" $ whnf (CPU.runN . (use aVxM LA.<#) . use) bVxM
  ]
{-# INLINEABLE benchVxM #-}
