{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Bench.Repa.Matrix
  ( MxM(..)
  , randomMxM
  , benchMxM
  , matrixToRepa
  ) where

import Control.DeepSeq
import Criterion
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Matrix
import Data.Array.Repa.Eval
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import System.Random

matrixToRepa ::
     (A.Prim e, A.Manifest r' A.Ix2 e, Target r e)
  => A.Matrix r' e
  -> Array r DIM2 e
matrixToRepa arr = computeS $ fromFunction (Z :. m :. n) $ \(Z :. i :. j) -> arr A.! (i A.:. j)
  where
    A.Sz2 m n = A.size arr

data MxM r e =
  MxM
    { aMxM :: !(Array r DIM2 e)
    , bMxM :: !(Array r DIM2 e)
    }

instance Source r e => NFData (MxM r e) where
  rnf (MxM a b) = a `deepSeqArray` b `deepSeqArray` ()

randomMxM ::
     forall e r. (A.Prim e, Random e, Target r e)
  => MxM r e
randomMxM =
  case A.randomMxM :: A.MxM A.P e of
    A.MxM a b -> MxM (matrixToRepa a) (matrixToRepa b)


showSizeMxM :: Source r e => MxM r e -> String
showSizeMxM MxM {..} = show m1 <> "x" <> show n1 <> " X " <> show m2 <> "x" <> show n2
  where
    Z :. m1 :. n1 = extent aMxM
    Z :. m2 :. n2 = extent bMxM


benchMxM :: MxM U Double -> Benchmark
benchMxM mxm@MxM {..} =
  bgroup (A.showsType @(MxM U Double) (" - (" <> showSizeMxM mxm <> ")"))
  [ bench "Seq" $ whnf (mmultS aMxM) bMxM
  , bench "Par" $ whnfIO (mmultP aMxM bMxM)
  ]
{-# INLINEABLE benchMxM #-}
