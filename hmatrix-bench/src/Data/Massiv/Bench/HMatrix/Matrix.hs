{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Bench.HMatrix.Matrix
  ( MxM(..)
  , randomMxM
  , benchMxM
  , MxV(..)
  , randomMxV
  , benchMxV
  , VxM(..)
  , randomVxM
  , benchVxM
  , matrixToHMatrix
  , vectorToHMatrix
  ) where

import Control.DeepSeq
import Criterion
import Numeric.LinearAlgebra as LA
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import System.Random
import Data.Typeable

matrixToHMatrix ::
     (A.Prim e, A.Manifest r A.Ix2 e, Element e) => A.Matrix r e -> Matrix e
matrixToHMatrix = fromLists . A.toLists

vectorToHMatrix ::
     (A.Prim e, A.Manifest r A.Ix1 e, Element e) => A.Vector r e -> Vector e
vectorToHMatrix = fromList . A.toList

data MxM e =
  MxM
    { aMxM :: !(Matrix e)
    , bMxM :: !(Matrix e)
    }

instance (NFData e, A.Storable e) => NFData (MxM e) where
  rnf (MxM a b) = a `deepseq` b `deepseq` ()

randomMxM ::
     forall e. (A.Prim e, Random e, Element e)
  => MxM e
randomMxM =
  case A.randomMxM :: A.MxM A.P e of
    A.MxM a b -> MxM (matrixToHMatrix a) (matrixToHMatrix b)


showSizeMxM :: (Container Vector e, Num e) => MxM e -> String
showSizeMxM MxM {..} = show m1 ++ "x" ++ show n1 ++ " X " ++ show m2 ++ "x" ++ show n2
  where
    (m1, n1) = size aMxM
    (m2, n2) = size bMxM


benchMxM :: forall e. (Typeable e, Container Vector e, Numeric e, Num e) => MxM e -> Benchmark
benchMxM mxm@MxM {..} =
  bgroup (A.showsType @(MxM e) (" - (" ++ showSizeMxM mxm ++ ")"))
  [ bench "Seq" $ whnf (aMxM LA.<>) bMxM
  ]
{-# INLINEABLE benchMxM #-}





data MxV e =
  MxV
    { aMxV :: !(Matrix e)
    , bMxV :: !(Vector e)
    }

randomMxV ::
     forall e. (A.Prim e, Random e, Element e)
  => MxV e
randomMxV =
  case A.randomMxV :: A.MxV A.P e of
    A.MxV a b -> MxV (matrixToHMatrix a) (vectorToHMatrix b)


showSizeMxV :: (Container Vector e, Num e) => MxV e -> String
showSizeMxV MxV {..} = show m1 ++ "x" ++ show n1 ++ " X " ++ show n ++ "x1"
  where
    (m1, n1) = size aMxV
    n = size bMxV

benchMxV :: forall e. (Typeable e, Container Vector e, Numeric e, Num e) => MxV e -> Benchmark
benchMxV mxv@MxV {..} =
  bgroup (A.showsType @(MxV e) (" - (" ++ showSizeMxV mxv ++ ")"))
  [ bench "Seq" $ whnf (aMxV LA.#>) bMxV
  ]
{-# INLINEABLE benchMxV #-}



data VxM e =
  VxM
    { aVxM :: !(Vector e)
    , bVxM :: !(Matrix e)
    }

randomVxM ::
     forall e. (A.Prim e, Random e, Element e)
  => VxM e
randomVxM =
  case A.randomVxM :: A.VxM A.P e of
    A.VxM a b -> VxM (vectorToHMatrix a) (matrixToHMatrix b)


showSizeVxM :: (Container Vector e, Num e) => VxM e -> String
showSizeVxM VxM {..} = "1x" ++ show n ++ " X " ++ show m2 ++ "x" ++ show n2
  where
    n = size aVxM
    (m2, n2) = size bVxM

benchVxM :: forall e. (Typeable e, Container Vector e, Numeric e, Num e) => VxM e -> Benchmark
benchVxM vxm@VxM {..} =
  bgroup (A.showsType @(VxM e) (" - (" ++ showSizeVxM vxm ++ ")"))
  [ bench "Seq" $ whnf (aVxM LA.<#) bVxM
  ]
{-# INLINEABLE benchVxM #-}
