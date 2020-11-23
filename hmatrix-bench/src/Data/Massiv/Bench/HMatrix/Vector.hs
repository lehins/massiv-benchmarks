{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Massiv.Bench.HMatrix.Vector
  ( randomV1
  , benchV1
  , VxV(..)
  , randomVxV
  , benchVxV
  , matrixToHMatrix
  , vectorToHMatrix
  ) where

import Control.DeepSeq
import Criterion
import qualified Data.Massiv.Array as A
import Data.Massiv.Bench.HMatrix.Matrix
import qualified Data.Massiv.Bench.Matrix as A
import qualified Data.Massiv.Bench.Vector as A
import Data.Typeable
import Numeric.LinearAlgebra as LA
import System.Random

randomV1 ::
     forall e. (A.Prim e, Random e, Element e)
  => Vector e
randomV1 = vectorToHMatrix (A.randomV1 :: A.Vector A.P e)


benchV1 :: forall e. (Typeable e, Container Vector e, Numeric e, Floating e) => Vector e -> Benchmark
benchV1 v =
  bgroup "normL2"
  [ bench (A.showsType @(Vector e) (" - (" ++ show (size v) ++ ")")) $
    whnf (\v' -> sqrt (v' LA.<.> v')) v
  ]
{-# INLINEABLE benchV1 #-}





data VxV e =
  VxV
    { aVxV :: !(Vector e)
    , bVxV :: !(Vector e)
    }

instance (NFData e, A.Storable e) => NFData (VxV e) where
  rnf (VxV a b) = a `deepseq` b `deepseq` ()


randomVxV ::
     forall e. (A.Prim e, Random e, Element e)
  => VxV e
randomVxV =
  case A.randomVxV :: A.VxV A.P e of
    A.VxV a b -> VxV (vectorToHMatrix a) (vectorToHMatrix b)


showSizeVxV :: (Container Vector e, Num e) => VxV e -> String
showSizeVxV VxV {..} = show n1 ++ " X " ++ show n2
  where
    n1 = size aVxV
    n2 = size bVxV

benchVxV :: forall e. (Typeable e, Container Vector e, Numeric e, Num e) => VxV e -> Benchmark
benchVxV mxv@VxV {..} =
  bgroup "dotProduct"
  [ bench  (A.showsType @(VxV e) (" - (" ++ showSizeVxV mxv ++ ")")) $ whnf (aVxV LA.<.>) bVxV
  ]
{-# INLINEABLE benchVxV #-}



-- data VxM e =
--   VxM
--     { aVxM :: !(Vector e)
--     , bVxM :: !(Matrix e)
--     }

-- randomVxM ::
--      forall e. (A.Prim e, Random e, Element e)
--   => VxM e
-- randomVxM =
--   case A.randomVxM :: A.VxM A.P e of
--     A.VxM a b -> VxM (vectorToHMatrix a) (matrixToHMatrix b)


-- showSizeVxM :: (Container Vector e, Num e) => VxM e -> String
-- showSizeVxM VxM {..} = "1x" ++ show n ++ " X " ++ show m2 ++ "x" ++ show n2
--   where
--     n = size aVxM
--     (m2, n2) = size bVxM

-- benchVxM :: forall e. (Typeable e, Container Vector e, Numeric e, Num e) => VxM e -> Benchmark
-- benchVxM vxm@VxM {..} =
--   bgroup (A.showsType @(VxM e) (" - (" ++ showSizeVxM vxm ++ ")"))
--   [ bench "Seq" $ whnf (aVxM LA.<#) bVxM
--   ]
-- {-# INLINEABLE benchVxM #-}
