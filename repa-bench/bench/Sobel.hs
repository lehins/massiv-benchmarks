{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Data.Array.Repa
import Data.Functor.Identity
import qualified Data.Massiv.Array as A
import Data.Massiv.Bench.Repa as R
import Data.Massiv.Bench.Repa.Sobel as R
import qualified Data.Massiv.Bench.Sobel as A
import Prelude as P

main :: IO ()
main = do
  let (m, n) = (1600, 1200)
  let !sz = Z :. m :. n
  defaultMain
    [ env (return (repaULightDIM2 sz)) $ \arr ->
        bgroup
          "Sobel"
          [ bgroup
              "Seq"
              [ bench "Horizontal - Repa" $
                  whnf (computeUnboxedS . mapSobelX) arr,
                bench "Vertical - Repa" $ whnf (computeUnboxedS . mapSobelY) arr,
                bench "Operator - Repa" $
                  whnf (computeUnboxedS . sobelOperator) arr
              ],
            bgroup
              "Par"
              [ bench "Horizontal - Repa" $
                  whnf (runIdentity . computeUnboxedP . mapSobelX) arr,
                bench "Vertical - Repa" $
                  whnf (runIdentity . computeUnboxedP . mapSobelY) arr,
                bench "Operator - Repa" $
                  whnf (runIdentity . computeUnboxedP . sobelOperator) arr
              ]
          ],
      A.sobelBenchGroup (A.Sz2 m n)
    ]
