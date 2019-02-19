{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Data.Array.Repa
import           Data.Functor.Identity
import           Data.Massiv.Bench.Repa            as R
import           Prelude                           as P



main :: IO ()
main = do
  let !sz = Z :. 600 :. 1000
  defaultMain
    [ env (return (repaULightDIM2 sz)) $ \arr ->
        bgroup
          "Append"
          [ bench "Seq" $ whnf (computeUnboxedS . append arr) arr
          , bench "Par" $ whnf (runIdentity . computeUnboxedP . append arr) arr
          ]
    ]
