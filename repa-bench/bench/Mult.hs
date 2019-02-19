{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Data.Array.Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Functor.Identity
import           Data.Massiv.Bench.Repa            as R
import           Prelude                           as P



main :: IO ()
main = do
  let !sz = Z :. 200 :. 600
      !arr = repaULightDIM2 sz
  defaultMain
    [ env (return (transpose2S arr)) $ \arr' ->
        bgroup
          "Mult"
          [ bgroup
              "Seq"
              [ bench "mmultS" $ whnf (mmultS arr) arr'
              ]
          , bgroup
              "Par"
              [ bench "mmultP" $ whnf (runIdentity . mmultP arr) arr
              ]
          ]
    ]
