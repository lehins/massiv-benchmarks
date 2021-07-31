{-# LANGUAGE BangPatterns #-}
module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Array.Repa as R
import Data.Massiv.Bench as A
import Data.Monoid
import Prelude as P
import Data.Massiv.Bench.Repa as R
import Data.Vector.Primitive as VP

main :: IO ()
main = do
  let !sz@(Sz2 m n) = Sz2 1600 12000
      !shape = Z R.:. m R.:. n
      !arrMassiv = arrRLightIx2 P Seq sz
      !arrRepa = repaULightDIM2 shape
      !vec = A.toPrimitiveVector arrMassiv
  defaultMain
    [ bgroup
        "Sum"
        [ env (return (arrRepa `R.deepSeqArray` vec `seq` arrMassiv)) $ \ arr ->
            bgroup
              "Seq"
              [ bench "Repa" $ whnf R.sumAllS arrRepa
              , bench "Vector" $ whnf VP.sum vec
              , bench "Massiv" $ whnf A.sum arr
              ]
        , env (return (arrRepa `R.deepSeqArray` setComp Par arrMassiv)) $ \ arr ->
            bgroup
              "Par"
              [ bench "Repa" $ whnfIO (R.sumAllP arrRepa)
              , bench "Massiv" $ whnf A.sum arr
              ]
        ]
    ]

