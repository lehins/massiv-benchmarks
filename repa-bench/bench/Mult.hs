module Main where

import Criterion.Main
import Data.Array.Repa
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import Data.Massiv.Bench.Repa.Matrix

main :: IO ()
main = do
  defaultMain
    [ bgroup "Repa" [benchMxM (randomMxM :: MxM U Double)]
    , bgroup "Massiv" [A.benchMxM (A.randomMxM :: A.MxM A.U Double)]
    ]
