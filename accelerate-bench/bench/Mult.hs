module Main where

import Criterion.Main
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import Data.Massiv.Bench.Accelerate.Matrix

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "Accelerate"
        [ benchMxM (randomMxM :: MxM Double)
        , benchVxM (randomVxM :: VxM Double)
        , benchMxV (randomMxV :: MxV Double)
        ]
    , bgroup
        "Massiv"
        [ A.benchMxM (A.randomMxM :: A.MxM A.P Double)
        , A.benchVxM (A.randomVxM :: A.VxM A.P Double)
        , A.benchMxV (A.randomMxV :: A.MxV A.P Double)
        ]
    ]
