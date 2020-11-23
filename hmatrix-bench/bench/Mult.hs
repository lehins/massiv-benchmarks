module Main where

import Criterion.Main
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Matrix as A
import Data.Massiv.Bench.HMatrix.Matrix

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "HMatrix"
        [ benchMxM (randomMxM :: MxM Double)
        , benchMxV (randomMxV :: MxV Double)
        , benchVxM (randomVxM :: VxM Double)
        ]
    , bgroup
        "Massiv"
        [ A.benchMxM (A.randomMxM :: A.MxM A.P Double)
        , A.benchMxV (A.randomMxV :: A.MxV A.P Double)
        , A.benchVxM (A.randomVxM :: A.VxM A.P Double)
        ]
    ]
