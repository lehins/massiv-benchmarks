module Main where

import Criterion.Main
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Bench.Vector as A
import Data.Massiv.Bench.Accelerate.Vector
import Data.Array.Accelerate

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "Accelerate"
        [ benchV1 (randomV1 :: Array DIM1 Double)
        , benchVxV (randomVxV :: VxV Double)
        ]
    , bgroup
        "Massiv"
        [ A.benchV1 (A.randomV1 :: A.Vector A.P Double)
        , A.benchVxV (A.randomVxV :: A.VxV A.P Double)
        ]
    ]
