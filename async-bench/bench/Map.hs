{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Control.DeepSeq
import           Data.Massiv.Bench
import           Data.Massiv.Core.Index
import           Prelude                 as P
import qualified UnliftIO.Async          as U (pooledMapConcurrently,
                                               pooledMapConcurrentlyN)


main :: IO ()
main = do
  defaultMain $ map (mkBench "return" return) [100, 10000, 100000]


mkBench :: (NFData a) => String -> (Int -> IO a) -> Int -> Benchmark
mkBench name f n =
  bgroup
    ("(" ++ show n ++ ") mapConcurrently - action: " ++ name)
    [ bgroup
        "UnliftIO"
        [ bench "Par" $ nfIO (U.pooledMapConcurrently f [0 .. n])
        , bench "Seq" $ nfIO (U.pooledMapConcurrentlyN 1 f [0 .. n])
        ]
    ]
