{-# LANGUAGE LambdaCase #-}
module Data.Massiv.Bench.Repa.Sobel where

import           Data.Array.Repa
import           Data.Array.Repa.Stencil
import           Data.Array.Repa.Stencil.Dim2

-- | Repa stencil base Sobel horizontal convolution
sobelX :: Num e => Stencil DIM2 e
sobelX = makeStencil2 3 3
         (\ix -> case ix of
                   Z :. -1 :.  1 -> Just 1
                   Z :.  0 :.  1 -> Just 2
                   Z :.  1 :.  1 -> Just 1
                   Z :. -1 :. -1 -> Just (-1)
                   Z :.  0 :. -1 -> Just (-2)
                   Z :.  1 :. -1 -> Just (-1)
                   _             -> Nothing)
{-# INLINE sobelX #-}



-- | Repa stencil base Sobel vertical convolution
sobelY :: Num e => Stencil DIM2 e
sobelY =
  makeStencil2 3 3 $ \case
    Z :.  1 :. -1 -> Just 1
    Z :.  1 :.  0 -> Just 2
    Z :.  1 :.  1 -> Just 1
    Z :. -1 :. -1 -> Just (-1)
    Z :. -1 :.  0 -> Just (-2)
    Z :. -1 :.  1 -> Just (-1)
    _             -> Nothing
{-# INLINE sobelY #-}



-- | Repa stencil base Sobel horizontal convolution
mapSobelX :: (Source r e, Num e) => Array r DIM2 e -> Array PC5 DIM2 e
mapSobelX = mapStencil2 BoundClamp sobelX
{-# INLINE mapSobelX #-}


-- | Repa stencil base Sobel vertical convolution
mapSobelY :: (Source r e, Num e) => Array r DIM2 e -> Array PC5 DIM2 e
mapSobelY = mapStencil2 BoundClamp sobelY
{-# INLINE mapSobelY #-}



sobelOperator :: (Floating b, Source r b) => Array r DIM2 b -> Array PC5 DIM2 b
sobelOperator arr = smap sqrt $ szipWith (+) arrX2 arrY2
  where
    arrX2 = smap (^ (2 :: Int)) (mapSobelX arr)
    arrY2 = smap (^ (2 :: Int)) (mapSobelY arr)
{-# INLINE sobelOperator #-}
