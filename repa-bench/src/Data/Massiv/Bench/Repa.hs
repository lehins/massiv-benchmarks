{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Data.Massiv.Bench.Repa where

import           Control.DeepSeq
import           Data.Array.Repa
import           Data.Array.Repa.Eval (Target)
import           Data.Massiv.Bench    (heavyFunc, lightFunc)


instance NFData Z where
  rnf z = z `seq` ()

instance Shape sh => NFData (sh :. Int) where
  rnf sh = sh `deepSeq` ()

instance (Shape sh, Source r e, Target r e) => NFData (Array r sh e) where
  rnf arr = arr `deepSeqArray` ()


repaDLightDIM2 :: DIM2 -> Array D DIM2 Double
repaDLightDIM2 sz = fromFunction sz (\(Z :. i :. j) -> lightFunc i j)
{-# INLINE repaDLightDIM2 #-}

repaDHeavyDIM2 :: DIM2 -> Array D DIM2 Double
repaDHeavyDIM2 sz = fromFunction sz (\(Z :. i :. j) -> heavyFunc i j)
{-# INLINE repaDHeavyDIM2 #-}



repaULightDIM2 :: DIM2 -> Array U DIM2 Double
repaULightDIM2 = computeUnboxedS . repaDLightDIM2
{-# INLINE repaULightDIM2 #-}

repaUHeavyDIM2 :: DIM2 -> Array U DIM2 Double
repaUHeavyDIM2 = computeUnboxedS . repaDHeavyDIM2
{-# INLINE repaUHeavyDIM2 #-}


