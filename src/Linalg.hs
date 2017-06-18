{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linalg (
  (#*#), (#+#), argmin, imin
)where

import Prelude as P
import Data.Array.Accelerate as A

type Matrix a = Array DIM2 a

infixl 7 #+#
-- | Matrix/vector addition
(#+#) :: (IsNum a, Elt a, Shape sh)
      => Acc (Array sh a) -> Acc (Array sh a) -> Acc (Array sh a)
(#+#) = A.zipWith (+)


infixl 7 #*#
-- | Matrix multiplication
(#*#) :: (IsNum e, Elt e)
      => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
(#*#) arr brr = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
  where
    Z :. rowsA :. _     = unlift (shape arr) :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = unlift (shape brr) :: Z :. Exp Int :. Exp Int
    arrRepl = A.replicate (lift $ Z :. All   :. colsB :. All) arr
    brrRepl = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)

-- | Convert each element e into pair (index, e)
indexed :: (Shape sh, Elt e) => Acc (Array sh e) -> Acc (Array sh (sh,e))
indexed xs = A.zip (generate (shape xs) id) xs

-- | Index of minimum element
argmin :: (Shape sh, Elt e, IsScalar e)
       => Acc (Array sh e) -> Exp sh
argmin arr = A.fst $ imin arr

imin :: forall sh e. (Shape sh, Elt e, IsScalar e)
       => Acc (Array sh e) -> Exp (sh, e)
imin xs = the $ fold1All f (indexed xs)
    where
        f a b =
            let (_ :: Exp sh, av :: Exp e) = unlift a
                (_ :: Exp sh, bv :: Exp e) = unlift b
            in  (av >* bv) ? (a, b)
