{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Linalg (
  (#+#), (#-#), (#*#), (#*^),
  argmin, imin,
  rowOf, colOf,
  identityMatrix, eta
)where

import Prelude as P
import Data.Array.Accelerate as A

type Matrix a = Array DIM2 a

infixl 7 #+#
-- | Matrix/vector addition
(#+#) :: (A.Num a, Elt a, Shape sh)
      => Acc (Array sh a) -> Acc (Array sh a) -> Acc (Array sh a)
(#+#) = A.zipWith (+)

infixl 7 #-#
-- | Matrix/vector subtraction
(#-#) :: (A.Num a, Elt a, Shape sh)
      => Acc (Array sh a) -> Acc (Array sh a) -> Acc (Array sh a)
(#-#) = A.zipWith (-)


infixl 7 #*^
-- | Matrix by vector multiplication
(#*^) :: (A.Num e, Elt e)
      => Acc (Matrix e) -> Acc (Vector e) -> Acc (Vector e)
(#*^) arr vec = A.fold (+) 0 $ A.zipWith (*) arr brr
  where
    Z :. rowsA :. _     = unlift (shape arr) :: Z :. Exp Int :. Exp Int
    brr = A.replicate (lift $ Z :. rowsA :. All) vec

infixl 7 #*#
-- | Matrix by matrix multiplication
(#*#) :: (A.Num e, Elt e)
      => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
(#*#) arr brr = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
  where
    Z :. rowsA :. _     = unlift (shape arr) :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = unlift (shape brr) :: Z :. Exp Int :. Exp Int
    arrRepl = A.replicate (lift $ Z :. All   :. colsB :. All) arr
    brrRepl = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)

-- | Index of minimum element
argmin :: (Shape sh, Elt e, A.Ord e)
       => Acc (Array sh e) -> Exp sh
argmin arr = A.fst $ imin arr

imin :: forall sh e. (Shape sh, Elt e, A.Ord e)
       => Acc (Array sh e) -> Exp (sh, e)
imin xs = the $ fold1All f (indexed xs)
    where
      f a b = let (_ :: Exp sh, av :: Exp e) = unlift a
                  (_ :: Exp sh, bv :: Exp e) = unlift b
              in  (av A.>= bv) ? (a, b)

nRows :: (Elt e) => Acc (Matrix e) -> Exp Int
nRows arr = A.fst $ unindex2 $ shape arr

nCols :: (Elt e) => Acc (Matrix e) -> Exp Int
nCols arr = A.snd $ unindex2 $ shape arr

get2 :: (Elt e) => Exp Int -> Exp Int -> Acc (Matrix e) -> Exp e
get2 r c arr = the (slice arr (lift (Z :. r :. c)))

rowOf :: (Elt e) => Exp Int -> Acc (Matrix e) -> Acc (Vector e)
rowOf i arr = slice arr (lift (Z :. i :. All))

colOf :: (Elt e) => Exp Int -> Acc (Matrix e) -> Acc (Vector e)
colOf i arr = slice arr (lift (Z :. All :. i))

-- | Constructor for the nxn dimensional identity matrix
identityMatrix :: forall a. (IsNum a, Elt a)
               => Int -> Acc (Matrix a)
identityMatrix n = use $ fromFunction (Z:.n:.n) aux
  where aux :: DIM2 -> a
        aux (Z:.i:.j) = if i P.== j then 1 else 0

-- | Eta matrix, which is the identity matrix whith row @r@ replaced with eta values
eta :: forall e. (Elt e, A.Fractional e)
    => Acc (Matrix e) -> Exp Int -> Exp Int -> Acc (Matrix e)
eta a r k = A.generate (lift (Z:.n:.n)) aux
  where
    n = nRows a
    aux :: (Elt e, A.Fractional e) => Exp DIM2 -> Exp e
    aux dim = let dim' = unindex2 dim
                  row = A.fst dim'
                  col = A.snd dim'
              in if col P.== r
                   -- Column r gets eta values
                   then if row P.== r
                        then 1.0 / get2 r k a
                        else - get2 row k a / get2 r k a
                   -- Identity matrix
                   else if row P.== col
                        then 1
                        else 0
