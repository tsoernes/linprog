{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module AccelUtils where

import Prelude as P
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I

-- $setup
-- >>> import Data.Array.Accelerate.Interpreter as I

type Matrix a = Array DIM2 a

infixl 7 #+#
-- | Matrix/vector addition
(#+#) :: (A.Num a, Elt a, Shape sh)
      => Acc (Array sh a) -> Acc (Array sh a) -> Acc (Array sh a)
(#+#) = A.zipWith (+)


infixl 7 #-#
-- | Matrix/vector subtraction
-- >>> run $ (use $ A.fromList (Z:.2) [0,0]) #-# (use $ A.fromList (Z:.2) [3,-5] :: Acc (Vector Int))
-- Vector (Z :. 2) [-3,5]
(#-#) :: (A.Num a, Elt a, Shape sh)
      => Acc (Array sh a) -> Acc (Array sh a) -> Acc (Array sh a)
(#-#) = A.zipWith (-)


-- infixl 7 ^*#
-- -- | Vector by matrix multiplication
-- -- TODO this is incorrecly implemented
-- -- >>> run $ (use $ A.fromList (Z:.3) [0,5,3]) ^*# (use $ A.fromList (Z:.3:.3) [1,1/3,-1/3, 0,1/2,0, 0,-1/3,1/3] :: Acc (Matrix Double))
-- -- Vector (Z :. 3) [0,3/2,1]
-- (^*#) :: (A.Num e, Elt e)
--       => Acc (Vector e) -> Acc (Matrix e) -> Acc (Vector e)
-- (^*#) vec arr = A.fold (+) 0 $ A.zipWith (*) brr arr
--   where
--     Z :. rowsA :. colsA = unlift (shape arr) :: Z :. Exp Int :. Exp Int
--     brr = A.replicate (lift $ Z :. All :. colsA) vec


-- infixl 7 #*^
-- -- | Matrix by vector multiplication
-- -- >>> run $ (use $ A.fromList (Z:.3:.3) [1,0,0, 0,1/2,0, 0,-1,1] :: Acc (Matrix Double)) #*^ (use $ A.fromList (Z:.3) [4,12,18])
-- -- Vector (Z :. 3) [4.0,6.0,6.0]
-- (#*^) :: (A.Num e, Elt e)
--       => Acc (Matrix e) -> Acc (Vector e) -> Acc (Vector e)
-- (#*^) arr vec = A.fold (+) 0 $ A.zipWith (*) arr brr
--   where
--     Z :. rowsA :. _ = unlift (shape arr) :: Z :. Exp Int :. Exp Int
--     brr = A.replicate (lift $ Z :. rowsA :. All) vec

-- infixl 7 #*#
-- -- | Matrix by matrix multiplication
-- (#*#) :: (A.Num e, Elt e)
--       => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
-- (#*#) arr brr = A.fold (+) 0 $ A.zipWith (*) arrRepl brrRepl
--   where
--     Z :. rowsA :. _     = unlift (shape arr) :: Z :. Exp Int :. Exp Int
--     Z :. _     :. colsB = unlift (shape brr) :: Z :. Exp Int :. Exp Int
--     arrRepl = A.replicate (lift $ Z :. All   :. colsB :. All) arr
--     brrRepl = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)

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

-- This seems dangerous for nested data parallelism
untup_ :: (Elt a, Elt b) => Exp (a, b) -> (Exp a, Exp b)
untup_ tup = (A.fst tup, A.snd tup)

nRows :: (Elt e) => Acc (Matrix e) -> Exp Int
nRows arr = A.fst $ unindex2 $ shape arr

nCols :: (Elt e) => Acc (Matrix e) -> Exp Int
nCols arr = A.snd $ unindex2 $ shape arr

get1 :: (Elt e) => Acc (Vector e) -> Exp Int -> Exp e
get1 arr i = arr ! index1 i

get2 :: (Elt e) => Acc (Matrix e) -> Exp Int -> Exp Int -> Exp e
get2 arr r c = arr ! index2 r c

rowOf :: (Elt e) => Exp Int -> Acc (Matrix e) -> Acc (Vector e)
rowOf i arr = slice arr (lift (Z :. i :. All))

colOf :: (Elt e) => Exp Int -> Acc (Matrix e) -> Acc (Vector e)
colOf i arr = slice arr (lift (Z :. All :. i))

vecOf :: (Elt e) => [e] -> Acc (Vector e)
vecOf es = use $ A.fromList (Z:. P.length es) es

arrOf :: (Elt e) => Int -> Int -> [e] -> Acc (Matrix e)
arrOf rows cols es = use $ A.fromList (Z:. rows :. cols) es

vecOf1 :: (Elt e) => Exp e -> Acc (Vector e)
vecOf1 e = reshape (constant (Z:.1)) $ unit e


-- | Fixed point of the Either monad. Feed the output of Right back into
-- the given function until a Left is produced. May never terminate.
fixEither :: (a -> Either b a) -> a -> b
fixEither f a = case f a of
  Left b -> b
  Right a' -> fixEither f a'

runExp :: (Elt e) => Exp e -> e
runExp e = indexArray (run (unit e)) Z

fstMap :: (Elt a, Elt b, Elt e) => (Exp a -> Exp b) -> Exp (a, e) -> Exp (b, e)
fstMap f tup = lift (f (A.fst tup), A.snd tup)

sndMap :: (Elt a, Elt b, Elt e) => (Exp a -> Exp b) -> Exp (e, a) -> Exp (e, b)
sndMap f tup = lift (A.fst tup, f (A.snd tup))

getScalar :: Scalar a -> a
getScalar scalar = indexArray scalar Z
