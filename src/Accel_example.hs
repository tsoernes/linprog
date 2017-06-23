{-# LANGUAGE RankNTypes #-}
module Accel_example where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Numeric.LinearAlgebra
-- Vector-vector: (<.>)
-- Matrix-vector: (#>)
-- Vector-matrix: (<#)
-- Matrix-matrix: (<>)
import Prelude as P
import Linalg

type Init = (Acc (Vector Double), Acc (Array DIM2 Double), Acc (Vector Double))
wyndor :: Init
wyndor = (c, a, b)
  where
    c = use $ fromList (Z:.2) [3, 5]
    a = use $ fromList (Z:.3:.2) [1, 0, 0, 2, 3, 2]
    b = use $ fromList (Z:.3) [4, 12, 18]

wyndorInitBF :: IR
wyndorInitBF = (k, b_inv, x_b, x_b_vars)
  where
    x_b_vars = use $ fromList (Z:.3) [3, 4, 5]
    x_b = use $ fromList (Z:.3) [4, 12, 18]
    b_inv = use $ fromList (Z:.3:.3) [1, 0, 0, 0, 1, 0, 0, 0, 1]
    -- k = use $ fromList Z [2]
    k = 2

-- type IR = (Acc (Scalar Int), Acc (Array DIM2 Double), Acc (Vector Double), Acc (Vector Int))
type IR = (Exp Int, Acc (Array DIM2 Double), Acc (Vector Double), Acc (Vector Int))
iterate :: Init -> IR
iterate (c, a, b) = nextIteration wyndorInitBF
  where
    nextIteration :: IR -> IR
    nextIteration (k, b_inv, x_b, x_b_vars) = undefined -- (k_new, b_inv_new, x_b_new, x_b_vars)
          where
            -- TODO: Examples in book are 1-indexed, while Accelerate arrays are 0-indexed.
            -- That's a problem.

            -- k: Entering basic variable (e.g. 2 for x_2)

            -- The column of coefficients for the entering basic variable
            entering_coeffs = if k P.<= A.length c
              -- Entering basic var is an original var;
              -- calculate only the necessary coefficients of b_inv * a
              then b_inv #> colOf k a
              -- Entering basic var is a slack var
              else colOf (k - A.length c) b_inv

            -- Minimum ratio test. TODO Does this handle 0's in entering_coeffs?
            -- Is negative values handled properly?
            r' = unindex1 $ argmin (A.zipWith (/) x_b entering_coeffs) -- leaving basic variable
            -- Z is row 0 but (should not) be included in either x_b or entering coeffs,
            -- need to start counting a 1
            r = r' + 1

            -- Identity matrix with its r'th column replaced by `eta`
            e = eta a r k
            b_inv_new = e <> b_inv

            -- Coefficients in the objective function (row 0) for the new basic variables
            c_b_new = A.map (\i -> (i A.< A.length c) ? (get1 i c, 0)) x_b_vars
            z_non_slack_coeffs = (c_b_new <# b_inv_new <> a) #-# c

            -- Determine the leaving basic variable by finding the most negative
            -- number in the row of Z (row 0)
            (leaving_var, coeff) = untup $ imin z_non_slack_coeffs
            -- If there's no negative coefficient, we've reached an optimal solution

            -- The new entering basic variable
            k_new = indexArray x_b_vars $ Z :. (leaving_var)
            -- _b_new = undefined -- Replace leaving variable with entering variable
