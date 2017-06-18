{-# LANGUAGE RankNTypes #-}
module Accel_example where

import Data.Array.Accelerate as A
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
            -- The column of coefficients for the entering basic variable
            test = (slice a (lift (Z:. All :. k)))
            test' = b_inv * test
            entering_coeffs = if k <= A.length c
              -- Entering basic var is an original var; calculate necessary coefficients of b_inv * a
              then undefined
              -- Entering basic var is a slack var
              else slice b_inv (lift (Z :. All :. (k - A.length c)))

            r' = unindex1 $ argmin (A.zipWith (/) x_b entering_coeffs) -- leaving basic variable
            r = r' + 1
            -- b_inv_new = e * b_inv
            -- e = undefined -- Identity matrix with its r'th column replaced by `eta`
            -- eta = undefined -- [eta_1, eta_2, .., eta_m] where m: number of constraints = number of rows in b_inv
            -- eta_i i = if i == r
            --   then 1 / a_rk
            --   else 0 --   -a_ik / a_rk
            -- c_b_new = undefined --
            -- c_b_new_i i = if i < A.length c
            --   then get c i
            --   else 0
            -- z_non_slack_coeffs = c_b_new * b_inv_new * a - c
            -- isOptimal = A.all (>0) z_non_slack_coeffs
            -- k_new = indexArray x_b_vars $ Z :. (argmin z_non_slack_coeffs)
            -- x
            -- _b_new = undefined -- Replace leaving variable with entering variable