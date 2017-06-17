module Accel_example where

import Data.Array.Accelerate as A
import Prelude as P

main :: IO ()
main = undefined

wyndor :: (Vector Double, Array DIM2 Double, Vector Double)
wyndor = (c, a, b)
  where
    c = fromList (Z:.2) [3, 5]
    a = fromList (Z:.3:.2) [1, 0, 0, 2, 3, 2]
    b = fromList (Z:.3) [4, 12, 18]

wyndor_init_bf :: (Scalar Int, Array DIM2 Double, Vector Double, Vector Int)
wyndor_init_bf = (k, b_inv, x_b, x_b_vars)
  where
    x_b_vars = fromList (Z:.3) [3, 4, 5]
    x_b = fromList (Z:.3) [4, 12, 18]
    b_inv = fromList (Z:.3:.3) [1, 0, 0, 0, 1, 0, 0, 0, 1]
    k = fromList Z [2]

nextIteration :: (Scalar Int, Array DIM2 Double, Vector Double, Vector Int) ->
                 (Scalar Int, Array DIM2 Double, Vector Double, Vector Int)
nextIteration (k, b_inv, x_b, x_b_vars) = undefined -- (k_new, b_inv_new, x_b_new, x_b_vars)
      where
        entering_coeffs = undefined -- The column corresponding to the entering basic variable (how to figure that out?) in the matrix `b_inv * a`
        -- argmin = undefined
        -- r = 1 + argmin (x_b / entering_coeffs) -- leaving basic variable
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

