module Simplex where

import Data.Array.Accelerate as A
import Prelude as P

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

