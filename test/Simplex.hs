module SimplexTest where

import Simplex
import AccelUtils

wyndor :: Init
wyndor = (c, a, b)
  where
    c = vecOf [3, 5]
    a = arrOf 3 2 [1, 0, 0, 2, 3, 2]
    b = vecOf [4, 12, 18]

wyndorInitBF :: IR
wyndorInitBF = (k, b_inv, x_b, x_b_vars)
  where
    x_b_vars = vecOf [3, 4, 5]
    x_b = vecOf [4, 12, 18]
    b_inv = arrOf 3 3 [1, 0, 0, 0, 1, 0, 0, 0, 1]
    k = 2


-- | Wyndor w/o last 2 constraints. Should result in unbounded Z (ref ItOR 9e p109)
wyndorUnbounded :: Init
wyndorUnbounded = (c, a, b)
  where
    c = vecOf [3, 5]
    a = arrOf 1 2 [1, 0]
    b = vecOf [4]
