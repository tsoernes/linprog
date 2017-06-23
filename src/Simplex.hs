{-# LANGUAGE RankNTypes #-}
module Accel_example where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Numeric.LinearAlgebra
-- Vector-vector: (<.>)
-- Matrix-vector: (#>)
-- Vector-matrix: (<#)
-- Matrix-matrix: (<>)
import Prelude as P hiding ((++))

import AccelUtils
import Problem

type Init = (Acc (Vector Double), Acc (Array DIM2 Double), Acc (Vector Double))
type IR = (Exp Int, Acc (Array DIM2 Double), Acc (Vector Double), Acc (Vector Int))

-- | Initilization: Introduce slack variables?? or do this earlier in pipeline
-- and obtain the initial basic variables
initSimplex :: Init -> IR
initSimplex (c, a, b) = undefined

simplex :: Init -> Solution
simplex p@(c, a, b) = fixEither nextIteration $ initSimplex p
  where
    nextIteration :: (Exp Int,                  -- k: Entering basic variable (e.g. 2 for x_2)
                      Acc (Array DIM2 Double),  -- b_inv: Inverse of basis matrix
                      Acc (Vector Double),      -- x_b: Values of the basic variables. Corresponds to RHS in tableu.
                      Acc (Vector Int))         -- x_b_vars: Basic variables
                  -> Either Solution IR
    nextIteration (k, b_inv, x_b, x_b_vars) = next
          where
            -- TODO: Examples in book are 1-indexed, while Accelerate arrays are 0-indexed.
            -- That's a problem.

            -- ##
            -- STEP 2: Determine leaving basic variable
            -- ##

            -- The column of coefficients for the entering basic variable
            entering_coeffs = if k P.<= A.length c
              -- Entering basic var is an original var;
              -- calculate only the necessary coefficients of b_inv * a
              then b_inv #> colOf k a
              -- Entering basic var is a slack var
              else colOf (k - A.length c) b_inv

            -- Minimum ratio test. TODO Does this handle 0's in entering_coeffs?
            -- Is negative values handled properly?
            leaving_col = argmin (A.zipWith (/) x_b entering_coeffs)
            r' = x_b_vars ! leaving_col -- leaving basic variable
            -- Z is row 0 but (should not) be included in either x_b or entering coeffs,
            -- so need to start counting a 1
            r = r' + 1

            -- ##
            -- STEP 3: Update b_inv, c_b and x_b to reflect the change of basic variables
            -- ##

            -- Identity matrix with its r'th column replaced by `eta`
            e = eta a r k
            b_inv_new = e <> b_inv

            -- Replace leaving variable with entering variable
            x_b_new = b_inv_new #> b
            i = unindex1 leaving_col
            x_b_vars_new = A.take (i - 1) x_b_vars ++ vec1 r ++ A.drop i x_b_vars
            -- Coefficients in the objective function (row 0) for the new basic variables
            c_b_new = A.map (\j -> (j A.< A.length c) ? (get1 c j, 0)) x_b_vars_new

            -- ##
            -- STEP 1 / Optimality test: Check if optimal,
            -- and if not, determine new entering basic variable
            -- ##

            -- TODO: Only calculate coeffs for the non-basic vars,
            -- since a basic var cannot be entering basic var
            z_non_slack_coeffs = (c_b_new <# b_inv_new <> a) #-# c
            z_slack_coeffs = c_b_new <# b_inv_new

            -- Determine the new entering basic variable by finding the most negative
            -- number in the row of Z (row 0)
            imin_nslack = imin z_non_slack_coeffs
            imin_slack = imin z_slack_coeffs
            (entering_var, coeff) = untup ((A.snd imin_nslack A.<= A.snd imin_slack) ?
                                           (imin_nslack, imin_slack))
            k_new = unindex1 entering_var

            -- If there's no negative coefficient, we've reached an optimal solution
            coeff' = indexArray (run (unit coeff)) Z
            next = if coeff' P.< 0
              then Right (k_new, b_inv_new, x_b_new, x_b_vars)
              else Left undefined
