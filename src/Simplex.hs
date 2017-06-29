{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Numeric.LinearAlgebra ((<.>), (#>), (<#), (<>))
-- Vector-vector: (<.>)
-- Matrix-vector: (#>)
-- Vector-matrix: (<#)
-- Matrix-matrix: (<>)
import Prelude as P hiding ((++))

import AccelUtils
import Problem

-- $setup
-- >>> 

type Init = (Acc (Vector Float), Acc (Matrix Float), Acc (Vector Float))
type IR = (Exp Int, Acc (Matrix Float), Acc (Vector Float), Acc (Vector Int))

-- | Initilization: Introduce slack variables?? or do this earlier in pipeline
-- and obtain the initial basic variables
initSimplex :: Init -> IR
initSimplex (c, a, b) = undefined

simplex :: Init -> Solution
simplex p@(c, a, b) = fixEither nextIteration $ initSimplex p
  where
    n_orig_vars = nCols a
    nextIteration :: (Exp Int,                  -- k: Entering basic variable (e.g. 2 for x_2)
                      Acc (Array DIM2 Float),  -- b_inv: Inverse of basis matrix
                      Acc (Vector Float),      -- x_b: Values of the basic variables. Corresponds to RHS in tableu.
                      Acc (Vector Int))         -- x_b_vars: Basic variables
                  -> Either Solution IR
    nextIteration (k, b_inv, x_b, x_b_vars) = unbounded_z >> optimal
          where
            -- TODO: Examples in book are 1-indexed, while Accelerate arrays are 0-indexed.
            -- That's a problem.

            -- ##
            -- Determine leaving basic variable (abbreviated bv)
            -- ##

            -- The column of coefficients for the entering basic variable
            entering_coeffs = if k P.<= n_orig_vars
              -- Entering basic var is an original var;
              -- calculate only the necessary coefficients of b_inv * a
              then b_inv #> colOf k a
              -- Entering basic var is a slack var
              else colOf (k - n_orig_vars) b_inv

            -- Minimum ratio test. Filter out non-negative coeffs in the col of
            -- the entering bv, then divide by the right-hand side (x_b).
            -- The row with the minimum value then represents the equation for the
            -- leaving bv.
            rowtups = A.izipWith tup3 x_b entering_coeffs -- (index, rhs, coeff)
            -- A.filter also returns the number of elems removed as snd val in a tuple
            inonneg = afst $ A.filter ((A.>0) . thrd) rowtups
            -- If there's no leaving basic var, i.e. no non-negative coeff in the column
            -- of the entering bv, then Z is unbounded.
            unbounded_z = if runExp $ A.length inonneg A.== 0
              then Left Unbounded
              else Right ()
            iratios = A.map div23 inonneg
            leaving_row = A.fst $ iCmpWith (\t1 t2 -> A.snd t1 A.< A.snd t2) iratios

            r' = x_b_vars ! leaving_row -- leaving basic variable
            -- Z is row 0 but (should not) be included in either x_b or entering coeffs,
            -- so need to start counting a 1
            r = r' + 1

            -- ##
            -- Update b_inv, c_b and x_b to reflect the change of basic variables
            -- ##

            -- Identity matrix with its r'th column replaced by `eta`
            e = eta a r k
            b_inv_new = e <> b_inv

            -- Replace leaving variable with entering variable
            x_b_new = b_inv_new #> b
            i = unindex1 leaving_row
            x_b_vars_new = A.take (i - 1) x_b_vars ++ vecOf1 r ++ A.drop i x_b_vars
            -- Coefficients in the objective function (row 0) for the new basic variables
            c_b_new = A.map (\j -> (j A.< A.length c) ? (get1 c j, 0)) x_b_vars_new

            -- ##
            -- Check if optimal, and if not, determine new entering basic variable
            -- ##

            (k_new, coeff) = runExp $ minZCoeff c a n_orig_vars b_inv_new c_b_new

            -- If there's no negative coefficient, we've reached an optimal solution
            optimal = if coeff P.>= 0
              then Left $ Optimal $ getSolution c_b_new x_b_new x_b_vars_new
              else Right (lift k_new, b_inv_new, x_b_new, x_b_vars)


-- | Get the current solution in the dense form (Z, [x1, x2, .., xn])
-- where n is the number of orig+slack vars
getSolution :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Int) -> (Float, [Float])
getSolution c_b x_b x_b_vars = (z, xs)
  where
    z = getScalar $ run $ c_b <.> x_b
    n_orig_vars = A.length c_b
    -- TODO: figure out num of slack vars
    n_slack_vars = undefined
    sh_n = lift $ Z:.n_orig_vars+n_slack_vars
    -- The basic vars in x_b_vars each have a value in the same index in x_b; non-basic vars have value 0
    xs = toList $ run $ scatter x_b_vars (fill sh_n 0) x_b


-- | Determine the variable with the minimum coefficient in the row of Z. Iff this
-- coefficient is non-negative, the simplex tableu has reached an optimal solution.
-- >>> let coeff = minZCoeff (vecOf [3.0,5]) (arrOf 3 3 [1,0,1,0,2,0,3,2,0] :: Acc (Matrix Float)) 3 (arrOf 3 3 [1,0,0,0,1/2,0,0,-1,1] :: Acc (Matrix Float)) (vecOf [0.0,5,0])
-- >>> runExp coeff
-- (0,-3.0)
minZCoeff :: Acc (Vector Float) -> Acc (Matrix Float) -> Exp Int -> Acc (Matrix Float) -> Acc (Vector Float)
           -> Exp (Int, Float)
minZCoeff c a n_orig_vars b_inv c_b = minCoeff
  where
    -- TODO: Only calculate coeffs for the non-basic vars,
    -- since a basic var cannot be entering basic var
    z_slack_coeffs = c_b <# b_inv
    z_non_slack_coeffs = (z_slack_coeffs <# a) #-# c

    -- Determine the new entering basic variable by finding the most negative
    -- number in the row of Z (row 0)
    imin_nslack = fstMap unindex1 $ imin z_non_slack_coeffs
    -- Slack variables indexes should at u where u is number of orig vars
    imin_slack = fstMap ((+n_orig_vars) . unindex1) $ imin z_slack_coeffs
    minCoeff = (A.snd imin_nslack A.<= A.snd imin_slack) ? (imin_nslack, imin_slack)

-- | Eta matrix, which is the identity matrix whith row @r@ replaced with eta values
-- >>> run $ eta (arrOf 3 3 [1,0,1,0,2,0,3,2,0] :: Acc (Matrix Float)) 1 1
-- Matrix (Z :. 3 :. 3)
--   [1.0,-0.0,0.0,
--    0.0, 0.5,0.0,
--    0.0,-1.0,1.0]
eta :: forall e. (Elt e, A.Fractional e)
    => Acc (Matrix e) -> Exp Int -> Exp Int -> Acc (Matrix e)
eta a r k = A.generate (lift (Z:.n:.n)) aux
  where
    n = nRows a
    aux :: (Elt e, A.Fractional e) => Exp DIM2 -> Exp e
    aux dim = let dim' = unindex2 dim
                  row = A.fst dim'
                  col = A.snd dim'
              in (col A.== r) ?
                   ((row A.== r) ?
                        (1.0 / get2 a r k, -get2 a row k / get2 a r k)
                   ,(row A.== col) ?
                        (1, 0))
