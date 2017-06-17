module Simplex where

main :: IO ()
main = undefined


data Bound x =  x :<=: Double
             |  x :>=: Double
             |  x :&: (Double,Double)
             |  x :==: Double
             |  Free x
             deriving Show

obj :: Bound t -> t
obj (x :<=: _) = x
obj (x :>=: _) = x
obj (x :&: _)  = x
obj (x :==: _) = x
obj (Free x)   = x


data Solution = Undefined
              | Feasible (Double, [Double])
              | Infeasible (Double, [Double])
              | NoFeasible
              | Optimal (Double, [Double])
              | Unbounded
              deriving Show

data Constraints = Dense   [ Bound [Double] ]

data Optimization = Maximize [Double]
                  | Minimize [Double]

objCoeffs :: Optimization -> [Double]
objCoeffs (Maximize li) = li
objCoeffs (Minimize li) = li

type Bounds = [Bound Int]

data LPprob = LPprob { lpObjfunc :: Optimization
                     , lpConstrs :: Constraints
                     , lpBounds :: Bounds
                     }

-- | Check that the problem definition is valid
verifyProblem :: Optimization -> Constraints -> Bounds -> Either String LPprob
verifyProblem opt (Dense constrs) bnds = LPprob <$> optsOK <*> constrsOK <*> bndsOK
  where
    optsOK = if all (\cs -> length (objCoeffs opt) == length (obj cs)) constrs
      then Right opt
      else Left "Number of objective function coefficients not mathing number of constraint coeffs"
    constrsOK = Right $ Dense constrs
    bndsOK = Right bnds
  -- TODO attempt converting to standard form

-- Ignoring problems which require artifical vars for now
simplex c a b = iteration initial_bf_sol
  where
    initial_bf_sol = undefined
    iteration (k, b_inv, x_b, x_b_vars) = (k_new, b_inv_new, x_b_new, x_b_vars)
      where
        -- k: entering basic variable
        entering_coeffs = undefined -- The column corresponding to the entering basic variable (how to figure that out?) in the matrix `b_inv * a`
        argmin = undefined
        r = 1 + argmin (x_b / entering_coeffs) -- leaving basic variable
        b_inv_new = e * b_inv
        e = undefined -- Identity matrix with its r'th column replaced by `eta`
        eta = undefined -- [eta_1, eta_2, .., eta_m] where m: number of constraints = number of rows in b_inv
        eta_i i = if i == r
          then 1 / a_rk
          else 0 --   -a_ik / a_rk
        c_b_new = undefined -- 
        c_b_new_i i = if i < length c
          then get c i
          else 0
        z_non_slack_coeffs = c_b_new * b_inv_new * a - c
        isOptimal = all (>0) z_non_slack_coeffs
        k_new = get x_b_vars (argmin z_non_slack_coeffs)
        x_b_new = undefined -- Replace leaving variable with entering variable

-- Determine a basic feasible solution and return the basic variables
basis = undefined
