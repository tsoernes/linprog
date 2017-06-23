module Problem where

import Prelude as P

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

data Constraints = Dense [ Bound [Double] ]

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

