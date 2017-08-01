import           Data.List (maximumBy)
import           Data.Ord  (comparing)

main :: IO ()
main = do
  print ""
  print ""


-- Kont 2015, Oppg3
-- Step: n = 1..4 : year
-- State: s_n: total number of ready-to-use fabs at the start of year 'n'
-- Decision: x_n: number of fabs to build year 'n'
-- Transition: s_n+1 = s_n + x_n
--      i.e total number fabs at the start of the next year equals
--      the total number of fabs at the start of this year plus
--      the number of fabs built this year
-- Base cases: see `fnOpt 5`


-- | Optimal decision and corresponding value for step 'n' and onwards, given state
fnOpt :: Int        -- ^ Step 'n'
      -> Int        -- ^ State 'sn'
      -> (Int, Int) -- ^ (Decision, Value)
fnOpt 5 _ = (0, 0) -- Can't build any fabs in the fifth year, so the cost is 0
fnOpt n sn = maximumBy (comparing snd) outcomes
  where
    outcomes = zip xs $ map (fn n sn) xs
    x_min = max (minFabs n - sn) 0
    xs = [x_min..3]


-- | Total value for step 'n' and onwards; given state and decision
fn :: Int -- ^ Step 'n', year
   -> Int -- ^ State 'sn', total number of fabs created so far
   -> Int -- ^ Decision 'xn', how many fabs to create this year
   -> Int -- ^ Cost
fn n sn xn = setupCostTotal n xn + snd (fnOpt n (sn+xn))

setupCostTotal :: Int -- ^ year 'n'
               -> Int -- ^ number of fabs 'x'
               -> Int -- ^ cost
setupCostTotal n x = 1000 + x * setupCost n

-- | Cost for creating a fab at year n
setupCost :: Int -> Int
setupCost n
  | n == 1 = 2000
  | n == 2 = 2300
  | n == 3 = 2100
  | n == 4 = 1800
  | otherwise = error $ "invalid input setupcost: " ++ show n


-- | Mimimum number of fabs that should be complete or in construction at year 'n'
minFabs :: Int -> Int
minFabs n
  | n == 1 = 1
  | n == 2 = 2
  | n == 3 = 4
  | n == 4 = 5
  | otherwise = error $ "invalid input minFabs: " ++ show n
