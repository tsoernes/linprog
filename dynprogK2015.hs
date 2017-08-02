{-# LANGUAGE TupleSections #-}

import           Data.List (minimumBy)
import           Data.Ord  (comparing)

-- Kont 2015, Oppg3
-- Step: n = 1..4 : year
-- State: s_n: total number of ready-to-use fabs at the start of year 'n'
-- Decision: x_n: number of fabs to build year 'n'
-- Transition: s_n+1 = s_n + x_n
--      i.e total number fabs at the start of the next year equals
--      the total number of fabs at the start of this year plus
--      the number of fabs built this year
-- Base cases: see `fnOpt 5`

main :: IO ()
main = do
  -- let n_sn_pairs = concatMap (\(n, sns) -> map (n,) sns) $ zip [1..4] possibleStates
  -- mapM_  (uncurry popt) n_sn_pairs
  print $ fnOpt 1 0
  -- "n: 1 sn: 0 dec,val: ([3,0,2,0,0],12200)"
  -- The optimal decions are to build 3 fabs year 1 (first year),
  -- 2 fabs year 3, which results in a total (minimum) cost of 12200
  print ""


-- | Pretty the input and the corresponding optimal output
popt :: Int -> Int -> IO ()
popt n sn = print $ "n: " ++ show n ++ " sn: " ++ show sn ++ " dec,val: " ++ show (fnOpt n sn)


-- | Optimal decisions and corresponding value for step 'n' onwards
fnOpt :: Int        -- ^ Step 'n'
      -> Int        -- ^ State 'sn'
      -> ([Int], Int) -- ^ (Decisions, Value)
fnOpt 5 _ = ([0], 0) -- Can't build any fabs in the fifth year, so the cost is 0
fnOpt n sn = minimumBy (comparing snd) outcomes
  where
    outcomes = zipWith (\x (xs', fn) -> (x:xs', fn)) xs $ map (fn n sn) xs
    x_min = max (minFabs n - sn) 0
    xs = [x_min..3]


-- | Future decisions (n+1) and total value for step 'n'
fn :: Int           -- ^ Step 'n', year
   -> Int           -- ^ State 'sn', total number of fabs created so far
   -> Int           -- ^ Decision 'xn', how many fabs to create this year
   -> ([Int], Int)    -- ^ (Decisions, Value)
fn n sn xn = (decisions, setupCostTotal n xn + value)
  where
    (decisions, value) = fnOpt (n+1) (sn+xn)

-- For states 1..4
possibleStates :: [[Int]]
possibleStates = [[0], [1,2,3], [2,3,4,5], [4,5]]

setupCostTotal :: Int -- ^ year 'n'
               -> Int -- ^ number of fabs 'x'
               -> Int -- ^ cost
setupCostTotal _ 0 = 0
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
