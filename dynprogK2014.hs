{-# LANGUAGE TupleSections #-}

import           Data.List (maximumBy, minimumBy)
import           Data.Ord  (comparing)

-- 2014 Kont, Oppg4
-- Step: n = 1..5 : Week
-- State: s_n: Number of beds in inventory
-- Decision: x_n: How many weeks of demand to cover with this weeks production
-- Transition: s_n+1 = s_n - demand(n) + demand (n+1) + demand (n+2) .. + demand (n+xn)
-- Base case(s)/edge conditions:

main :: IO ()
main = do
  -- let n_sn_pairs = concatMap (\(n, sns) -> map (n,) sns) $ zip [1..4] possibleStates
  -- mapM_  (uncurry popt) n_sn_pairs
  print $ fnOpt 1 100
  print ""


-- | Pretty the input and the corresponding optimal output
popt :: Int -> Int -> IO ()
popt n sn = print $ "n: " ++ show n ++ " sn: " ++ show sn ++ " dec,val: " ++ show (fnOpt n sn)


-- | Optimal decisions and corresponding value for step 'n' onwards
fnOpt :: Int        -- ^ Step 'n'
      -> Int        -- ^ State 'sn'
      -> ([Int], Int) -- ^ (Decisions, Value)
fnOpt 6 _  = ([], 0) -- Can't produce beds week 6
fnOpt n sn | sn < demand n = ([], maxBound-9999999) -- If demand is not satisfied
fnOpt n sn = minimumBy (comparing snd) $ map (fn n sn) decisions
  where
    decisions = [0..(6-n)]


-- | Decisions and total value for step 'n'
fn :: Int           -- ^ Step 'n'
   -> Int           -- ^ State 'sn'
   -> Int           -- ^ Decision 'xn'
   -> ([Int], Int)  -- ^ (Decisions, Value)
fn n sn xn = (xn : decisions, value + prod_cost + hold_cost)
  where
    (decisions, value) = fnOpt (n+1) $ transition n sn xn
    nBeds' = nBeds n xn
    prod = if nBeds' > 0 then 1 else 0
    prod_cost = 50000 * prod + 2000 * nBeds'
    -- Only beds in inventory at start of the week have holding cost
    hold_cost = 100 * sn

transition :: Int -> Int -> Int -> Int
transition n sn xn = sn - demand n + nBeds n xn

nBeds n xn = sum (map (demand n+) [1..xn])

demand :: Int -> Int
demand n
  | n == 1 = 100
  | n == 2 = 150
  | n == 3 = 150
  | n == 4 = 200
  | n == 5 = 200
  | otherwise = error $ "invalid input demand: " ++ show n
