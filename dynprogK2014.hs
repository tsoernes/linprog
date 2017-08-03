{-# LANGUAGE TupleSections #-}

import           Data.List (maximumBy, minimumBy)
import           Data.Ord  (comparing)

-- 2014 Kont, Oppg4
-- Step: n = 1..5 : Week
-- State: s_n: Number of weeks of demand covered by inventory (including this week)
-- Decision: x_n: How many weeks of demand to cover with this weeks production
-- Transition: s_n+1 = s_n + x_n - 1
-- Base case(s)/edge conditions:

main :: IO ()
main = do
  -- let n_sn_pairs = concatMap (\(n, sns) -> map (n,) sns) $ zip [1..4] possibleStates
  -- mapM_  (uncurry popt) n_sn_pairs
  print $ fnOpt 1 1
  print ""


-- | Pretty the input and the corresponding optimal output
popt :: Int -> Int -> IO ()
popt n sn = print $ "n: " ++ show n ++ " sn: " ++ show sn ++ " dec,val: " ++ show (fnOpt n sn)


-- | Optimal decisions and corresponding value for step 'n' onwards
fnOpt :: Int        -- ^ Step 'n'
      -> Int        -- ^ State 'sn'
      -> ([Int], Int) -- ^ (Decisions, Value)
fnOpt 6 _  = ([], 0) -- Can't produce beds week 6
fnOpt n sn | sn <= 0 = ([], maxBound-99999999) -- If demand is not satisfied
fnOpt n sn = minimumBy (comparing snd) $ map (fn n sn) decisions
  where
    decisions = [0..(5-n)]


-- | Decisions and total value for step 'n'
fn :: Int           -- ^ Step 'n'
   -> Int           -- ^ State 'sn'
   -> Int           -- ^ Decision 'xn'
   -> ([Int], Int)  -- ^ (Decisions, Value)
fn n sn xn = (xn : decisions, value + prod_cost + hold_cost)
  where
    (decisions, value) = fnOpt (n + 1) (sn + xn - 1)
    n_inv = sum (map (demand n+) [0..sn-1])
    n_prod = sum (map (demand n+sn+) [1..xn])
    prod = if n_prod > 0 then 1 else 0
    prod_cost = 50000 * prod + 2000 * n_prod
    -- Only beds in inventory at start of the week have holding cost
    hold_cost = 100 * n_inv

demand :: Int -> Int
demand n
  | n == 1 = 100
  | n == 2 = 150
  | n == 3 = 150
  | n == 4 = 200
  | n == 5 = 200
  | otherwise = error $ "invalid input demand: " ++ show n
