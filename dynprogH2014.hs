{-# LANGUAGE TupleSections #-}

import           Data.List (maximumBy, minimumBy)
import           Data.Ord  (comparing)

-- 2014, Oppg4
-- Step: n = 1..5 : Order
-- State: s_n: Number of weeks left available for orders
-- Decision: x_n: Whether or not to accept order n
-- Transition: s_n+1 = s_n - x_n * duration(n)
-- Base case(s)/edge conditions: See `fnOpt 6 _` and `fnOpt _ sn` for `sn < 0`

main :: IO ()
main = do
  -- let n_sn_pairs = concatMap (\(n, sns) -> map (n,) sns) $ zip [1..4] possibleStates
  -- mapM_  (uncurry popt) n_sn_pairs
  print $ fnOpt 1 4
  print ""


-- | Pretty the input and the corresponding optimal output
popt :: Int -> Int -> IO ()
popt n sn = print $ "n: " ++ show n ++ " sn: " ++ show sn ++ " dec,val: " ++ show (fnOpt n sn)


-- | Optimal decisions and corresponding value for step 'n' onwards
fnOpt :: Int        -- ^ Step 'n'
      -> Int        -- ^ State 'sn'
      -> ([Int], Int) -- ^ (Decisions, Value)
fnOpt _ sn | sn < 0 = ([], minBound) -- Using more time than available gives infinitely bad solution
fnOpt 6 _  = ([], 0) -- Can't take orders week 6
fnOpt n sn = maximumBy (comparing snd) $ map (fn n sn) [0, 1]


-- | Decisions and total value for step 'n'
fn :: Int           -- ^ Step 'n'
   -> Int           -- ^ State 'sn'
   -> Int           -- ^ Decision 'xn'
   -> ([Int], Int)  -- ^ (Decisions, Value)
fn n sn xn = (xn : decisions, (xn * profit n) + value)
  where
    (decisions, value) = fnOpt (n+1) (sn - (xn * duration n))

profit :: Int -> Int
profit n
  | n == 1 = 8
  | n == 2 = 36
  | n == 3 = 41
  | n == 4 = 25
  | n == 5 = 19
  | otherwise = error $ "invalid input profit: " ++ show n


duration :: Int -> Int
duration n
  | n == 1 = 1
  | n == 2 = 2
  | n == 3 = 3
  | n == 4 = 2
  | n == 5 = 1
  | otherwise = error $ "invalid input duration: " ++ show n
