main :: IO ()
main = do
  print ""
  print $ fnOptAll 0


-- | Optimal decision and its value for each possible state for year 'n'
fnOptAll :: Int           -- ^ Year 'n'
         -> [(Int, Int)]  -- ^ For each possible state 0 through n; a decision and net cost pair
fnOptAll n = map (fnOpt n) [0..n]


-- | Optimal decision and its value for year 'n' given state
fnOpt :: Int        -- ^ Year 'n'
      -> Int        -- ^ State 'sn'
      -> (Int, Int) -- ^ (Keep/sell decision, Net cost/profit)
fnOpt 3 sn = (1, salesProfit sn)
fnOpt n sn = if keep > sell
 then (0, keep)
 else (1, sell)
  where
    keep = fn n sn 0
    sell  = fn n sn 1


-- | The net cost/profit for year 'n' and onwards; given state and decision
fn :: Int -- ^ Year 'n'
   -> Int -- ^ State 'sn', i.e. age of bike in years at the beginning of the year
   -> Int -- ^ Whether to sell old bike and buy a new one (1) or not (0)
   -> Int -- ^ Net cost/profit
fn n sn xn
  -- = if xn == 1
  --      then salesProfit sn + bikeCost + maintenanceCost 0 + snd (fnOpt (n+1) 1)
  --      else maintenanceCost sn + snd (fnOpt (n+1) (sn+1))
  = xn * salesProfit sn
  - xn * bikeCost
  - maintenanceCost sn'
  + snd (fnOpt (n+1) (sn'+1))
    where
      sn' = (1-xn) * sn -- Age of bike which is 0 if a new bike is bought


bikeCost :: Int
bikeCost = 7600


salesProfit :: Int -> Int
salesProfit age
  | age <  1 = 0 -- error $ "can't sell bike " ++ show age ++ " years after buying it"
  | age == 1 = 5000
  | age == 2 = 2800
  | age == 3 = 1200
  | age >  3 = 0


maintenanceCost :: Int -> Int
maintenanceCost age
  | age <  0 = error $ "can't have negative age: " ++ show age
  | age == 0 = 200
  | age == 1 = 400
  | age == 2 = 800
  | age == 3 = 1200
  | age == 4 = 2400
  | age == 5 = 3200
  | age >= 6 = error $ "can't keep a bike this long: " ++ show age
