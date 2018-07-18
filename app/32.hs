{-# LANGUAGE TupleSections #-}
import Data.List ( nub )

--"39*186=7254"

-- # * #### = ####
-- ## * ### = ####

isPandigital :: [Int] -> Bool
isPandigital ns = let n = ns >>= show
                  in all ('0'/=) n && length n == length (nub n)

factors n = [ (multiplicand, multiplier) 
            | multiplier <- [1 .. floor (sqrt (fromIntegral n))]
            , let (multiplicand, m) = divMod n multiplier
            , m == 0 ]

digit = (1+) . floor . logBase 10 . fromIntegral

digitCheck (n,m) = digit n == 4 && digit m == 1 || digit n == 3 && digit m == 2

pan = filter isPandigital $ 
      map (\(x,(y,z)) -> [x,y,z]) $ 
      concatMap (\n -> map (n,) $ filter digitCheck $ factors n) [1000..9999]

main = print $ sum $ nub $ map head pan
