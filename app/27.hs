{-# language TupleSections #-}
import Data.Numbers.Primes ( primes )
import Data.List ( maximumBy )
import Data.Function ( on )
{-
n^2 + a n + b |a| < 1000, |b| < 1000
nは0からなので、bは1000未満に限られる。168個
aは-2*sqrt(b)より大きい。
-}

isPrime n = if n <= 1 
            then False 
            else let sn = floor $ sqrt $ fromIntegral n
                 in all (\x -> mod n x /= 0) $ takeWhile (sn >=) primes

f a b n = n^2 + a*n + b

bs = takeWhile (1000 >) primes

check a b = length $ takeWhile isPrime $ map (f a b) [1..]

asbs = concatMap as bs

as b = map (,b) [lower..999]
    where lower = truncate $ negate $ 2 * sqrt (fromIntegral b)

main = print $ uncurry (*) $ fst $ maximumBy (compare `on` snd) $ zip asbs $ map (uncurry check) asbs