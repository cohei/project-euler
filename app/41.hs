import Data.List (permutations)
import Data.Numbers.Primes (primes)

pan n = map (foldl1 ((+) . (10*))) $ permutations [1..n]

main = print $ maximum $ filter isPrime $ concatMap pan [1..9]

isPrime n = all (0/=) $ map (n `mod`) ps
    where n2 = floor $ sqrt $ fromIntegral n
          ps = takeWhile (n2>=) primes
