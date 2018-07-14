import Data.List
import Data.Numbers.Primes

isTruncatablePrime p = all (`elem` (takeWhile (p/=) primes)) $ lefts p ++ rights p

primes' = filter ((\n -> n /= 1 && n /= 9) . (`mod` 10)) primes

main = print $ sum $ take 11 $ drop 4 $ filter isTruncatablePrime primes'

lefts = reverse . tail . takeWhile (0/=) . iterate (`div` 10)
-- 1234 -> [1,12,123]

rights n = takeWhile (n/=) $ map (n `mod`) [ 10^m | m <- [1..] ]
-- 1234 -> [4,34,234]
