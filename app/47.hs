import Data.List
import Data.Numbers.Primes (primes)

primeFactors 1 = []
primeFactors n = smallest : primeFactors (div n smallest)
    where smallest = head $ filter ((0==) . (n `mod`)) primes

factorsOf n m = n == length (nub (primeFactors m))

f n xs = filter (all (factorsOf n)) $ transpose $ take n $ tails xs

main = print $ head $ head $ f 4 [1..]
