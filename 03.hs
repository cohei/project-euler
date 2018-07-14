import Control.Arrow

--main = print $ factors $ combine n

type Prime = Integer

n = 600851475143 :: Integer

combine :: Integer -> [(Integer,Prime)]
combine n = zip (repeat n) $ takeWhile ((n>=).(^2)) primes

factors :: [(Integer,Prime)] -> [Prime]
factors [] = []
factors ((n,p):ts) | n `mod` p == 0 = p : factors ts'
                   | otherwise      = factors ts
    where ts' = map (first (arr (`div`p))) ts
          ts'' = zip (map ((`div`2).fst) ts) (map snd ts) 

primes :: [Prime]
primes = 2 : sieve [3,5..]
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

factors2 :: Integer -> [Integer]
factors2 n = [x | x <- [1..n], n `mod` x == 0]

factorization :: Integer -> [Integer]
factorization 1 = []
factorization x = v : factorization (x `div` v)
    where v = (factors2 x) !! 1

main = print $ maximum $ factorization n