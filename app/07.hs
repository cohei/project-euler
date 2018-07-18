primes = 2 : sieve [3,5..]
    where sieve (p:ns) = p : sieve [n | n <- ns, mod n p /= 0]

main = print $ primes !! 10000

-- 104743