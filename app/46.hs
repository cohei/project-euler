import Data.Numbers.Primes

composites = step [9,11..]  primes

step oos@(o:os) pps@(p:ps) 
    | o == p = step os ps
    | o >  p = step oos ps
    | o <  p = o : step os pps

isPSS n = any p $ takeWhile (2<=) (map (n-) primes)
    where p c = let m = sqrt (fromIntegral c / 2)
                in fromIntegral (floor m) == m

main = print $ head $ dropWhile isPSS composites
