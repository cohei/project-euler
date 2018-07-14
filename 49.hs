import Data.Function
import Data.Numbers.Primes
import Data.List
import Data.Ord
import Util

primes' = takeWhile (10000>) $ dropWhile (1000>) primes -- 4 digits

part = groupBy ((==) `on` f) . sortBy (comparing f)
    where f = sort . digits

defor = filter ((3<=) . length)

tup3 ns = [ (x,y,z) | x <- ns, y <- ns, z <- ns, x < y, y < z ]

p (x,y,z) = z-y == y-x

tri2ls (x,y,z) = [x,y,z]

main = putStrLn $ concatMap show $ tri2ls $ 
       head $ filter ((1487,4817,8147)/=) $ filter p $ concatMap tup3 $ defor $ part primes'
