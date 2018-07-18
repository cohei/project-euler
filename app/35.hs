import Data.Numbers.Primes
import Data.List

circulars xs = init $ zipWith (++) (tails xs) (inits xs)

ps = map show $ takeWhile (<10^6) primes

isCircularPrime x = all (`elem` ps) $ tail $ circulars x

main = print $ length $ filter isCircularPrime ps
