import Data.List

c :: Integral a => a -> a -> a
c n r = fact n `div` (fact r * fact (n-r))

fact 0 = 1
fact n = product [1..n]

main = print . length $ [ undefined | n <- [1..100], r <- [0..n], c n r > 10^6 ]
-- 4075

