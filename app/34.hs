import Data.List

digits = unfoldr (\n -> if n == 0 then Nothing else Just (ex (divMod n 10)))

ex (x,y) = (y,x)

fac 0 = 1
fac 1 = 1
fac 2 = 2
fac 3 = 6
fac 4 = 24
fac 5 = 120
fac 6 = 720
fac 7 = 5040
fac 8 = 40320
fac 9 = 362880

fac' n = product [1..n]

p n = n == sum (map fac (digits n))

main = print $ sum $ take 2 $ drop 2 $ filter p [1..]
