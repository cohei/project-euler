import Data.List

isPalindrome xs = xs == reverse xs

ex (x,y) = (y,x)
digits base = unfoldr (\n -> if n == 0 then Nothing else Just (ex (divMod n base)))

main = print $ sum $ filter (\n -> all isPalindrome [digits 2 n, digits 10 n]) [1..10^6-1]
