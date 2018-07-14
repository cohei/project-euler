triplet = [(a,b,c) | a <- [1..500], b <- [a..999], let c = 1000-a-b, c >= 1, a+b > c, a^2+b^2==c^2]

abc (a,b,c) = a*b*c

main = print $ abc $ head $ triplet