main = print $ sum $ take 6 $ filter p [2..354294]

g 0 = []
g n = let (a,r) = divMod n 10
      in r : g a

p n = n == sum (map (^5) $ g n)



f n = concat [ show n, " digit ", replicate n '9', "\n9^5 * ", show n, " = "
             , show $ 9^5*n, "\n" ]

findUpper = putStr $ [1..10] >>= f
{-
この数の上界は354294

1 digit 9
9^5 * 1 = 59049
2 digit 99
9^5 * 2 = 118098
3 digit 999
9^5 * 3 = 177147
4 digit 9999
9^5 * 4 = 236196
5 digit 99999
9^5 * 5 = 295245
6 digit 999999
9^5 * 6 = 354294
7 digit 9999999
9^5 * 7 = 413343
8 digit 99999999
9^5 * 8 = 472392
9 digit 999999999
9^5 * 9 = 531441
10 digit 9999999999
9^5 * 10 = 590490
-}