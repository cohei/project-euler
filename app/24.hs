import Data.List ( permutations, sort )

main = putStrLn $ show =<< (sort $ permutations [0..9]) !! (1000000-1)

fact = product . enumFromTo 1
{-
fact 9 * 2 + fact 8 * 6 + fact 7 * 6 + fact 6 * 2 + fact 5 * 5 + fact 4 * 1 + fact 3 * 2 + fact 2 * 2

2662512200

0123456789

2783915604
-> 2783915460
-}