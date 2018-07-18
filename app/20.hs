import Data.List (unfoldr)

main = print $ sum $ unfoldr f $ product [1..100]

f n | n == 0    = Nothing
    | otherwise = Just $ ex $ divMod n 10

ex (x,y) = (y,x)