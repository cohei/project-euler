import Data.List

main = print $ sum $ unfoldr f (2^1000)

f n | n == 0    = Nothing
    | otherwise = Just $ ex $ divMod n 10

ex (x,y) = (y,x)