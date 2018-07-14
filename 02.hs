main = print . sum . takeWhile (4000000>=) . filter even $ fib

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)