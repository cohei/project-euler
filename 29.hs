import Data.List ( nub )

ns = [2..100]

main = print $ length $ nub [ x^y | x <- ns, y <- ns ]