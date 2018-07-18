{-
perimeter 周囲の長さ
right angle triangle 直角三角形
-}

import Data.List
import Data.Ord
import Data.Function

pyth = [ [sum,x,y,z] | x <- [1..500], y <- [1..x], z <- [1..y]
       , x^2==y^2+z^2, let sum = x+y+z, sum <= 1000 ]
-- 周が1000までなので、一番長い辺x も500弱まで (499,498,1)みたいな。

main = print $ head $ head $ maximumBy (comparing length) $
       groupBy ((==) `on` head) $ sortBy (comparing head) pyth
