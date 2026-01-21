{-# OPTIONS_GHC -i.. #-} -- きかないのでghciで :set -i.. して
import Data.List (nub, maximum)

import Utility (toDigits)

{-
"99" > "987" == True

1けたでは
concatnatedProduct 1 [1..9] = [1,2,3,4,5,6,7,8,9]
concatnatedProduct 9 [1..5] = [9,1,8,2,7,3,6,4,5]
のみ
2けたではn=4,3
3けたではn=3
4けたではn=2
n>1から
5けた以上はない
-}

concatnatedProduct :: Integer -> [Integer] -> [Integer]
concatnatedProduct n ns = concat $ map (toDigits 10) $ map (n*) ns

products = [ concatnatedProduct n ns | n <- [10..9999], ns <- [ [1..m] | m <- [1..4] ] ]

isPandigital ns = ns == nub ns

products' = filter isPandigital $
            filter ((9==) . length) $
            filter (0 `notElem`) products

main = putStrLn $ concatMap show $ maximum products'
