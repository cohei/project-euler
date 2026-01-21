import Data.Bool.Extras ( bool )
import Data.List (group, sort)
import Data.Numbers.Primes

import Utility (toDigits, fromDigits)

-- 同じ数字の桁の数字を換えるため、
-- 1の位は換えない。なぜなら、素数であるためには奇数でなくてはならず、
-- 8種類を確保できない。
-- 取り替える桁が1つ、2つということもない。換えるたびにmodulo 3が動くために
-- 10個中3または4つが3の倍数となり、8種類を確保できない。
-- 取り替える桁数が3の倍数のときのみ、ずっと3の倍数にならない。
-- 6桁の取り替えもありえるが、ここでは3桁を取り替えるように仮定する。
-- 1110からスタートできる。

type Digits = [Int]

hasSame3Digits :: Int -> Bool -- which is smaller than 3
hasSame3Digits = elem 3 . map length . group . sort . filter (2 >=) .
                 init . toDigits 10

replace3Digits :: Int -> [Int]
replace3Digits n
    | d > 2     = []
    | otherwise = n : map (\to -> fromDigits 10 $ replace d to ns) [d+1..9]
    where
      ns = toDigits 10 n
      d = head . head . filter ((3==) . length) . group . sort . init $ ns

replace :: Int -> Int -> Digits -> Digits
replace from to = map (\n -> bool n to $ from == n)

primesWith3Digits :: [Int]
primesWith3Digits = filter hasSame3Digits . dropWhile (1110 >) $ primes

hasProperReplacement :: Int -> Bool
hasProperReplacement = (8 <=) . length . filter isPrime . replace3Digits

main :: IO ()
main = print . head . filter hasProperReplacement $ primesWith3Digits
-- == 121313
