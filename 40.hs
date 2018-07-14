-- -i..  してください
import Util

frac = concatMap digits [1..]

proc = product $ map ((frac !!) . (subtract 1) . (10^)) [0..6]

main = print proc
