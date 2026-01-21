import Utility (toDigits)

frac = concatMap (toDigits 10) [1..]

proc = product $ map ((frac !!) . (subtract 1) . (10^)) [0..6]

main = print proc
