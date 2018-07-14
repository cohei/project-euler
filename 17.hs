oneTo9 = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tenTo19 = ["ten","eleven", "twelve", "thirteen", "fourteen", "fifteen",
           "sixteen", "seventeen", "eighteen", "nineteen"]

twentyTo90by10 = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

oneTo99 = oneTo9 ++ tenTo19 ++ [x ++ y | x <- twentyTo90by10, y <- ([] : oneTo9)]

x01Tox99 = map ("and"++) oneTo99

oneTo999 = oneTo99 ++ [x ++ "hundred" ++ y | x <- oneTo9, y <- ([] : x01Tox99)]

main = print $ length $ concat oneTo999 ++ "onethousand"