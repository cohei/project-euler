import Data.Char
import Data.List

file = readFile "words42.txt" 

exec :: String -> [String]
exec = map read . words . map f

f ',' = ' '
f x = x

str2num = sum . map (subtract n . ord)
    where n = ord 'A' - 1

tri = map (\n -> n * (n+1) `div` 2) [1..]

main = do fi <- file
          let ws = exec fi
              max = (26*) $ maximum $ map length ws
              tri' = takeWhile (max>=) tri 
          print $ length $ filter (`elem` tri') $ map str2num ws
