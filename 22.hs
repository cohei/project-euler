import Data.Char ( ord )
import Data.List ( sort )

main :: IO ()
main = print . sum . zipWith (*) [1..] . map worth . sort . read . ('[':) . (++"]") =<< readFile "names.txt"
                
worth :: String -> Int
worth = sum . map ((subtract 64) . ord)
