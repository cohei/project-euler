-- 1 Jan 1900 Mon
-- 1 Jan 1901 to 31 Dec 2000

data WeekDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq,Ord,Enum,Show)

weekDay :: [WeekDay]
weekDay = cycle [Sun .. Sat]

weekDayFromMon = tail weekDay

monthDay :: [Int]
monthDay = [31,28,31,30,31,30,31,31,30,31,30,31]

monthDay_leap = let x:y:zs = monthDay in x:(y+1):zs

yearToMonthDay n = if isLeap n then monthDay_leap else monthDay

century :: [Int]
century = [1900..2000]

isLeap :: Int -> Bool
isLeap n = mod n 4 == 0 && mod n 100 /= 0 || mod n 400 == 0

flagTop n = True : replicate (n-1) False

sunAndMonthStart (x,y) = x == Sun && y == True

main = print $ length $ filter sunAndMonthStart $ drop 365 
     $ zip weekDayFromMon $ century >>= yearToMonthDay >>= flagTop
