import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  [y1,m1,d1,y2,m2,d2] <- getl $ split toInt
  when (y1 > 0 && m1 > 0 && d1 > 0 && y2 > 0 && m2 > 0 && d2 > 0) $ do
    print $ solve (y1,m1,d1) (y2,m2,d2)
    main

solve :: YMD -> YMD -> Int
solve (y1,m1,d1) (y2,m2,d2) = ytod y1 y2 + yday (y2,m2,d2) - yday (y1,m1,d1)

leapyear :: Int -> Bool
leapyear y
  | y `mod` 400 == 0 = True
  | y `mod` 100 == 0 = False
  | y `mod` 4 == 0 = True
  | otherwise = False

yday :: YMD -> Int
yday (y,m,d)
  | leapyear y = d + (sum . take m) ms2
  | otherwise = d + (sum . take m) ms1
  where
    ms1 = [0,31,28,31,30,31,30,31,31,30,31,30]
    ms2 = [0,31,29,31,30,31,30,31,31,30,31,30]

ytod :: Int -> Int -> Int
ytod y1 y2 = (sum . map (\x -> if leapyear x then 366 else 365)) [y1 .. y2 - 1]

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

type YMD = (Int,Int,Int)

tests :: [Test]
tests = [
  "leapyear1" ~: True ~=? leapyear 2000,
  "leapyear2" ~: False ~=? leapyear 2005,
  "yday1" ~: 365 ~=? yday (2001,12,31),
  "yday2" ~: 1 ~=? yday (2005,1,1),
  "yday3" ~: 74 ~=? yday (2100,3,15),
  "yday4" ~: 366 ~=? yday (2008,12,31),
  "yday5" ~: 1 ~=? yday (2400,1,1),
  "yday6" ~: 111 ~=? yday (2800,4,20),
  "ytod1" ~: 0 ~=? ytod 2010 2010,
  "ytod2" ~: 365 ~=? ytod 2011 2012,
  "ytod3" ~: 1096 ~=? ytod 2012 2015,
  "ytod4" ~: 4018 ~=? ytod 1999 2010,
  "solve1" ~: 1 ~=? solve (2006,9,2) (2006,9,3),
  "solve2" ~: 70 ~=? solve (2006,9,2) (2006,11,11),
  "solve3" ~: 366 ~=? solve (2004,1,1) (2005,1,1),
  "solve4" ~: 2192 ~=? solve (2000,1,1) (2006,1,1),
  "solve5" ~: 36890 ~=? solve (2000,1,1) (2101,1,1)
  ]
