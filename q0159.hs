import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl $ wrds toInt) >>= print
    main
    
solve :: [[Int]] -> Int
solve = fst . minimumBy cmpBMI . map bmi

bmi :: [Int] -> (Int, Double)
bmi [i, h, w] = (i, (10000 * fromIntegral w) / (fromIntegral h ^ 2))

cmpBMI :: (Int,Double) -> (Int,Double) -> Ordering
cmpBMI (i1, b1) (i2, b2)
  | bb1 == bb2 = compare i1 i2
  | otherwise = compare bb1 bb2
  where
    bb1 = abs (b1 - 22)
    bb2 = abs (b2 - 22)

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

tests :: [Test]
tests = [
  "test1" ~: (1001,40.32) ~=? bmi [1001, 123, 61],
  "test2" ~: LT ~=? cmpBMI (100, 123.4) (1000, 123.5),
  "test3" ~: GT ~=? cmpBMI (300, 123.5) (200, 123.5),
  "test4" ~: (1001, 112.3) ~=?  minimumBy cmpBMI [(300, 123.5), (200, 123.5), (1001, 112.3)],
  "test5" ~: (200, 123.5) ~=?  minimumBy cmpBMI [(300, 123.5), (200, 123.5), (1001, 152.3)],
  "test6" ~: (2100, 22.2) ~=?  minimumBy cmpBMI [(300, 123.5), (200, 123.5), (1001, 152.3), (2000, 20.3), (2100, 22.2)]
  ]
