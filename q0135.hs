import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n $ map toDbl . splitOn ":"
  mapM_ putStrLn $ solve xs

solve :: [[Double]] -> [String]
solve = map f
  where
    f [h,m]
      | a < 30.0 = "alert"
      | a < 90.0 = "warning"
      | otherwise = "safe"
      where a = diffAng h m

lha :: Double -> Double
lha m = m * 6

sha :: Double -> Double -> Double
sha h m = h * 30 + m * 0.5

diffAng :: Double -> Double -> Double
diffAng h m = let a = abs (la - sa) in min a (360 - a)
  where
    la = lha m
    sa = sha h m

toInt :: String -> Int
toInt s = read s

toDbl :: String -> Double
toDbl s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine


tests :: [Test]
tests = [
  "diffAng1" ~: 15 ~=? diffAng 6 30,
  "diffAng2" ~: 142.5 ~=? diffAng 10 15
  ]
