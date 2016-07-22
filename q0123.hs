import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- getc $ solve . split toDbl
  mapM_ putStrLn xs

solve :: [Double] -> String
solve [t1,t2] = judge t1 t2

judge :: Double -> Double -> String
judge t1 t2
  | t1 < 35.5 && t2 < 71.0 = "AAA"
  | t1 < 37.5 && t2 < 77.0 = "AA"
  | t1 < 40.0 && t2 < 83.0 = "A"
  | t1 < 43.0 && t2 < 89.0 = "B"
  | t1 < 50.0 && t2 < 105.0 = "C"
  | t1 < 55.0 && t2 < 116.0 = "D"
  | t1 < 70.0 && t2 < 148.0 = "E"
  | otherwise = "NA"

toDbl :: String -> Double
toDbl s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents


tests :: [Test]
tests = [
  "judge1" ~: "AAA" ~=? judge 23.4 70.0,
  "judge2" ~: "AA" ~=? judge 25.4 71.1,
  "judge3" ~: "A" ~=? judge 38.4 70.0,
  "judge4" ~: "B" ~=? judge 41.9 85.2,
  "judge5" ~: "C" ~=? judge 48.0 80.0,
  "judge6" ~: "D" ~=? judge 52.5 110.0,
  "judge7" ~: "E" ~=? judge 63.3 137.7,
  "judge8" ~: "NA" ~=? judge 20.6 150.7
  ]
