import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.IntMap as IM
import Text.Printf
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n toDbl
  mapM_ disp $ solve xs

solve :: [Double] -> [(Int,Int)]
solve xs = IM.assocs $ count xs

disp :: (Int,Int) -> IO ()
disp (i,j) = printf "%d:%s\n" i (replicate j '*')

judge :: Double -> Int
judge x
  | x < 165.0 = 1
  | x < 170.0 = 2
  | x < 175.0 = 3
  | x < 180.0 = 4
  | x < 185.0 = 5
  | otherwise = 6

count :: [Double] -> IM.IntMap Int
count = foldl f imp
  where
    imp = IM.fromList [(i,0)| i <- [1..6]]
    f mp h = IM.insertWith (+) (judge h) 1 mp

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
  "test1" ~: IM.fromList [(1,0),(2,1),(3,2),(4,0),(5,1),(6,0)] ~=? count [183,172,166,174],
  "test2" ~: [(1,0),(2,1),(3,2),(4,0),(5,1),(6,0)] ~=? solve [183,172,166,174]
  ]
