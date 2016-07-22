import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Printf
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- rgetl n toInt
  mapM_ solve (zip [1..] xs)

solve :: (Int,Int) -> IO ()
solve (i,j) = do
  let xs = take 10 . tail $ iterate msm j
  printf "Case %d:\n" i
  mapM_ print xs

msm :: Int -> Int
msm n = let n2 = n ^ 2 in n2 `div` 100 `mod` 10000

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine

tests :: [Test]
tests = [
  "test1" ~: 151 ~=? msm 123,
  "test2" ~: 228 ~=? msm 151,
  "test3" ~: 7725 ~=? msm 9888
  ]
