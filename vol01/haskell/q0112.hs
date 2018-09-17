import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- rgetl n toInt
    print $ solve xs
    main
    
solve :: [Int] -> Int
solve xs = sum . zipWith (*) [0 .. length xs - 1] . sortBy (\a b -> b `compare` a) $ xs

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine

tests :: [Test]
tests = [
  "solve1" ~: 0 ~=? solve [33],
  "solve2" ~: 2 ~=? solve [55,2],
  "solve3" ~: 31 ~=? solve [2,6,4,3,9]
  ]
