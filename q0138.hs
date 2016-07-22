import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- rgetl 8 words
  ys <- rgetl 8 words
  zs <- rgetl 8 words
  mapM_ (putStrLn . unwords) $ solve xs ys zs

solve :: [[String]] -> [[String]] -> [[String]] -> [[String]]
solve xs ys zs = x1 ++ y1 ++ z1 ++ rs
  where
    (x1,x2) = top2 xs
    (y1,y2) = top2 ys
    (z1,z2) = top2 zs
    (rs,_) = top2 $ x2 ++ y2 ++ z2
    
top2 :: [[String]] -> ([[String]],[[String]])
top2 xs = (take 2 sxs, drop 2 sxs)
  where
    sxs = sortBy (compare `on` (toDbl . (!!1))) xs

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
  "test1" ~: ([["5", "22.88"],["3", "23.00"]],[["10", "24.79"],["18", "25.46"],["16", "26.23"]]) ~=? top2 [["18", "25.46"],["16", "26.23"],["3", "23.00"],["10", "24.79"],["5", "22.88"]]
  ]
