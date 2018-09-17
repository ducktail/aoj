import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  xs <- getc $ splitOn ","
  mapM_ print $ solve xs

solve :: [[String]] -> [Int]
solve = map (toInt . head) . filter f
  where
    f [_,sw,sh] = bmi w h >= 25.0
      where
        w = toDbl sw
        h = toDbl sh

bmi :: Double -> Double -> Double
bmi w h = w / (h ^ 2)
  
toInt :: String -> Int
toInt s = read s

toDbl :: String -> Double
toDbl s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
