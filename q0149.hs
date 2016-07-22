import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)
import qualified Data.IntMap as IM

main :: IO ()
main = do
  xs <- getc $ split toDbl
  mapM_ (\(x,y) -> putStrLn $ show x ++ " " ++ show y) $ solve xs

solve :: [[Double]] -> [(Int,Int)]
solve xs = zip (IM.elems lm) (IM.elems rm)
  where
    ls = map (\[x,y] -> (eyetest x, eyetest y)) xs
    (lm,rm) = foldl (\(lmp,rmp) (l,r) -> (IM.insertWith (+) l 1 lmp, IM.insertWith (+) r 1 rmp)) (IM.fromList [(0,0),(1,0),(2,0),(3,0)], IM.fromList [(0,0),(1,0),(2,0),(3,0)]) ls
      
eyetest :: Double -> Int
eyetest x
  | x < 0.2 = 3
  | x < 0.6 = 2
  | x < 1.1 = 1
  | otherwise = 0

toDbl :: String -> Double
toDbl s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
