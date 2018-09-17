import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  [n, s] <- input (map read . words) :: IO [Int]
  when (n /= 0 || s /= 0) $ do print $ solve n s; main
   
solve :: Int -> Int -> Int
solve n s = length . filter (\x -> sum x == s) . filter (\x -> length x == n) $ filterM (\x -> [True,False]) [0..9]
 
input :: (String -> a) -> IO a
input f = f <$> getLine
