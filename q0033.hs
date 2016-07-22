import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- readLn :: IO Int
  xs <- input n (map read . words) :: IO [[Int]]
  mapM_ putStrLn $ solve xs
 
solve :: [[Int]] -> [String]
solve zs = map (f [] []) zs
  where 
    f _ _ [] = "YES"
    f [] [] (x:xs) = f [x] [] xs
    f [] (r:rs) (x:xs) = if r < x then f [] (x:r:rs) xs else f [x] (r:rs) xs
    f (l:ls) [] (x:xs) = if l < x then f (x:l:ls) [] xs else f (l:ls) [x] xs
    f (l:ls) (r:rs) (x:xs) | l > x && r > x = "NO"
                           | l < x && x < r = f (x:l:ls) (r:rs) xs
                           | r < x && x < l = f (l:ls) (x:r:rs) xs
                           | l < r && r < x = f (l:ls) (x:r:rs) xs
                           | otherwise = f (x:l:ls) (r:rs) xs
 
input :: Int -> (String -> a) -> IO [a]
input n f = map f <$> replicateM n getLine
