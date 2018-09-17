import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- map (map read . words) . lines <$> getContents
  mapM_ putStrLn $ map (\[x,y] -> show x ++ " " ++ show y) $ map gl xs
 
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd b ( a `mod` b )
 
gl :: [Int] -> [Int]
gl [a, b] = [g, a * b `div` g]
  where
    g = gcd' a b
