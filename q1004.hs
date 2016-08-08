import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getc (map toInt) >>= mapM_ print

solve :: [Int] -> [Int]
solve = map f
  where
    f n
      | n == 1 = 0
      | n == 2 = 0
      | n == 3 = 1
      | even n = if isPn (n-1) then 2 else 0
      | otherwise = sum $ zipWith (\a b -> if isPn a && isPn b then 1 else 0) ls (reverse ls)
      where
        ls = [1,3..n]

toInt :: String -> Int
toInt = read

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents

pns :: [Int]
pns = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101]

isPn :: Int -> Bool
isPn 1 = False
isPn x = all ((/= 0) . mod x) $ takeWhile ((<= x) . (^2)) pns
