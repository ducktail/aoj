import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- lines <$> getContents :: IO [String]
  mapM_ print $ solve xs
 
solve :: [String] -> [Int]
solve = map (toAl 0 . map toInt)
 
toAl :: Int -> [Int] -> Int
toAl s [x] = s + x
toAl s (x:y:ys) | x < y = toAl (s - x) (y:ys)
                | otherwise = toAl (s + x) (y:ys)
 
toInt :: Char -> Int
toInt x | x == 'I' = 1
        | x == 'V' = 5
        | x == 'X' = 10
        | x == 'L' = 50
        | x == 'C' = 100
        | x == 'D' = 500
        | x == 'M' = 1000
