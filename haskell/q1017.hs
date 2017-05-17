import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  getc solve >>= mapM_ print

solve :: [String] -> [Int]
solve [] = []
solve (x:y:ys) = (last $ foldl rsf [0..n-1] cs) : solve ys
  where
    [n,r] = map toInt $ words x
    cs = map toInt $ words y
    m = n `div` 2
    rsf :: [Int] -> Int -> [Int]
    rsf xs c = f (drop m xs) (take m xs) []
      where
        f :: [Int] -> [Int] -> [Int] -> [Int]
        f as bs cs
          | null as && null bs = cs
          | otherwise = f (drop c as) (drop c bs) (cs ++ (take c as) ++ (take c bs))
        
toInt :: String -> Int
toInt = read

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
