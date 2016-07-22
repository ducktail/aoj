import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- getc $ solve . split toInt
  mapM_ putStrLn xs

solve :: [Int] -> String
solve [y,m,d]
  | g == "pre-meiji" = g
  | g == "meiji" = g ++ " " ++ show (y - 1867) ++ " " ++ show m ++ " " ++ show d
  | g == "taisho" = g ++ " " ++ show (y - 1911) ++ " " ++ show m ++ " " ++ show d
  | g == "showa" = g ++ " " ++ show (y - 1925) ++ " " ++ show m ++ " " ++ show d
  | otherwise = g ++ " " ++ show (y - 1988) ++ " " ++ show m ++ " " ++ show d
  where
    g = gengo y m d

gengo y m d
  | y < 1868 = "pre-meiji"
  | y == 1868 && m < 9 = "pre-meiji"
  | y == 1868 && m == 9 && d < 8 = "pre-meiji"
  | y < 1912 = "meiji"
  | y == 1912 && m < 7 = "meiji"
  | y == 1912 && m == 7 && d < 30 = "meiji"
  | y < 1926 = "taisho"
  | y == 1926 && m < 12 = "taisho"
  | y == 1926 && m == 12 && d < 25 = "taisho"
  | y < 1989 = "showa"
  | y == 1989 && m == 1 && d < 8 = "showa"
  | otherwise = "heisei"

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
