import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inputData []
  (putStr . intercalate "\n") $ map leapyear xs

solve :: Int -> Int
solve x = x + 100

inputData :: [(Int,Int)] -> IO [(Int,Int)]
inputData acc = do
  [s,t] <- getl $ split toInt
  if s == 0 && t == 0
    then return (reverse acc)
    else inputData ((s,t):acc)

leapyear :: (Int,Int) -> String
leapyear (s,t)
  | null cs = "NA\n"
  | otherwise = (unlines . map show) cs
  where
    ys = [s..t]
    f y = if y `mod` 400 == 0 then True else if y `mod` 100 == 0 then False else if y `mod` 4 == 0 then True else False
    cs = filter f ys
    
toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
