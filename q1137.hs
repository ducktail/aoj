import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n (words <$> getLine) >>= mapM_ putStrLn

solve :: [[String]] -> [String]
solve = map (toMCXI . sum . map fromMCXI)

fromMCXI :: String -> Int
fromMCXI = fst . foldl f (0, 1)
  where
    f (a, i) c
      | c == 'm' = (a + i * 1000, 1)
      | c == 'c' = (a + i * 100, 1)
      | c == 'x' = (a + i * 10, 1)
      | c == 'i' = (a + i, 1)
      | otherwise = (a, digitToInt c)

toMCXI :: Int -> String
toMCXI n = concat $ zipWith ($) (map f "mcxi") (map g [3,2,1,0])
  where
    f c i
      | i == 0 = []
      | i == 1 = [c]
      | otherwise = [intToDigit i, c]
    g i = n `div` (10 ^ i) `mod` 10

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
