import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  unwords . map show . solve <$> getl (wrds toInt) >>= putStrLn
  unwords . map show . solve <$> getl (wrds toInt) >>= putStrLn
  unwords . map show . solve <$> getl (wrds toInt) >>= putStrLn

solve :: [Int] -> [Int]
solve [h1,m1,s1,h2,m2,s2] = [dt `div` 3600, dt `mod` 3600 `div` 60, dt `mod` 60]
  where
    dt = 3600 * (h2 - h1) + 60 * (m2 - m1) + s2 - s1

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
