import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getl (wrds toInt) >>= print

solve :: [Int] -> Int
solve = f . (>= 1000) . sum . zipWith (*) [1,5,10,50,100,500]
  where f True = 1
        f False = 0

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
