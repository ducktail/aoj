import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- getl $ wrds toInt
  unless (all (== 0) xs) $ do
    print $ solve xs
    main

solve :: [Int] -> Int
solve [b,r,g,c,s,t] = 100 + 77 * b + 51 * r + 4 * g + (-1) * c + (-3) * k
  where
    k = t - b * 6 - r * 4 - g - c - s

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
