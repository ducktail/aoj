import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,k] <- getl $ wrds toInt
  unless (n == 0 && k == 0) $ do
    solve <$> getl (wrds toInt) <*> replicateM n (getl $ wrds toInt) >>= putStrLn
    main

solve :: [Int] -> [[Int]] -> String
solve ss bs
  | and (zipWith (>=) ss (map sum . transpose $ bs)) = "Yes"
  | otherwise = "No"

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
