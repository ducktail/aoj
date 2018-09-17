import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [r,c] <- getl (wrds toInt)
  unless (r == 0 && c == 0) $ do
    putStrLn $ solve r c
    main

solve :: Int -> Int -> String
solve r c
  | odd r && odd c = "no"
  | otherwise = "yes"

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
