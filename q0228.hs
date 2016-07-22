import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == (-1)) $ do
    solve <$> replicateM n (getl toInt) >>= mapM_ putStrLn
    main

solve :: [Int] -> [String]
solve xs = zipWith h (10:xs) xs
  where
    ms = ["0111111","0000110","1011011","1001111","1100110",
          "1101101","1111101","0100111","1111111","1101111","0000000"]
    f x y
      | x == y = '0'
      | otherwise = '1'
    g x y = zipWith f x y
    h x y = g (ms !! x) (ms !! y)
    
toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
