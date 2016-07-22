import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Ratio

main :: IO ()
main = do
  n <- getl toDbl
  unless (n < 0) $ do
    putStrLn . solve $ n
    main

solve :: Double -> String
solve n = g $ foldl f (toRational n,"") bs
  where
    bs = [128%1,64%1,32%1,16%1,8%1,4%1,2%1,1%1,0%1,1%2,1%4,1%8,1%16]
    f (t,xs) m
      | m == 0 = (t, '.':xs)
      | t >= m = (t - m, '1':xs)
      | otherwise = (t, '0':xs)
    g (t,xs)
      | t > 0%1 = "NA"
      | otherwise = reverse xs
    
toDbl :: String -> Double
toDbl = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
