import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  t <- getl toInt
  unless (t == 0) $ do
    n <- getl toInt
    solve t <$> replicateM n (getl $ wrds toInt) >>= putStrLn
    main

solve :: Int -> [[Int]] -> String
solve t xs
  | t <= rt = "OK"
  | otherwise = show $ t - rt
  where
    rt = sum $ map (\[s,f] -> f - s) xs
    
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
