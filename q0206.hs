import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  l <- getl toInt
  unless (l == 0) $ do
    solve l <$> replicateM 12 (getl $ wrds toInt) >>= putStrLn
    main
  
solve :: Int -> [[Int]] -> String
solve l xs = f 1 0 xs
  where
    f _ _ [] = "NA"
    f a c ([m, n]:ys) = if l <= c + m - n then (show a) else f (a + 1) (c + m - n) ys
    
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
