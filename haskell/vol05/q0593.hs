import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  [n,m] <- getl $ wrds toInt
  solve <$> replicateM n (getl toInt) <*> replicateM m (getl toInt) >>= print

solve :: [Int] -> [Int] -> Int
solve ns = head . head . sortBy (flip compare `on` length) . group . sort . map f
  where
    ls = zip [1..] ns
    f x = fst . head $ dropWhile (\y -> snd y > x) ls

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
