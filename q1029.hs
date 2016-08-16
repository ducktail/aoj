import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- getl $ wrds toInt
  unless (n == 0 && m == 0) $ do
    if n == 0 || m == 0 then
      solve [] <$> (getl $ wrds toInt) >>= print
    else
      solve <$> (getl $ wrds toInt) <*> (getl $ wrds toInt) >>= print
    main

solve :: [Int] -> [Int] -> Int
solve ls rs = maximum $ zipWith (-) ts (0:ts)
  where
    ts = sort $ ls ++ rs

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
