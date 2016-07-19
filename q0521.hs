import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    print $ solve n
    main

solve :: Int -> Int
solve n = fst $ foldl f (0,1000 - n) [500,100,50,10,5,1]
  where
    f (c,m) l = (m `div` l + c, m `mod` l)

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
