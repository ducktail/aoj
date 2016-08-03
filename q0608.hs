import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 5 (getl toInt) >>= print

solve :: [Int] -> Int
solve [a,b,c,d,p] = min (f p) (g p)
  where
    f x = x * a
    g x
      | x <= c = b
      | otherwise = b + (x - c) * d

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
