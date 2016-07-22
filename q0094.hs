import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [a,b] <- getl $ split toDbl
  print $ solve a b

solve :: Double -> Double -> Double
solve a b = (a * b) / 3.305785

toDbl :: String -> Double
toDbl s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
