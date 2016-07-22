import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inpi toDbl
  print $ solve xs

solve :: [Double] -> Double
solve xs = last ys - head ys
  where ys = sort xs
    
toDbl :: String -> Double
toDbl s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents
