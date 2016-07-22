import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inpi toDbl
  mapM_ print $ solve xs

solve :: [Double] -> [Int]
solve xs = map height xs

height :: Double -> Int
height x = 1 + (ceiling $ x * x / 98.0)

toDbl :: String -> Double
toDbl s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents
