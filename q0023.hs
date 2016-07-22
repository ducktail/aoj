import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  n <- readLn :: IO Int
  xs <- input n (map read . words) :: IO [[Double]]
  mapM_ print $ solve xs
 
solve :: [[Double]] -> [Int]
solve = map f
  where f :: [Double] -> Int
        f [xa, ya, ra, xb, yb, rb] = intsect (Circle xa ya ra) (Circle xb yb rb)
 
input :: Int -> (String -> a) -> IO [a]
input n f = map f <$> replicateM n getLine

data Circle = Circle {getX::Double, getY::Double, getR::Double}
 
dist :: Circle -> Circle -> Double
dist ca cb = sqrt $ (getX ca - getX cb) ^ 2 + (getY ca - getY cb) ^ 2
 
intsect :: Circle -> Circle -> Int
intsect ca cb | d - getR ca - getR cb > eps = 0
              | getR ca - getR cb - d > eps = 2
              | getR cb - getR ca - d > eps = -2
              | otherwise = 1
  where d = dist ca cb
 
eps :: Double
eps = 0.0000000001
