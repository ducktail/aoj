import Control.Applicative
import Control.Monad
import Control.Arrow ((&&&))
import Data.List

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    putStrLn $ solve n
    main

solve :: Int -> String
solve n
   | s > 2 * n = "abundant number"
   | s < 2 * n = "deficient number"
   | otherwise = "perfect number"
  where
    s = sum . divisors . primeFactors $ n

primeFactors :: Int -> [(Int, Int)]
primeFactors 1 = []
primeFactors n = map (head &&& length) $ group $ f [] 2 n
  where
    f rs d x
      | d ^ 2 > x = x : rs
      | x `mod` d == 0 = f (d:rs) d (x `div` d)
      | otherwise = f rs (d + 1) x

divisors :: [(Int,Int)] -> [Int]
divisors [] = [1]
divisors ((d, n):rs) = do
  x <- map (d^) [0..n]
  y <- divisors rs
  return $ x * y
