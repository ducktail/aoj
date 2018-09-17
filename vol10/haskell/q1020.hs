import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (delete)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Text.Printf

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> words <$> getLine >>= printf "%f\n"
    main

solve :: Int -> [String] -> Double
solve n [s, t, b] = f n (iv b // [(idx s, 1.0)]) ! (idx t)
  where f p bd | p == 0 = bd
               | otherwise = f (p-1) (V.accum (+) (iv b) g)
          where g = do
                  i <- mvi b
                  j <- [i-5, i-1, i+1, i+5]
                  if bd ! j < 0 then return (i, 0.25 * (bd ! i))
                    else return (i, 0.25 * (bd ! j))

mvi :: String -> [Int]
mvi b = delete (idx b) [6, 7, 8, 11, 12, 13, 16, 17, 18]

iv :: String -> Vector Double
iv b = V.replicate 25 (-1) // [(i, 0) | i <- mvi b]

idx :: String -> Int
idx "A" = 6
idx "B" = 7
idx "C" = 8
idx "D" = 11
idx "E" = 12
idx "F" = 13
idx "G" = 16
idx "H" = 17
idx "I" = 18
idx _ = -1
