import Control.Applicative
import Control.Monad
import Data.Bool (bool)

main :: IO ()
main = do
  [a, b] <- f
  unless (a == 0 && b == 0) $ do
    putStrLn $ solve a b
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> String
solve a b = bool "a" "b" $ f a < f b
  where
    f x = let fs = fcs x in 2 * (head fs) - sum fs

fcs x = f [] x 2
  where
    f fs x d
      | x < d ^ 2 = if x == 1 then fs else x : fs
      | x `mod` d == 0 = f (d : fs) (g x d) (d + 1)
      | otherwise = f fs x (d + 1)
    g x d
      | x `mod` d == 0 = g (x `div` d) d
      | otherwise = x
