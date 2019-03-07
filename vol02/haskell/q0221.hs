import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [m, n] <- f
  unless (m == 0 && n == 0) $ do
    solve m <$> replicateM n getLine >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [String] -> String
solve m ss = unwords . map show  $ f 1 ss ([1..m],[])
  where
    f :: Int -> [String] -> ([Int], [Int]) -> [Int]
    f _ _ ([u],[]) = [u]
    f _ [] (us, vs) = sort $ us ++ vs
    f x ss ([], vs) = f x ss (reverse vs, [])
    f x (s:ss) (u:us, vs)
      | fizzbuzz x == s = f (x + 1) ss (us, u:vs)
      | otherwise = f (x + 1) ss (us, vs)

fizzbuzz :: Int -> String
fizzbuzz x = case (x `mod` 3, x `mod` 5) of
  (0, 0) -> "FizzBuzz"
  (0, _) -> "Fizz"
  (_, 0) -> "Buzz"
  (_, _) -> show x
