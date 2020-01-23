import Control.Applicative
import Control.Monad
import Data.List
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> f >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [Int] -> String
solve n is = g $ foldl' f (16, 16, 16, 100.0) ls
  where
    ls = do
      s <- [0 .. 15]
      a <- [0 .. 15]
      c <- [0 .. 15]
      return (s, a, c)
    f (xs, xa, xc, xh) (s, a, c) = let m = 256
                                       eps = 1e-9
                                       h = entropy n (outstr s a c m is)
                                   in if xh - h > eps then (s, a, c, h) else (xs, xa, xc, xh)
    g (s, a, c, _) = unwords $ map show [s, a, c]

prand :: Int -> Int -> Int -> Int -> [Int]
prand s a c m = unfoldr (\r -> let r' = (a * r + c) `mod` m in Just(r', r')) s

outstr :: Int -> Int -> Int -> Int -> [Int] -> [Int]
outstr s a c m is = let prs = prand s a c m
                    in zipWith (\x y -> (x + y) `mod` m) is prs

entropy :: Int -> [Int] -> Double
entropy n os = let mp = foldl' (\m x -> IM.insertWith (+) x 1 m) IM.empty os
                   f h x = let xn = (fromIntegral x) / (fromIntegral n) in h + xn * log xn
               in (-1.0) * IM.foldl' f 0 mp
