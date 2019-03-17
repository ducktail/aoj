import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f <*> f >>= prt
    main
  where
    f = map read <$> words <$> getLine
    prt xs
      | null xs = putStrLn "NA"
      | otherwise = mapM_ print xs

solve :: [[Int]] -> [Int] -> [Int]
solve xss [p, q, r, c]= map head $ filter g xss
  where
    f p q r = 4 * p + 9 * q + 4 * r
    g [si, pi, qi, ri] = pi <= p && qi <= q && ri <= r && f pi qi ri <= c
