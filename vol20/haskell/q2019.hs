import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve m <$> sortBy (flip compare) <$> map g <$> replicateM n f >>= print
    main
  where
    f = map read <$> words <$> getLine
    g [d, p] = (p, d)
    
solve :: Int -> [(Int, Int)] -> Int
solve m = fst . foldl' f (0, m)
  where
    f (ct, bg) (p, d)
      | bg >= d = (ct, bg - d)
      | otherwise = (ct + (d - bg) * p, 0)
