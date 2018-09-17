import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, unless)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n (read <$> getLine) <*> readLn >>= print
    main
    
solve :: Int -> [Int] -> Int -> Int
solve n xs k = bsearch xs n k

bsearch :: [Int] -> Int -> Int -> Int
bsearch xs n k = f 0 0 (n-1)
  where f ct l r | l > r = ct
                 | v == k = ct + 1
                 | v > k = f (ct + 1) l (m - 1)
                 | otherwise = f (ct + 1) (m + 1) r
          where m = (l + r) `div` 2
                v = xs !! m
