import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)

main :: IO ()
main = do
  t <- readLn
  unless (t == 0) $ do
    solve t <$> replicateM 9 (read <$> getLine) >>= print
    main

solve :: Int -> [Int] -> Int
solve t xs = t - (sum xs)
