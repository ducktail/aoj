import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= mapM_ print
    main
  where
    f = map read <$> words <$> getLine

solve :: [[Integer]] -> [Integer]
solve xss = let ps = map f xss
                g = foldl1' gcd ps
            in map (flip div g) ps
  where
    pon = (product . map head) xss
    f [d, v] = pon `div` d * v
