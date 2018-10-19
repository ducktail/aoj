import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: [[Int]] -> String
solve xs = let ys = map sum xs
           in unwords . map show $ [maximum ys,  minimum ys]
