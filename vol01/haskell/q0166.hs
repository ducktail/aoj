import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  m <- readLn
  unless (m == 0) $ do
    us <- replicateM (m - 1) readLn
    n <- readLn
    vs <- replicateM (n - 1) readLn
    print $ solve us vs
    main

solve :: [Int] -> [Int] -> Int
solve us vs
  | abs (su - sv) < eps = 0
  | su > sv = 1
  | otherwise = 2
  where
    toRad x = pi * x / 180.0
    f xs = let ts = (360 - sum xs) : xs
           in sum . map (sin . toRad . fromIntegral) $ ts
    su = f us
    sv = f vs

eps :: Double
eps = 1e-9
