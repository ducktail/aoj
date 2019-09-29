import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  solve <$> readLn <*> f >>= prt
  loop
  where
    f = map read <$> words <$> getLine
    loop = do
      n <- readLn
      unless (n == 0) $ do
        putStrLn ""
        solve n <$> f >>= prt
        loop

solve :: Int -> [Int] -> [Int]
solve n [x, l, c, d] = let sm = 10 * x + 50 * l + 100 * c + 500 * d
                           cg = sm - n
                           (cd, r1) = divMod cg 500
                           (cc, r2) = divMod r1 100
                           (cl, r3) = divMod r2 50
                           cx = r3 `div` 10
                       in [ max 0 (x - cx),
                            max 0 (l - cl),
                            max 0 (c - cc),
                            max 0 (d - cd) ]
                         
prt :: [Int] -> IO ()
prt [a, b, c, d] = do
  when (a > 0) $ putStrLn $ "10 " ++ show a
  when (b > 0) $ putStrLn $ "50 " ++ show b
  when (c > 0) $ putStrLn $ "100 " ++ show c
  when (d > 0) $ putStrLn $ "500 " ++ show d
