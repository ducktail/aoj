import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  unless (n == 0) $ do
    solve <$> g <*> replicateM n g >>= mapM_ print
    putStrLn"+++++"
    main
  where
    f = ((,) <$> head <*> (head . tail)) <$> map read <$> words <$> getLine
    g = do
      m <- readLn
      replicateM m f

solve :: [(Int, Int)] -> [[(Int, Int)]] -> [Int]
solve as = map fst . filter (f rs) . zip [1..]
  where
    rs = let vs = tov as
      in rot vs ++ rot (reverse vs)
    f rs (i, xs) = (tov xs) `elem` rs

tov :: [(Int, Int)] -> [(Int, Int)]
tov = tail >>= zipWith f
  where
    f (a, b) (c, d) = (a - c, b - d)

rot :: [(Int, Int)] -> [[(Int, Int)]]
rot vs = [vs, xs, ys, zs]
  where
    f (a, b) = (-b , a)
    xs = map f vs
    ys = map f xs
    zs = map f ys
