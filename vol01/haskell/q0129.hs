import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    wss <- replicateM n f
    m <- readLn
    map (solve wss) <$> replicateM m f >>= mapM_ putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: [[Int]] -> [Int] -> String
solve wss ots
  | any (inout ots) wss = "Safe"
  | any (obstacle ots) wss = "Safe"
  | otherwise = "Danger"

inout :: [Int] -> [Int] -> Bool
inout [tx, ty, sx, sy] [wx, wy, r] = lt2 > r2 && ls2 < r2 || lt2 < r2 && ls2 > r2
  where
    lt2 = (tx - wx) ^ 2 + (ty - wy) ^ 2
    ls2 = (sx - wx) ^ 2 + (sy - wy) ^ 2
    r2 = r ^ 2

outout :: [Int] -> [Int] -> Bool
outout [tx, ty, sx, sy] [wx, wy, r] = lt2 > r2 && ls2 > r2
  where
    lt2 = (tx - wx) ^ 2 + (ty - wy) ^ 2
    ls2 = (sx - wx) ^ 2 + (sy - wy) ^ 2
    r2 = r ^ 2

obstacle :: [Int] -> [Int] -> Bool
obstacle [tx, ty, sx, sy] [wx, wy, r] = outout [tx, ty, sx, sy] [wx, wy, r] &&
                                        dotv1v2 >= 0 &&
                                        dotv1v2 <= l2v1 &&
                                        len2 (dotv1v2 * (fst v1) - l2v1 * (fst v2), dotv1v2 * (snd v1) - l2v1 * (snd v2)) <= l2v1 ^ 2 * r ^ 2
  where
    dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
    len2 (x, y) = x ^ 2 + y ^ 2
    v1 = (sx - tx, sy - ty)
    v2 = (wx - tx, wy - ty)
    l2v1 = len2 v1
    dotv1v2 = dot v1 v2
