import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (transpose, partition)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n (map read <$> words <$> getLine) >>= print
    main

solve :: [[Int]] -> Int
solve = f 0
  where f pt xss | cpt == 0 = pt
                 | otherwise = f (pt + cpt) zss
          where (cpt, yss) = foldr g (0, []) xss
                g [a, b, c, d, e] (ct, as) | all (==a) [b, c, d, e] = (ct + 5 * a, [0,0,0,0,0]:as)
                                           | all (==a) [b, c, d] = (ct + 4 * a, [0,0,0,0,e]:as)
                                           | all (==b) [c, d, e] = (ct + 4 * b, [a,0,0,0,0]:as)
                                           | all (==a) [b, c] = (ct + 3 * a, [0,0,0,d,e]:as)
                                           | all (==b) [c, d] = (ct + 3 * b, [a,0,0,0,e]:as)
                                           | all (==c) [d, e] = (ct + 3 * c, [a,b,0,0,0]:as)
                                           | otherwise = (ct, [a,b,c,d,e]:as)
                zss = transpose . map h . transpose $ yss
                h xs = let (ys, zs) = partition (==0) xs in ys ++ zs
