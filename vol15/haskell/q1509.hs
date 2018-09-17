import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- f
  unless (all (==0) xs) $ do
    solve xs <$> f >>= print
    main
  where f = map read <$> words <$> getLine
  
solve :: [Int] -> [Int] -> Int
solve [a, b, c, d, e] [na, nb, nc] | hl >= d = hl * e + sum' bs
                                   | otherwise = min (sum' ls) (d * e + sum' (f d ls))
  where (as, bs) = partition (\(x,_) -> x >= e) ls
        ls = sortBy (flip compare) [(a,na),(b,nb),(c,nc)]
        hl = (sum . map snd) as
        sum' = sum . map (\(x, y) -> x * y)
        f _ [] = []
        f x ((y,z):zs) | x >= z = f (x-z) zs
                       | otherwise = (y, z-x) : zs
