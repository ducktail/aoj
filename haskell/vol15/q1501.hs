import Control.Applicative
import Data.List
import Data.Function (on)

main :: IO ()
main = solve <$> f >>= print
  where f = map read <$> words <$> getLine

solve :: [Integer] -> Integer
solve [r, c, a1, a2, b1, b2] = flip mod 100000007 . sum . map (comb <$> sum <*> (!!1)) $ filter ((==mv).sum) ps
  where ps = map (map abs . zipWith (-) [a1, a2]) [[b1, b2],
                                                   [b1+r, b2],
                                                   [b1-r, b2],
                                                   [b1, b2+c],
                                                   [b1, b2-c],
                                                   [b1+r, b2+c],
                                                   [b1-r, b2+c],
                                                   [b1+r, b2-c],
                                                   [b1-r, b2-c]]
        mv = sum . minimumBy (compare `on` sum) $ ps

comb :: Integer -> Integer -> Integer
comb n m | n < m = 0
         | m == 0 = 1
         | n > 2 * m = comb n (n-m)
         | otherwise = flip mod 100000007 . foldl' f 1 $ zip [n-m+1 .. n] [1 .. m]
  where f a (i,j) = a * i `div` j
