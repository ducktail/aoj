import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve [w, h, c] = let g = gcd w h
                  in w * h `div` g `div` g * c
