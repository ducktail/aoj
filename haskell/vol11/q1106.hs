import Control.Applicative ((<$>))
import Control.Monad (unless)

main :: IO ()
main = do
  [a, b, c] <- map read <$> words <$> getLine
  unless (a == 0 && b == 0 && c == 0) $ do
    putStrLn $ solve a b c
    main

solve :: Int -> Int -> Int -> String
solve a b c | d < 0 = "Impossible"
            | d /= srd ^ 2 = "Impossible"
            | otherwise = unwords . map show $ if p' > r' then [p', q', r', s']
                                               else if p' < r' then [r', s', p', q']
                                                    else if q' > s' then [p', q', r', s']
                                                         else [r', s', p', q']
  where d = b ^ 2 - 4 * a * c
        srd = floor . sqrt . fromIntegral $ d
        (p', q') = let g = gcd (2*a) (b-srd) in ((2*a) `div` g, (b-srd) `div` g)
        (r', s') = let g = gcd (2*a) (b+srd) in ((2*a) `div` g, (b+srd) `div` g)
