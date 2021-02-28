import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [a, b, d] <- f
  unless (a == 0 && b == 0 && d == 0) $ do
    putStrLn . unwords $ map show $ solve a b d
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int -> [Int]
solve a b d = f na nb nx ny
  where
    (x, y, gcd) = emdm a b
    na = a `div` gcd
    nb = b `div` gcd
    nd = d `div` gcd
    nx = x * nd
    ny = y * nd
    f a b x y = if c0 < c1 && c0 < c2 then f a b (x + b) (y - a)
                else if c2 < c0 && c2 < c1 then f a b (x - b) (y + a)
                     else if c0 > c1 && c1 < c2 then [abs x, abs y]
                     else if c0 == c1 && m0 < m1 then f a b (x + b) (y - a)
                     else if c0 == c1 then [abs x, abs y]
                          else if c1 == c2 && m1 > m2 then f a b (x - b) (y + a)
                               else [abs x, abs y]
      where
        c0 = abs (x + b) + abs (y - a)
        c1 = abs x + abs y
        c2 = abs (x - b) + abs (y + a)
        m0 = a * abs (x + b)  + b * abs (y - a)
        m1 = a * abs x + b * abs y
        m2 = a * abs (x - b) + b * abs (y + a)

emdm :: Int -> Int -> (Int, Int, Int)
emdm a b = f (1, 0, a) (0, 1, b)
  where
    f (x0, y0, r0) (x1, y1, r1) =
      let (q, r) = divMod r0 r1 in
        if r == 0 then (x1, y1, r1)
        else f (x1, y1, r1) (x0 - x1 * q, y0 - y1 * q, r)
