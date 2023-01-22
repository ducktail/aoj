import Control.Applicative

main :: IO ()
main = do
  [h1, m1] <- f
  [h2, m2] <- f
  print $ solve (h1 * 60 + m1) (h2 * 60 + m2)
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> Int
solve t1 t2 = sum . (tail >>= zipWith f) $ [t1 .. t2]
  where
    f ta tb = sum $ zipWith segPwc [ab, bb, cb, db] [aa, ba, ca, da]
      where
        (ha, ma) = ta `divMod` 60
        (aa, ba) = ha `divMod` 10
        (ca, da) = ma `divMod` 10
        (hb, mb) = tb `divMod` 60
        (ab, bb) = hb `divMod` 10
        (cb, db) = mb `divMod` 10

segPwc :: Int -> Int -> Int
segPwc a b = case (a, b) of
  (1, 2) -> 4
  (2, 3) -> 1
  (3, 4) -> 1
  (4, 5) -> 2
  (5, 6) -> 1
  (6, 7) -> 1
  (7, 8) -> 3
  (9, 0) -> 1
  (2, 0) -> 1
  (3, 0) -> 2
  (5, 0) -> 2
  _ -> 0
