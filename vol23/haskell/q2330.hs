import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n = f 0 20
  where
    f l r
      | l + 1 == r = r
      | otherwise = let m = (l + r) `div` 2
                    in case compare (3 ^ m) n of
                        GT -> f l m
                        EQ -> m
                        LT -> f m r
