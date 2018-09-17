import Control.Applicative ((<$>), (<*>))

main :: IO ()
main = solve <$> f <*> f <*> f >>= putStrLn
  where f = map read <$> words <$> getLine

solve :: [Int] ->  [Int] -> [Int] -> String
solve [h1, h2] [s1, s2] [a, b, c, d] = case compare (f h1 h2) (f s1 s2) of
                                        GT -> "hiroshi"
                                        EQ -> "even"
                                        LT -> "kenjiro"
  where f i y = i * a + y * b + (i `div` 10) * c + (y `div` 20) * d
