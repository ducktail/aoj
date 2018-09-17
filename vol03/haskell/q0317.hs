import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve [d, l] = let (q, r) = divMod d l
               in q + r
