import Control.Applicative ((<$>), (<*>))
import Data.List(sort, intercalate)
import Text.Printf

main :: IO ()
main = solve <$> f <*> f >>= putStrLn
  where f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> String
solve (_:as) (_:bs) = intercalate " " . map h . g . sort $ f (f [] as) bs
  where f ls (h:m:hm) = f ((h,m):ls) hm
        f ls [] = ls
        g (x:y:ys) | x == y = g (y:ys)
                   | otherwise = x : g (y:ys)
        g [x] = [x]
        h :: (Int, Int) -> String
        h (h, m) = printf "%d:%02d" h m
