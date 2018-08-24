import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n = minimum . map (product . (zipWith (\x y -> x ^ (y-1)) [2,3,5])) $ f n
  where
    f 4 = [[4],[2,2]]
    f 6 = [[6],[3,2]]
    f 8 = [[8],[4,2],[2,2,2]]
    f 9 = [[9],[3,3]]
    f 10 = [[10],[5,2]]
    f 12 = [[12],[6,2],[4,3],[3,2,2]]
    f x = [[x]]
