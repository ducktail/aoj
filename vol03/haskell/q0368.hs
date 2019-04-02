import Control.Applicative

main :: IO ()
main = solve <$> f >>= mapM_ putStrLn
  where
    f = words <$> getLine

solve :: [String] -> [String]
solve [sw, sh, c] = [l1] ++ l2s ++ [l3] ++ l2s ++ [l1]
  where
    w = read sw :: Int
    h = read sh :: Int
    l1 = "+" ++ replicate (w - 2) '-' ++ "+"
    l2 = "|" ++ replicate (w - 2) '.' ++ "|"
    l2s = replicate ((h - 3) `div` 2) l2
    l3 = "|" ++ replicate ((w - 3) `div` 2) '.' ++ c ++ replicate ((w - 3) `div` 2) '.' ++ "|"
