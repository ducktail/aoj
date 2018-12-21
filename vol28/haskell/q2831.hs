import Control.Applicative

main :: IO ()
main = getLine >> solve <$> getLine >>= print

solve :: String -> Int
solve = (+ 1) . length . fst . break (\(x, y) -> x == 'x' && y == 'x') . (tail >>= zip)
