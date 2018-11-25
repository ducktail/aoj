import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> f <*> getLine >>= putStrLn
    main
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> String -> String
solve ks ss = zipWith f ss (cycle ks)
  where
    tbl = cycle (['Z', 'Y' .. 'A'] ++ ['z', 'y' .. 'a'])
    f c d = head $ drop d $ dropWhile (/= c) tbl
