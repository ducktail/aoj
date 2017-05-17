import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  rs <- getLine
  unless (rs == "-") $ do
    solve rs <$> getLine <*> getLine >>= putStrLn
    main

solve :: String -> String -> String -> String
solve rs gs ds = f rs (tail gs) (head gs) [] ds
  where
    f :: String -> String -> Char -> String -> String -> String
    f xs ys c zs ps
      | null xs && null ys = reverse (c:zs)
      | null ps = f xs (tail ys) (head ys) (c:zs) ps
      | c == head ps = f (tail xs) ys (head xs) zs (tail ps)
      | otherwise = f xs (tail ys) (head ys) (c:zs) ps
