import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getc (map $ wrds toInt) >>= mapM_ print

solve :: [[Int]] -> [Int]
solve = map f
  where
    f [v,d] = let (x:xs) = take v fibs in g [] [x] xs
      where
        g rs us vs
          | null vs = length rs + 1
          | null nus = g (us : rs) [head vs] (tail vs)
          | otherwise = g rs (nus ++ us) (vs \\ nus)
          where
            nus = nub [j |i <- us, j <- vs, abs (i - j) < d]

fibs :: [Int]
fibs = 2:3:zipWith (\x y -> (x+y) `mod` 1001) fibs (tail fibs)

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
