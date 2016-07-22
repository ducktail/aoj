import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- inpi toInt
  mapM_ print $ solve xs

solve :: [Int] -> [Int]
solve = map npn

npn :: Int -> Int
npn n = length . takeWhile (<= n) $ pns

pns :: [Int]
pns = [2,3] ++ f [] (tail pns) 5
  where
    f rs (x:xs) n = ps ++ f (rs ++ [x]) xs (x ^ 2 + 2)
      where
        g y = all ((/= 0) . (mod y)) rs
        ps = filter g [n, n+2 .. x ^ 2 - 2]

toInt :: String -> Int
toInt s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents
