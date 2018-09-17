import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  ns <- inpi toInt
  mapM_ (putStrLn . unwords . map show) $ solve ns

solve :: [Int] -> [[Int]]
solve = map pnnn
    
pnnn :: Int -> [Int]
pnnn n = [lpn, upn]
  where
    lpn = last $ takeWhile (< n) pns
    upn = head $ dropWhile (<= n) pns
    
pns :: [Int]
pns = 2 : sieve [3,5..]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

toInt :: String -> Int
toInt s = read s

inpi :: (String -> a) -> IO [a]
inpi f = map f . lines <$> getContents
