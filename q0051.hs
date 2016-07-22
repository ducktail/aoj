import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- inp toInt
  xs <- inpn n solve
  mapM_ print xs

solve :: String -> Int
solve s = mx - mn
  where
    ss = sort s
    rs = reverse ss
    mn = read ss
    mx = read rs

toInt :: String -> Int
toInt s = read s

inp :: (String -> a) -> IO a
inp f = f <$> getLine

inpn :: Int -> (String -> a) -> IO [a]
inpn n f = map f <$> replicateM n getLine
