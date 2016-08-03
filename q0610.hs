import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [h,w] <- getl $ wrds toInt
  solve <$> replicateM h getLine >>= mapM_ (putStrLn . unwords . map show)

solve :: [String] -> [[Int]]
solve xs = map (f (-1)) xs
  where
    f _ [] = []
    f p (y:ys)
      | y == 'c' = 0 : f 0 ys
      | otherwise = let q = if p == (-1) then (-1) else (p+1) in q : f q ys

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
