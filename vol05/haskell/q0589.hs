import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.Map as M

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n (getl words) >>= mapM_ putStrLn

solve :: [[String]] -> [String]
solve = map h . sortBy g . M.assocs . foldl (\mp (k,a) -> M.insertWith (+) k a mp) M.empty . map f
  where
    f [x,y] = (x, toInt y)
    g (x,_) (y,_)
      | length x == length y  = x `compare` y
      | otherwise = length x `compare` length y
    h (x,y) = x ++ " " ++ show y

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
