import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  n <- getl toInt
  solve <$> replicateM n getLine >>= mapM_ print

solve :: [String] -> [Int]
solve = map train

train :: String -> Int
train xs = Set.size $ foldl (f xs) Set.empty [1 .. length xs - 1]
  where
    f :: String -> Set String -> Int -> Set String
    f ss st i = foldl (flip Set.insert) st $ [cs ++ ds | cs <- [as, reverse as], ds <- [bs, reverse bs]] ++ [ds ++ cs | cs <- [as, reverse as], ds <- [bs, reverse bs]]
      where
        (as,bs) = splitAt i ss

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
