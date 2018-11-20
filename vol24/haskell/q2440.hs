import Control.Applicative
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  am <- S.fromList <$> replicateM n getLine
  m <- readLn
  solve am <$> replicateM m getLine >>= mapM_ putStrLn

solve :: Set String -> [String] -> [String]
solve am us = reverse . fst $ foldl' f ([], True) us
  where
    f (mes, lck) u
      | S.notMember u am = (("Unknown " ++ u) : mes, lck)
      | lck = (("Opened by " ++ u) : mes, not lck)
      | otherwise = (("Closed by " ++ u) : mes, not lck)
