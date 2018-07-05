import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (transpose, group)

main :: IO ()
main = do
  [n, m, d] <- map read <$> words <$> getLine
  solve d <$> replicateM n getLine >>= print

solve :: Int -> [String] -> Int
solve d ss = cnt xs + cnt ys
  where f = map length . filter ((=='.') . head) . group
        xs = concatMap f ss
        ys = concatMap f . transpose $ ss
        cnt = sum . map (\x -> max 0 (x-d+1))
