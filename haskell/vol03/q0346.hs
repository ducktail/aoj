import Control.Applicative ((<$>))
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Bool (bool)

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= putStrLn

solve :: [Int] -> String
solve = bool "no" "yes" . all f . chunksOf 4 . sort
  where f = and . (tail >>= zipWith (==))
