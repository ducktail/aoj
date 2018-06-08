import Control.Applicative ((<$>))
import Data.Bool (bool)
import Data.List (sort)

main :: IO ()
main = solve <$> sort <$> map read <$> words <$> getLine >>= putStrLn

solve :: [Int] -> String
solve [e1, e2, e3, e4] = bool "no" "yes" $ e1 == e2 && e3 == e4
