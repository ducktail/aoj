import Control.Applicative ((<$>))
import Data.List (group)

main :: IO ()
main = getLine >> solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve xs | null os = 1
         | otherwise = (+1) . maximum . map length $ os
  where os = filter ((== 1) . head) . group $ xs
