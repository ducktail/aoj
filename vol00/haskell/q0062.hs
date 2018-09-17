import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
  xs <- getc $ solve . map digitToInt
  mapM_ print xs

solve :: [Int] -> Int
solve xs
  | null ys = y
  | otherwise = solve (y:ys)
  where (y:ys) = rdc xs
        
rdc :: [Int] -> [Int]
rdc [x] = []
rdc (x:y:ys) = add x y : rdc (y:ys)
  where
    add a b = (a + b) `mod` 10

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
