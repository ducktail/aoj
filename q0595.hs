import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Bits ((.&.), (.|.), shift, rotate, testBit)
import Data.Array.Unboxed

main :: IO ()
main = do
  getLine
  solve <$> getLine >>= print

solve :: String -> Int
solve xs = flip mod mv $ sum $ elems $ foldl g ia ys
  where
    (y:ys) = map f xs
    f x
      | x == 'J' = 2
      | x == 'O' = 1
      | otherwise = 0
    ia = accumArray (+) 0 (1,7) [(i,1)|i <- [4..7], testBit i y] :: UArray Int Int
    g :: UArray Int Int -> Int -> UArray Int Int
    g a x = accumArray (\p q -> (p + q) `mod` mv) 0 (1,7) [(i, a ! j) | i <- [1..7], j <- [1..7], testBit i x, i .&. j /= 0]
      
toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine

mv :: Int
mv = 10007
