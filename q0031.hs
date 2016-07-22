import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Bits

main = do
  xs <- input read :: IO [Int]
  mapM_ putStrLn $ solve xs
 
solve :: [Int] -> [String]
solve = map weight

weight :: Int -> String
weight x = intercalate " " . map show . filter (/= 0) $ map (\i -> if testBit x i then 2 ^ i else 0) [0..9]

-- solve :: [Int] -> [String]
-- solve xs = map f xs
--   where f z = intercalate " " . filter (not . null) $ zipWith (\x y -> if x == 1 then y else "") (i2b z) tbl
--  
-- i2b :: Int -> [Int]
-- i2b 0 = []
-- i2b x = x `mod` 2 : i2b (x `div` 2)
--  
-- tbl :: [String]
-- tbl = ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512"]
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents
