import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.IntMap as IM

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    xs <- rgetl n $ split toInt
    let rs = solve xs
    if null rs then putStrLn "NA" else mapM_ print rs
    main

solve :: [[Int]] -> [Int]
solve xs = (nub $ map (!! 0) xs) `intersect` cs
  where
    im = foldl (\m [i,t,s] -> IM.insertWith (+) i ((fromIntegral t) * (fromIntegral s)) m) IM.empty xs :: IM.IntMap Integer
    cs = IM.keys $ IM.filter (>= 1000000) im

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
