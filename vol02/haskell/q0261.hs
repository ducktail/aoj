import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

import Data.Time.Calendar

main :: IO ()
main = do
  s <- getLine
  unless (s == "#") $ do
    putStrLn $ solve s
    main

solve :: String -> String
solve s
  | length ds == 3 = intercalate "." $ map show $ ce_maya ds
  | otherwise = intercalate "." $ map show $ maya_ce ds
  where
    ds = map read $ splitOn "." s :: [Int]
    

ce_maya :: [Int] -> [Int]
ce_maya [cy, cm, cd] = g $ reverse $ f [] (fromIntegral td) [20 * 18 * 20 * 20, 20 * 18 * 20, 20 * 18, 20]
  where
    d = fromGregorian (fromIntegral cy) cm cd
    td = diffDays d (fromGregorian 2012 12 21)
    f ls d [x] = let (q, r) = divMod d x in (r : q : ls)
    f ls d (x:xs) = let (q, r) = divMod d x in
                     f (q:ls) r xs
    g (x:xs) = (x `mod` 13) : xs

maya_ce :: [Int] -> [Int]
maya_ce [b, ka, t, w, ki] = [fromIntegral cy, cm, cd]
  where
    td = ki + 20 * w + 20 * 18 * t + 20 * 18 * 20 * ka + 20 * 18 * 20 * 20 * b
    bst  = fromGregorian 2012 12 21
    (cy, cm, cd) = toGregorian (addDays (fromIntegral td) bst)
