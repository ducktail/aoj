import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> replicateM n getLine >>= print
    main

solve :: Int -> [String] -> Int
solve n ss = f 1
  where
    cs = map code ss
    f k
      | k > 50 = (-1)
      | otherwise = let st = S.fromList $ map (take k) cs
                    in if S.size st == n
                       then k
                       else f (k + 1)

code :: String -> String
code s = let vs = "aiueo"
             xs = map snd $ filter (flip elem vs . fst) $ zip s (tail s)
         in head s : xs
