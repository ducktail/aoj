import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  replicateM n (solve <$> f <*> f) >>= mapM_ putStrLn
  where
    f = map toCard <$> words <$> getLine

solve :: [[Int]] -> [[Int]] -> String
solve [dl0, dl1] pls
  | ipt == [21, 11] = "blackjack"
  | otherwise = f ipt pls
  where
    ipt = calcbj dl0 dl1
    f pt pls
      | null pt = "bust"
      | head pt > 17 || pt == [17] = show . head $ pt
      | otherwise = f (calcbj pt (head pls)) (tail pls)

toCard :: String -> [Int]
toCard x
  | x == "A" = [11, 1]
  | x `elem` ["T", "J", "Q", "K"] = [10]
  | otherwise = [read x]

calcbj :: [Int] -> [Int] -> [Int]
calcbj xs ys = nub $ do
  x <- xs
  y <- ys
  guard $ x + y < 22
  return $ x + y
