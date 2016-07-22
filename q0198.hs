import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve n <$> replicateM n (getl words) >>= print
    main

solve :: Int -> [[String]] -> Int
solve n xs = n - ((length . nubBy sameDice . map roll) xs)

roll :: [String] -> [String]
roll [c1,"Red",c3,c4,c5,c6] = ["Red",c6,c3,c4,c1,c5]
roll [c1,c2,"Red",c4,c5,c6] = ["Red",c2,c6,c1,c5,c4]
roll [c1,c2,c3,"Red",c5,c6] = ["Red",c2,c1,c6,c5,c3]
roll [c1,c2,c3,c4,"Red",c6] = ["Red",c1,c3,c4,c6,c2]
roll [c1,c2,c3,c4,c5,"Red"] = ["Red",c2,c4,c3,c5,c1]
roll x = x

sameDice :: [String] -> [String] -> Bool
sameDice xs [c1,c2,c3,c4,c5,c6] = xs `elem` ys
  where ys = [[c1,c4,c2,c5,c3,c6],[c1,c5,c4,c3,c2,c6],[c1,c3,c5,c2,c4,c6],[c1,c2,c3,c4,c5,c6]]

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
