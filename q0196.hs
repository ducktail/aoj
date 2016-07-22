import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n (getl words) >>= mapM_ putStrLn
    main

solve :: [[String]] -> [String]
solve = map fst . sortBy g . map f
  where f (y:ys) = (y, ((length . filter (=="0")) ys, (length . filter (=="1")) ys))
        g a b = if (fst.snd) a == (fst.snd) b
                then (snd.snd) a `compare` (snd.snd) b
                else (fst.snd) b `compare` (fst.snd) a
                                                   
toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
