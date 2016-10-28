import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl read
  unless (n == 1) $ do
    putStrLn $ solve n
    main

solve :: Int -> String
solve x = show x ++ ": " ++ (unwords . map show) ans
  where
    ans = filter isPn $ nub $ as ++ (reverse bs)
    as = filter ((== 0).(mod x)) $ takeWhile ((<= x).(^2)) msnum
    bs = map (div x) as

msnum :: [Int]
msnum = 1:6:map (+7) msnum

pns :: [Int]
pns = 6 : filter isPn (drop 2 msnum)

isPn :: Int -> Bool
isPn 1 = False
isPn x = all ((/= 0) . mod x) $ takeWhile ((<= x) . (^2)) pns

getl :: (String -> a) -> IO a
getl f = f <$> getLine
