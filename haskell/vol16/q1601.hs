import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n (length <$> getLine) >>= print
    main

solve :: [Int] -> Int
solve = f 1
  where
    f i ls
      | isTanku ls = i
      | otherwise = f (i + 1) (tail ls)

isTanku :: [Int] -> Bool
isTanku = f 0 0
  where
    f _ _ [] = False
    f 0 cw (c:cs)
      | cw + c < 5 = f 0 (cw + c) cs
      | cw + c > 5 = False
      | otherwise = f 1 0 cs
    f 1 cw (c:cs)
      | cw + c < 7 = f 1 (cw + c) cs
      | cw + c > 7 = False
      | otherwise = f 2 0 cs
    f 2 cw (c:cs)
      | cw + c < 5 = f 2 (cw + c) cs
      | cw + c > 5 = False
      | otherwise = f 3 0 cs
    f 3 cw (c:cs)
      | cw + c < 7 = f 3 (cw + c) cs
      | cw + c > 7 = False
      | otherwise = f 4 0 cs
    f 4 cw (c:cs)
      | cw + c < 7 = f 4 (cw + c) cs
      | cw + c > 7 = False
      | otherwise = True
