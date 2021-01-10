import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  z <- readLn
  unless (z == 0) $ do
    print $ solve z
    main

solve :: Int -> Int
solve z = f (z ^ 3) 1 (z - 1) 0
  where
    f z3 x y mx =
      if x > y then z3 - mx
      else let w = x ^ 3 + y ^ 3
      in if w > z3 then f z3 x (y - 1) mx
         else f z3 (x + 1) y (max mx w)
