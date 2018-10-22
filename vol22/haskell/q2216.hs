import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [a, b] <- f
  unless (a == 0 && b == 0) $ do
    putStrLn $ solve (b - a)
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> String
solve x = let (_, ys) = foldr (\a (b,ls) -> let (q, r) = divMod b a in (r, q:ls)) (x, []) [100, 500, 1000]
          in unwords . map show $ ys
