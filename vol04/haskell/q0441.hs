import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= putStrLn . unwords . map show

solve :: Int -> [Int]
solve d = let (x, byo) = d `divMod` 60
              (doo, hun) = x `divMod` 60
          in [doo, hun, byo]
