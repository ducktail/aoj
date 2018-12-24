import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= putStrLn

solve :: Int -> String
solve a = let (h, m) = divMod (2 * a) 60
          in unwords . map show $ [h, m]
