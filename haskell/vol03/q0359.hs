import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> readLn >>= putStrLn

solve :: Int -> String
solve n = week !! (n `mod` 7)
  where week = ["thu", "fri", "sat", "sun", "mon", "tue", "wed"]
home
