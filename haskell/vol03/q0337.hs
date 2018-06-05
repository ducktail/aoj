import Control.Applicative ((<$>))

main :: IO ()
main = solve <$> map read <$> words <$> getLine >>= putStrLn
  
solve :: [Int] -> String
solve [e, y] | e == 1 = show $ y + 1867
             | e == 2 = show $ y + 1911
             | e == 3 = show $ y + 1925
             | e == 4 = show $ y + 1988
             | y < 1912 = 'M' : show (y - 1867)
             | y < 1926 = 'T' : show (y - 1911)
             | y < 1989 = 'S' : show (y - 1925)
             | otherwise = 'H' : show (y - 1988)
