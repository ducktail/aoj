import Control.Applicative

main :: IO ()
main = getLine >> solve <$> getLine >>= putStrLn

solve :: String -> String
solve (j:o:i:rs)
  | j == 'j' && o == 'o' && i == 'i' = 'J' : 'O' : 'I' : solve rs
  | otherwise = j : solve (o:i:rs)
solve rs = rs
