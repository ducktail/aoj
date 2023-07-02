import Control.Applicative

main :: IO ()
main = solve <$> readLn >>= putStrLn

solve :: Int -> String
solve w = (unwords . map show) [6 * h, 2 * h, h]
  where
    h = bsearch 1 100 (freeboard w)

bsearch fl ps f =
  if fl + 1 == ps then ps
  else
    let m = (fl + ps) `div` 2 in
      if f m then bsearch fl m f
      else bsearch m ps f

freeboard :: Int -> Int -> Bool
freeboard w h =
  hh * h >= 30 * hh + (w + 50) * 1000
  where
    hh = 12 * h * h
