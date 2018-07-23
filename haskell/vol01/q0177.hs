import Control.Applicative ((<$>))
import Control.Monad (unless)

main :: IO ()
main = do
  xs <- words <$> getLine
  unless (all (=="-1") xs) $ do
    print $ solve xs
    main

solve :: [String] -> Int
solve [a, b, c, d] = round $ r * (acos ((sin lat1) * (sin lat2) + (cos lat1) * (cos lat2) * (cos (lng1 - lng2))))
  where r = 6378.1
        lat1 = toRad (read a)
        lng1 = toRad (read b)
        lat2 = toRad (read c)
        lng2 = toRad (read d)
        toRad :: Double -> Double
        toRad x = x * (acos (-1)) / 180
