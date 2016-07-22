import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- getc solve
  mapM_ putStrLn xs

solve :: String -> String
solve xs = maybe "NA" id $ decode "" xs

decode :: String -> String -> Maybe String
decode acc [] = Just $ reverse acc
decode acc (s1:s2:ss)
  | s1 >= '1' && s1 <= '6' && s2 >= '1' && s2 <= '5' = decode ((tr s1 s2) : acc) ss
  | otherwise = Nothing
decode _ _ = Nothing

tr :: Char -> Char -> Char
tr i j = tbl !! (((digitToInt i) - 1) * 5 + ((digitToInt j) - 1))
  where
    tbl = "abcdefghijklmnopqrstuvwxyz.?! "

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents

tests :: [Test]
tests = [
  "tr1" ~: 'h' ~=? tr '2' '3',
  "tr2" ~: 's' ~=? tr '4' '4',
  "tr3" ~: '!' ~=? tr '6' '4',
  "decode1" ~: Just "naruto" ~=? decode "" "341143514535",
  "decode2" ~: Nothing ~=? decode "" "314",
  "solve1" ~: "do you wanna go to aizu?" ~=? solve "143565553551655311343411652235654535651124615163",
  "solve2" ~: "NA" ~=? solve "4"
  ]
