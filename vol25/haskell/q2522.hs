import Control.Applicative
import Data.Bool (bool)
import Data.Char (isDigit, isLower, isUpper)

main :: IO ()
main = solve <$> getLine >>= putStrLn

solve :: String -> String
solve pw = bool "INVALID" "VALID" $ length pw >= 6 &&
           any isDigit pw &&
           any isUpper pw &&
           any isLower pw
