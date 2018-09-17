import Control.Monad
import Data.Char (toUpper)

main :: IO ()
main = interact $ map toUpper

