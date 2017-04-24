module LibAux where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs