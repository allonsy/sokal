{- Alec Snyder 
Spew Program
-}


import Suck
import System.Random


isEndSentence :: String -> Bool
isEndSentence [] = False
isEndSentence (x:xs)
    | x == '.' = True
    | otherwise = isEndSentence xs

chooseOne :: [(Int,Int)] -> StdGen -> (Int, StdGen)
chooseOne ls g = (res, newgen) where
    res = (freqs ls) !! pos
    (pos, newgen) = randomR (0,len) g
    len = length (freqs ls)
    freqs [] = []
    freqs (x:xs) = (replicate (fst x) (snd x)) ++ freqs xs
