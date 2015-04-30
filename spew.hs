{- Alec Snyder 
Spew Program
-}


import Suck
import System.Random
import Data.Array
import System.Environment
import Control.Monad

type FastModel = Array Int (String, [(Int,Int)])

main :: IO ()
main = do
    arg <- getArgs
    when (length arg < 2) $ do
        usage
        error "Incorrect number of arguments"
    let urlfile = head arg
    let numWords = read (arg !! 1)
    putStrLn "Running Suck"
    runSuck urlfile
    contents <- readFile "sokal.model"
    let mod = map read (lines contents) :: [(String, [(Int,Int)])]
    let arrMod = array (0,((length mod)-1)) (zip [0..] mod)
    putStrLn "Model read in by spew. Generating markov text"
    rand <- getStdGen
    let (startPos, gen) = randomR (0, (snd (bounds arrMod))) rand
    let out = runSpew arrMod gen "" startPos numWords
    putStrLn ""
    putStrLn "========"
    putStrLn ""
    putStrLn out

usage :: IO ()
usage = do
    putStrLn "Usage: spew urlfile wordlimit (Int)"
    putStrLn ""

runSpew :: FastModel -> StdGen -> String -> Int -> Int -> String
runSpew mod gen str pos len
    | isEndSentence thisWord && len<0 = str ++ thisWord
    | otherwise = runSpew mod newGen (str ++ thisWord ++ " ") newPos (len -1) where
        thisWord = fst current
        modlen = snd (bounds mod)
        current = mod ! pos
        randNext = randomR (0,modlen) gen
        potential = chooseOne (snd current) gen
        (newPos, newGen) = nextPosGen
        nextPosGen
            | snd current == [] = newStart (fst randNext) mod (snd randNext)
            | otherwise = potential
            

newStart :: Int -> FastModel -> StdGen -> (Int, StdGen)
newStart pos mod gen
    | isEndSentence (fst (this)) = chooseOne (snd this) gen
    | otherwise = newStart newPos mod newGen where
        this = mod ! pos
        (newPos, newGen) = chooseOne (snd this) gen

isEndSentence :: String -> Bool
isEndSentence [] = False
isEndSentence (x:xs)
    | x == '.' = True
    | otherwise = isEndSentence xs

chooseOne :: [(Int,Int)] -> StdGen -> (Int, StdGen)
chooseOne ls g = (res, newgen) where
    res = (freqs ls) !! pos
    (pos, newgen) = randomR (0,len -1) g
    len = length (freqs ls)
    freqs [] = []
    freqs (x:xs) = (replicate (fst x) (snd x)) ++ freqs xs
    
getNextWord :: FastModel -> Int -> StdGen -> (Int,StdGen)
getNextWord mod pos gen = chooseOne (snd (mod ! pos)) gen

testModel :: FastModel -> StdGen -> String -> Int -> Int -> String
testModel mod gen str pos len
    | len < 0 = str ++ (fst (mod ! pos))
    | otherwise = testModel mod newGen (str ++ (fst (mod ! pos)) ++ " ") newPos (len -1) where
        (newPos, newGen) = getNextWord mod pos gen
