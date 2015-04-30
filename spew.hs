{- Alec Snyder 
Spew Program
See README.md for instructions and additional documentation
github at https://github.com/allonsy/sokal

Note: I know that this can be accomplished using the state monad directly,
which is in fact what I do here, passing around the random state is my classic verbose style
Essentially, a lot of my return types are (Int, StdGen) which conviently is 'State StdGen Int'
-}

import Suck
import System.Random
import Data.Array
import System.Environment
import Control.Monad

type FastModel = Array Int (String, [(Int,Int)])

main :: IO ()
main = do
    arg <- getArgs --grab the arguments
    when (length arg < 2 || (length arg) > 3) $ do --usage error
        usage
        error "Incorrect number of arguments"
    let urlfile = head arg --read in args
    let numWords = read (arg !! 1)
    putStrLn "Running Suck"
    runSuck urlfile
    putStrLn "Reading in model file into spew"
    contents <- readFile "sokal.model"
    let mod = map read (lines contents) :: [(String, [(Int,Int)])] --parse the model file
    let arrMod = array (0,((length mod)-1)) (zip [0..] mod) --create our array model
    putStrLn "Model read in by spew. Generating markov text"
    rand <- getStdGen
    let (startPos, gen) = randomR (0, (snd (bounds arrMod))) rand --find random starting point
    let out = runSpew arrMod gen "" startPos numWords --run the markov model
    if (length arg) < 3 --print to stdout or to file
        then do
            putStrLn ""
            putStrLn "========"
            putStrLn ""
            putStrLn out
        else do
            writeFile (arg !! 2) out

usage :: IO ()
usage = do
    putStrLn "Usage: sokal urlfile wordlimit(Int) [-o outputFile]"
    putStrLn ""

runSpew :: FastModel -> StdGen -> String -> Int -> Int -> String --takes in a model, random number generator, string, current position in the FSM and length and outputs the markov string
runSpew mod gen str pos len
    | isEndSentence thisWord && len<0 = str ++ thisWord
    | otherwise = runSpew mod newGen (str ++ thisWord ++ " ") newPos (len -1) where
        current = mod ! pos --grabs the current word
        thisWord = fst current --grabs the current word
        randNext = randomR (bounds mod) gen --grabs a random start point
        (newPos, newGen) = chooseOne mod (snd current) gen --grabs the next word
         {- (newPos, newGen) = nextPosGen
        nextPosGen --if we have an empty association list, we go to newStart
            | snd current == [] = newStart (fst randNext) mod (snd randNext)
            | otherwise = potential -}
            

newStart :: Int -> FastModel -> StdGen -> (Int, StdGen) --given a random start point, it steps through the finite state machine to go to the nearest beginning of the sentence
newStart pos mod gen
    | isEndSentence (fst (this)) = chooseOne mod (snd this) gen --if the sentence is over, we start up with the next word
    | otherwise = newStart newPos mod newGen where --otherwise, we go to the next node in the FSM and try again
        this = mod ! pos
        (newPos, newGen) = chooseOne mod (snd this) gen

isEndSentence :: String -> Bool --Does this end the sentence?
isEndSentence [] = False
isEndSentence (x:xs)
    | x == '.' = True
    | otherwise = isEndSentence xs

chooseOne :: FastModel -> [(Int,Int)] -> StdGen -> (Int, StdGen) --takes in a list of freqency and position pairs and randomly finds the next word weighted by the given distribution
chooseOne mod [] gen = newStart fresh mod freshGen where --empty list, go to newStart
    (fresh, freshGen) = randomR (bounds mod) gen
chooseOne mod ls g = (res, newgen) where
    (pos, newgen) = randomR (0,len -1) g --grabs a random index into the freqs array
    res = (freqs ls) !! pos --grabs the position of the next word in the Finite state machine
    len = length (freqs ls)
    freqs [] = [] --function that creates a distribution of next words based on the frequency of the first word in the tuple
    freqs (x:xs) = (replicate (fst x) (snd x)) ++ freqs xs
