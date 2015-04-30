{- Alec Snyder
- lab 1
- Sokal text generator
- suck.hs
-}
module Suck (runSuck) where

import Prelude hiding (map,filter)
import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad
import Text.StringLike hiding (empty)
import Data.Char
import Data.Map.Strict hiding (findIndex)
import Data.List hiding (insert)

type PrimitiveModel = Map (String,String) [String]

type ProcessedModel = [(String, [(Int,Int)])]

runSuck :: String -> IO ()
runSuck file= do
    contents <- readFile file
    putStrLn "Successfully read in url file"
    let ls = lines contents
    putStrLn "Connecting to servers and downloading content..."
    responses <- mapM grabResp ls --grab the HTML
    putStrLn "Parsing in DOM..."
    let souped = newMap parseTags responses --grab the tagsoup of the HTML
    putStrLn "Filtering DOM..."
    let process = processWords $ filterSoup souped --filter soup for p tags, turn it into text, and parse into words
    putStrLn "Generating Models"
    let prim = generatePrim process empty --generate primitive model
    let final = primToProcess prim --turn to processed model
    putStrLn "Printing Model to File"
    let addNewLine w = (show w) ++ "\n" --add a new line after every entry of the list is displayed
    writeFile "sokal.model" (concatMap (addNewLine) final) --write to file

grabResp :: String -> IO String --takes in a url and returns the HTML at that page
grabResp str = do 
    makeRequest <- simpleHTTP (getRequest str)
    getResponseBody makeRequest

filterSoup :: (StringLike str, Show str) => [[Tag str]] -> [[str]] --tages in the p tags (and the stuff in between) and take out any non text tags
filterSoup ls = newMap ((newMap fromTagText) . (newFilter isTagText) . (flip extractP False)) ls

extractP :: StringLike str => [Tag str] -> Bool -> [Tag str] --takes in a list of tags and returns a list of tags but only those in between the <p> and </p> tags
extractP [] _ = []
extractP (x:xs) True --if true, then we are inside a p tag
    | isTagClose x && x ~== TagClose "p" = x : extractP xs False --closing the p tage so we append the final p and go to false
    | otherwise = x : extractP xs True --inside a p tag
extractP (x:xs ) False --outside a p tag
    | isTagOpen x && x ~== TagOpen "p" [] = x : extractP xs True --we open a p tag and therefore, go to true and append this tag to the list
    | otherwise = extractP xs False --we ignore this tag and proceed

processWords :: [[String]] -> [String] --takes in a list of list of strings and formats it into a list of words, omitting any newline characters and unprintable characters (due to bad unicode -> ascii translating)
processWords ls = newMap ((newMap toLower) . (newFilter isPrint)) $ (words (concat (((newMap ((newFilter (/='\n')) . concat) ls))))) --I feel that this can be simplified into a foldr however I haven't the brainpower to get the compositions to type check yet

generatePrim :: [String] -> PrimitiveModel -> PrimitiveModel --takes in a list of words (from process words) and a start point primitive model and outputs a primitive model
generatePrim (x:y:[]) pm -- if these are the last two words
    | member (x,y) pm = pm 
    | otherwise = insert (x,y) [] pm
generatePrim (x:y:z:xs) pm
    | member (x,y) pm = generatePrim (y:z:xs) (adjust (++ [z]) (x,y) pm) --if we have seen these before, add what comes after to the list
    | otherwise = generatePrim (y:z:xs) (insert (x,y) [z] pm) --otherwise, add this to the dictionary

getFreqs :: PrimitiveModel -> [((String,String), [(Int,String)])] --It takes in a primitive model and it looks at the list of words following this pair and creates a list of (occurrences of this word, word itself) tuples
getFreqs pm = freqs (keys pm) where
    freqs [] = []
    freqs (x:xs) = (x,countOcc (pm ! x) (nub (pm ! x))) : freqs xs --iterate over the nubbed list but count occurrences in the original list
    countOcc _ [] =[]
    countOcc orig (x:xs) = ((length (elemIndices x orig)),x) : countOcc orig xs --traverse the list and see how many we have of each item

interProcessed :: [((String,String), [(Int,String)])] -> [((String,String), [(Int,Int)])] --takes in the result from getFreqs and associates the words in the frequency list with the index of the corresponding pair in the soon to be built array
interProcessed ls = newMap grabIndex ls where
    grabIndex ((a,b), corr) = ((a,b), changeCorr corr b)
    keyList = newMap fst ls --make a temporary map so we can quickly grab the index of a given (x,y) pair
    keyMap = fromList (zip keyList [0..]) --map of (x,y) word pairs to index
    changeCorr [] _ = []
    changeCorr ((num,word):xs) match = (num, keyMap ! (match,word)):changeCorr xs match --find the index in the dictionary

processModel :: [((String,String), [(Int,Int)])] -> ProcessedModel --takes in the interprocessed model and removes the first element of each (x,y) pair to make this a hidden second order markov model
processModel xs = newMap remFirst xs where
    remFirst ((a,b), corr) = (b,corr)

primToProcess :: PrimitiveModel -> ProcessedModel --streamlines the primitive to processed model processing
primToProcess pm = processModel $ interProcessed $ getFreqs pm

newMap :: (a -> b) -> [a] -> [b] --included because map and filter conflicted with Data.Map's definition, identical to the prelude's definition
newMap _ [] = []
newMap f (x:xs) = f x : newMap f xs

newFilter :: (a -> Bool) -> [a] -> [a]
newFilter p [] = []
newFilter p (x:xs)
    | p x = x : newFilter p xs
    | otherwise = newFilter p xs
