{- Alec Snyder
- lab 1
- Sokal text generator
- suck.hs
-}
module Main where

import Prelude hiding (map,filter)
import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad
import Text.StringLike hiding (empty)
import Data.Char
import Data.Map.Lazy hiding (findIndex)
import Data.List hiding (insert)

type PrimitiveModel = Map (String,String) [String]

type ProcessedModel = [(String, [(Int,Int)])]

main :: IO ()
main = do
    contents <- readFile "urls.txt"
    putStrLn "Successfully read in url file"
    let ls = lines contents
    putStrLn "Connecting to servers and downloading content..."
    responses <- mapM grabResp ls
    putStrLn "Parsing in DOM..."
    let souped = newMap parseTags responses
    putStrLn "Filtering DOM..."
    let process = processWords $ filterSoup souped
    putStrLn "Generating Models"
    let tempy = generatePrim process empty
    let final = primToProcess tempy
    putStrLn $ show $ final !! 0
   

grabResp :: String -> IO String
grabResp str = do 
    makeRequest <- simpleHTTP (getRequest str)
    getResponseBody makeRequest

processWords :: [[String]] -> [String]
processWords ls = newFilter isWord (words (concat ((newMap (newFilter (/='\n')) (newMap concat ls)))))


isWord :: String -> Bool
isWord [] = False
isWord (x:xs)
    | isAlphaNum x = True
    | otherwise = isWord xs

extractP :: StringLike str => [Tag str] -> Bool -> [Tag str]
extractP [] _ = []
extractP (x:xs) True
    | isTagClose x && x ~== TagClose "p" = x : extractP xs False
    | otherwise = x : extractP xs True
extractP (x:xs ) False
    | isTagOpen x && x ~== TagOpen "p" [] = x : extractP xs True
    | otherwise = extractP xs False

filterSoup :: (StringLike str, Show str) => [[Tag str]] -> [[str]]
filterSoup ls = newMap ((newMap fromTagText) . (newFilter isTagText) . (flip extractP False)) ls

generatePrim :: [String] -> PrimitiveModel -> PrimitiveModel
generatePrim (x:y:[]) pm
    | member (x,y) pm = pm
    | otherwise = insert (x,y) [] pm
generatePrim (x:y:z:xs) pm
    | member (x,y) pm = generatePrim (y:z:xs) (adjust (++ [z]) (x,y) pm)
    | otherwise = generatePrim (y:z:xs) (insert (x,y) [z] pm)

newMap :: (a -> b) -> [a] -> [b]
newMap _ [] = []
newMap f (x:xs) = f x : newMap f xs

newFilter :: (a -> Bool) -> [a] -> [a]
newFilter p [] = []
newFilter p (x:xs)
    | p x = x : newFilter p xs
    | otherwise = newFilter p xs

getFreqs :: PrimitiveModel -> [((String,String), [(Int,String)])]
getFreqs pm = freqs (keys pm) where
    freqs [] = []
    freqs (x:xs) = (x,countOcc (pm ! x) (nub (pm ! x))) : freqs xs
    countOcc _ [] =[]
    countOcc orig (x:xs) = ((length (elemIndices x orig)),x) : countOcc orig xs

interProcessed :: [((String,String), [(Int,String)])] -> [((String,String), [(Int,Int)])]
interProcessed ls = newMap grabIndex ls where
    grabIndex ((a,b), corr) = ((a,b), changeCorr corr b)
    changeCorr [] _ = []
    changeCorr ((num,word):xs) match = case findIndex (findMatch (match,word)) ls of
        Just pos -> (num,pos):changeCorr xs match
        Nothing -> changeCorr xs match
    findMatch targ check = fst check == targ

processModel :: [((String,String), [(Int,Int)])] -> ProcessedModel
processModel xs = newMap remFirst xs where
    remFirst ((a,b), corr) = (b,corr)

primToProcess :: PrimitiveModel -> ProcessedModel
primToProcess pm = processModel $ interProcessed $ getFreqs pm
