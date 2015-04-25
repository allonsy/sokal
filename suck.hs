{- Alec Snyder
- lab 1
- Sokal text generator
- suck.hs
-}
module Main where

import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad

main :: IO ()
main = do
    contents <- readFile "urls.txt"
    putStrLn "Successfully read in url file"
    let ls = lines contents
    putStrLn "Connecting to servers and downloading content..."
    responses <- mapM grabResp ls
    putStrLn "Parsing in DOM..."
    let souped = map parseTags responses
    putStrLn "Done"
    --putStrLn $ show (souped !! 0)

grabResp :: String -> IO String
grabResp str = do 
    makeRequest <- simpleHTTP (getRequest str)
    getResponseBody makeRequest
