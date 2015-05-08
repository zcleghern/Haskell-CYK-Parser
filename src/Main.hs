module Main where

import System.IO
import Control.Monad
import CFGrammar
import CYK

getLines = liftM lines . readFile

main :: IO ()
main = do
        putStrLn "CYK Parser \nEnter a text file name of a CFG in Chomsky Normal Form"
        filename <- getLine
        list <- getLines filename
        let g = buildGrammar list
        putStrLn "\n\nCFG G ="
        putStrLn $ show g
        option g
        
parseString :: CFG -> IO ()
parseString g = do
        putStrLn "Enter string to parse"
        w <- getLine
        putStrLn . show $ parse w g
        option g
        
option :: CFG -> IO ()
option g = do
        putStrLn "\n\nEnter 1 to check if a string is in the language, 2 to select a new grammar, 3 to quit"
        str <- getLine
        case str of
                "1" -> parseString g
                "2" -> main
                "3" -> return ()
                _ -> do
                        putStrLn "Invalid input."
                        option g       