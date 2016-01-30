module Main where

import System.Environment
import Data.Char

import Solve
import Sudoku

-- Main function
-- usage: hsudoku -f <file.sud> | -s 81 tiles
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-f", file] -> do
            sud <- readSudoku file
            let sud' = solve sud
            if isSolved sud' then putStrLn "Solved:"
                             else putStrLn "Could not solve:"
            putStr $ printBoard sud'
        ("-s":rows) -> do
            let sud = map (map char2board) rows
            putStr $ printBoard sud
        _      -> usage
  where
    usage = do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <sudoku file>"

-- Read a sudoku file from disk
-- Expected format is 9 lines with 9 chars on each line
-- Empty spaces are denoted with periods (.).
readSudoku :: FilePath -> IO Board
readSudoku f = readFile f >>= return . map (map char2board) . lines

char2board :: Char -> Maybe Int
char2board c | c == '.'   = Nothing
             | ord c < 49 = error "bad char value"
             | ord c > 57 = error "bad char value"
             | otherwise  = Just $ ord c - 49

-- Just run all the sudokus
testBench :: IO ()
testBench = do
    easy <- mapM (\f -> readSudoku f >>= return . isSolved . solve) easyPaths
    putStrLn $ "Easy sudokus: " ++ (show $ passed easy) ++ "/" ++ (show $ length easy) ++ " passed"
    hard <- mapM (\f -> readSudoku f >>= return . isSolved . solve) hardPaths
    putStrLn $ "Hard sudokus: " ++ (show $ passed hard) ++ "/" ++ (show $ length hard) ++ " passed"
  where easyPaths = [ "sudokus/easy" ++ (show i) ++ ".sud" | i <- [1..50]]
        hardPaths = [ "sudokus/hard" ++ (show i) ++ ".sud" | i <- [1..95]]
        passed = length . filter (==True)

