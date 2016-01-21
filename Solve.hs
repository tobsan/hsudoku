module Solve where

{-
    Copyright (c) Tobias Olausson, 2015

    This file is part of hsudoku

    hsudoku is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hsudoku is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hsudoku.  If not, see <http://www.gnu.org/licenses/>.
 -}

import Data.Maybe
import Data.Char
import System.Environment

import Sudoku

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            sud <- readSudoku file
            let sud' = solve sud
            if isSolved sud' then putStrLn "Solved:"
                             else putStrLn "Could not solve:"
            putStr $ printBoard sud'
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
  where
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


-- Main solve function
solve :: Board -> Board
solve board | board == board'' = board''
            | otherwise        = solve board''
  where board'  = solveSingleCandidate board (getCandidates board)
        board'' = solveSinglePosition board' (getCandidates board')

-- Solves naked candidates (only one possible value)
solveSingleCandidate :: Board -> [Candidate] -> Board
solveSingleCandidate board []                       = board
solveSingleCandidate board (((row,col),Nothing):cs) = solveSingleCandidate board cs
solveSingleCandidate board (((row,col),Just c):cs)  = case c of
    []  -> error $ "empty candidate list is impossible at " ++ show (row,col) 
    [n] -> solveSingleCandidate (setCell board row col n) cs
    ns  -> solveSingleCandidate board cs

-- Solves single positions (only one cell where a value is possible)
solveSinglePosition :: Board -> [Candidate] -> Board
solveSinglePosition board cs =
    let board' = foldl checkSingle board $ map row [0..8]
        board'' = foldl checkSingle board' $ map col [0..8]
    in foldl checkSingle board'' $ map box [0..8]
  where
    row y = filter ((== y) . fst . fst) cs
    col x = filter ((== x) . snd . fst) cs
    box i = filter (isBox i . fst) cs
    isBox i yx = yx `elem` [ (y,x) | y <- take 3 $ [(i `div` 3) * 3 ..]
                                   , x <- take 3 $ [(i `mod` 3) * 3 ..] ]

    checkSingle board cs = checkSingle' board cs 8
    checkSingle' :: Board -> [Candidate] -> Int -> Board
    checkSingle' board cs i | i < 0     = board
                            | otherwise = 
        let board' = case filter (== i) (onlyCandidates cs) of
                [c] -> let ((y,x),_) = head $ dropWhile (maybe True (notElem c) . snd) cs
                       in setCell board y x c
                _   -> board
        in checkSingle' board' cs (i-1)

solveNakedPair :: Board -> [Candidate] -> Board
solveNakedPair board cs = undefined
  where
    row y = filter ((== y) . fst . fst) cs
    col x = filter ((== x) . snd . fst) cs

-- Solve by trial and error
solveTrialError :: Board -> [Candidate] -> Board
solveTrialError board cs = solve' board $ filter (isJust . snd) cs
  where
    solve' board (((y,x),cands):cs) = undefined

