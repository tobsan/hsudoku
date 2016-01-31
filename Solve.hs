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
import Sudoku

-- Solve one step of a sudoku
solveStep :: Board -> Maybe Board
solveStep board | board /= board'  = Just board'
                | board /= board'' = Just board''
                | otherwise        = Nothing
  where board' = solveSingleCandidate board (getCandidates board)
        board'' = solveSinglePosition board' (getCandidates board')
        -- Add more here. TODO: Better solution for this
        -- TODO: Do this with some fold-ish thing?

solve :: Board -> Board
solve board = case solveStep board of
    Nothing     -> board
    Just board' -> solve board'

-- Solves naked candidates (only one possible value)
solveSingleCandidate :: Board -> [Candidate] -> Board
solveSingleCandidate board []                       = board
solveSingleCandidate board (((_,_),Nothing):cs) = solveSingleCandidate board cs
solveSingleCandidate board (((row,col),Just c):cs)  = case c of
    []  -> errorEmptyCandidate row col
    [n] -> solveSingleCandidate (setCell board row col n) cs
    _   -> solveSingleCandidate board cs

-- Solves single positions (only one cell where a value is possible)
solveSinglePosition :: Board -> [Candidate] -> Board
solveSinglePosition b cs =
    let b' = foldl checkSingle b $ map (candRow $ getCandidates b) [0..8]
        b'' = foldl checkSingle b' $ map (candCol $ getCandidates b') [0..8]
    in foldl checkSingle b'' $ map (candBox $ getCandidates b'') [0..8]
  where
    checkSingle board cs = checkSingle' board cs 8
    checkSingle' :: Board -> [Candidate] -> Int -> Board
    checkSingle' board cs i =
        let board' = case filter (== i) (onlyCandidates cs) of
                [c] -> let ((y,x),_) = head $ dropWhile (maybe True (notElem c) . snd) cs
                       in setCell board y x c
                _   -> board
        in if i == 0 then board' else checkSingle' board' cs (i-1)

-- Naked pairs can be used to remove other candidates. As such, it does not
-- alter the board, but rather the list of candidates.
solveNakedPair :: [Candidate] -> [Candidate]
solveNakedPair cs = foldl checkPair cs allPairs
  where
    allPairs = [ (y,x) | y <- [0..8], x <- [0..8] ]
    checkPair :: [Candidate] -> (Int, Int) -> [Candidate]
    checkPair cands (y,x) = undefined
--        let cands' = checkPair' cands (y,x) $ map row

-- Solve by trial and error
solveTrialError :: Board -> [Candidate] -> Board
solveTrialError board cs = solve' board $ filter (isJust . snd) cs
  where
    solve' board []                 = board
    solve' board (((y,x),cands):cs) = case cands of
        Nothing          -> solve' board cs
        Just []          -> board -- errorEmptyCandidate y x
        Just (cand:more) ->
            let board' = setCell board y x cand
                board'' = solveTrialError board' $ getCandidates board'
            in case isValid board'' of
                True  -> board''
                False -> if null more
                         then board
                         else solve' board (((x,y),Just more):cs)

-- Convenience function for whenever a candidate list is empty, which
-- should be an impossible state
errorEmptyCandidate :: Int -> Int -> a
errorEmptyCandidate y x = error $ "empty candidate list is impossible at "
                       ++ show (y,x)
