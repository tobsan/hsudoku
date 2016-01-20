module Sudoku where

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

import Data.List
import Data.Maybe
import Data.Char

-- 9 of something
type Block = [Maybe Int]

-- Candidate numbers for a position
type Candidate = ((Int,Int), Maybe [Int])

-- Base enumeration is just rows
type Board = [Block]

{-
 - Enumeration of rows as follows
___________________
|________0_________|
|________1_________|
|________2_________|
|________3_________|
|________4_________|
|________5_________|
|________6_________|
|________7_________|
|________8_________|

 -}
getRow :: Board -> Int -> Block
getRow = (!!)

-- Utility to get at index
getCell :: Board -> Int -> Int -> Maybe Int
getCell board row col = (getRow board row) !! col

-- Update at index
setCell :: Board -> Int -> Int -> Int -> Board
setCell [] _ _ _ = []
setCell (b:bs) 0 x v = (take x b ++ (Just v : drop (x+1) b)) : bs
setCell (b:bs) y x v = b : setCell bs (y-1) x v

{-
 - Enumeration of cols as follows
__________________
| | | | | | | | | |
| | | | | | | | | |
| | | | | | | | | |
| | | | | | | | | |
|0|1|2|3|4|5|6|7|8|
| | | | | | | | | |
| | | | | | | | | |
| | | | | | | | | |
|_|_|_|_|_|_|_|_|_|

 -}
getCol :: Board -> Int -> Block
getCol = (!!) . transpose

{-
 - Enumeration of boxes as follows
__________________
|     |     |     |
|  0  |  1  |  2  |
|_____|_____|_____|
|     |     |     |
|  3  |  4  |  5  |
|_____|_____|_____|
|     |     |     |
|  6  |  7  |  8  |
|_____|_____|_____|

-}
getBox :: Board -> Int -> Block
getBox board ix = concatMap (\row -> take 3 $ drop startCol row) rows
  where startRow = (ix `div` 3) * 3
        startCol = (ix `mod` 3) * 3
        rows = map (getRow board) [startRow..startRow+2]

-- Is this board a valid Sudoku?
-- Each number may only appear once in every row, column and box
-- And the sudoku is of right size
--
-- TODO: Check validity of generated candidate list
--
isValid :: Board -> Bool
isValid board 
    | length board /= 9          = False
    | any ((/=9) . length) board = False
    | otherwise = and $ map (\i -> f getRow i && f getCol i && f getBox i) [0..8]
  where
    f g i = let x = (catMaybes $ g board i)
            in nub x == x

-- Is this board correct and fully solved?
isSolved :: Board -> Bool
isSolved board = isValid board && Nothing `notElem` (concat board)

-- Print a board, empty cells are periods
printBoard :: Board -> String
printBoard []                  = []
printBoard ([]:bs)             = '\n' : printBoard bs
printBoard ((Nothing:bs):bss) = '.' : printBoard (bs:bss)
printBoard ((Just b :bs):bss) = chr (b+49) : printBoard (bs:bss)

-- Generate a list of candidate numbers for each cell in the board
getCandidates :: Board -> [Candidate]
getCandidates b = concatMap (\y -> map (\x -> ((y,x),getCandidate b y x)) [0..8]) [0..8]

-- Calculates the candidate numbers for a given position
getCandidate :: Board -> Int -> Int -> Maybe [Int]
getCandidate board rx cx 
    | isJust $ getCell board rx cx = Nothing
    | otherwise                    = Just $ [0..8] \\ taken
  where row = getRow board rx
        col = getCol board cx
        box = getBox board $ ((rx `div` 3) * 3) + (cx `div` 3)
        taken = catMaybes $ map head $ group (row ++ col ++ box)

-- Extract only the numbers from a list of candidates.
onlyCandidates :: [Candidate] -> [Int]
onlyCandidates = concat . catMaybes . map snd

