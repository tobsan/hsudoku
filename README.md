hsudoku is a sudoku solver written in Haskell.

When I studied functional programming at the university we got to write
a sudoku solver in Haskell by using backtracking. Now that is not especially
efficient, and it does not represent how humans think about sudokus.

Nowadays, I don't get to do any Haskell at work, so this is mostly just to
make sure I won't forget anything, and a neat way to solve sudokus by using
the same techniques humans do.

How to run
----------
You'll need a Haskell compiler and/or interpreter.

To run compiled:
```
$ make
$ ./hsudoku -f "test.sud"
```

To run interpreted:
```
$ ghci Main.hs
*Solve> sud <- readSudoku "test.sud"
*Solve> putStrLn $ printBoard $ solve sud

Example input
-------------
When using the -s parameter for inputing sudoku strings from command line, the
board should be formatted as 9 groups of 9 characters. A character can be
either a number of 1-9, or a . to indicate a "hole". Here is an example:

```
.7...6... 9......41 ..8..9.5. .9...7..2 ..3...8.. 4..8...1. .8.3..9.. 16......7
...5...8.
```

Corresponds to:
```
  7       6
9             4 1
    8     9   5
  9       7     2
    3       8
4     8       1
  8   3     9
1 6             7
      5       8

```

Related projects
----------------
Check out [sudoku_ocr](https://github.com/jonte/sudoku_ocr), which can do OCR
of sudoku images and output to a format readable by this solver.
