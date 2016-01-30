all:
	ghc -O2 -o hsudoku Main.hs

clean:
	rm *.hi *.o hsudoku
