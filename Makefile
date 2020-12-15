all: game.exe

game.exe: game.hs
	ghc game.hs

clean:
	rm *.hi *.o *.exe