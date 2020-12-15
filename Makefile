all: joc.exe

joc.exe: joc.hs
	ghc joc.hs

clean:
	rm *.hi *.o *.exe