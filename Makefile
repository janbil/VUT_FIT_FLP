all:
	cd src ; ghc --make -Wall -o ../flp22-fun Main.hs

clean:
	rm -f *.o *.hi flp22-fun