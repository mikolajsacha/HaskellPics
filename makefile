make:
	ghc -O -threaded -rtsopts --make hspics.hs
clean:
	rm *.hi *.o
