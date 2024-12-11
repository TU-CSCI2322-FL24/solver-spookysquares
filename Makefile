# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o spooky Main.hs

prof:
	ghc --make -prof -o spooky Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f spooky
	rm -f *.hi
	rm -f *.o

